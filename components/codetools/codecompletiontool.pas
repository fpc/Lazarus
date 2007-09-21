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
    TCodeCompletionCodeTool enhances TMethodJumpingCodeTool.
    
    Code Completion is
      - complete properties
          - complete property statements
          - add private variables and private access methods
      - add missing method bodies
          - add useful statements
      - add missing forward proc bodies
      - add missing semicolons at end of procedures
      - complete event assignments
      - complete local variables
      - complete local variables as parameter
      - insert header comment for classes

  ToDo:
    -add code for index properties (TList, TFPList, array of, Pointer array)
      TList:
        property Items[Index: integer]: AType;
        -> creates via dialog
          property Items[Index: integer]: Type2 read GetItems write SetItems;
          private FItems: TList;
          private function GetItems(Index: integer): Type2;
            begin
              Result:=Type2(FItems[Index]);
            end;
          private procedure SetItems(Index: integer; const AValue: Type2);
            begin
              FItems[Index]:=Type2;
            end;
          public constructor Create;
            begin
              FItems:=TList.Create;
            end;
          public destructor Destroy; override;
            begin
              FItems.Free;
              inherited Destroy;
            end;

    -ProcExists: search procs in ancestors too
    -VarExists: search vars in ancestors too
}
unit CodeCompletionTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStrConsts, CodeTree, CodeAtom,
  CustomCodeTool, PascalParserTool, MethodJumpTool, FindDeclarationTool,
  KeywordFuncLists, CodeToolsStructs, BasicCodeTools, LinkScanner,
  SourceChanger, CodeGraph, AVL_Tree;

type
  TNewClassPart = (ncpPrivateProcs, ncpPrivateVars,
                   ncpProtectedProcs, ncpProtectedVars,
                   ncpPublicProcs, ncpPublicVars,
                   ncpPublishedProcs, ncpPublishedVars);
                   
  TNewVarLocation = (
    ncpvPrivate,ncpvProtected,ncpvPublic,ncpvPublished,ncpvLocal
    );
                   
const
  NewClassPartVisibilty: array[TNewClassPart] of TPascalClassSection = (
    pcsPrivate, pcsPrivate,
    pcsProtected, pcsProtected,
    pcsPublic, pcsPublic,
    pcsPublished, pcsPublished
    );
  
type
  TCodeCompletionCodeTool = class;

  TOnGetNewVariableLocation = function(Tool: TCodeCompletionCodeTool;
                           const VariableName: string; var VariableType: string;
                           IsMethod: boolean; NewLocation: TNewVarLocation
                           ): boolean;
                           
  { TCodeCompletionCodeTool }

  TCodeCompletionCodeTool = class(TMethodJumpingCodeTool)
  private
    ASourceChangeCache: TSourceChangeCache;
    FCodeCompleteClassNode: TCodeTreeNode; // the class that is to be completed
    FCompletingStartNode: TCodeTreeNode; // the first variable/method/GUID node in FCodeCompleteClassNode
    FAddInheritedCodeToOverrideMethod: boolean;
    FCompleteProperties: boolean;
    FirstInsert: TCodeTreeNodeExtension; // list of insert requests
    FOnGetNewVariableLocation: TOnGetNewVariableLocation;
    FSetPropertyVariablename: string;
    JumpToProcName: string;
    NewClassSectionIndent: array[TPascalClassSection] of integer;
    NewClassSectionInsertPos: array[TPascalClassSection] of integer;
    fFullTopLvlName: string;// used by OnTopLvlIdentifierFound
    fNewMainUsesSectionUnits: TAVLTree; // tree of PChar
    procedure AddNewPropertyAccessMethodsToClassProcs(ClassProcs: TAVLTree;
        const TheClassName: string);
    procedure CheckForOverrideAndAddInheritedCode(ClassProcs: TAVLTree);
    function CompleteProperty(PropNode: TCodeTreeNode): boolean;
    procedure SetCodeCompleteClassNode(const AClassNode: TCodeTreeNode);
    procedure SetCodeCompleteSrcChgCache(const AValue: TSourceChangeCache);
    function OnTopLvlIdentifierFound(Params: TFindDeclarationParams;
        const FoundContext: TFindContext): TIdentifierFoundResult;
  protected
    procedure FreeClassInsertionList;
    procedure InsertNewClassParts(PartType: TNewClassPart);
    function InsertAllNewClassParts: boolean;
    function InsertClassHeaderComment: boolean;
    function InsertMissingClassSemicolons: boolean;
    function InsertAllNewUnitsToMainUsesSection: boolean;
    function CreateMissingProcBodies: boolean;
    function NodeExtIsVariable(ANodeExt: TCodeTreeNodeExtension): boolean;
    function NodeExtHasVisibilty(ANodeExt: TCodeTreeNodeExtension;
      Visibility: TPascalClassSection): boolean;
    procedure FindInsertPositionForForwardProc(
           SourceChangeCache: TSourceChangeCache;
           ProcNode: TCodeTreeNode; var Indent, InsertPos: integer);
    procedure FindInsertPositionForProcInterface(var Indent, InsertPos: integer;
           SourceChangeCache: TSourceChangeCache);
    function CheckLocalVarAssignmentSyntax(CleanCursorPos: integer;
           var VarNameAtom,AssignmentOperator,TermAtom: TAtomPosition): boolean;
    function AddLocalVariable(CleanCursorPos: integer; OldTopLine: integer;
                          VariableName, VariableType: string;
                          var NewPos: TCodeXYPosition; var NewTopLine: integer;
                          SourceChangeCache: TSourceChangeCache): boolean;
    procedure AdjustCursor(OldCodePos: TCodePosition; OldTopLine: integer;
                          var NewPos: TCodeXYPosition; var NewTopLine: integer);
    function AddVariable(CursorNode: TCodeTreeNode;
                      CleanCursorPos,OldTopLine: integer;
                      const VariableName, NewType: string;
                      out NewPos: TCodeXYPosition; out NewTopLine: integer;
                      SourceChangeCache: TSourceChangeCache): boolean;
    procedure AddNeededUnitToMainUsesSection(AnUnitName: PChar);
    function CompleteLocalVariableAssignment(CleanCursorPos,
                       OldTopLine: integer; CursorNode: TCodeTreeNode;
                       var NewPos: TCodeXYPosition; var NewTopLine: integer;
                       SourceChangeCache: TSourceChangeCache): boolean;
    function CompleteLocalVariableAsParameter(CleanCursorPos,
                       OldTopLine: integer; CursorNode: TCodeTreeNode;
                       var NewPos: TCodeXYPosition; var NewTopLine: integer;
                       SourceChangeCache: TSourceChangeCache): boolean;
    function CompleteMethod(CleanCursorPos, OldTopLine: integer;
                           CursorNode: TCodeTreeNode;
                           var NewPos: TCodeXYPosition; var NewTopLine: integer;
                           SourceChangeCache: TSourceChangeCache): boolean;
  protected
    property CodeCompleteClassNode: TCodeTreeNode
                     read FCodeCompleteClassNode write SetCodeCompleteClassNode;
    property CodeCompleteSrcChgCache: TSourceChangeCache
                       read ASourceChangeCache write SetCodeCompleteSrcChgCache;
  public
    constructor Create;
    function CompleteCode(CursorPos: TCodeXYPosition; OldTopLine: integer;
                          out NewPos: TCodeXYPosition; out NewTopLine: integer;
                          SourceChangeCache: TSourceChangeCache): boolean;
    function AddPublishedVariable(const UpperClassName,VarName, VarType: string;
                      SourceChangeCache: TSourceChangeCache): boolean; override;
    function GetRedefinitionNodeText(Node: TCodeTreeNode): string;
    function FindRedefinitions(out TreeOfCodeTreeNodeExt: TAVLTree;
                        WithEnums: boolean): boolean;
    function RemoveRedefinitions(TreeOfCodeTreeNodeExt: TAVLTree;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FindAliasDefinitions(out TreeOfCodeTreeNodeExt: TAVLTree;
                                  OnlyWrongType: boolean): boolean;
    function FixAliasDefinitions(TreeOfCodeTreeNodeExt: TAVLTree;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FindConstFunctions(out TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function ReplaceConstFunctions(TreeOfCodeTreeNodeExt: TAVLTree;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FindTypeCastFunctions(out TreeOfCodeTreeNodeExt: TAVLTree): boolean;
    function ReplaceTypeCastFunctions(TreeOfCodeTreeNodeExt: TAVLTree;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function MovePointerTypesToTargetSections(
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FixForwardDefinitions(SourceChangeCache: TSourceChangeCache
                                   ): boolean;
    function GatherUnitDefinitions(out TreeOfCodeTreeNodeExt: TAVLTree;
                      OnlyInterface, ExceptionOnRedefinition: boolean): boolean;
    function BuildUnitDefinitionGraph(
                        out DefinitionsTreeOfCodeTreeNodeExt: TAVLTree;
                        out Graph: TCodeGraph; OnlyInterface: boolean): boolean;
    procedure WriteCodeGraphDebugReport(Graph: TCodeGraph);

    // custom class completion
    function InitClassCompletion(const UpperClassName: string;
                                 SourceChangeCache: TSourceChangeCache): boolean;
    function ApplyClassCompletion(AddMissingProcBodies: boolean): boolean;
    function ProcExistsInCodeCompleteClass(
                                    const NameAndParamsUpCase: string): boolean;
    function VarExistsInCodeCompleteClass(const UpperName: string): boolean;
    procedure AddClassInsertion(
        const CleanDef, Def, IdentifierName: string;
        TheType: TNewClassPart; PosNode: TCodeTreeNode = nil;
        const Body: string = '');

    property SetPropertyVariablename: string read FSetPropertyVariablename
                                             write FSetPropertyVariablename;
    property CompleteProperties: boolean read FCompleteProperties
                                         write FCompleteProperties;
    property AddInheritedCodeToOverrideMethod: boolean
                                        read FAddInheritedCodeToOverrideMethod
                                        write FAddInheritedCodeToOverrideMethod;
    property OnGetNewVariableLocation: TOnGetNewVariableLocation
                 read FOnGetNewVariableLocation write FOnGetNewVariableLocation;
  end;

  
implementation

type
  TNodeMoveEdge = class
  public
    GraphNode: TCodeGraphNode;
    DestPos: integer;
    TologicalLevel: integer;
    SrcPos: integer;
  end;
  
function CompareNodeMoveEdges(NodeMove1, NodeMove2: Pointer): integer;
var
  Node1: TNodeMoveEdge;
  Node2: TNodeMoveEdge;
begin
  Node1:=TNodeMoveEdge(NodeMove1);
  Node2:=TNodeMoveEdge(NodeMove2);
  if Node1.DestPos>Node2.DestPos then
    Result:=1
  else if Node1.DestPos<Node2.DestPos then
    Result:=-1
  else if Node1.TologicalLevel>Node2.TologicalLevel then
    Result:=1
  else if Node1.TologicalLevel<Node2.TologicalLevel then
    Result:=-1
  else if Node1.SrcPos>Node2.SrcPos then
    Result:=1
  else if Node1.SrcPos<Node2.SrcPos then
    Result:=-1
  else
    Result:=0;
end;


{ TCodeCompletionCodeTool }

function TCodeCompletionCodeTool.ProcExistsInCodeCompleteClass(
  const NameAndParamsUpCase: string): boolean;
// NameAndParams should be uppercase and contains the proc name and the
// parameter list without names and default values
// and should not contain any comments and no result type
var ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  // search in new nodes, which will be inserted
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if CompareTextIgnoringSpace(ANodeExt.Txt,NameAndParamsUpCase,true)=0 then begin
      Result:=true;
      exit;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
  if not Result then begin
    // ToDo: check ancestor procs too
    // search in current class
    Result:=(FindProcNode(FCompletingStartNode,NameAndParamsUpCase,[phpInUpperCase])<>nil);
  end;
end;

procedure TCodeCompletionCodeTool.SetCodeCompleteClassNode(
  const AClassNode: TCodeTreeNode);
begin
  FreeClassInsertionList;
  FCodeCompleteClassNode:=AClassNode;
  BuildSubTreeForClass(FCodeCompleteClassNode);
  FCompletingStartNode:=FCodeCompleteClassNode.FirstChild;
  while (FCompletingStartNode<>nil) and (FCompletingStartNode.FirstChild=nil) do
    FCompletingStartNode:=FCompletingStartNode.NextBrother;
  if FCompletingStartNode<>nil then
    FCompletingStartNode:=FCompletingStartNode.FirstChild;
  JumpToProcName:='';
end;

procedure TCodeCompletionCodeTool.SetCodeCompleteSrcChgCache(
  const AValue: TSourceChangeCache);
begin
  ASourceChangeCache:=AValue;
  ASourceChangeCache.MainScanner:=Scanner;
end;

function TCodeCompletionCodeTool.OnTopLvlIdentifierFound(
  Params: TFindDeclarationParams; const FoundContext: TFindContext
  ): TIdentifierFoundResult;
var
  TrimmedIdentifier: string;
begin
  if not (fdfTopLvlResolving in Params.Flags) then exit;
  with Params do begin
    case NewNode.Desc of
    ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition,ctnGenericType:
      TrimmedIdentifier:=NewCodeTool.ExtractDefinitionName(NewNode);
    ctnProperty:
      TrimmedIdentifier:=NewCodeTool.ExtractPropName(NewNode,false);
    else
      TrimmedIdentifier:=GetIdentifier(Params.Identifier);
    end;
  end;
  fFullTopLvlName:=fFullTopLvlName+TrimmedIdentifier;
  Result:=ifrSuccess;
end;

function TCodeCompletionCodeTool.VarExistsInCodeCompleteClass(
  const UpperName: string): boolean;
var ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  // search in new nodes, which will be inserted
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if CompareTextIgnoringSpace(ANodeExt.Txt,UpperName,true)=0 then begin
      Result:=true;
      exit;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
  if not Result then begin
    // ToDo: check ancestor vars too
    // search in current class
    Result:=(FindVarNode(FCompletingStartNode,UpperName)<>nil);
  end;
end;

procedure TCodeCompletionCodeTool.AddClassInsertion(
  const CleanDef, Def, IdentifierName: string; TheType: TNewClassPart;
  PosNode: TCodeTreeNode; const Body: string);
{ add an insert request entry to the list of insertions
  For example: a request to insert a new variable or a new method to the class

  CleanDef:  The sceleton of the new insertion. e.g. the variablename or the
             method header without parameter names.
  Def:       The insertion code.
  IdentifierName: e.g. the variablename or the method name
  TheType:   see TNewClassPart
  PosNode:   optional. The node, to which the request belongs. e.g. the
             property node, if the insert is the auto created private variable.
  Body:      optional. Normally a method body is auto created. This overrides
             the body code.

}
var NewInsert, InsertPos, LastInsertPos: TCodeTreeNodeExtension;
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeCompletionCodeTool.AddClassInsertion] CleanDef="',CleanDef,'" Def="',Def,'" Identifiername="',Identifiername,'" Body="',Body,'"');
  {$ENDIF}
  NewInsert:=NodeExtMemManager.NewNode;
  with NewInsert do begin
    Node:=PosNode;
    Txt:=CleanDef;
    ExtTxt1:=Def;
    ExtTxt2:=IdentifierName;
    ExtTxt3:=Body;
    Flags:=ord(TheType);
  end;
  if FirstInsert=nil then begin
    FirstInsert:=NewInsert;
    exit;
  end;
  if ASourceChangeCache.BeautifyCodeOptions.ClassPartInsertPolicy=cpipLast then
  begin
    // add as last to inserts
    InsertPos:=FirstInsert;
    while (InsertPos.Next<>nil) do
      InsertPos:=InsertPos.Next;
    InsertPos.Next:=NewInsert;
  end else begin
    // insert alphabetically
    InsertPos:=FirstInsert;
    LastInsertPos:=nil;
    //DebugLn('GGG "',InsertPos.Txt,'" "',CleanDef,'" ',CompareTextIgnoringSpace(InsertPos.Txt,CleanDef,false));
    while (InsertPos<>nil)
    and (CompareTextIgnoringSpace(InsertPos.Txt,CleanDef,false)>=0) do begin
      LastInsertPos:=InsertPos;
      InsertPos:=InsertPos.Next;
    end;
    if LastInsertPos<>nil then begin
      // insert after LastInsertPos
      NewInsert.Next:=LastInsertPos.Next;
      LastInsertPos.Next:=NewInsert;
    end else begin
      // insert as first
      NewInsert.Next:=InsertPos;
      FirstInsert:=NewInsert;
    end;
    {InsertPos:=FirstInsert;
    while InsertPos<>nil do begin
      DebugLn(' HHH ',InsertPos.Txt);
      InsertPos:=InsertPos.Next;
    end;}
  end;
end;

procedure TCodeCompletionCodeTool.FreeClassInsertionList;
// dispose all new variables/procs definitions
var ANodeExt: TCodeTreeNodeExtension;
begin
  while FirstInsert<>nil do begin
    ANodeExt:=FirstInsert;
    FirstInsert:=FirstInsert.Next;
    NodeExtMemManager.DisposeNode(ANodeExt);
  end;
  FreeAndNil(fNewMainUsesSectionUnits);
end;

function TCodeCompletionCodeTool.NodeExtIsVariable(
  ANodeExt: TCodeTreeNodeExtension): boolean;
begin
  Result:=(ANodeExt.Flags=ord(ncpPrivateVars))
       or (ANodeExt.Flags=ord(ncpPublishedVars));
end;

function TCodeCompletionCodeTool.NodeExtHasVisibilty(
  ANodeExt: TCodeTreeNodeExtension; Visibility: TPascalClassSection): boolean;
begin
  case Visibility of
  pcsPrivate:
    Result:=(ANodeExt.Flags=ord(ncpPrivateVars))
         or (ANodeExt.Flags=ord(ncpPrivateProcs));
  pcsProtected:
    Result:=(ANodeExt.Flags=ord(ncpProtectedVars))
         or (ANodeExt.Flags=ord(ncpProtectedProcs));
  pcsPublic:
    Result:=(ANodeExt.Flags=ord(ncpPublicVars))
         or (ANodeExt.Flags=ord(ncpPublicProcs));
  pcsPublished:
    Result:=(ANodeExt.Flags=ord(ncpPublishedVars))
         or (ANodeExt.Flags=ord(ncpPublishedProcs));
  else
    Result:=false;
  end;
end;

procedure TCodeCompletionCodeTool.FindInsertPositionForForwardProc(
  SourceChangeCache: TSourceChangeCache; ProcNode: TCodeTreeNode; var Indent,
  InsertPos: integer);

  procedure SetIndentAndInsertPos(Node: TCodeTreeNode; Behind: boolean);
  begin
    Indent:=GetLineIndent(Src,Node.StartPos);
    if Behind then
      InsertPos:=FindLineEndOrCodeAfterPosition(Node.EndPos)
    else
      InsertPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
  end;

var
  NearestProcNode, StartSearchProc: TCodeTreeNode;
  IsInInterface: boolean;
  ProcBodyNodes, ForwardProcNodes: TAVLTree;
  ProcAVLNode, NearestAVLNode: TAVLTreeNode;
  ProcNodeExt, NearestNodeExt: TCodeTreeNodeExtension;
  InsertBehind: boolean;
  NearestAVLNodeInFront: TAVLTreeNode;
  NearestAVLNodeBehind: TAVLTreeNode;
  ProcPosInFront: Integer;
  ProcPosBehind: Integer;
  EmptyLinesInFront: Integer;
  EmptyLinesBehind: Integer;
begin
  IsInInterface:=ProcNode.HasParentOfType(ctnInterface);
  if IsInInterface then begin
    // forward proc in interface
    StartSearchProc:=FindImplementationNode;
    if StartSearchProc=nil then
      RaiseException('Implementation section not found');
    if StartSearchProc.FirstChild<>nil then begin
      // implementation not empty
      StartSearchProc:=StartSearchProc.FirstChild
    end else begin
      // implementation is empty
      // -> add it as first body
      Indent:=GetLineIndent(Src,StartSearchProc.StartPos);
      InsertPos:=StartSearchProc.StartPos+length('implementation');
      exit;
    end;
  end else begin
    // forward proc in code
    // start searching for bodies behind proc
    StartSearchProc:=ProcNode.NextBrother;
    if StartSearchProc=nil then begin
      // There are no nodes behind
      // -> insert code directly behind
      SetIndentAndInsertPos(ProcNode,true);
      exit;
    end;
  end;

  if SourceChangeCache.BeautifyCodeOptions.KeepForwardProcOrder then begin
    // KeepForwardProcOrder: gather all procs and try to insert the new body
    //  in the same order of other forward proc definitions.
    ForwardProcNodes:=nil;
    ProcAVLNode:=nil;
    ProcBodyNodes:=nil;
    ProcNodeExt:=nil;
    
    try
      // gather all forward procs definitions on the same level
      ForwardProcNodes:=GatherProcNodes(ProcNode.Parent.FirstChild,
                 [phpInUpperCase,phpIgnoreProcsWithBody,phpIgnoreMethods],'');

      // gather all proc bodies
      ProcBodyNodes:=GatherProcNodes(StartSearchProc,
                     [phpInUpperCase,phpIgnoreForwards,phpIgnoreMethods],'');
                     
      // remove current forward proc from tree
      ProcAVLNode:=FindAVLNodeWithNode(ForwardProcNodes,ProcNode);
      if ProcAVLNode=nil then
        RaiseException('TCodeCompletionCodeTool.FindInsertPositionForForwardProc '
         +' Internal Error, current forward proc not found');
      ProcNodeExt:=TCodeTreeNodeExtension(ProcAVLNode.Data);
      ForwardProcNodes.Delete(ProcAVLNode);

      // remove all forward procs without bodies
      IntersectProcNodes(ForwardProcNodes,ProcBodyNodes,true);
      
      // sort forward proc definitions with source position
      ForwardProcNodes.OnCompare:=@CompareCodeTreeNodeExtWithNodeStartPos;
      
      // For debugging:
      {ProcAVLNode:=ForwardProcNodes.FindLowest;
      while ProcAVLNode<>nil do begin
        NearestProcNode:=TCodeTreeNodeExtension(ProcAVLNode.Data).Node;
        DebugLn('FindInsertPositionForForwardProc B ',NearestProcNode.StartPos,' "',copy(Src,NearestProcNode.StartPos,20),'"');
        ProcAVLNode:=ForwardProcNodes.FindSuccessor(ProcAVLNode);
      end;}

      // find nearest forward procs (distance measured in chars)
      NearestAVLNode:=ForwardProcNodes.FindNearest(ProcNodeExt);
      if NearestAVLNode<>nil then begin
      
        //DebugLn('FindInsertPositionForForwardProc Nearest ',TCodeTreeNodeExtension(NearestAVLNode.Data).Node.StartPos,' ',ProcNode.StartPos);

        // find nearest forward procs in front and after
        if TCodeTreeNodeExtension(NearestAVLNode.Data).Node.StartPos
          <ProcNode.StartPos
        then begin
          NearestAVLNodeInFront:=NearestAVLNode;
          NearestAVLNodeBehind:=ForwardProcNodes.FindPrecessor(NearestAVLNode);
        end else begin
          NearestAVLNodeInFront:=ForwardProcNodes.FindSuccessor(NearestAVLNode);
          NearestAVLNodeBehind:=NearestAVLNode;
        end;
        
        // choose the nearest of both (distance measured in emtpy lines,
        // this way blocks of procs are kept)
        if (NearestAVLNodeInFront<>nil) and (NearestAVLNodeBehind<>nil) then
        begin
          ProcPosInFront:=
               TCodeTreeNodeExtension(NearestAVLNodeInFront.Data).Node.StartPos;
          ProcPosBehind:=
               TCodeTreeNodeExtension(NearestAVLNodeBehind.Data).Node.StartPos;
          EmptyLinesInFront:=EmptyCodeLineCount(Src,
                       ProcPosInFront,ProcNode.StartPos,Scanner.NestedComments);
          EmptyLinesBehind:=EmptyCodeLineCount(Src,
                        ProcNode.StartPos,ProcPosBehind,Scanner.NestedComments);
          //DebugLn('FindInsertPositionForForwardProc Nearest InFront or After: EmptyLinesInFront=',EmptyLinesInFront,' EmptyLinesBehind=',EmptyLinesBehind);
          if EmptyLinesInFront<EmptyLinesBehind then
            NearestAVLNode:=NearestAVLNodeInFront
          else
            NearestAVLNode:=NearestAVLNodeBehind;
        end;
        
        NearestNodeExt:=TCodeTreeNodeExtension(NearestAVLNode.Data);
        NearestProcNode:=NearestNodeExt.Node;
        
        //DebugLn('FindInsertPositionForForwardProc C ',NearestProcNode.StartPos,' "',copy(Src,NearestProcNode.StartPos,20),'"');
        InsertBehind:=NearestProcNode.StartPos<ProcNode.StartPos;

        // the corresponding body was linked by IntersectProcNodes in Data
        NearestAVLNode:=TAVLTreeNode(NearestNodeExt.Data);
        NearestNodeExt:=TCodeTreeNodeExtension(NearestAVLNode.Data);
        NearestProcNode:=NearestNodeExt.Node;
        SetIndentAndInsertPos(NearestProcNode,InsertBehind);
        exit;
      end;
      
    finally
      // clean up
      ProcNodeExt.Free;
      if ProcBodyNodes<>nil then begin
        ProcBodyNodes.FreeAndClear;
        ProcBodyNodes.Free;
      end;
      if ForwardProcNodes<>nil then begin
        ForwardProcNodes.FreeAndClear;
        ForwardProcNodes.Free;
      end;
    end;
  end;
  
  if SourceChangeCache.BeautifyCodeOptions.ForwardProcBodyInsertPolicy
    = fpipInFrontOfMethods
  then begin
    // Try to insert new proc in front of existing methods
    
    // find first method
    NearestProcNode:=StartSearchProc;
    while (NearestProcNode<>nil) and (not NodeIsMethodBody(NearestProcNode)) do
      NearestProcNode:=NearestProcNode.NextBrother;
    if NearestProcNode<>nil then begin
      // the comments in front of the first method probably belong to the class
      // Therefore insert behind the node in front of the first method
      if NearestProcNode.PriorBrother<>nil then
        SetIndentAndInsertPos(NearestProcNode.PriorBrother,true)
      else begin
        Indent:=GetLineIndent(Src,NearestProcNode.StartPos);
        InsertPos:=NearestProcNode.Parent.StartPos;
        while (InsertPos<=NearestProcNode.StartPos)
        and (not IsSpaceChar[Src[InsertPos]]) do
          inc(InsertPos);
      end;
      exit;
    end;
  end else if SourceChangeCache.BeautifyCodeOptions.ForwardProcBodyInsertPolicy
    = fpipBehindMethods
  then begin
    // Try to insert new proc behind existing methods

    // find last method (go to last brother and search backwards)
    NearestProcNode:=StartSearchProc;
    while (NearestProcNode.NextBrother<>nil) do
      NearestProcNode:=NearestProcNode.NextBrother;
    while (NearestProcNode<>nil) and (not NodeIsMethodBody(NearestProcNode)) do
      NearestProcNode:=NearestProcNode.PriorBrother;
    if NearestProcNode<>nil then begin
      SetIndentAndInsertPos(NearestProcNode,true);
      exit;
    end;
  end;
  
  // Default position: Insert behind last node
  NearestProcNode:=StartSearchProc;
  while (NearestProcNode.NextBrother<>nil) do
    NearestProcNode:=NearestProcNode.NextBrother;
  if NearestProcNode<>nil then begin
    SetIndentAndInsertPos(NearestProcNode,true);
    exit;
  end;

  RaiseException('TCodeCompletionCodeTool.FindInsertPositionForForwardProc '
   +' Internal Error: no insert position found');
end;

procedure TCodeCompletionCodeTool.FindInsertPositionForProcInterface(
  var Indent, InsertPos: integer; SourceChangeCache: TSourceChangeCache);
var
  InsertNode: TCodeTreeNode;
begin
  InsertNode:=FindInterfaceNode;
  if InsertNode<>nil then begin
    // there is an interface
    // -> append at end of interface
    InsertPos:=FindLineEndOrCodeInFrontOfPosition(InsertNode.EndPos,true);
    Indent:=GetLineIndent(Src,InsertNode.EndPos);
  end;
  if InsertPos<1 then begin
    // there is no interface
    // -> insert in front of any proc
    InsertNode:=FindFirstSectionChild;
    while (InsertNode<>nil) and (InsertNode.Desc<>ctnProcedure) do
      InsertNode:=InsertNode.NextBrother;
    if InsertNode<>nil then begin
      InsertPos:=FindLineEndOrCodeInFrontOfPosition(InsertNode.StartPos,true);
      Indent:=GetLineIndent(Src,InsertPos);
    end;
  end;
  if InsertPos<1 then begin
    InsertNode:=FindFirstSectionChild;
    if InsertNode<>nil then begin
      Indent:=GetLineIndent(Src,InsertNode.StartPos);
      if InsertNode.Desc=ctnUsesSection then
        // insert behind uses section
        InsertPos:=FindLineEndOrCodeAfterPosition(InsertNode.EndPos)
      else
        // insert as first
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(InsertNode.StartPos);
    end else begin
      // insert in interface or somewhere at start
      InsertNode:=Tree.Root;
      InsertPos:=FindLineEndOrCodeInFrontOfPosition(InsertNode.EndPos,true);
      Indent:=GetLineIndent(Src,InsertNode.EndPos);
    end;
  end;
end;

function TCodeCompletionCodeTool.CheckLocalVarAssignmentSyntax(
  CleanCursorPos: integer; var VarNameAtom, AssignmentOperator,
  TermAtom: TAtomPosition): boolean;
// check for VarName:=Term
begin
  Result:=false;
  MoveCursorToCleanPos(CleanCursorPos);
  
  // find variable name
  GetIdentStartEndAtPosition(Src,CleanCursorPos,
    VarNameAtom.StartPos,VarNameAtom.EndPos);
  //debugln('TCodeCompletionCodeTool.CheckLocalVarAssignmentSyntax A ',GetAtom(VarNameAtom),' "',copy(Src,CleanCursorPos,10),'"');
  if VarNameAtom.StartPos=VarNameAtom.EndPos then exit;
  MoveCursorToAtomPos(VarNameAtom);
  if AtomIsKeyWord then exit;
  
  // find assignment operator
  ReadNextAtom;
  if not AtomIs(':=') then exit;
  AssignmentOperator:=CurPos;
  
  // find term
  ReadNextAtom;
  TermAtom.StartPos:=CurPos.StartPos;
  TermAtom.EndPos:=FindEndOfExpression(TermAtom.StartPos);

  Result:=TermAtom.EndPos>TermAtom.StartPos;
end;

function TCodeCompletionCodeTool.AddLocalVariable(
  CleanCursorPos: integer; OldTopLine: integer;
  VariableName, VariableType: string;
  var NewPos: TCodeXYPosition;
  var NewTopLine: integer; SourceChangeCache: TSourceChangeCache): boolean;
var
  CursorNode, BeginNode, VarSectionNode, VarNode: TCodeTreeNode;
  Indent, InsertPos: integer;
  InsertTxt: string;
  OldCodePos: TCodePosition;
begin
  //DebugLn('TCodeCompletionCodeTool.AddLocalVariable A ');
  Result:=false;
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if not CleanPosToCodePos(CleanCursorPos,OldCodePos) then begin
    RaiseException('TCodeCompletionCodeTool.AddLocalVariable Internal Error: '
      +'CleanPosToCodePos');
  end;

  // find parent block node at cursor
  BeginNode:=CursorNode.GetNodeOfType(ctnBeginBlock);
  if (BeginNode=nil) or (BeginNode.Parent=nil) then begin
    DebugLn('TCodeCompletionCodeTool.AddLocalVariable - Not in Begin Block');
    exit;
  end;

  // find last 'var' section node
  VarSectionNode:=BeginNode;
  while (VarSectionNode<>nil) and (VarSectionNode.Desc<>ctnVarSection) do
    VarSectionNode:=VarSectionNode.PriorBrother;

  InsertTxt:=VariableName+':'+VariableType+';';
  //DebugLn('TCodeCompletionCodeTool.AddLocalVariable C ',InsertTxt,' ');

  if (VarSectionNode<>nil) and (VarSectionNode.FirstChild<>nil) then begin
    // there is already a var section
    // -> append variable
    VarNode:=VarSectionNode.FirstChild;
    // search last variable in var section
    while (VarNode.NextBrother<>nil) do
      VarNode:=VarNode.NextBrother;
    Indent:=GetLineIndent(Src,VarNode.StartPos);
    if PositionsInSameLine(Src,VarSectionNode.StartPos,VarNode.StartPos) then
      inc(Indent,SourceChangeCache.BeautifyCodeOptions.Indent);
    InsertPos:=FindLineEndOrCodeAfterPosition(VarNode.EndPos);
  end else begin
    // there is no var section yet
    // -> create a new var section and append variable
    Indent:=GetLineIndent(Src,BeginNode.StartPos);
    InsertTxt:='var'+SourceChangeCache.BeautifyCodeOptions.LineEnd
               +GetIndentStr(Indent+SourceChangeCache.BeautifyCodeOptions.Indent)
               +InsertTxt;
    InsertPos:=BeginNode.StartPos;
  end;
  
  // insert new code
  InsertTxt:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                InsertTxt,Indent);
  //DebugLn('TCodeCompletionCodeTool.AddLocalVariable E ',InsertTxt,' ');
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,InsertTxt);
  if not SourceChangeCache.Apply then exit;

  // adjust cursor position
  AdjustCursor(OldCodePos,OldTopLine,NewPos,NewTopLine);

  Result:=true;
end;

procedure TCodeCompletionCodeTool.AdjustCursor(OldCodePos: TCodePosition;
  OldTopLine: integer; var NewPos: TCodeXYPosition; var NewTopLine: integer);
begin
  OldCodePos.Code.AdjustPosition(OldCodePos.P);
  NewPos.Code:=OldCodePos.Code;
  OldCodePos.Code.AbsoluteToLineCol(OldCodePos.P,NewPos.Y,NewPos.X);
  NewTopLine:=NewPos.Y-VisibleEditorLines+1;
  if NewTopLine<1 then NewTopLine:=1;
  if NewTopLine<OldTopLine then
    NewTopLine:=OldTopLine;
  //DebugLn('TCodeCompletionCodeTool.AdjustCursor END NewPos: Line=',NewPos.Y,' Col=',NewPos.X,' NewTopLine=',NewTopLine);
end;

function TCodeCompletionCodeTool.AddVariable(CursorNode: TCodeTreeNode;
  CleanCursorPos,
  OldTopLine: integer; const VariableName, NewType: string;
  out NewPos: TCodeXYPosition;
  out NewTopLine: integer; SourceChangeCache: TSourceChangeCache): boolean;
var
  VarLocation: TNewVarLocation;
  IsMethod: Boolean;
  VarType: String;
begin
  // ask what for location of new variable
  VarLocation:=ncpvLocal;
  VarType:=NewType;
  if Assigned(OnGetNewVariableLocation) then begin
    IsMethod:=NodeIsInAMethod(CursorNode);
    if not OnGetNewVariableLocation(Self,VariableName,VarType,
                                    IsMethod,VarLocation) then exit;
  end;

  // all needed parameters found
  Result:=true;

  // add local variable
  if not AddLocalVariable(CleanCursorPos, OldTopLine,
    VariableName, VarType,
    NewPos, NewTopLine, SourceChangeCache)
  then
    RaiseException('CompleteLocalVariableAssignment Internal error: AddLocalVariable');
end;

procedure TCodeCompletionCodeTool.AddNeededUnitToMainUsesSection(
  AnUnitName: PChar);
begin
  if fNewMainUsesSectionUnits=nil then
    fNewMainUsesSectionUnits:=
                         TAVLTree.Create(TListSortCompare(@CompareIdentifiers));
  //DebugLn(['TCodeCompletionCodeTool.AddNeededUnitToMainUsesSection AnUnitName="',AnUnitName,'"']);
  if fNewMainUsesSectionUnits.Find(AnUnitName)<>nil then exit;
  fNewMainUsesSectionUnits.Add(AnUnitName);
end;

function TCodeCompletionCodeTool.CompleteLocalVariableAssignment(
  CleanCursorPos, OldTopLine: integer;
  CursorNode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  VarNameAtom, AssignmentOperator, TermAtom: TAtomPosition;
  NewType: string;
  Params: TFindDeclarationParams;
begin
  Result:=false;

  {$IFDEF CTDEBUG}
  DebugLn('  CompleteLocalVariableAssignment: A');
  {$ENDIF}
  if not ((CursorNode.Desc=ctnBeginBlock)
          or CursorNode.HasParentOfType(ctnBeginBlock)) then exit;
  if CursorNode.Desc=ctnBeginBlock then
    BuildSubTreeForBeginBlock(CursorNode);
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);

  {$IFDEF CTDEBUG}
  DebugLn('  CompleteLocalVariableAssignment: B CheckLocalVarAssignmentSyntax ...');
  {$ENDIF}
  // check assignment syntax
  if not CheckLocalVarAssignmentSyntax(CleanCursorPos,
    VarNameAtom,AssignmentOperator,TermAtom)
  then
    exit;

  // search variable
  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    {$IFDEF CTDEBUG}
    DebugLn('  CompleteLocalVariableAssignment: check if variable is already defined ...');
    {$ENDIF}
    // check if identifier exists
    Result:=IdentifierIsDefined(VarNameAtom,CursorNode,Params);
    if Result then begin
      MoveCursorToCleanPos(VarNameAtom.StartPos);
      ReadNextAtom;
      RaiseExceptionFmt(ctsIdentifierAlreadyDefined,[GetAtom]);
    end;

    {$IFDEF CTDEBUG}
    DebugLn('  CompleteLocalVariableAssignment: Find type of term ...',
    ' Term="',copy(Src,TermAtom.StartPos,TermAtom.EndPos-TermAtom.StartPos),'"');
    {$ENDIF}
    // find type of term
    NewType:=FindTermTypeAsString(TermAtom,CursorNode,Params);
    if NewType='' then
      RaiseException('CompleteLocalVariableAssignment Internal error: NewType=""');

  finally
    Params.Free;
    DeactivateGlobalWriteLock;
  end;
  
  Result:=AddVariable(CursorNode,CleanCursorPos,OldTopLine,GetAtom(VarNameAtom),
                      NewType,NewPos,NewTopLine,SourceChangeCache);
end;

function TCodeCompletionCodeTool.CompleteLocalVariableAsParameter(
  CleanCursorPos, OldTopLine: integer; CursorNode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  VarNameAtom, ProcNameAtom: TAtomPosition;
  ParameterIndex: integer;
  Params: TFindDeclarationParams;
  ParameterNode: TCodeTreeNode;
  TypeNode: TCodeTreeNode;
  NewType: String;
  IgnorePos: TCodePosition;
begin
  Result:=false;

  {$IFDEF CTDEBUG}
  DebugLn('  CompleteLocalVariableAsParameter: A');
  {$ENDIF}
  if not ((CursorNode.Desc=ctnBeginBlock)
          or CursorNode.HasParentOfType(ctnBeginBlock)) then exit;
  if CursorNode.Desc=ctnBeginBlock then
    BuildSubTreeForBeginBlock(CursorNode);
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);

  {$IFDEF CTDEBUG}
  DebugLn('  CompleteLocalVariableAsParameter: B CheckLocalVarAsParameterSyntax ...');
  {$ENDIF}
  // check parameter syntax
  if not CheckParameterSyntax(CursorNode,CleanCursorPos,
                              VarNameAtom,ProcNameAtom,ParameterIndex)
  then
    exit;
  if not IsValidIdent(GetAtom(VarNameAtom)) then exit;

  { $IFDEF CTDEBUG}
  DebugLn('  CompleteLocalVariableAsParameter VarNameAtom=',GetAtom(VarNameAtom),' ProcNameAtom=',GetAtom(ProcNameAtom),' ParameterIndex=',dbgs(ParameterIndex));
  { $ENDIF}

  // search variable
  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    {$IFDEF CTDEBUG}
    DebugLn('  CompleteLocalVariableAsParameter: check if variable is already defined ...');
    {$ENDIF}
    // check if identifier exists
    Result:=IdentifierIsDefined(VarNameAtom,CursorNode,Params);
    if Result then begin
      MoveCursorToCleanPos(VarNameAtom.StartPos);
      ReadNextAtom;
      RaiseExceptionFmt(ctsIdentifierAlreadyDefined,[GetAtom]);
    end;

    { $IFDEF CTDEBUG}
    DebugLn('  CompleteLocalVariableAsParameter: Find declaration of parameter list ...  Identifier="',GetAtom(ProcNameAtom),'"');
    { $ENDIF}
    // find declaration of parameter list
    Params.ContextNode:=CursorNode;
    Params.SetIdentifier(Self,@Src[ProcNameAtom.StartPos],nil);
    Params.Flags:=fdfGlobals+[fdfSearchInParentNodes,fdfSearchInAncestors,
                              fdfFindVariable,fdfIgnoreCurContextNode];
    CleanPosToCodePos(VarNameAtom.StartPos,IgnorePos);
    IgnoreErrorAfter:=IgnorePos;
    try
      if not FindIdentifierInContext(Params) then exit;
      // TODO: fix the bug, that even if fdfExceptionOnNotFound is not set,
      //       and exeception is raised
      //if not FindDeclarationOfIdentAtCursor(Params) then exit;
    finally
      ClearIgnoreErrorAfter;
    end;
    NewType:='';
    if Params.NewNode<>nil then begin
      DebugLn('TCodeCompletionCodeTool.CompleteLocalVariableAsParameter Proc/PropNode=',Params.NewNode.DescAsString,' ',copy(Params.NewCodeTool.Src,Params.NewNode.StartPos,50));
      ParameterNode:=Params.NewCodeTool.FindNthParameterNode(Params.NewNode,
                                                             ParameterIndex);
      if (ParameterNode=nil)
      and (Params.NewNode.Desc in [ctnProperty,ctnProcedure]) then begin
        DebugLn('  CompleteLocalVariableAsParameter Procedure does not have so many parameters');
        exit;
      end;
      if ParameterNode<>nil then begin
        DebugLn('TCodeCompletionCodeTool.CompleteLocalVariableAsParameter ParameterNode=',ParameterNode.DescAsString,' ',copy(Params.NewCodeTool.Src,ParameterNode.StartPos,50));
        TypeNode:=FindTypeNodeOfDefinition(ParameterNode);
        if TypeNode=nil then begin
          DebugLn('  CompleteLocalVariableAsParameter Parameter has no type');
          exit;
        end;
        NewType:=copy(Params.NewCodeTool.Src,TypeNode.StartPos,
                      TypeNode.EndPos-TypeNode.StartPos);
        DebugLn('TCodeCompletionCodeTool.CompleteLocalVariableAsParameter NewType=',NewType);
        if NewType='' then
          RaiseException('CompleteLocalVariableAsParameter Internal error: NewType=""');
      end;
      //DebugLn('  CompleteLocalVariableAsParameter Dont know: ',Params.NewNode.DescAsString);
    end;

    if NewType='' then begin
      exit;
    end;
    
  finally
    Params.Free;
    DeactivateGlobalWriteLock;
  end;

  Result:=AddVariable(CursorNode,CleanCursorPos,OldTopLine,GetAtom(VarNameAtom),
                      NewType,NewPos,NewTopLine,SourceChangeCache);
end;

function TCodeCompletionCodeTool.CompleteMethod(
  CleanCursorPos, OldTopLine: integer;
  CursorNode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  CurClassName: String;
  ProcNode: TCodeTreeNode;
  CleanProcCode: String;
  ProcCode: String;
  ProcName: String;
  OldCodePos: TCodePosition;
begin
  Result:=false;

  {$IFDEF VerboseCompleteMethod}
  DebugLn(['TCodeCompletionCodeTool.CompleteMethod A ',ProcNode<>nil]);
  {$ENDIF}
  // check if cursor in a method
  ProcNode:=CursorNode.GetNodeOfType(ctnProcedure);
  {$IFDEF VerboseCompleteMethod}
  DebugLn(['TCodeCompletionCodeTool.CompleteMethod B ',ProcNode<>nil]);
  {$ENDIF}
  if (ProcNode=nil) and (CursorNode.Desc=ctnProcedure) then
    ProcNode:=CursorNode;
  {$IFDEF VerboseCompleteMethod}
  DebugLn(['TCodeCompletionCodeTool.CompleteMethod C ',ProcNode<>nil]);
  {$ENDIF}
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)
  or (not NodeIsMethodBody(ProcNode)) then begin
    {$IFDEF VerboseCompleteMethod}
    DebugLn(['TCodeCompletionCodeTool.CompleteMethod node is not a method body ',ProcNode<>nil]);
    {$ENDIF}
    exit;
  end;

  // find corresponding class declaration
  CurClassName:=ExtractClassNameOfProcNode(ProcNode);
  if CurClassName='' then begin
    DebugLn(['CompleteMethod ExtractClassNameOfProcNode failed']);
    exit;
  end;
  //DebugLn(['CompleteMethod CurClassName=',CurClassName]);
  CodeCompleteClassNode:=FindClassNodeInUnit(CurClassName,true,false,false,true);

  // check if method declaration already exists
  ProcName:=ExtractProcName(ProcNode,[phpWithoutClassName]);
  CleanProcCode:=ExtractProcHead(ProcNode,
                   [phpWithoutClassKeyword,phpWithoutClassName,phpInUpperCase]);
  if ProcExistsInCodeCompleteClass(CleanProcCode) then begin
    // proc exists already
    MoveCursorToCleanPos(CleanCursorPos);
    RaiseExceptionFmt(ctsIdentifierAlreadyDefined,[ProcName]);
  end;

  // store old cursor position
  if not CleanPosToCodePos(CleanCursorPos,OldCodePos) then begin
    RaiseException('TCodeCompletionCodeTool.AddLocalVariable Internal Error: '
      +'CleanPosToCodePos');
  end;

  CodeCompleteSrcChgCache:=SourceChangeCache;

  // add method declaration
  ProcCode:=ExtractProcHead(ProcNode,
    [phpWithStart,phpWithoutClassName,phpWithVarModifiers,phpWithParameterNames,
     phpWithDefaultValues,phpWithResultType,phpWithCallingSpecs]);
  {$IFDEF VerboseCompleteMethod}
  DebugLn(['TCodeCompletionCodeTool.CompleteMethod Adding ProcName="',ProcName,'"']);
  {$ENDIF}
  AddClassInsertion(CleanProcCode,ProcCode,ProcName,ncpPrivateProcs);

  // apply changes
  Result:=ApplyClassCompletion(false);

  // adjust cursor position
  AdjustCursor(OldCodePos,OldTopLine,NewPos,NewTopLine);
  //DebugLn(['TCodeCompletionCodeTool.CompleteMethod END OldCodePos.P=',OldCodePos.P,' OldTopLine=',OldTopLine,' NewPos=',DbgsCXY(NewPos),' NewTopLine=',NewTopLine]);
end;

function TCodeCompletionCodeTool.AddPublishedVariable(const UpperClassName,
  VarName, VarType: string; SourceChangeCache: TSourceChangeCache): boolean;
begin
  Result:=false;
  if (UpperClassName='') or (VarName='') or (VarType='')
  or (SourceChangeCache=nil) or (Scanner=nil) then exit;
  // find classnode
  BuildTree(false);
  if not EndOfSourceFound then exit;
  // initialize class for code completion
  CodeCompleteClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
  CodeCompleteSrcChgCache:=SourceChangeCache;
  // check if variable already exists
  if not VarExistsInCodeCompleteClass(UpperCaseStr(VarName)) then begin

  end else begin
    AddClassInsertion(UpperCaseStr(VarName),
                      VarName+':'+VarType+';',VarName,ncpPublishedVars);
    if not InsertAllNewClassParts then
      RaiseException(ctsErrorDuringInsertingNewClassParts);
    // apply the changes
    if not SourceChangeCache.Apply then
      RaiseException(ctsUnableToApplyChanges);
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.GetRedefinitionNodeText(Node: TCodeTreeNode
  ): string;
begin
  case Node.Desc of
  ctnProcedure:
    Result:=ExtractProcHead(Node,[phpInUpperCase,phpWithoutSemicolon]);
  ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition,ctnEnumIdentifier,
  ctnGenericType:
    Result:=ExtractDefinitionName(Node);
  else
    Result:='';
  end;
end;

function TCodeCompletionCodeTool.FindRedefinitions(
  out TreeOfCodeTreeNodeExt: TAVLTree; WithEnums: boolean): boolean;
var
  AllNodes: TAVLTree;

  procedure AddRedefinition(Redefinition, Definition: TCodeTreeNode;
    const NodeText: string);
  var
    NodeExt: TCodeTreeNodeExtension;
  begin
    DebugLn(['AddRedefinition ',NodeText,' Redefined=',CleanPosToStr(Redefinition.StartPos),' Definition=',CleanPosToStr(Definition.StartPos)]);
    if TreeOfCodeTreeNodeExt=nil then
      TreeOfCodeTreeNodeExt:=TAVLTree.Create(@CompareCodeTreeNodeExt);
    NodeExt:=NodeExtMemManager.NewNode;
    NodeExt.Node:=Redefinition;
    NodeExt.Data:=Definition;
    NodeExt.Txt:=NodeText;
    TreeOfCodeTreeNodeExt.Add(NodeExt);
  end;
  
  procedure AddDefinition(Node: TCodeTreeNode; const NodeText: string);
  var
    NodeExt: TCodeTreeNodeExtension;
  begin
    NodeExt:=NodeExtMemManager.NewNode;
    NodeExt.Node:=Node;
    NodeExt.Txt:=NodeText;
    AllNodes.Add(NodeExt);
  end;
  
var
  Node: TCodeTreeNode;
  NodeText: String;
  AVLNode: TAVLTreeNode;
begin
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  BuildTree(true);

  AllNodes:=TAVLTree.Create(@CompareCodeTreeNodeExt);
  try
    Node:=Tree.Root;
    while Node<>nil do begin
      case Node.Desc of
      ctnImplementation, ctnInitialization, ctnFinalization,
      ctnBeginBlock, ctnAsmBlock:
        // skip implementation
        break;
      ctnVarDefinition, ctnTypeDefinition, ctnConstDefinition, ctnProcedure,
      ctnEnumIdentifier, ctnGenericType:
        begin
          NodeText:=GetRedefinitionNodeText(Node);
          AVLNode:=FindCodeTreeNodeExtAVLNode(AllNodes,NodeText);
          if AVLNode<>nil then begin
            AddRedefinition(Node,TCodeTreeNodeExtension(AVLNode.Data).Node,NodeText);
          end else begin
            AddDefinition(Node,NodeText);
          end;
          if WithEnums
          and (Node.FirstChild<>nil)
          and (Node.FirstChild.Desc=ctnEnumerationType) then
            Node:=Node.FirstChild
          else
            Node:=Node.NextSkipChilds;
        end;
      else
        Node:=Node.Next;
      end;
    end;
  finally
    NodeExtMemManager.DisposeAVLTree(AllNodes);
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.RemoveRedefinitions(
  TreeOfCodeTreeNodeExt: TAVLTree;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  AVLNode: TAVLTreeNode;
  NodesToDo: TAVLTree;
  Node: TCodeTreeNode;
  StartNode: TCodeTreeNode;
  EndNode: TCodeTreeNode;
  IsListStart: Boolean;
  IsListEnd: Boolean;
  StartPos: LongInt;
  EndPos: LongInt;
begin
  Result:=false;
  if SourceChangeCache=nil then exit;
  if (TreeOfCodeTreeNodeExt=nil) or (TreeOfCodeTreeNodeExt.Count=0) then
    exit(true);
  SourceChangeCache.MainScanner:=Scanner;

  NodesToDo:=TAVLTree.Create;
  try
    // put the nodes to remove into the NodesToDo
    AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
    while AVLNode<>nil do begin
      NodesToDo.Add(TCodeTreeNodeExtension(AVLNode.Data).Node);
      AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
    end;
    
    // delete all redefinitions
    while NodesToDo.Count>0 do begin
      // find a block of redefinitions
      StartNode:=TCodeTreeNode(NodesToDo.Root.Data);
      EndNode:=StartNode;
      while (StartNode.PriorBrother<>nil)
      and (NodesToDo.Find(StartNode.PriorBrother)<>nil) do
        StartNode:=StartNode.PriorBrother;
      while (EndNode.NextBrother<>nil)
      and (NodesToDo.Find(EndNode.NextBrother)<>nil) do
        EndNode:=EndNode.NextBrother;
        
      // check if a whole section is deleted
      if (StartNode.PriorBrother=nil) and (EndNode.PriorBrother=nil)
      and (StartNode.Parent<>nil)
      and (StartNode.Parent.Desc in AllDefinitionSections) then begin
        StartNode:=StartNode.Parent;
        EndNode:=StartNode;
      end;
      
      // compute nice code positions to delete
      StartPos:=FindLineEndOrCodeInFrontOfPosition(StartNode.StartPos);
      EndPos:=FindLineEndOrCodeAfterPosition(EndNode.EndPos);
      
      // check list of definitions
      if EndNode.Desc in AllIdentifierDefinitions then begin
        // check list definition. For example:
        //  delete, delete: char;    ->   delete whole
        //  a,delete, delete: char;  ->   a: char;
        //  delete,delete,c: char;   ->   c: char;
        //  a,delete,delete,c: char; ->   a,c:char;
        IsListStart:=(StartNode.PriorBrother<>nil)
                 and (StartNode.PriorBrother.FirstChild<>nil);
        IsListEnd:=(EndNode.FirstChild<>nil);
        if IsListStart and IsListEnd then begin
          // case 1: delete, delete: char;    ->   delete whole
        end else begin
          // case 2-4: keep type
          // get start position of first deleting identifier
          StartPos:=StartNode.StartPos;
          // get end position of last deleting identifier
          EndPos:=EndNode.StartPos+GetIdentLen(@Src[EndNode.StartPos]);
          if IsListEnd then begin
            // case 2: a,delete, delete: char;  ->   a: char;
            // delete comma in front of start too
            MoveCursorToCleanPos(StartNode.PriorBrother.StartPos);
            ReadNextAtom; // read identifier
            ReadNextAtom; // read comma
            StartPos:=CurPos.StartPos;
          end else begin
            // case 3,4
            // delete comma behind end too
            MoveCursorToCleanPos(EndNode.StartPos);
            ReadNextAtom; // read identifier
            ReadNextAtom; // read comma
            EndPos:=CurPos.StartPos;
          end;
        end;
      end;
      
      // replace
      DebugLn(['TCodeCompletionCodeTool.RemoveRedefinitions deleting:']);
      debugln('"',copy(Src,StartPos,EndPos-StartPos),'"');
      
      if not SourceChangeCache.Replace(gtNone,gtNone,StartPos,EndPos,'') then
        exit;
      
      // remove nodes from NodesToDo
      Node:=StartNode;
      repeat
        NodesToDo.Remove(Node);
        if Node=EndNode then break;
        Node:=Node.Next;
      until false;
    end;
  finally
    NodesToDo.Free;
  end;
  
  Result:=SourceChangeCache.Apply;
end;

function TCodeCompletionCodeTool.FindAliasDefinitions(out
  TreeOfCodeTreeNodeExt: TAVLTree; OnlyWrongType: boolean): boolean;
// finds all public definitions of the form 'const A = B;'
var
  AllNodes: TAVLTree;

  procedure CheckAlias(Node: TCodeTreeNode);
  var
    ReferingNode: TCodeTreeNode;
    ReferingNodeText: String;
    ReferingPos: LongInt;
    NodeExt: TCodeTreeNodeExtension;
    AVLNode: TAVLTreeNode;
    BracketStartPos: LongInt;
    NeededType: TCodeTreeNodeDesc;
    
    procedure GetReferingNode;
    begin
      if ReferingNodeText<>'' then exit;
      ReferingNodeText:=GetIdentifier(@Src[ReferingPos]);
      AVLNode:=FindCodeTreeNodeExtAVLNode(AllNodes,ReferingNodeText);
      if (AVLNode<>nil) then begin
        NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
        ReferingNode:=NodeExt.Node;
      end;
    end;
    
  begin
    // check if definition is an alias
    // Example: const A = B;  or   const A = B();
    if (Node.Parent=nil) then exit;
    if not (Node.Parent.Desc in [ctnConstSection,ctnTypeSection]) then exit;
    // this is a const or type
    MoveCursorToNodeStart(Node);
    // read A
    ReadNextAtom;
    if CurPos.Flag<>cafWord then exit;
    // read =
    ReadNextAtom;
    if CurPos.Flag<>cafEqual then exit;
    // read B
    ReadNextAtom;
    if CurPos.Flag<>cafWord then exit;
    ReferingPos:=CurPos.StartPos;
    ReadNextAtom;
    if CurPos.Flag=cafRoundBracketOpen then begin
      BracketStartPos:=CurPos.StartPos;
      ReadTilBracketClose(true);
      //BracketEndPos:=CurPos.StartPos;
      ReadNextAtom;
    end else
      BracketStartPos:=0;
    if CurPos.Flag<>cafSemicolon then exit;
    
    ReferingNode:=nil;
    NeededType:=ctnNone;

    if BracketStartPos>0 then begin
      if WordIsKeyWord.DoIt(@Src[ReferingPos]) then exit;
      // this is a type cast
      NeededType:=ctnConstDefinition;
    end else begin
      // this is a const or type alias
      //DebugLn(['TCodeCompletionCodeTool.FindAliasDefinitions Alias: ',Node.DescAsString,' ',ExtractNode(Node,[])]);
      if OnlyWrongType then begin
        GetReferingNode;
        if (ReferingNode<>nil) then begin
          NeededType:=ReferingNode.Desc;
        end;
      end;
    end;
    if NeededType=ctnNone then exit;
    if (NeededType<>Node.Desc) or (not OnlyWrongType) then begin
      // add alias
      if NeededType<>Node.Desc then begin
        DebugLn(['TCodeCompletionCodeTool.FindAliasDefinitions Wrong: ',Node.DescAsString,' ',ExtractNode(Node,[]),' ',Node.DescAsString,'<>',ReferingNode.DescAsString]);
      end;
      if TreeOfCodeTreeNodeExt=nil then
        TreeOfCodeTreeNodeExt:=TAVLTree.Create(@CompareCodeTreeNodeExt);
      NodeExt:=NodeExtMemManager.NewNode;
      NodeExt.Node:=Node;
      NodeExt.Txt:=GetRedefinitionNodeText(Node);
      NodeExt.Data:=ReferingNode;
      NodeExt.Flags:=NeededType;
      TreeOfCodeTreeNodeExt.Add(NodeExt);
    end;
  end;

var
  NodeExt: TCodeTreeNodeExtension;
  Node: TCodeTreeNode;
  NodeText: String;
  AVLNode: TAVLTreeNode;
begin
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  BuildTree(true);

  AllNodes:=TAVLTree.Create(@CompareCodeTreeNodeExt);
  try
    Node:=Tree.Root;
    while Node<>nil do begin
      case Node.Desc of
      ctnImplementation, ctnInitialization, ctnFinalization,
      ctnBeginBlock, ctnAsmBlock:
        // skip implementation
        break;
      ctnTypeDefinition, ctnConstDefinition:
        begin
          if OnlyWrongType then begin
            // remember the definition
            NodeText:=GetRedefinitionNodeText(Node);
            AVLNode:=FindCodeTreeNodeExtAVLNode(AllNodes,NodeText);
            if AVLNode=nil then begin
              // add new node
              NodeExt:=NodeExtMemManager.NewNode;
              NodeExt.Node:=Node;
              NodeExt.Txt:=NodeText;
              AllNodes.Add(NodeExt);
            end else begin
              // update node
              NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
              NodeExt.Node:=Node;
            end;
          end;

          Node:=Node.NextSkipChilds;
        end;
      ctnProcedure:
        Node:=Node.NextSkipChilds;
      else
        Node:=Node.Next;
      end;
    end;

    Node:=Tree.Root;
    while Node<>nil do begin
      case Node.Desc of
      ctnImplementation, ctnInitialization, ctnFinalization,
      ctnBeginBlock, ctnAsmBlock:
        // skip implementation
        break;
      ctnTypeDefinition, ctnConstDefinition:
        begin
          CheckAlias(Node);
          Node:=Node.NextSkipChilds;
        end;
      ctnProcedure:
        Node:=Node.NextSkipChilds;
      else
        Node:=Node.Next;
      end;
    end;

  finally
    NodeExtMemManager.DisposeAVLTree(AllNodes);
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.FixAliasDefinitions(
  TreeOfCodeTreeNodeExt: TAVLTree; SourceChangeCache: TSourceChangeCache
  ): boolean;
{ replaces public dummy functions with a constant.
  The function body will be removed.
  See the function FindAliasDefinitions.
}
  function FindReferingNode(DefNode: TCodeTreeNode): TCodeTreeNode;
  var
    AVLNode: TAVLTreeNode;
    NodeExt: TCodeTreeNodeExtension;
  begin
    AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
    while AVLNode<>nil do begin
      NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
      if NodeExt.Node=DefNode then begin
        Result:=TCodeTreeNode(NodeExt.Data);
        exit;
      end;
      AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
    end;
    Result:=nil;
  end;

var
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  DefNode: TCodeTreeNode;
  ReferingNode: TCodeTreeNode;
  NextAVLNode: TAVLTreeNode;
  ReferingNodeInFront: TCodeTreeNode;
  ReferingNodeBehind: TCodeTreeNode;
  NewSrc: String;
  FromPos: LongInt;
  ReferingType: TCodeTreeNodeDesc;
begin
  Result:=false;
  if SourceChangeCache=nil then exit;
  if (TreeOfCodeTreeNodeExt=nil) or (TreeOfCodeTreeNodeExt.Count=0) then
    exit(true);
  SourceChangeCache.MainScanner:=Scanner;

  // remove all nodes which can not be handled here
  AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
  while AVLNode<>nil do begin
    NextAVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
    NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
    DefNode:=NodeExt.Node;
    ReferingNode:=TCodeTreeNode(NodeExt.Data);
    ReferingType:=TCodeTreeNodeDesc(NodeExt.Flags);
    if (ReferingNode=nil)
    or (not (ReferingNode.Desc in [ctnTypeDefinition,ctnConstDefinition]))
    or (DefNode.Desc=ReferingType) then begin
      TreeOfCodeTreeNodeExt.Delete(AVLNode);
    end;
    AVLNode:=NextAVLNode;
  end;

  // insert additional sections
  AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
  while AVLNode<>nil do begin
    NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
    DefNode:=NodeExt.Node;
    ReferingNode:=TCodeTreeNode(NodeExt.Data);
    ReferingType:=TCodeTreeNodeDesc(NodeExt.Flags);

    //DebugLn(['TCodeCompletionCodeTool.FixAliasDefinitions Old=',DefNode.DescAsString,' New=',NodeDescToStr(ReferingType)]);

    case ReferingType of
    ctnTypeDefinition: NewSrc:='type';
    ctnConstDefinition: NewSrc:='const';
    else NewSrc:='bug';
    end;
    
    // check in front
    if DefNode.PriorBrother=nil then begin
      // this is the start of the section
      MoveCursorToNodeStart(DefNode.Parent);
      ReadNextAtom;
      if not SourceChangeCache.Replace(gtNone,gtNone,
        CurPos.StartPos,CurPos.EndPos,NewSrc) then exit;
    end else begin
      // this is not the start of the section
      ReferingNodeInFront:=FindReferingNode(DefNode.PriorBrother);
      if (ReferingNodeInFront=nil)
      or (ReferingNodeInFront.Desc<>ReferingType) then begin
        // the node in front has a different section
        FromPos:=FindLineEndOrCodeInFrontOfPosition(DefNode.StartPos);
        if not SourceChangeCache.Replace(gtEmptyLine,gtNewLine,
           FromPos,FromPos,NewSrc) then exit;
      end;
    end;
    
    // check behind
    if DefNode.NextBrother=nil then begin
      // this is the end of the section
    end else begin
      // this is not the end of the section
      ReferingNodeBehind:=FindReferingNode(DefNode.NextBrother);
      if ReferingNodeBehind<>nil then begin
        // the next node will change the section
      end else begin
        // the next node should stay in the same type of section
        case DefNode.NextBrother.Desc of
        ctnTypeDefinition: NewSrc:='type';
        ctnConstDefinition: NewSrc:='const';
        else NewSrc:='';
        end;
        if NewSrc<>'' then begin
          FromPos:=FindLineEndOrCodeInFrontOfPosition(DefNode.NextBrother.StartPos);
          if not SourceChangeCache.Replace(gtEmptyLine,gtNewLine,
             FromPos,FromPos,NewSrc) then exit;
        end;
      end;
    end;

    AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
  end;
  Result:=SourceChangeCache.Apply;
end;

function TCodeCompletionCodeTool.FindConstFunctions(
  out TreeOfCodeTreeNodeExt: TAVLTree): boolean;
{ find public dummy functions that can be replaced with a constant
  For example:
  
      function MPI_CONVERSION_FN_NULL : PMPI_Datarep_conversion_function;
      begin
         MPI_CONVERSION_FN_NULL:=PMPI_Datarep_conversion_function(0);
      end;
      
   Where the expression only contains unit defined types, constants,
   variables, built-in const functions and no members nor functions.

    NodeExt.Txt: description
    NodeExt.Node: definition node
    NodeExt.Data: function body node
    NodeExt.ExtTxt1: ExtractCode(ExprStart,ExprEnd,[]);
}
var
  Definitions: TAVLTree;

  procedure CheckProcNode(ProcNode: TCodeTreeNode);
  // check if node is a function (not class function)
  var
    Node: TCodeTreeNode;
    FuncName: String;
    ExprStart: LongInt;
    NodeText: String;
    NodeExt: TCodeTreeNodeExtension;
    ExprEnd: LongInt;
    ResultNodeExt: TCodeTreeNodeExtension;

    function CheckExprIdentifier(Identifier: PChar): boolean;
    var
      NodeExt: TCodeTreeNodeExtension;
    begin
      Result:=true;
      if CompareIdentifiers('Result',Identifier)=0 then exit;
      if CompareIdentifiers(PChar(FuncName),Identifier)=0 then exit;
      // check for const and type definitions
      NodeExt:=FindCodeTreeNodeExt(Definitions,GetIdentifier(Identifier));
      if (NodeExt<>nil) and (NodeExt.Node<>nil) then begin
        if NodeExt.Node.Desc in [ctnConstDefinition,ctnTypeDefinition] then
          exit;
      end;
      // check for compiler built in operators, constants and types
      if IsWordBuiltInFunc.DoIt(Identifier) then exit;
      if WordIsBinaryOperator.DoIt(Identifier) then exit;
      if WordIsPredefinedFPCIdentifier.DoIt(Identifier) then exit;
      Result:=false;
    end;

  begin
    if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then exit;
    //DebugLn(['CheckProcNode START ',ExtractProcHead(ProcNode,[])]);
    MoveCursorToNodeStart(ProcNode);
    // read 'function'
    ReadNextAtom;
    if not UpAtomIs('FUNCTION') then exit;
    // read name
    ReadNextAtom;
    FuncName:=GetAtom;
    ReadNextAtom;
    if CurPos.Flag=cafRoundBracketOpen then begin
      // skip optional empty parameter list ()
      ReadNextAtom;
      if CurPos.Flag<>cafRoundBracketClose then exit;
      ReadNextAtom;
    end;
    // read :
    if CurPos.Flag<>cafColon then exit;
    // read result type
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;

    // check if there is a public definition of the procedure
    NodeText:=GetRedefinitionNodeText(ProcNode);
    if TreeOfCodeTreeNodeExt<>nil then begin
      ResultNodeExt:=FindCodeTreeNodeExt(TreeOfCodeTreeNodeExt,NodeText);
      if ResultNodeExt<>nil then begin
        DebugLn(['CheckProcNode function exists twice']);
        exit;
      end;
    end;

    NodeExt:=FindCodeTreeNodeExt(Definitions,NodeText);
    if (NodeExt=nil) or (NodeExt.Node=nil) or (NodeExt.Node.Desc<>ctnProcedure)
    then begin
      DebugLn(['CheckProcNode function is not public NodeText=',NodeText]);
      exit;
    end;

    // check child nodes only contain the proc head and a begin block
    Node:=ProcNode.FirstChild;
    if Node=nil then exit;
    if Node.Desc=ctnProcedureHead then begin
      Node:=Node.NextBrother;
      if Node=nil then exit;
    end;
    if Node.Desc<>ctnBeginBlock then exit;

    //DebugLn(['CheckProcNode has begin block']);
    
    // check begin block is only a single assignment
    MoveCursorToNodeStart(Node);
    // read begin
    ReadNextAtom;
    // read 'Result' or 'FunctionName'
    ReadNextAtom;
    if (not UpAtomIs('RESULT')) and (not AtomIs(FuncName)) then exit;
    // read :=
    ReadNextAtom;
    if not UpAtomIs(':=') then exit;
    // read expression
    ReadNextAtom;
    ExprStart:=CurPos.StartPos;
    ExprEnd:=ExprStart;
    while (CurPos.EndPos<=Node.EndPos) do begin
      if (CurPos.Flag in [cafSemicolon,cafEnd]) then
        break;
      // check if all identifiers can be used in a constant expression
      if AtomIsIdentifier(false)
      and not CheckExprIdentifier(@Src[CurPos.StartPos]) then
        exit;
      ExprEnd:=CurPos.EndPos;
      ReadNextAtom;
    end;
    if ExprStart=ExprEnd then exit;
    
    //DebugLn(['CheckProcNode FOUND']);
    
    // save values
    ResultNodeExt:=NodeExtMemManager.NewNode;
    ResultNodeExt.Txt:=NodeText;
    ResultNodeExt.Node:=NodeExt.Node;
    ResultNodeExt.Data:=ProcNode;
    ResultNodeExt.ExtTxt1:=ExtractCode(ExprStart,ExprEnd,[]);
    if TreeOfCodeTreeNodeExt=nil then
      TreeOfCodeTreeNodeExt:=TAVLTree.Create(@CompareCodeTreeNodeExt);
    TreeOfCodeTreeNodeExt.Add(ResultNodeExt);
  end;
  
var
  Node: TCodeTreeNode;
begin
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;

  try
    BuildTree(false);

    // first step: find all unit identifiers (excluding implementation section)
    if not GatherUnitDefinitions(Definitions,true,true) then exit;
    
    // now check all functions
    Node:=Tree.Root;
    while Node<>nil do begin
      case Node.Desc of
      ctnInterface, ctnUsesSection, ctnBeginBlock, ctnAsmBlock, ctnProcedureHead,
      ctnTypeSection, ctnConstSection, ctnVarSection, ctnResStrSection:
        Node:=Node.NextSkipChilds;
      ctnProcedure:
        begin
          CheckProcNode(Node);
          Node:=Node.NextSkipChilds;
        end;
      else
        Node:=Node.Next;
      end;
    end;
    
  finally
    NodeExtMemManager.DisposeAVLTree(Definitions);
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.ReplaceConstFunctions(
  TreeOfCodeTreeNodeExt: TAVLTree; SourceChangeCache: TSourceChangeCache
  ): boolean;
{ replaces public dummy functions with a constant.
  The function body will be removed.
  See the function FindConstFunctions.
}
  function IsConstSectionNeeded(Node: TCodeTreeNode): boolean;
  var
    AVLNode: TAVLTreeNode;
    NodeExt: TCodeTreeNodeExtension;
  begin
    if Node.PriorBrother.Desc=ctnConstSection then exit(false);
    AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
    while AVLNode<>nil do begin
      NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
      if NodeExt.Node=Node.PriorBrother then begin
        // the function in front will be replaced too
        exit(false);
      end;
      AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
    end;
    Result:=true;
  end;

var
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  DefNode: TCodeTreeNode;
  BodyNode: TCodeTreeNode;
  Expr: String;
  FromPos: LongInt;
  ToPos: LongInt;
  NewSrc: String;
begin
  Result:=false;
  if SourceChangeCache=nil then exit;
  if (TreeOfCodeTreeNodeExt=nil) or (TreeOfCodeTreeNodeExt.Count=0) then
    exit(true);
  SourceChangeCache.MainScanner:=Scanner;

  AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
  while AVLNode<>nil do begin
    NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
    DebugLn(['TCodeCompletionCodeTool.ReplaceConstFunctions ',NodeExt.Txt]);
    DefNode:=NodeExt.Node;
    BodyNode:=TCodeTreeNode(NodeExt.Data);
    Expr:=NodeExt.ExtTxt1;
    DebugLn(['TCodeCompletionCodeTool.ReplaceConstFunctions Expr=',Expr]);
    
    // remove body node
    FromPos:=FindLineEndOrCodeInFrontOfPosition(BodyNode.StartPos);
    ToPos:=FindLineEndOrCodeAfterPosition(BodyNode.EndPos);
    if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13]) then begin
      inc(ToPos);
      if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13])
      and (Src[ToPos-1]<>Src[ToPos]) then
        inc(ToPos);
    end;
    DebugLn(['TCodeCompletionCodeTool.ReplaceConstFunctions Body="',copy(Src,FromPos,ToPos-FromPos),'"']);
    SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');
    
    // replace definition
    FromPos:=DefNode.StartPos;
    ToPos:=DefNode.EndPos;
    if Src[ToPos]=';' then inc(ToPos);// add semicolon
    NewSrc:=GetIndentStr(SourceChangeCache.BeautifyCodeOptions.Indent)
      +ExtractProcName(DefNode,[])+' = '+Expr+';';
    SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,NewSrc);
    // add 'const' keyword
    if IsConstSectionNeeded(DefNode) then begin
      FromPos:=FindLineEndOrCodeInFrontOfPosition(DefNode.StartPos);
      SourceChangeCache.Replace(gtEmptyLine,gtNewLine,FromPos,FromPos,'const');
    end;

    AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
  end;
  Result:=SourceChangeCache.Apply;
end;

function TCodeCompletionCodeTool.FindTypeCastFunctions(out
  TreeOfCodeTreeNodeExt: TAVLTree): boolean;
{ find public dummy functions that can be replaced with a type
  For example:

  function PMPI_Win_f2c(win : longint) : MPI_Win;
    begin
       PMPI_Win_f2c:=MPI_Win(win);
    end;

   Where the expression is Result := ResultType(Parameter).

    NodeExt.Txt: description
    NodeExt.Node: definition node
    NodeExt.Data: function body node
    NodeExt.ExtTxt1: ResultType
}
var
  Definitions: TAVLTree;

  procedure CheckProcNode(ProcNode: TCodeTreeNode);
  // check if node is a function (not class function)
  var
    Node: TCodeTreeNode;
    FuncName: PChar;
    NodeText: String;
    NodeExt: TCodeTreeNodeExtension;
    ResultNodeExt: TCodeTreeNodeExtension;
    ParamName: PChar;
    ResultType: PChar;
  begin
    if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then exit;
    //DebugLn(['CheckProcNode START ',ExtractProcHead(ProcNode,[])]);
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    // read 'function'
    if not UpAtomIs('FUNCTION') then exit;
    ReadNextAtom;
    // read name
    if CurPos.Flag<>cafWord then exit;
    FuncName:=@Src[CurPos.StartPos];
    ReadNextAtom;
    // read (
    if CurPos.Flag<>cafRoundBracketOpen then exit;
    ReadNextAtom;
    // read optional const
    if UpAtomIs('CONST') then
      ReadNextAtom;
    // read parameter name
    if CurPos.Flag<>cafWord then exit;
    ParamName:=@Src[CurPos.StartPos];
    ReadNextAtom;
    // read :
    if CurPos.Flag<>cafColon then exit;
    ReadNextAtom;
    // read parameter type
    if CurPos.Flag<>cafWord then exit;
    ReadNextAtom;
    // read )
    if CurPos.Flag<>cafRoundBracketClose then exit;
    ReadNextAtom;
    // read :
    if CurPos.Flag<>cafColon then exit;
    // read result type
    ReadNextAtom;
    if CurPos.Flag<>cafWord then exit;
    ResultType:=@Src[CurPos.StartPos];

    // check if there is a public definition of the procedure
    NodeText:=GetRedefinitionNodeText(ProcNode);
    if TreeOfCodeTreeNodeExt<>nil then begin
      ResultNodeExt:=FindCodeTreeNodeExt(TreeOfCodeTreeNodeExt,NodeText);
      if ResultNodeExt<>nil then begin
        DebugLn(['CheckProcNode function exists twice']);
        exit;
      end;
    end;

    NodeExt:=FindCodeTreeNodeExt(Definitions,NodeText);
    if (NodeExt=nil) or (NodeExt.Node=nil) or (NodeExt.Node.Desc<>ctnProcedure)
    then begin
      DebugLn(['CheckProcNode function is not public NodeText=',NodeText]);
      exit;
    end;

    // check child nodes only contain the proc head and a begin block
    Node:=ProcNode.FirstChild;
    if Node=nil then exit;
    if Node.Desc=ctnProcedureHead then begin
      Node:=Node.NextBrother;
      if Node=nil then exit;
    end;
    if Node.Desc<>ctnBeginBlock then exit;

    //DebugLn(['CheckProcNode has begin block']);

    // check begin block is only a single assignment
    MoveCursorToNodeStart(Node);
    // read begin
    ReadNextAtom;
    // read 'Result' or 'FunctionName'
    ReadNextAtom;
    if CurPos.Flag<>cafWord then exit;
    if (not UpAtomIs('RESULT'))
    and (CompareIdentifiers(FuncName,@Src[CurPos.StartPos])<>0) then exit;
    // read :=
    ReadNextAtom;
    if not UpAtomIs(':=') then exit;
    // read type cast to result type
    ReadNextAtom;
    if CurPos.Flag<>cafWord then exit;
    if (CompareIdentifiers(ResultType,@Src[CurPos.StartPos])<>0) then exit;
    // read (
    ReadNextAtom;
    if CurPos.Flag<>cafRoundBracketOpen then exit;
    // read parameter
    ReadNextAtom;
    if CurPos.Flag<>cafWord then exit;
    if (CompareIdentifiers(ParamName,@Src[CurPos.StartPos])<>0) then exit;
    // read )
    ReadNextAtom;
    if CurPos.Flag<>cafRoundBracketClose then exit;
    //DebugLn(['CheckProcNode FOUND']);

    // save values
    ResultNodeExt:=NodeExtMemManager.NewNode;
    ResultNodeExt.Txt:=NodeText;
    ResultNodeExt.Node:=NodeExt.Node;
    ResultNodeExt.Data:=ProcNode;
    ResultNodeExt.ExtTxt1:=GetIdentifier(ResultType);
    if TreeOfCodeTreeNodeExt=nil then
      TreeOfCodeTreeNodeExt:=TAVLTree.Create(@CompareCodeTreeNodeExt);
    TreeOfCodeTreeNodeExt.Add(ResultNodeExt);
  end;

var
  Node: TCodeTreeNode;
begin
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  try
    BuildTree(false);
  
    // first step: find all unit identifiers (excluding implementation section)
    if not GatherUnitDefinitions(Definitions,true,true) then exit;

    // now check all functions
    Node:=Tree.Root;
    while Node<>nil do begin
      case Node.Desc of
      ctnInterface, ctnUsesSection, ctnBeginBlock, ctnAsmBlock, ctnProcedureHead,
      ctnTypeSection, ctnConstSection, ctnVarSection, ctnResStrSection:
        Node:=Node.NextSkipChilds;
      ctnProcedure:
        begin
          CheckProcNode(Node);
          Node:=Node.NextSkipChilds;
        end;
      else
        Node:=Node.Next;
      end;
    end;

  finally
    NodeExtMemManager.DisposeAVLTree(Definitions);
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.ReplaceTypeCastFunctions(
  TreeOfCodeTreeNodeExt: TAVLTree; SourceChangeCache: TSourceChangeCache
  ): boolean;
{ replaces public dummy functions with a type.
  The function body will be removed.
  See the function FindTypeCastFunctions.
}
  function IsTypeSectionNeeded(Node: TCodeTreeNode): boolean;
  var
    AVLNode: TAVLTreeNode;
    NodeExt: TCodeTreeNodeExtension;
  begin
    if Node.PriorBrother.Desc=ctnTypeSection then exit(false);
    AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
    while AVLNode<>nil do begin
      NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
      if NodeExt.Node=Node.PriorBrother then begin
        // the function in front will be replaced too
        exit(false);
      end;
      AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
    end;
    Result:=true;
  end;

var
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  DefNode: TCodeTreeNode;
  BodyNode: TCodeTreeNode;
  Expr: String;
  FromPos: LongInt;
  ToPos: LongInt;
  NewSrc: String;
begin
  Result:=false;
  if SourceChangeCache=nil then exit;
  if (TreeOfCodeTreeNodeExt=nil) or (TreeOfCodeTreeNodeExt.Count=0) then
    exit(true);
  SourceChangeCache.MainScanner:=Scanner;

  AVLNode:=TreeOfCodeTreeNodeExt.FindLowest;
  while AVLNode<>nil do begin
    NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
    DebugLn(['TCodeCompletionCodeTool.ReplaceTypeCastFunctions ',NodeExt.Txt]);
    DefNode:=NodeExt.Node;
    BodyNode:=TCodeTreeNode(NodeExt.Data);
    Expr:=NodeExt.ExtTxt1;
    DebugLn(['TCodeCompletionCodeTool.ReplaceTypeCastFunctions Expr=',Expr]);

    // remove body node
    FromPos:=FindLineEndOrCodeInFrontOfPosition(BodyNode.StartPos);
    ToPos:=FindLineEndOrCodeAfterPosition(BodyNode.EndPos);
    if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13]) then begin
      inc(ToPos);
      if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13])
      and (Src[ToPos-1]<>Src[ToPos]) then
        inc(ToPos);
    end;
    DebugLn(['TCodeCompletionCodeTool.ReplaceTypeCastFunctions Body="',copy(Src,FromPos,ToPos-FromPos),'"']);
    SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');

    // replace definition
    FromPos:=DefNode.StartPos;
    ToPos:=DefNode.EndPos;
    if Src[ToPos]=';' then inc(ToPos);// add semicolon
    NewSrc:=GetIndentStr(SourceChangeCache.BeautifyCodeOptions.Indent)
      +ExtractProcName(DefNode,[])+' = '+Expr+';';
    SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,NewSrc);
    // add 'type' keyword
    if IsTypeSectionNeeded(DefNode) then begin
      FromPos:=FindLineEndOrCodeInFrontOfPosition(DefNode.StartPos);
      SourceChangeCache.Replace(gtEmptyLine,gtNewLine,FromPos,FromPos,'type');
    end;

    AVLNode:=TreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
  end;
  Result:=SourceChangeCache.Apply;
end;

function TCodeCompletionCodeTool.MovePointerTypesToTargetSections(
  SourceChangeCache: TSourceChangeCache): boolean;
const
  NodeMovedFlag = 1;
var
  NodeMoves: TCodeGraph;// an edge means, move the FromNode in front of the ToNode
  
  procedure InitNodeMoves;
  begin
    if NodeMoves=nil then
      NodeMoves:=TCodeGraph.Create;
  end;
  
  procedure ClearNodeMoves;
  begin
    FreeAndNil(NodeMoves);
  end;
  
  procedure AddMove(Node, InsertInFrontOf: TCodeTreeNode);
  begin
    if Node=InsertInFrontOf then exit;
    if Node=nil then RaiseException('inconsistency');
    if InsertInFrontOf=nil then RaiseException('inconsistency');
    NodeMoves.AddEdge(Node,InsertInFrontOf);
  end;
  
  function WholeSectionIsMoved(SectionNode: TCodeTreeNode): boolean;
  var
    Node: TCodeTreeNode;
    GraphNode: TCodeGraphNode;
  begin
    Node:=SectionNode.FirstChild;
    while Node<>nil do begin
      GraphNode:=NodeMoves.GetGraphNode(Node,false);
      if (GraphNode=nil) or (GraphNode.OutTreeCount=0) then
        exit(false);
      Node:=Node.NextBrother;
    end;
    Result:=true;
  end;
  
  function ApplyNodeMove(GraphNode: TCodeGraphNode; MoveNode: boolean;
    InsertPos, Indent: integer): boolean;
  // if MoveNode=true then move code of GraphNode.Node to InsertPos
  // Always: move recursively all nodes that should be moved to GraphNode too
  var
    AVLNode: TAVLTreeNode;
    GraphEdge: TCodeGraphEdge;
    Node: TCodeTreeNode;
    FromPos: LongInt;
    ToPos: LongInt;
    NodeSrc: String;
  begin
    Result:=false;
    Node:=GraphNode.Node;
    // marked as moved
    GraphNode.Flags:=NodeMovedFlag;
    DebugLn(['ApplyNodeMoves ',ExtractNode(Node,[])]);
    if MoveNode then begin
      FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
      ToPos:=FindLineEndOrCodeAfterPosition(Node.EndPos);
      NodeSrc:=GetIndentStr(Indent)+Trim(copy(Src,FromPos,ToPos-FromPos));
      // remove
      if (Node.PriorBrother=nil)
      and (Node.Parent<>nil) and (Node.Parent.Desc in AllDefinitionSections)
      and WholeSectionIsMoved(Node.Parent)
      then begin
        // the whole section is moved and this is the first node of the section
        // remove the section header too
        FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.Parent.StartPos);
      end;
      DebugLn(['ApplyNodeMove Remove: "',copy(Src,FromPos,ToPos-FromPos),'"']);
      if not SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'') then exit;
      // insert
      DebugLn(['ApplyNodeMove Insert: "',NodeSrc,'"']);
      if not SourceChangeCache.Replace(gtNewLine,gtNewLine,
        InsertPos,InsertPos,NodeSrc) then exit;
    end;
    // move dependent nodes
    if GraphNode.InTree<>nil then begin
      AVLNode:=GraphNode.InTree.FindLowest;
      while AVLNode<>nil do begin
        GraphEdge:=TCodeGraphEdge(AVLNode.Data);
        if not ApplyNodeMove(GraphEdge.FromNode,true,InsertPos,Indent) then exit;
        AVLNode:=GraphNode.InTree.FindSuccessor(AVLNode);
      end;
    end;
    Result:=true;
  end;
  
  function ApplyNodeMoves(ExceptionOnCircle: boolean): boolean;
  var
    GraphEdge: TCodeGraphEdge;
    ListOfGraphNodes: TFPList;
    i: Integer;
    GraphNode: TCodeGraphNode;
    InsertPos: LongInt;
    Indent: LongInt;
  begin
    Result:=false;
    if NodeMoves.Edges.Count=0 then exit(true);
    
    // check that every node has no more than one destination
    GraphNode:=NodeMoves.FindGraphNodeWithNumberOfOutEdges(2,-1);
    if GraphNode<>nil then begin
      DebugLn(['TCodeCompletionCodeTool.MovePointerTypesToTargetSections.ApplyNodeMoves inconsistency: node should be moved to several places: ',ExtractNode(GraphNode.Node,[])]);
      raise Exception.Create('TCodeCompletionCodeTool.MovePointerTypesToTargetSections.ApplyNodeMoves node should be moved to several places');
    end;
    
    // sort topologically and break all circles
    repeat
      GraphEdge:=NodeMoves.GetTopologicalSortedList(ListOfGraphNodes,true,false,true);
      if GraphEdge=nil then break;
      if ExceptionOnCircle then
        raise Exception.Create('TCodeCompletionCodeTool.MovePointerTypesToTargetSections.ApplyNodeMoves found circle: From='+ExtractNode(GraphEdge.FromNode.Node,[])+' To='+ExtractNode(GraphEdge.ToNode.Node,[]));
      DebugLn(['TCodeCompletionCodeTool.MovePointerTypesToTargetSections.ApplyNodeMoves break circle: From=',ExtractNode(GraphEdge.FromNode.Node,[]),' To=',ExtractNode(GraphEdge.ToNode.Node,[])]);
      NodeMoves.DeleteEdge(GraphEdge);
      ListOfGraphNodes.Free;
    until false;
    
    for i:=0 to ListOfGraphNodes.Count-1 do begin
      GraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
      DebugLn(['ApplyNodeMoves i=',i,' ',ExtractNode(GraphNode.Node,[]),' InFrontCnt=',GraphNode.InTreeCount,' BehindCnt=',GraphNode.OutTreeCount]);
    end;
    
    { apply changes
      the ListOfGraphNodes is sorted topologically with nodes at end must be
      moved first
      For example:
        var AnArray: array[0..EndValue] of char;
        const EndValue = TMyInteger(1);
        type TMyInteger = longint;
      Edges: TMyInteger -> AnArray
             EndValue -> AnArray
      List:
    }
    NodeMoves.ClearNodeFlags;
    for i:=ListOfGraphNodes.Count-1 downto 0 do begin
      GraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
      if GraphNode.Flags=0 then begin
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(GraphNode.Node.StartPos);
        Indent:=GetLineIndent(Src,GraphNode.Node.StartPos);
        if not ApplyNodeMove(GraphNode,false,InsertPos,Indent) then exit;
      end;
    end;
    Result:=SourceChangeCache.Apply;
  end;

var
  Definitions: TAVLTree;// tree of TCodeTreeNodeExtension
  Graph: TCodeGraph;
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  Node: TCodeTreeNode;
  GraphNode: TCodeGraphNode;
  RequiredAVLNode: TAVLTreeNode;
  GraphEdge: TCodeGraphEdge;
  RequiredNode: TCodeTreeNode;
  RequiredTypeNode: TCodeTreeNode;
begin
  Result:=false;
  if (SourceChangeCache=nil) or (Scanner=nil) then exit;
  NodeMoves:=nil;
  Definitions:=nil;
  Graph:=nil;
  try
    // move the pointer types to the same type sections
    if not BuildUnitDefinitionGraph(Definitions,Graph,false) then exit;
    SourceChangeCache.MainScanner:=Scanner;
    if Definitions=nil then exit(true);
    InitNodeMoves;
    
    AVLNode:=Definitions.FindLowest;
    while AVLNode<>nil do begin
      NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
      Node:=NodeExt.Node;
      if (Node.Desc=ctnTypeDefinition) and (Node.FirstChild<>nil)
      and (Node.FirstChild.Desc=ctnPointerType) then begin
        // this is a pointer type
        // check if it only depends on the type nodes of a single section
        //DebugLn(['MovePointerTypesToTargetSections Pointer=',ExtractNode(Node,[])]);
        RequiredTypeNode:=nil;
        GraphNode:=Graph.GetGraphNode(Node,false);
        if GraphNode.OutTree<>nil then begin
          RequiredAVLNode:=GraphNode.OutTree.FindLowest;
          while RequiredAVLNode<>nil do begin
            GraphEdge:=TCodeGraphEdge(RequiredAVLNode.Data);
            RequiredNode:=GraphEdge.ToNode.Node;
            if (RequiredNode.Desc=ctnTypeDefinition)
            and (RequiredNode.Parent.Desc=ctnTypeSection) then begin
              //DebugLn(['MovePointerTypesToTargetSections required=',ExtractNode(RequiredNode,[])]);
              if RequiredTypeNode=nil then begin
                RequiredTypeNode:=RequiredNode;
              end
              else if RequiredTypeNode.Parent<>RequiredNode.Parent then begin
                DebugLn(['MovePointerTypesToTargetSections required nodes in different type sections']);
                RequiredTypeNode:=nil;
                break;
              end;
            end else begin
              DebugLn(['MovePointerTypesToTargetSections required nodes are not only types']);
              RequiredTypeNode:=nil;
              break;
            end;
            RequiredAVLNode:=GraphNode.OutTree.FindSuccessor(RequiredAVLNode);
          end;
        end;
        if (RequiredTypeNode<>nil) then begin
          // this pointer type depends only on the type nodes of a single type
          // section
          if (Node.Parent<>RequiredNode.Parent) then begin
            // pointer type is in other section => move
            DebugLn(['MovePointerTypesToTargetSections move Pointer=',ExtractNode(Node,[]),' Required=',ExtractNode(RequiredNode,[])]);
            AddMove(Node,RequiredNode);
          end;
        end;
      end;
      AVLNode:=Definitions.FindSuccessor(AVLNode);
    end;
    Result:=ApplyNodeMoves(false);
  finally
    NodeExtMemManager.DisposeAVLTree(Definitions);
    Graph.Free;
    ClearNodeMoves;
  end;
end;

function TCodeCompletionCodeTool.FixForwardDefinitions(
  SourceChangeCache: TSourceChangeCache): boolean;

  function UpdateGraph(var Definitions: TAVLTree; var Graph: TCodeGraph;
    Rebuild: boolean): boolean;
  begin
    if Definitions<>nil then begin
      NodeExtMemManager.DisposeAVLTree(Definitions);
      Definitions:=nil;
    end;
    if Graph<>nil then begin
      Graph.Free;
      Graph:=nil;
    end;
    if Rebuild then
      Result:=BuildUnitDefinitionGraph(Definitions,Graph,true)
    else
      Result:=true;
  end;

  function CreateTypeSectionForCircle(CircleOfGraphNodes: TFPList;
    var Definitions: TAVLTree; var Graph: TCodeGraph): boolean;
  // CircleOfGraphNodes is a list of TCodeGraphNode that should be moved
  // to a new type section
  
    function IndexOfNode(Node: TCodeTreeNode): integer;
    begin
      Result:=CircleOfGraphNodes.Count-1;
      while (Result>=0)
      and (TCodeGraphNode(CircleOfGraphNodes[Result]).Node<>Node) do
        dec(Result);
    end;
  
  var
    i: Integer;
    GraphNode: TCodeGraphNode;
    Node: TCodeTreeNode;
    NewTxt: String;
    EndGap: TGapTyp;
    InsertPos: LongInt;
    Indent: LongInt;
    FromPos: LongInt;
    ToPos: LongInt;
  begin
    // check if whole type sections are moved and combine them
    i:=CircleOfGraphNodes.Count-1;
    while i>=0 do begin
      GraphNode:=TCodeGraphNode(CircleOfGraphNodes[i]);
      Node:=GraphNode.Node;
      if Node.Parent.Desc=ctnTypeSection then begin
        if IndexOfNode(Node.Parent)>=0 then begin
          // the whole type section of this type will be moved
          // => remove this type
          CircleOfGraphNodes.Delete(i);
        end else begin
          // check if all types of this type section will be moved
          Node:=Node.Parent.FirstChild;
          while (Node<>nil) and (IndexOfNode(Node)>=0) do
            Node:=Node.NextBrother;
          if Node=nil then begin
            // all types of this type section will be moved
            // => remove the type and add the type section instead
            CircleOfGraphNodes.Delete(i);
            CircleOfGraphNodes.Add(Graph.AddGraphNode(GraphNode.Node.Parent));
          end;
        end;
      end;
      dec(i);
    end;
  
    // create new type section
    // Note: InsertPos must be outside the types and type sections which are moved
    GraphNode:=TCodeGraphNode(CircleOfGraphNodes[0]);
    Node:=GraphNode.Node;
    if Node.Parent.Desc=ctnTypeSection then
      Node:=Node.Parent;
    InsertPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
    Indent:=GetLineIndent(Src,Node.StartPos);
    SourceChangeCache.Replace(gtEmptyLine,gtNewLine,InsertPos,InsertPos,
      GetIndentStr(Indent)+'type');
    inc(Indent,SourceChangeCache.BeautifyCodeOptions.Indent);
    // move the types
    for i:=0 to CircleOfGraphNodes.Count-1 do begin
      GraphNode:=TCodeGraphNode(CircleOfGraphNodes[i]);
      Node:=GraphNode.Node;
      if i=CircleOfGraphNodes.Count-1 then
        EndGap:=gtEmptyLine
      else
        EndGap:=gtNewLine;
      if Node.Desc=ctnTypeSection then begin
        // remove type section
        FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
        ToPos:=FindLineEndOrCodeAfterPosition(Node.EndPos,true);
        DebugLn(['CreateTypeSectionForCircle Removing type section: ',ExtractCode(FromPos,ToPos,[])]);
        SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');
        // add all types of type section to new type section
        if Node.FirstChild<>nil then begin
          FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.FirstChild.StartPos);
          ToPos:=FindLineEndOrCodeAfterPosition(Node.LastChild.EndPos);
          NewTxt:=GetIndentStr(Indent)+ExtractCode(FromPos,ToPos,[phpWithComments]);
          DebugLn(['CreateTypeSectionForCircle Adding types: ',NewTxt]);
          SourceChangeCache.Replace(gtNewLine,EndGap,InsertPos,InsertPos,NewTxt);
        end;
      end else if Node.Desc in [ctnTypeDefinition,ctnGenericType] then begin
        // remove type
        FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
        ToPos:=FindLineEndOrCodeAfterPosition(Node.EndPos);
        DebugLn(['CreateTypeSectionForCircle Removing node: ',ExtractCode(FromPos,ToPos,[])]);
        SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');
        // add type to new type section
        NewTxt:=GetIndentStr(Indent)+ExtractNode(Node,[phpWithComments]);
        DebugLn(['CreateTypeSectionForCircle Adding type: ',NewTxt]);
        SourceChangeCache.Replace(gtNewLine,EndGap,InsertPos,InsertPos,NewTxt);
      end else
        raise Exception.Create('inconsistency');
    end;
    // apply changes
    Result:=SourceChangeCache.Apply;
    if not Result then exit;
    // rebuild graph
    Result:=UpdateGraph(Definitions,Graph,true);
  end;

  function FixCircle(var Definitions: TAVLTree;
    var Graph: TCodeGraph; CircleNode: TCodeGraphNode): boolean;
  var
    CircleOfGraphNodes: TFPList; // list of TCodeGraphNode

    procedure RaiseCanNotFixCircle(const Msg: string);
    var
      i: Integer;
      GraphNode: TCodeGraphNode;
      s: String;
    begin
      DebugLn(['RaiseCanNotFixCircle Msg="',Msg,'"']);
      s:='Can not auto fix a circle in definitions: '+Msg;
      for i:=0 to CircleOfGraphNodes.Count-1 do begin
        GraphNode:=TCodeGraphNode(CircleOfGraphNodes[i]);
        DebugLn(['  ',i,': ',GetRedefinitionNodeText(GraphNode.Node)]);
      end;
      raise Exception.Create(s);
    end;
    
  var
    i: Integer;
    GraphNode: TCodeGraphNode;
    ParentNode: TCodeTreeNode;
    Node: TCodeTreeNode;
    NeedsMoving: Boolean;
  begin
    Result:=false;
    CircleOfGraphNodes:=nil;
    try
      // get all nodes of this CircleOfGraphNodes
      Graph.GetMaximumCircle(CircleNode,CircleOfGraphNodes);
      // check if all nodes are types
      for i:=0 to CircleOfGraphNodes.Count-1 do begin
        GraphNode:=TCodeGraphNode(CircleOfGraphNodes[i]);
        if not (GraphNode.Node.Desc in [ctnTypeDefinition,ctnGenericType])
        then begin
          RaiseCanNotFixCircle('Only types can build circles, not '+GraphNode.Node.DescAsString);
        end;
      end;
      NeedsMoving:=false;
      // check if the whole type CircleOfGraphNodes has one parent
      ParentNode:=TCodeGraphNode(CircleOfGraphNodes[0]).Node.Parent;
      for i:=1 to CircleOfGraphNodes.Count-1 do begin
        GraphNode:=TCodeGraphNode(CircleOfGraphNodes[i]);
        if GraphNode.Node.Parent<>ParentNode then begin
          DebugLn(['FixCircle circle is not yet in one type section -> needs moving']);
          NeedsMoving:=true;
          break;
        end;
      end;
      // check if the parent only contains the CircleOfGraphNodes nodes
      if not NeedsMoving then begin
        Node:=ParentNode.FirstChild;
        while Node<>nil do begin
          i:=CircleOfGraphNodes.Count-1;
          while (i>=0) and (TCodeGraphNode(CircleOfGraphNodes[i]).Node<>Node) do dec(i);
          if i<0 then begin
            DebugLn(['FixCircle circle has not yet its own type section -> needs moving']);
            NeedsMoving:=true;
            break;
          end;
          Node:=Node.NextBrother;
        end;
      end;
      
      if NeedsMoving then begin
        DebugLn(['TCodeCompletionCodeTool.FixForwardDefinitions.FixCircle moving types into one type section']);
        Result:=CreateTypeSectionForCircle(CircleOfGraphNodes,Definitions,Graph);
        exit;
      end else begin
        // remove definitions nodes and use the type section instead
        DebugLn(['FixCircle already ok']);
        Graph.CombineNodes(CircleOfGraphNodes,Graph.GetGraphNode(ParentNode,true));
      end;

    finally
      CircleOfGraphNodes.Free;
    end;
    Result:=true;
  end;
  
  function CheckCircles(var Definitions: TAVLTree;
    var Graph: TCodeGraph): boolean;
  var
    ListOfGraphNodes: TFPList;
    CircleEdge: TCodeGraphEdge;
  begin
    Result:=false;
    ListOfGraphNodes:=nil;
    try
      Graph.DeleteSelfCircles;
      repeat
        //WriteCodeGraphDebugReport(Graph);
        CircleEdge:=Graph.GetTopologicalSortedList(ListOfGraphNodes,true,false,false);
        if CircleEdge=nil then break;
        DebugLn(['FixForwardDefinitions.CheckCircles Circle found containing ',
          GetRedefinitionNodeText(CircleEdge.FromNode.Node),
          ' and ',
          GetRedefinitionNodeText(CircleEdge.ToNode.Node)]);
        if not FixCircle(Definitions,Graph,CircleEdge.FromNode) then exit;
      until false;
    finally
      ListOfGraphNodes.Free;
    end;
    Result:=true;
  end;
  
  function MoveNodes(TreeOfNodeMoveEdges: TAVLTree): boolean;
  // TreeOfNodeMoveEdges is a tree of TNodeMoveEdge
  // it is sorted for insert position (i.e. left node must be inserted
  //   in front of right node)
  
    function NodeWillBeMoved(Node: TCodeTreeNode): boolean;
    var
      AVLNode: TAVLTreeNode;
      CurMove: TNodeMoveEdge;
      GraphNode: TCodeGraphNode;
    begin
      AVLNode:=TreeOfNodeMoveEdges.FindLowest;
      while AVLNode<>nil do begin
        CurMove:=TNodeMoveEdge(AVLNode.Data);
        GraphNode:=CurMove.GraphNode;
        if GraphNode.Node=Node then exit(true);
        AVLNode:=TreeOfNodeMoveEdges.FindSuccessor(AVLNode);
      end;
      Result:=false;
    end;
    
    function GetFirstVarDefSequenceNode(Node: TCodeTreeNode): TCodeTreeNode;
    begin
      while (Node.PriorBrother<>nil) and (Node.PriorBrother.FirstChild=nil) do
        Node:=Node.PriorBrother;
      Result:=Node;
    end;

    function GetLastVarDefSequenceNode(Node: TCodeTreeNode): TCodeTreeNode;
    begin
      Result:=nil;
      while (Node<>nil) do begin
        Result:=Node;
        if (Node.FirstChild<>nil) then break;
        Node:=Node.NextBrother;
      end;
    end;

    function WholeVarDefSequenceWillBeMoved(Node: TCodeTreeNode): boolean;
    // test, if all variable definitions of a sequence will be moved
    // example: var a,b,c: integer;
    begin
      Node:=GetFirstVarDefSequenceNode(Node);
      while (Node<>nil) do begin
        if not NodeWillBeMoved(Node) then exit(false);
        if (Node.FirstChild<>nil) then break;// this is the last of the sequence
        Node:=Node.NextBrother;
      end;
      Result:=true;
    end;
    
    function WholeSectionWillBeMoved(Node: TCodeTreeNode): boolean;
    // test, if all child nodes will be moved
    begin
      Node:=Node.FirstChild;
      while (Node<>nil) do begin
        if not NodeWillBeMoved(Node) then exit(false);
        Node:=Node.NextBrother;
      end;
      Result:=true;
    end;
  
  var
    AVLNode: TAVLTreeNode;
    CurMove: TNodeMoveEdge;
    GraphNode: TCodeGraphNode;// move what
    PosGraphNode: TCodeGraphNode;// move where (in front of)
    Node: TCodeTreeNode;
    FromPos: LongInt;
    ToPos: LongInt;
    DestNode: TCodeTreeNode;
    NextAVLNode: TAVLTreeNode;
    NextMove: TNodeMoveEdge;
    NextGraphNode: TCodeGraphNode;// move what next
    NextPosGraphNode: TCodeGraphNode;// move where next (in front of)
    NextInsertAtSamePos: boolean;
    NeedSection: TCodeTreeNodeDesc;
    LastSection: TCodeTreeNodeDesc;
    LastInsertAtSamePos: boolean;
    InsertPos: LongInt;
    Indent: LongInt;
    DestSection: TCodeTreeNodeDesc;
    NewTxt: String;
    DestNodeInFront: TCodeTreeNode;
  begin
    Result:=false;
    AVLNode:=TreeOfNodeMoveEdges.FindLowest;
    LastSection:=ctnNone;
    LastInsertAtSamePos:=false;
    DestNode:=nil;
    DestSection:=ctnNone;
    // process every move
    while AVLNode<>nil do begin
      CurMove:=TNodeMoveEdge(AVLNode.Data);
      GraphNode:=CurMove.GraphNode;// move what
      PosGraphNode:=TCodeGraphNode(GraphNode.Data);// move where (in front of)
      NextAVLNode:=TreeOfNodeMoveEdges.FindSuccessor(AVLNode);
      if NextAVLNode<>nil then begin
        NextMove:=TNodeMoveEdge(NextAVLNode.Data);
        NextGraphNode:=NextMove.GraphNode;// move what next
        NextPosGraphNode:=TCodeGraphNode(NextGraphNode.Data);// move where next
        NextInsertAtSamePos:=NextPosGraphNode=PosGraphNode;
      end else begin
        NextInsertAtSamePos:=false;
      end;
      DebugLn(['MoveNodes: move ',
        GetRedefinitionNodeText(GraphNode.Node),' ',CleanPosToStr(GraphNode.Node.StartPos),
        ' (TopoLvl=',CurMove.TologicalLevel,')',
        ' in front of ',GetRedefinitionNodeText(PosGraphNode.Node),' ',CleanPosToStr(PosGraphNode.Node.StartPos)
        ]);
      Node:=GraphNode.Node;
      DestNode:=PosGraphNode.Node;
      
      // remove node
      if (Node.Parent<>nil)
      and (Node.Parent.Desc in AllDefinitionSections)
      and WholeSectionWillBeMoved(Node.Parent) then begin
        // the whole type/var/const section will be moved
        if Node.PriorBrother=nil then begin
          // this is the first node of the section
          // => remove the whole section
          FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.Parent.StartPos);
          ToPos:=FindLineEndOrCodeAfterPosition(Node.Parent.EndPos,true);
        end else begin
          // this is not the first node of the section
          // => remove nothing
          FromPos:=0;
          ToPos:=0;
        end;
      end
      else if Node.Desc=ctnVarDefinition then begin
        // removing a variable definition can be tricky, because for example
        // var a,b,c: integer;
        if Node.FirstChild<>nil then begin
          // this is the last of a sequence
          if WholeVarDefSequenceWillBeMoved(Node) then begin
            // the whole variable definition will be moved
            // and this is the last of the sequence
            // => remove the whole definition (names and type)
            FromPos:=FindLineEndOrCodeInFrontOfPosition(
                                     GetFirstVarDefSequenceNode(Node).StartPos);
            ToPos:=FindLineEndOrCodeAfterPosition(
                                   GetLastVarDefSequenceNode(Node).EndPos,true);
          end else if NodeWillBeMoved(Node.PriorBrother) then begin
            // this is for example: var a,b,c: integer
            // and only b and c will be moved. The b, plus the space behind was
            // already marked for removal
            // => remove the c and the space behind
            FromPos:=Node.StartPos;
            MoveCursorToNodeStart(Node);
            ReadNextAtom;// read identifier
            AtomIsIdentifier(true);
            ToPos:=FindLineEndOrCodeAfterPosition(CurPos.EndPos,true);
          end else begin
            // this is for example: var a,b: integer
            // and only b will be moved.
            // => remove ,b plus the space behind
            MoveCursorToNodeStart(Node.PriorBrother);
            ReadNextAtom;// read identifier
            AtomIsIdentifier(true);
            ReadNextAtom;// read comma
            if not AtomIsChar(',') then RaiseCharExpectedButAtomFound(',');
            FromPos:=CurPos.StartPos;
            ReadNextAtom;// read identifier
            AtomIsIdentifier(true);
            ReadNextAtom;//read colon
            if not AtomIsChar(':') then RaiseCharExpectedButAtomFound(':');
            ToPos:=CurPos.StartPos;
          end;
        end else begin
          // this is not the last of a sequence
          if WholeVarDefSequenceWillBeMoved(Node) then begin
            // the whole sequence will be moved. This is done by the last node.
            // => nothing to do
            FromPos:=0;
            ToPos:=0;
          end else begin
            // remove the b,
            FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
            MoveCursorToNodeStart(Node);
            ReadNextAtom;// read identifier
            AtomIsIdentifier(true);
            ReadNextAtom;// read comma
            if not AtomIsChar(',') then RaiseCharExpectedButAtomFound(',');
            ToPos:=CurPos.StartPos;
          end;
        end;
      end else begin
        // remove the whole node
        FromPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
        ToPos:=FindLineEndOrCodeAfterPosition(Node.EndPos);
      end;
      if ToPos>FromPos then begin
        DebugLn(['MoveNodes remove "',ExtractCode(FromPos,ToPos,[]),'"']);
        if not SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'') then
          exit;
      end;

      // find needed section type
      if Node.Desc in AllIdentifierDefinitions then
        NeedSection:=Node.Parent.Desc
      else
        NeedSection:=ctnNone;

      // find insert position
      if not LastInsertAtSamePos then begin
        //DebugLn(['MoveNodes LastInsertAtSamePos=false, compute destination ...']);
        if (DestNode.Desc in AllIdentifierDefinitions) then begin
          DestNode:=GetFirstVarDefSequenceNode(DestNode);
          DestSection:=DestNode.Parent.Desc;
          if DestNode.PriorBrother<>nil then begin
            // the destination is in front of a definition, but in the middle
            // of a section
            // example: type a=char; | b=byte;
            // => insert in front of destination
            //DebugLn(['MoveNodes destination is middle of a section. Node in front=',GetRedefinitionNodeText(DestNode.PriorBrother)]);
          end else begin
            // the destination is the first node of a section
            // example: type | t=char;
            if NeedSection=DestSection then begin
              // insertion needs the same section type
              // => insert in front of destination
            end else begin
              // insertion needs another section type
              // => insert in front of the section
              DestNode:=DestNode.Parent;
            end;
            //DebugLn(['MoveNodes destination is first node of a section ']);
          end;
        end else begin
          // the destination is not in a section
          // example: in front of a type section
          // => insert in front of destination
          // find the section in front
          DestNodeInFront:=DestNode.PriorBrother;
          while (DestNodeInFront<>nil) and NodeWillBeMoved(DestNodeInFront) do
            DestNodeInFront:=DestNodeInFront.PriorBrother;
          if (DestNodeInFront<>nil)
          and (DestNodeInFront.Desc in AllDefinitionSections) then
            DestSection:=DestNodeInFront.Desc
          else
            DestSection:=ctnNone;
          //DebugLn(['MoveNodes destination is not in a section']);
        end;
        InsertPos:=FindLineEndOrCodeAfterPosition(DestNode.StartPos);
        Indent:=GetLineIndent(Src,DestNode.StartPos);
        //DebugLn(['MoveNodes DestNode=',GetRedefinitionNodeText(DestNode),':',DestNode.DescAsString,' DestSection=',NodeDescToStr(DestSection)]);
      end;
      
      // start a new section if needed
      //DebugLn(['MoveNodes LastInsertAtSamePos=',LastInsertAtSamePos,' NeedSection=',NodeDescToStr(NeedSection),' LastSection=',NodeDescToStr(LastSection),' DestSection=',NodeDescToStr(DestSection)]);
      if (LastInsertAtSamePos and (NeedSection<>LastSection))
      or ((not LastInsertAtSamePos) and (NeedSection<>DestSection)) then begin
        // start a new section
        case NeedSection of
        ctnVarSection: NewTxt:='var';
        ctnConstSection: NewTxt:='const';
        ctnResStrSection: NewTxt:='resourcestring';
        ctnTypeSection: NewTxt:='type';
        ctnLabelSection: NewTxt:='label';
        else NewTxt:='';
        end;
        if NewTxt<>'' then begin
          DebugLn(['MoveNodes start new section: insert "',NewTxt,'"']);
          if not SourceChangeCache.Replace(gtEmptyLine,gtNewLine,
                                           InsertPos,InsertPos,NewTxt)
          then
            exit;
          Indent:=SourceChangeCache.BeautifyCodeOptions.Indent;
        end;
      end;

      // insert node
      if Node.Desc=ctnVarDefinition then begin
        NewTxt:=GetIdentifier(@Src[Node.StartPos]);
        MoveCursorToNodeStart(GetLastVarDefSequenceNode(Node));
        ReadNextAtom;
        AtomIsIdentifier(true);
        ReadNextAtom;
        if not AtomIsChar(':') then RaiseCharExpectedButAtomFound(':');
        FromPos:=CurPos.StartPos;
        ToPos:=Node.EndPos;
        NewTxt:=NewTxt+ExtractCode(FromPos,ToPos,[phpWithComments]);
      end else begin
        FromPos:=Node.StartPos;
        ToPos:=FindLineEndOrCodeAfterPosition(Node.EndPos);
        NewTxt:=ExtractCode(FromPos,ToPos,[phpWithComments]);
      end;
      NewTxt:=GetIndentStr(Indent)+NewTxt;
      DebugLn(['MoveNodes insert "',NewTxt,'"']);
      if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
        NewTxt) then exit;

      // restore destination section if needed
      if not NextInsertAtSamePos then begin
        // this was the last insertion at this destination
        if (NeedSection<>DestSection)
        and (DestSection in AllDefinitionSections) then begin
          // restore the section of destination
          case DestSection of
          ctnVarDefinition: NewTxt:='var';
          ctnConstDefinition: NewTxt:='const';
          ctnResStrSection: NewTxt:='resourcestring';
          ctnTypeSection: NewTxt:='type';
          ctnLabelSection: NewTxt:='label';
          else NewTxt:='';
          end;
          if NewTxt<>'' then begin
            DebugLn(['MoveNodes restore destination  section: insert "',NewTxt,'"']);
            if not SourceChangeCache.Replace(gtEmptyLine,gtNewLine,
                                             InsertPos,InsertPos,NewTxt)
            then
              exit;
          end;
        end;
      end;

      LastSection:=NeedSection;
      LastInsertAtSamePos:=NextInsertAtSamePos;
      AVLNode:=NextAVLNode;
    end;
    Result:=SourceChangeCache.Apply;
  end;

  function CheckOrder(var Definitions: TAVLTree;
    var Graph: TCodeGraph): boolean;
  // sort definitions topologically in source
  // the Graph must be acyclic
  var
    ListOfGraphNodes: TFPList;
    CircleEdge: TCodeGraphEdge;
    i: Integer;
    GraphNode: TCodeGraphNode;
    AVLNode: TAVLTreeNode;
    UsedByGraphNode: TCodeGraphNode;
    PosGraphNode: TCodeGraphNode;
    PosUsedByGraphNode: TCodeGraphNode;
    NodeMoveEdges: TAVLTree;
    NewMoveEdge: TNodeMoveEdge;
  begin
    Result:=false;
    ListOfGraphNodes:=nil;
    NodeMoveEdges:=TAVLTree.Create(@CompareNodeMoveEdges);
    try
      //WriteCodeGraphDebugReport(Graph);
      
      // create a topologically sorted list
      CircleEdge:=Graph.GetTopologicalSortedList(ListOfGraphNodes,false,true,false);
      if CircleEdge<>nil then
        raise Exception.Create('not acyclic');

      { set the GraphNode.Data to those GraphNodes leaves
        with the lowest Node.StartPos
        For example:
          var AnArray: array[0..EndValue] of char;
          const EndValue = TMyInteger(1);
          type TMyInteger = integer;
        EndValue must be moved in front of AnArray
        and TMyInteger must be moved in front of EndValue and AnArray.
        The topological list gives:
          TMyInteger
          EndValue
          AnArray
        NOTE: topological order alone can not be used,
          because unrelated definitions will be mixed somehow.
      }
      // init the destinations
      for i:=0 to ListOfGraphNodes.Count-1 do begin
        GraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
        //DebugLn(['CheckOrder ',GetRedefinitionNodeText(GraphNode.Node)]);
        GraphNode.Data:=GraphNode;
      end;
      // calculate the destinations as minimum of all dependencies
      for i:=ListOfGraphNodes.Count-1 downto 0 do begin
        GraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
        if GraphNode.InTree<>nil then begin
          AVLNode:=GraphNode.InTree.FindLowest;
          while AVLNode<>nil do begin
            UsedByGraphNode:=TCodeGraphEdge(AVLNode.Data).FromNode;
            // for example: type TMyPointer = TMyInteger;
            // GraphNode.Node is TMyInteger
            // UsedByGraphNode.Node is TMyPointer
            //DebugLn(['CheckOrder GraphNode=',GetRedefinitionNodeText(GraphNode.Node),' UsedBy=',GetRedefinitionNodeText(UsedByGraphNode.Node)]);
            PosGraphNode:=TCodeGraphNode(GraphNode.Data);
            PosUsedByGraphNode:=TCodeGraphNode(UsedByGraphNode.Data);
            if PosGraphNode.Node.StartPos>PosUsedByGraphNode.Node.StartPos then
              GraphNode.Data:=PosUsedByGraphNode;
            AVLNode:=GraphNode.InTree.FindSuccessor(AVLNode);
          end;
        end;
      end;
      // create the list of moves
      // sorted for: 1. destination position,
      //             2. topological level,
      //             3. origin position in source
      for i:=0 to ListOfGraphNodes.Count-1 do begin
        GraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
        PosGraphNode:=TCodeGraphNode(GraphNode.Data);
        if GraphNode<>PosGraphNode then begin
          DebugLn(['CheckOrder Move: ',
            GetRedefinitionNodeText(GraphNode.Node),' ',CleanPosToStr(GraphNode.Node.StartPos),
            ' TopoLvl=',GraphNode.Flags,
            ' in front of ',GetRedefinitionNodeText(PosGraphNode.Node),' ',CleanPosToStr(PosGraphNode.Node.StartPos)
            ]);
          NewMoveEdge:=TNodeMoveEdge.Create;
          NewMoveEdge.GraphNode:=GraphNode;
          NewMoveEdge.DestPos:=PosGraphNode.Node.StartPos;
          NewMoveEdge.TologicalLevel:=GraphNode.Flags;
          NewMoveEdge.SrcPos:=GraphNode.Node.StartPos;
          NodeMoveEdges.Add(NewMoveEdge);
        end;
      end;
      
      Result:=MoveNodes(NodeMoveEdges);
    finally
      NodeMoveEdges.FreeAndClear;
      NodeMoveEdges.Free;
      ListOfGraphNodes.Free;
    end;
  end;
  
var
  Definitions: TAVLTree;
  Graph: TCodeGraph;
begin
  Result:=false;
  if (SourceChangeCache=nil) or (Scanner=nil) then exit;
  Definitions:=nil;
  Graph:=nil;
  try
    // Workaround:
    // move the pointer types to the same type sections
    //if not MovePointerTypesToTargetSections(SourceChangeCache) then exit;
    //exit(true);
    
    if not BuildUnitDefinitionGraph(Definitions,Graph,true) or (Graph=nil) then
      exit;
    SourceChangeCache.MainScanner:=Scanner;
    // fix circles
    if not CheckCircles(Definitions,Graph) then exit;
    // now the graph is acyclic and nodes can be moved
    if not CheckOrder(Definitions,Graph) then exit;
  finally
    UpdateGraph(Definitions,Graph,false);
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.GatherUnitDefinitions(out
  TreeOfCodeTreeNodeExt: TAVLTree;
  OnlyInterface, ExceptionOnRedefinition: boolean): boolean;
  
  procedure RaiseRedefinition(Node1, Node2: TCodeTreeNode);
  begin
    raise Exception.Create('redefinition found: '+GetRedefinitionNodeText(Node1)
      +' at '+CleanPosToStr(Node1.StartPos)
      +' and at '+CleanPosToStr(Node2.StartPos));
  end;

  procedure AddDefinition(Node: TCodeTreeNode);
  var
    NodeExt: TCodeTreeNodeExtension;
    NodeText: String;
  begin
    NodeText:=GetRedefinitionNodeText(Node);
    NodeExt:=FindCodeTreeNodeExt(TreeOfCodeTreeNodeExt,NodeText);
    if NodeExt<>nil then begin
      if NodeIsForwardProc(NodeExt.Node)
      and (not NodeIsForwardProc(Node)) then begin
        // this is the procedure body of the forward definition -> skip
        exit;
      end;
      if ExceptionOnRedefinition then
        RaiseRedefinition(NodeExt.Node,Node);
    end;
    NodeExt:=NodeExtMemManager.NewNode;
    NodeExt.Txt:=NodeText;
    TreeOfCodeTreeNodeExt.Add(NodeExt);
    NodeExt.Node:=Node;
  end;

var
  Node: TCodeTreeNode;
begin
  Result:=false;
  TreeOfCodeTreeNodeExt:=nil;
  BuildTree(OnlyInterface);

  // find all unit identifiers (excluding sub types)
  TreeOfCodeTreeNodeExt:=TAVLTree.Create(@CompareCodeTreeNodeExt);
  Node:=Tree.Root;
  while Node<>nil do begin
    case Node.Desc of
    ctnProcedureHead, ctnParameterList, ctnInitialization, ctnFinalization,
    ctnBeginBlock, ctnAsmBlock:
      Node:=Node.NextSkipChilds;
    ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition,ctnEnumIdentifier,
    ctnGenericType:
      begin
        // add or update definition
        AddDefinition(Node);

        if (Node.Desc=ctnTypeDefinition)
        and (Node.FirstChild<>nil)
        and (Node.FirstChild.Desc=ctnEnumerationType) then
          Node:=Node.FirstChild
        else
          Node:=Node.NextSkipChilds;
      end;
    ctnProcedure:
      begin
        AddDefinition(Node);
        Node:=Node.NextSkipChilds;
      end;
    else
      if OnlyInterface and (Node.Desc=ctnImplementation) then
        break;
      Node:=Node.Next;
    end;
  end;
  
  Result:=true;
end;

function TCodeCompletionCodeTool.BuildUnitDefinitionGraph(out
  DefinitionsTreeOfCodeTreeNodeExt: TAVLTree; out Graph: TCodeGraph;
  OnlyInterface: boolean): boolean;
  
  procedure CheckRange(Node: TCodeTreeNode; FromPos, ToPos: integer);
  // search the range for defined identifiers
  // and add edges to graph
  var
    Identifier: String;
    NodeExt: TCodeTreeNodeExtension;
  begin
    if (FromPos>=ToPos) or (FromPos<1) then exit;
    //DebugLn(['CheckRange Range="',dbgstr(Src[FromPos..ToPos-1]),'"']);
    MoveCursorToCleanPos(FromPos);
    repeat
      ReadNextAtom;
      if (CurPos.StartPos>=ToPos) or (CurPos.StartPos>SrcLen) then break;
      if AtomIsIdentifier(false) then begin
        Identifier:=GetAtom;
        NodeExt:=FindCodeTreeNodeExt(DefinitionsTreeOfCodeTreeNodeExt,
                                     Identifier);
        if NodeExt<>nil then begin
          if Graph=nil then
            Graph:=TCodeGraph.Create;
          //if Graph.GetEdge(Node,NodeExt.Node,false)=nil then
          //  DebugLn(['CheckRange AddEdge: ',GetRedefinitionNodeText(Node),' uses ',GetRedefinitionNodeText(NodeExt.Node)]);
          Graph.AddEdge(Node,NodeExt.Node);
        end;
      end;
    until false;
  end;
  
  procedure CheckSubNode(Node, SubNode: TCodeTreeNode);
  var
    ProcHead: TCodeTreeNode;
    ParamList: TCodeTreeNode;
    ChildNode: TCodeTreeNode;
    FunctionResult: TCodeTreeNode;
  begin
    //DebugLn(['CheckSubNode ',GetRedefinitionNodeText(Node),' ',GetRedefinitionNodeText(SubNode)]);
    case SubNode.Desc of
    
    ctnTypeDefinition,ctnVarDefinition,ctnGenericType,ctnConstDefinition:
      begin
        ChildNode:=FindTypeNodeOfDefinition(SubNode);
        if ChildNode<>nil then begin
          CheckSubNode(Node,ChildNode);
        end else if SubNode.Desc=ctnConstDefinition then begin
          CheckRange(Node,ChildNode.StartPos,SubNode.EndPos);
        end;
      end;
      
    ctnProcedure:
      begin
        BuildSubTreeForProcHead(SubNode,FunctionResult);
        ProcHead:=SubNode.FirstChild;
        ParamList:=ProcHead.FirstChild;
        if ParamList<>nil then begin
          ChildNode:=ParamList.FirstChild;
          while ChildNode<>nil do begin
            if (ChildNode.Desc=ctnVarDefinition) and (ChildNode.FirstChild<>nil)
            then begin
              CheckSubNode(Node,ChildNode);
            end;
            ChildNode:=ChildNode.NextBrother;
          end;
        end;
        if FunctionResult<>nil then begin
          CheckRange(Node,FunctionResult.StartPos,
                     FunctionResult.StartPos
                     +GetIdentLen(@Src[FunctionResult.StartPos]));
        end;
      end;

    ctnRecordType, ctnClassInterface, ctnClass:
      begin
        ChildNode:=SubNode.FirstChild;
        while (ChildNode<>nil) and (ChildNode.HasAsParent(SubNode)) do begin
          if ChildNode.Desc in AllIdentifierDefinitions then begin
            CheckSubNode(Node,ChildNode);
            ChildNode:=ChildNode.NextSkipChilds;
          end else
            ChildNode:=ChildNode.Next;
        end;
      end;
      
    else
      CheckRange(Node,SubNode.StartPos,SubNode.Parent.EndPos);
    end;
  end;
  
var
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  Node: TCodeTreeNode;
begin
  Result:=false;
  DefinitionsTreeOfCodeTreeNodeExt:=nil;
  Graph:=nil;
  if not GatherUnitDefinitions(DefinitionsTreeOfCodeTreeNodeExt,false,true) then
    exit;
  if DefinitionsTreeOfCodeTreeNodeExt=nil then exit(true);
  
  AVLNode:=DefinitionsTreeOfCodeTreeNodeExt.FindLowest;
  while AVLNode<>nil do begin
    NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
    Node:=NodeExt.Node;
    CheckSubNode(Node,Node);
    AVLNode:=DefinitionsTreeOfCodeTreeNodeExt.FindSuccessor(AVLNode);
  end;

  Result:=true;
end;

procedure TCodeCompletionCodeTool.WriteCodeGraphDebugReport(Graph: TCodeGraph);

  function NodeToStr(Node: TCodeTreeNode): string;
  begin
    case Node.Desc of
    ctnProcedure:
      Result:=ExtractProcHead(Node,[phpInUpperCase,phpWithoutSemicolon]);
    ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition,ctnEnumIdentifier,
    ctnGenericType:
      Result:=ExtractDefinitionName(Node);
    else
      Result:=Node.DescAsString;
    end;
    Result:=Result+'{'+CleanPosToStr(Node.StartPos)+'}';
  end;

var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
  Node: TCodeTreeNode;
  Cnt: LongInt;
  EdgeAVLNode: TAVLTreeNode;
  Edge: TCodeGraphEdge;
begin
  DebugLn(['TCodeCompletionCodeTool.WriteCodeGraphDebugReport ',DbgSName(Graph),
    ' NodeCount=',Graph.Nodes.Count,
    ' EdgeCount=',Graph.Edges.Count]);
  Graph.ConsistencyCheck;
  AVLNode:=Graph.Nodes.FindLowest;
  while AVLNode<>nil do begin
    GraphNode:=TCodeGraphNode(AVLNode.Data);
    Node:=GraphNode.Node;
    DebugLn(['  ',NodeToStr(Node),' needs ',GraphNode.OutTreeCount,' definitions, is used by ',GraphNode.InTreeCount,' definitions.']);
    if GraphNode.OutTreeCount>0 then begin
      DbgOut('    Needs:');
      EdgeAVLNode:=GraphNode.OutTree.FindLowest;
      Cnt:=0;
      while EdgeAVLNode<>nil do begin
        inc(Cnt);
        if Cnt=5 then begin
          DbgOut(' ...');
          break;
        end;
        Edge:=TCodeGraphEdge(EdgeAVLNode.Data);
        DbgOut(' '+NodeToStr(Edge.ToNode.Node));
        EdgeAVLNode:=GraphNode.OutTree.FindSuccessor(EdgeAVLNode);
      end;
      DebugLn;
    end;
    if GraphNode.InTreeCount>0 then begin
      DbgOut('    Used by:');
      EdgeAVLNode:=GraphNode.InTree.FindLowest;
      Cnt:=0;
      while EdgeAVLNode<>nil do begin
        inc(Cnt);
        if Cnt=5 then begin
          DbgOut(' ...');
          break;
        end;
        Edge:=TCodeGraphEdge(EdgeAVLNode.Data);
        DbgOut(' '+NodeToStr(Edge.FromNode.Node));
        EdgeAVLNode:=GraphNode.InTree.FindSuccessor(EdgeAVLNode);
      end;
      DebugLn;
    end;
    AVLNode:=Graph.Nodes.FindSuccessor(AVLNode);
  end;
end;

function TCodeCompletionCodeTool.InitClassCompletion(
  const UpperClassName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  ClassNode: TCodeTreeNode;
begin
  Result:=false;
  BuildTree(false);
  if not EndOfSourceFound then exit;
  if (SourceChangeCache=nil) or (Scanner=nil) then exit;
  ClassNode:=FindClassNodeInUnit(UpperClassName,true,false,false,true);
  if (ClassNode=nil) then exit;
  CodeCompleteClassNode:=ClassNode;
  CodeCompleteSrcChgCache:=SourceChangeCache;
  FreeClassInsertionList;
  Result:=true;
end;

function TCodeCompletionCodeTool.ApplyClassCompletion(
  AddMissingProcBodies: boolean): boolean;
begin
  Result:=false;
  try
    // insert all new class parts
    if not InsertAllNewClassParts then
      RaiseException(ctsErrorDuringInsertingNewClassParts);
    // insert all missing proc bodies
    if AddMissingProcBodies and (not CreateMissingProcBodies) then
      RaiseException(ctsErrorDuringCreationOfNewProcBodies);
    // apply the changes
    if not CodeCompleteSrcChgCache.Apply then
      RaiseException(ctsUnableToApplyChanges);
    Result:=true;
  finally
    FreeClassInsertionList;
  end;
end;

function TCodeCompletionCodeTool.CompleteProperty(
  PropNode: TCodeTreeNode): boolean;
{
 examples:
   property Visible;
   property Count: integer;
   property Color: TColor read FColor write SetColor;
   property Items[Index1, Index2: integer]: integer read GetItems; default;
   property X: integer index 1 read GetCoords write SetCoords stored IsStored;
   property C: char read GetC stored False default 'A';
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8, IColor;

   property specifiers without parameters:
     ;nodefault, ;default

   property specifiers with parameters:
     index <id or number>, read <id>, write <id>, stored <id>,
     default <constant>, implements <id>[,<id>...]
}
type
  TPropPart = (ppName,       // property name
               ppParamList,  // param list
               ppType,       // type identifier
               ppIndexWord,  // 'index'
               ppIndex,      // index constant
               ppReadWord,   // 'read'
               ppRead,       // read identifier
               ppWriteWord,  // 'write'
               ppWrite,      // write identifier
               ppStoredWord, // 'stored'
               ppStored,     // stored identifier
               ppImplementsWord,// 'implements'
               ppImplements, // implements identifier
               ppDefaultWord,// 'default'  (the default value keyword,
                             //             not the default property)
               ppDefault,    // default constant
               ppNoDefaultWord// 'nodefault'
               );

var
  Parts: array[TPropPart] of TAtomPosition;
  PartIsAtom: array[TPropPart] of boolean;

  procedure ReadSimpleSpec(SpecWord, SpecParam: TPropPart);
  // allowed after simple specifier like 'read':
  //   one semicolon or an <identifier> or an <identifier>.<identifier>
  //   or a specifier
  begin
    if Parts[SpecWord].StartPos>=1 then
      RaiseExceptionFmt(ctsPropertySpecifierAlreadyDefined,[GetAtom]);
    Parts[SpecWord]:=CurPos;
    ReadNextAtom;
    if AtomIsChar(';') then exit;
    AtomIsIdentifier(true);
    if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then exit;
    Parts[SpecParam]:=CurPos;
    ReadNextAtom;
    if CurPos.Flag=cafPoint then begin
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
      PartIsAtom[SpecParam]:=false;
      Parts[SpecParam].EndPos:=CurPos.EndPos;
    end;
  end;

var AccessParam, AccessParamPrefix, CleanAccessFunc, AccessFunc,
  CleanParamList, ParamList, PropType, ProcBody, VariableName: string;
  InsertPos: integer;
  BeautifyCodeOpts: TBeautifyCodeOptions;
  
  procedure InitCompleteProperty;
  var APart: TPropPart;
  begin
    for APart:=Low(TPropPart) to High(TPropPart) do begin
      Parts[APart].StartPos:=-1;
      PartIsAtom[APart]:=true;
    end;
  end;
  
  procedure ReadPropertyKeywordAndName;
  begin
    MoveCursorToNodeStart(PropNode);
    ReadNextAtom; // read 'property'
    ReadNextAtom; // read name
    Parts[ppName]:=CurPos;
    ReadNextAtom;
  end;
  
  procedure ReadPropertyParamList;
  begin
    if AtomIsChar('[') then begin
      // read parameter list '[ ... ]'
      Parts[ppParamList].StartPos:=CurPos.StartPos;
      InitExtraction;
      if not ReadParamList(true,true,[phpInUpperCase,phpWithoutBrackets])
      then begin
        {$IFDEF CTDEBUG}
        DebugLn('[TCodeCompletionCodeTool.CompleteProperty] error parsing param list');
        {$ENDIF}
        RaiseException(ctsErrorInParamList);
      end;
      CleanParamList:=GetExtraction;
      Parts[ppParamList].EndPos:=CurPos.EndPos;
    end else
      CleanParamList:='';
  end;
  
  procedure ReadPropertyType;
  begin
    ReadNextAtom; // read type
    if (CurPos.StartPos>PropNode.EndPos)
    or UpAtomIs('END') or AtomIsChar(';') or (not AtomIsIdentifier(false))
    or AtomIsKeyWord then begin
      // no type name found -> ignore this property
      RaiseExceptionFmt(ctsPropertTypeExpectedButAtomFound,[GetAtom]);
    end;
    Parts[ppType]:=CurPos;
    ReadNextAtom;
  end;
  
  procedure ReadIndexSpecifier;
  begin
    if UpAtomIs('INDEX') then begin
      if Parts[ppIndexWord].StartPos>=1 then
        RaiseException(ctsIndexSpecifierRedefined);
      Parts[ppIndexWord]:=CurPos;
      ReadNextAtom;
      if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then
        RaiseExceptionFmt(ctsIndexParameterExpectedButAtomFound,[GetAtom]);
      Parts[ppIndex].StartPos:=CurPos.StartPos;
      ReadConstant(true,false,[]);
      Parts[ppIndex].EndPos:=LastAtoms.GetValueAt(0).EndPos;
      PartIsAtom[ppIndex]:=false;
    end;
  end;
  
  procedure ReadReadSpecifier;
  begin
    if UpAtomIs('READ') then ReadSimpleSpec(ppReadWord,ppRead);
  end;
  
  procedure ReadWriteSpecifier;
  begin
    if UpAtomIs('WRITE') then ReadSimpleSpec(ppWriteWord,ppWrite);
  end;
  
  procedure ReadOptionalSpecifiers;
  begin
    while (CurPos.StartPos<PropNode.EndPos)
    and (not (CurPos.Flag in [cafSemicolon,cafEnd])) do begin
      if UpAtomIs('STORED') then begin
        ReadSimpleSpec(ppStoredWord,ppStored);
      end else if UpAtomIs('DEFAULT') then begin
        if Parts[ppDefaultWord].StartPos>=1 then
          RaiseException(ctsDefaultSpecifierRedefined);
        Parts[ppDefaultWord]:=CurPos;
        ReadNextAtom;
        if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos) then
          RaiseExceptionFmt(ctsDefaultParameterExpectedButAtomFound,[GetAtom]);
        Parts[ppDefault].StartPos:=CurPos.StartPos;
        ReadConstant(true,false,[]);
        Parts[ppDefault].EndPos:=LastAtoms.GetValueAt(0).EndPos;
        PartIsAtom[ppDefault]:=false;
      end else if UpAtomIs('NODEFAULT') then begin
        if Parts[ppNoDefaultWord].StartPos>=1 then
          RaiseException(ctsNodefaultSpecifierDefinedTwice);
        Parts[ppNoDefaultWord]:=CurPos;
        ReadNextAtom;
      end else if UpAtomIs('IMPLEMENTS') then begin
        ReadSimpleSpec(ppImplementsWord,ppImplements);
        while CurPos.Flag=cafComma do begin
          ReadNextAtom;
          AtomIsIdentifier(true);
          if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos) then
            RaiseExceptionFmt(ctsIndexParameterExpectedButAtomFound,[GetAtom]);
          ReadNextAtom;
        end;
      end else
        RaiseExceptionFmt(ctsStrExpectedButAtomFound,[';',GetAtom]);
    end;
    if (CurPos.StartPos>PropNode.EndPos) then
      RaiseException('Reparsing error (Complete Property)');
  end;
  
  procedure CompleteReadSpecifier;
  begin
    // check read specifier
    VariableName:='';
    if not PartIsAtom[ppRead] then exit;
    if (Parts[ppReadWord].StartPos<=0) and (Parts[ppWriteWord].StartPos>0) then
      exit;
    {$IFDEF CTDEBUG}
    DebugLn('[TCodeCompletionCodeTool.CompleteProperty] read specifier needed');
    {$ENDIF}
    AccessParamPrefix:=BeautifyCodeOpts.PropertyReadIdentPrefix;
    if Parts[ppRead].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppRead].StartPos,
                        Parts[ppRead].EndPos-Parts[ppRead].StartPos)
    else begin
      if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
      or (AnsiCompareText(AccessParamPrefix,
              LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
      begin
        // create the default read identifier for a function
        AccessParam:=AccessParamPrefix+copy(Src,Parts[ppName].StartPos,
                                   Parts[ppName].EndPos-Parts[ppName].StartPos);
      end else begin
        // create the default read identifier for a variable
        AccessParam:=BeautifyCodeOpts.PrivateVariablePrefix
                                 +copy(Src,Parts[ppName].StartPos,
                                   Parts[ppName].EndPos-Parts[ppName].StartPos);
      end;
    end;

    // complete read identifier in property definition
    if (Parts[ppRead].StartPos<0) and CompleteProperties then begin
      // insert read specifier
      if Parts[ppReadWord].StartPos>0 then begin
        // 'read' keyword exists -> insert read identifier behind
        InsertPos:=Parts[ppReadWord].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           AccessParam);
      end else begin
        // 'read' keyword does not exist -> insert behind index and type
        if Parts[ppIndexWord].StartPos>0 then
          InsertPos:=Parts[ppIndexWord].EndPos
        else if Parts[ppIndex].StartPos>0 then
          InsertPos:=Parts[ppIndex].EndPos
        else
          InsertPos:=Parts[ppType].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           BeautifyCodeOpts.BeautifyKeyWord('read')+' '+AccessParam);
      end;
    end;

    // check if read access method exists
    if (Parts[ppParamList].StartPos>0) then begin
      if (Parts[ppIndexWord].StartPos<1) then begin
        // param list, no index
        CleanAccessFunc:=UpperCaseStr(AccessParam)+'('+CleanParamList+');';
      end else begin
        // index + param list
        CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER;'
                        +CleanParamList+');';
      end;
    end else begin
      if (Parts[ppIndexWord].StartPos<1) then begin
        // no param list, no index
        CleanAccessFunc:=UpperCaseStr(AccessParam)+';';
      end else begin
        // index, no param list
        CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER);';
      end;
    end;
    if ProcExistsInCodeCompleteClass(CleanAccessFunc) then exit;

    // check if read access variable exists
    if (Parts[ppParamList].StartPos<1) and (Parts[ppIndexWord].StartPos<1)
    and VarExistsInCodeCompleteClass(UpperCaseStr(AccessParam)) then exit;

    // complete read access specifier
    if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
    or (AnsiCompareText(AccessParamPrefix,
            LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
    begin
      // the read identifier is a function
      {$IFDEF CTDEBUG}
      DebugLn('[TCodeCompletionCodeTool.CompleteProperty] CleanAccessFunc ',CleanAccessFunc,' does not exist');
      {$ENDIF}
      // add insert demand for function
      // build function code
      if (Parts[ppParamList].StartPos>0) then begin
        MoveCursorToCleanPos(Parts[ppParamList].StartPos);
        ReadNextAtom;
        InitExtraction;
        if not ReadParamList(true,true,[phpWithParameterNames,
                             phpWithoutBrackets,phpWithVarModifiers,
                             phpWithComments])
        then begin
          {$IFDEF CTDEBUG}
          DebugLn('[TCodeCompletionCodeTool.CompleteProperty] Error reading param list');
          {$ENDIF}
          RaiseException(ctsErrorInParamList);
        end;
        ParamList:=GetExtraction;
        if (Parts[ppIndexWord].StartPos<1) then begin
          // param list, no index
          AccessFunc:='function '+AccessParam
                      +'('+ParamList+'):'+PropType+';';
        end else begin
          // index + param list
          AccessFunc:='function '+AccessParam
                      +'(Index:integer;'+ParamList+'):'+PropType+';';
        end;
      end else begin
        if (Parts[ppIndexWord].StartPos<1) then begin
          // no param list, no index
          AccessFunc:='function '+AccessParam+':'+PropType+';';
        end else begin
          // index, no param list
          AccessFunc:='function '+AccessParam
                      +'(Index:integer):'+PropType+';';
        end;
      end;
      // add new Insert Node
      if CompleteProperties then
        AddClassInsertion(CleanAccessFunc,AccessFunc,AccessParam,
                          ncpPrivateProcs,PropNode);
    end else begin
      // the read identifier is a variable
      VariableName:=AccessParam;
      // variable does not exist yet -> add insert demand for variable
      AddClassInsertion(UpperCaseStr(VariableName),
         VariableName+':'+PropType+';',VariableName,ncpPrivateVars,PropNode);
    end;
  end;
  
  procedure CompleteWriteSpecifier;
  begin
    // check write specifier
    if not PartIsAtom[ppWrite] then exit;
    if (Parts[ppWriteWord].StartPos<1) and (Parts[ppReadWord].StartPos>0) then
      exit;
    {$IFDEF CTDEBUG}
    DebugLn('[TCodeCompletionCodeTool.CompleteProperty] write specifier needed');
    {$ENDIF}
    AccessParamPrefix:=BeautifyCodeOpts.PropertyWriteIdentPrefix;
    if Parts[ppWrite].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppWrite].StartPos,
            Parts[ppWrite].EndPos-Parts[ppWrite].StartPos)
    else
      AccessParam:=AccessParamPrefix+copy(Src,Parts[ppName].StartPos,
            Parts[ppName].EndPos-Parts[ppName].StartPos);

    // complete property definition for write specifier
    if (Parts[ppWrite].StartPos<0) and CompleteProperties then begin
      // insert write specifier
      if Parts[ppWriteWord].StartPos>0 then begin
        // 'write' keyword exists -> insert write identifier behind
        InsertPos:=Parts[ppWriteWord].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           AccessParam);
      end else begin
        // 'write' keyword does not exist
        //  -> insert behind type, index and write specifier
        if Parts[ppRead].StartPos>0 then
          InsertPos:=Parts[ppRead].EndPos
        else if Parts[ppReadWord].StartPos>0 then
          InsertPos:=Parts[ppReadWord].EndPos
        else if Parts[ppIndexWord].StartPos>0 then
          InsertPos:=Parts[ppIndexWord].EndPos
        else if Parts[ppIndex].StartPos>0 then
          InsertPos:=Parts[ppIndex].EndPos
        else
          InsertPos:=Parts[ppType].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           BeautifyCodeOpts.BeautifyKeyWord('write')+' '+AccessParam);
      end;
    end;

    // check if write method exists
    if (Parts[ppParamList].StartPos>0) then begin
      if (Parts[ppIndexWord].StartPos<1) then begin
        // param list, no index
        CleanAccessFunc:=UpperCaseStr(AccessParam)+'('+CleanParamList+';'
                           +' :'+UpperCaseStr(PropType)+');';
      end else begin
        // index + param list
        CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER;'
                  +CleanParamList+'; :'+UpperCaseStr(PropType)+');';
      end;
    end else begin
      if (Parts[ppIndexWord].StartPos<1) then begin
        // no param list, no index
        CleanAccessFunc:=UpperCaseStr(AccessParam)
                            +'( :'+UpperCaseStr(PropType)+');';
      end else begin
        // index, no param list
        CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER;'
                            +' :'+UpperCaseStr(PropType)+');';
      end;
    end;
    if ProcExistsInCodeCompleteClass(CleanAccessFunc) then exit;

    // check if write variable exists
    if (Parts[ppParamList].StartPos<1) and (Parts[ppIndexWord].StartPos<1)
    and VarExistsInCodeCompleteClass(UpperCaseStr(AccessParam)) then exit;

    // complete class
    if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
    or (AnsiCompareText(AccessParamPrefix,
            LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
    begin
      // add insert demand for function
      // build function code
      ProcBody:='';
      if (Parts[ppParamList].StartPos>0) then begin
        MoveCursorToCleanPos(Parts[ppParamList].StartPos);
        ReadNextAtom;
        InitExtraction;
        if not ReadParamList(true,true,[phpWithParameterNames,
                             phpWithoutBrackets,phpWithVarModifiers,
                             phpWithComments])
        then
          RaiseException(ctsErrorInParamList);
        ParamList:=GetExtraction;
        if (Parts[ppIndexWord].StartPos<1) then begin
          // param list, no index
          AccessFunc:='procedure '+AccessParam
                      +'('+ParamList+';const '+SetPropertyVariablename+': '
                      +PropType+');';
        end else begin
          // index + param list
          AccessFunc:='procedure '+AccessParam
                      +'(Index:integer;'+ParamList+';'
                      +'const '+SetPropertyVariablename+': '+PropType+');';
        end;
      end else begin
        if (Parts[ppIndexWord].StartPos<1) then begin
          // no param list, no index
          AccessFunc:=
            'procedure '+AccessParam
            +'(const '+SetPropertyVariablename+': '+PropType+');';
          if VariableName<>'' then begin
            { read spec is a variable -> add simple assign code to body
              For example:
              
              procedure SetMyInt(AValue: integer);
              begin
                if FMyInt=AValue then exit;
                FMyInt:=AValue;
              end;
            
            }
            ProcBody:=
              'procedure '
              +ExtractClassName(PropNode.Parent.Parent,false)+'.'+AccessParam
              +'(const '+SetPropertyVariablename+': '+PropType+');'
              +BeautifyCodeOpts.LineEnd
              +'begin'+BeautifyCodeOpts.LineEnd
              +GetIndentStr(BeautifyCodeOpts.Indent)
                +'if '+VariableName+'='+SetPropertyVariablename+' then exit;'
                +BeautifyCodeOpts.LineEnd
              +GetIndentStr(BeautifyCodeOpts.Indent)
                +VariableName+':='+SetPropertyVariablename+';'
                +BeautifyCodeOpts.LineEnd
              +'end;';
          end;
        end else begin
          // index, no param list
          AccessFunc:='procedure '+AccessParam
                      +'(Index:integer; const '+SetPropertyVariablename+': '
                      +PropType+');';
        end;
      end;
      // add new Insert Node
      if CompleteProperties then
        AddClassInsertion(CleanAccessFunc,AccessFunc,AccessParam,
                          ncpPrivateProcs,PropNode,ProcBody);
    end else begin
      // the write identifier is a variable
      // -> add insert demand for variable
      if CompleteProperties then
        AddClassInsertion(UpperCaseStr(AccessParam),
           AccessParam+':'+PropType+';',AccessParam,ncpPrivateVars,PropNode);
    end;
  end;
  
  procedure CompleteStoredSpecifier;
  begin
    // check stored specifier
    if not PartIsAtom[ppStored] then exit;
    if (Parts[ppStoredWord].StartPos<1) then exit;
    {$IFDEF CTDEBUG}
    DebugLn('[TCodeCompletionCodeTool.CompleteProperty] stored specifier needed');
    {$ENDIF}
    if Parts[ppStored].StartPos>0 then begin
      if (CompareIdentifiers(@Src[Parts[ppStored].StartPos],'False')=0)
      or (CompareIdentifiers(@Src[Parts[ppStored].StartPos],'True')=0) then
        exit;
      AccessParam:=copy(Src,Parts[ppStored].StartPos,
            Parts[ppStored].EndPos-Parts[ppStored].StartPos);
    end else
      AccessParam:=copy(Src,Parts[ppName].StartPos,
        Parts[ppName].EndPos-Parts[ppName].StartPos)
        +BeautifyCodeOpts.PropertyStoredIdentPostfix;
    CleanAccessFunc:=UpperCaseStr(AccessParam);
    // check if procedure exists
    if (not ProcExistsInCodeCompleteClass(CleanAccessFunc+';'))
    and (not VarExistsInCodeCompleteClass(CleanAccessFunc))
    then begin
      // add insert demand for function
      // build function code
      AccessFunc:='function '+AccessParam+':boolean;';
      CleanAccessFunc:=CleanAccessFunc+';';
      // add new Insert Node
      if CompleteProperties then
        AddClassInsertion(CleanAccessFunc,AccessFunc,AccessParam,
                          ncpPrivateProcs,PropNode);
    end;
    if Parts[ppStored].StartPos<0 then begin
      // insert stored specifier
      InsertPos:=Parts[ppStoredWord].EndPos;
      if CompleteProperties then
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
                                   AccessParam);
    end;
  end;

begin
  Result:=false;
  InitCompleteProperty;
  ReadPropertyKeywordAndName;
  ReadPropertyParamList;
  
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeCompletionCodeTool.CompleteProperty] Checking Property ',GetAtom);
  {$ENDIF}
  if not AtomIsChar(':') then begin
    {$IFDEF CTDEBUG}
    DebugLn('[TCodeCompletionCodeTool.CompleteProperty] no type : found -> ignore property');
    {$ENDIF}
    // no type -> ignore this property
    Result:=true;
    exit;
  end;
  
  ReadPropertyType;
  // parse specifiers
  ReadIndexSpecifier;
  ReadReadSpecifier;
  ReadWriteSpecifier;
  ReadOptionalSpecifiers;
  PropType:=copy(Src,Parts[ppType].StartPos,
               Parts[ppType].EndPos-Parts[ppType].StartPos);
               
  // complete property
  BeautifyCodeOpts:=ASourceChangeCache.BeautifyCodeOptions;
  CompleteReadSpecifier;
  CompleteWriteSpecifier;
  CompleteStoredSpecifier;
  
  Result:=true;
end;

procedure TCodeCompletionCodeTool.InsertNewClassParts(PartType: TNewClassPart);
var ANodeExt: TCodeTreeNodeExtension;
  ClassSectionNode, ANode, InsertNode: TCodeTreeNode;
  Indent, InsertPos: integer;
  CurCode: string;
  IsVariable, InsertBehind: boolean;
  Visibility: TPascalClassSection;
begin
  ANodeExt:=FirstInsert;
  Visibility:=NewClassPartVisibilty[PartType];
  // insert all nodes of specific type
  while ANodeExt<>nil do begin
    IsVariable:=NodeExtIsVariable(ANodeExt);
    if (cardinal(ord(PartType))=ANodeExt.Flags) then begin
      // search a destination section
      if Visibility=pcsPublished then begin
        // insert into first published section
        ClassSectionNode:=FCodeCompleteClassNode.FirstChild;
        // the first class section is always a published section, even if there
        // is no 'published' keyword. If the class starts with the 'published'
        // keyword, then it will be more beautiful to insert vars and procs to
        // this second published section
        if (ClassSectionNode.FirstChild=nil)
        and (ClassSectionNode.NextBrother<>nil)
        and (ClassSectionNode.NextBrother.Desc=ctnClassPublished)
        then
          ClassSectionNode:=ClassSectionNode.NextBrother;
      end else if ANodeExt.Node<>nil then begin
        // search a section of the same Visibility in front of the node
        ClassSectionNode:=ANodeExt.Node.Parent.PriorBrother;
        while (ClassSectionNode<>nil)
        and (ClassSectionNode.Desc<>ClassSectionNodeType[Visibility]) do
          ClassSectionNode:=ClassSectionNode.PriorBrother;
      end;
      if ClassSectionNode=nil then begin
        // there is no existing class section node
        // -> insert in the new one
        Indent:=NewClassSectionIndent[Visibility]
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
        InsertPos:=NewClassSectionInsertPos[Visibility];
      end else begin
        // there is an existing class section to insert into
        
        // find a nice insert position
        InsertNode:=nil; // the new part will be inserted after this node
                         //   nil means insert as first
        InsertBehind:=true;
        ANode:=ClassSectionNode.FirstChild;
        
        // skip the class GUID
        if (ANode<>nil) and (ANode.Desc=ctnClassGUID) then
          ANode:=ANode.NextBrother;

        // insert methods behind variables
        if not IsVariable then begin
          while (ANode<>nil) and (ANode.Desc=ctnVarDefinition) do begin
            InsertNode:=ANode;
            ANode:=ANode.NextBrother;
          end;
        end;
        
        // find a nice position between similar siblings
        case ASourceChangeCache.BeautifyCodeOptions.ClassPartInsertPolicy of
        
        cpipAlphabetically:
          begin
            while ANode<>nil do begin
              if (IsVariable) then begin
                // the insertion is a new variable
                if (ANode.Desc<>ctnVarDefinition)
                or (CompareNodeIdentChars(ANode,ANodeExt.Txt)<0) then
                  break;
              end else begin
                // the insertion is a new method
                case ANode.Desc of
                
                ctnProcedure:
                  begin
                    CurCode:=ExtractProcName(ANode,[]);
                    if AnsiCompareStr(CurCode,ANodeExt.ExtTxt2)>0 then
                      break;
                  end;
                  
                ctnProperty:
                  begin
                    if ASourceChangeCache.BeautifyCodeOptions
                        .MixMethodsAndProperties then
                    begin
                      CurCode:=ExtractPropName(ANode,false);
                      if AnsiCompareStr(CurCode,ANodeExt.ExtTxt2)>0 then
                        break;
                    end else
                      break;
                  end;
                  
                end;
              end;
              InsertNode:=ANode;
              ANode:=ANode.NextBrother;
            end;
          end;
          
        else
          // cpipLast
          begin
            while ANode<>nil do begin
              if (IsVariable) then begin
                // the insertion is a variable
                if (ANode.Desc<>ctnVarDefinition) then
                  break;
              end else begin
                // the insertion is a method
                if (not ASourceChangeCache.BeautifyCodeOptions
                   .MixMethodsAndProperties)
                and (ANode.Desc=ctnProperty) then
                  break;
              end;
              InsertNode:=ANode;
              ANode:=ANode.NextBrother;
            end;
          end
        end;
        
        if InsertNode<>nil then begin
        
          if (not IsVariable) and (InsertNode.Desc=ctnVarDefinition)
          and (InsertNode.NextBrother<>nil) then begin
            // insertion is a new method and it should be inserted behind
            // variables. Because methods and variables should be separated
            // there is a next node, insert the new method in front of the next
            // node, instead of inserting it right behind the variable.
            // This makes sure to use existing separation comments/empty lines.
            InsertNode:=InsertNode.NextBrother;
            InsertBehind:=false;
          end;
          
          Indent:=GetLineIndent(Src,InsertNode.StartPos);
          if InsertBehind then begin
            // insert behind InsertNode
            InsertPos:=FindLineEndOrCodeAfterPosition(InsertNode.EndPos);
          end else begin
            // insert in front of InsertNode
            InsertPos:=InsertNode.StartPos;
          end;
        end else begin
          // insert as first variable/proc
          Indent:=GetLineIndent(Src,ClassSectionNode.StartPos)
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
          InsertPos:=FindLineEndOrCodeAfterPosition(ClassSectionNode.StartPos);
        end;
      end;
      CurCode:=ANodeExt.ExtTxt1;
      CurCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                          CurCode,Indent);
      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.InsertNewClassParts:');
      DebugLn(CurCode);
      {$ENDIF}
      ASourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
         CurCode);
      if (not IsVariable)
      and (ASourceChangeCache.BeautifyCodeOptions.MethodInsertPolicy
        =mipClassOrder) then
      begin
        // this was a new method definition and the body should be added in
        // Class Order
        // -> save information about the inserted position
        ANodeExt.Position:=InsertPos;
      end;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
end;
  
function TCodeCompletionCodeTool.InsertAllNewClassParts: boolean;
var
  PublishedKeyWordNeeded: boolean;

  function GetTopMostPositionNode(Visibility: TPascalClassSection
    ): TCodeTreeNode;
  var
    ANodeExt: TCodeTreeNodeExtension;
  begin
    Result:=nil;
    ANodeExt:=FirstInsert;
    while ANodeExt<>nil do begin
      if ((Result=nil)
      or (Result.StartPos>ANodeExt.Node.StartPos))
        and (NodeExtHasVisibilty(ANodeExt,pcsPrivate))
      then
        Result:=ANodeExt.Node;
      ANodeExt:=ANodeExt.Next;
    end;
  end;
  
  function GetFirstNodeExtWithVisibility(Visibility: TPascalClassSection
    ): TCodeTreeNodeExtension;
  begin
    Result:=FirstInsert;
    while Result<>nil do begin
      if NodeExtHasVisibilty(Result,Visibility) then
        break;
      Result:=Result.Next;
    end;
  end;
  
  procedure AddClassSection(Visibility: TPascalClassSection);
  var
    TopMostPositionNode: TCodeTreeNode;
    SectionNode: TCodeTreeNode;
    SectionKeyWord: String;
    ANode: TCodeTreeNode;
  begin
    NewClassSectionInsertPos[Visibility]:=-1;
    NewClassSectionIndent[Visibility]:=0;
    if GetFirstNodeExtWithVisibility(Visibility)=nil then exit;
    // search topmost position node for this Visibility
    TopMostPositionNode:=GetTopMostPositionNode(Visibility);
    SectionNode:=nil;
    // search a Visibility section in front of topmost position node
    if TopMostPositionNode<>nil then
      SectionNode:=TopMostPositionNode.Parent.PriorBrother
    else
      SectionNode:=FCodeCompleteClassNode.LastChild;
    while (SectionNode<>nil)
    and (SectionNode.Desc<>ClassSectionNodeType[Visibility]) do
      SectionNode:=SectionNode.PriorBrother;
    if (SectionNode<>nil) then begin
      exit;
    end;
    { There is no section of this Visibility in front (or at all)
      -> Insert a new section in front of topmost node.
      Normally the best place for a new section is at the end of
      the first published section. But if a variable is already
      needed in the first published section, then the new section
      must be inserted in front of all }
    if (TopMostPositionNode<>nil)
    and (FCodeCompleteClassNode.FirstChild.EndPos>TopMostPositionNode.StartPos)
    then begin
      // topmost node is in the first section
      // -> insert the new section as the first section
      ANode:=FCodeCompleteClassNode.FirstChild;
      NewClassSectionIndent[Visibility]:=GetLineIndent(Src,ANode.StartPos);
      if (ANode.FirstChild<>nil) and (ANode.FirstChild.Desc<>ctnClassGUID)
      then
        NewClassSectionInsertPos[Visibility]:=ANode.StartPos
      else
        NewClassSectionInsertPos[Visibility]:=ANode.FirstChild.EndPos;
      if (not PublishedKeyWordNeeded)
      and (CompareNodeIdentChars(ANode,'PUBLISHED')<>0) then begin
        PublishedKeyWordNeeded:=true;
        NewClassSectionInsertPos[pcsPublished]:=
          NewClassSectionInsertPos[Visibility];
        NewClassSectionIndent[pcsPublished]:=
          NewClassSectionIndent[Visibility];
      end;
    end else begin
      ANode:=nil;
      case Visibility of
      pcsProtected:
        // insert after last private section
        ANode:=FindLastClassSection(FCodeCompleteClassNode,ctnClassPrivate);
      pcsPublic:
        begin
          // insert after last private, protected section
          ANode:=FindClassSection(FCodeCompleteClassNode,ctnClassProtected);
          if ANode=nil then
            ANode:=FindClassSection(FCodeCompleteClassNode,ctnClassPrivate);
        end;
      end;
      if ANode=nil then begin
        // default: insert new section behind first published section
        ANode:=FCodeCompleteClassNode.FirstChild;
      end;
      NewClassSectionIndent[Visibility]:=GetLineIndent(Src,ANode.StartPos);
      NewClassSectionInsertPos[Visibility]:=ANode.EndPos;
    end;
    SectionKeyWord:=PascalClassSectionKeywords[Visibility];
    ASourceChangeCache.Replace(gtNewLine,gtNewLine,
      NewClassSectionInsertPos[Visibility],
      NewClassSectionInsertPos[Visibility],
      GetIndentStr(NewClassSectionIndent[Visibility])+
        ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord(SectionKeyWord));
  end;

begin
  Result:=InsertClassHeaderComment;
  if not Result then exit;

  Result:=InsertMissingClassSemicolons;
  if not Result then exit;

  if FirstInsert=nil then begin
    Result:=true;
    exit;
  end;
  PublishedKeyWordNeeded:=false;// 'published' keyword after first private section needed

  AddClassSection(pcsPrivate);
  InsertNewClassParts(ncpPrivateVars);
  InsertNewClassParts(ncpPrivateProcs);

  AddClassSection(pcsProtected);
  InsertNewClassParts(ncpProtectedVars);
  InsertNewClassParts(ncpProtectedProcs);

  AddClassSection(pcsPublic);
  InsertNewClassParts(ncpPublicVars);
  InsertNewClassParts(ncpPublicProcs);

  if PublishedKeyWordNeeded then begin
    ASourceChangeCache.Replace(gtNewLine,gtNewLine,
      NewClassSectionInsertPos[pcsPublished],
      NewClassSectionInsertPos[pcsPublished],
      GetIndentStr(NewClassSectionIndent[pcsPublished])+
        ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('published'));
  end;
  InsertNewClassParts(ncpPublishedVars);
  InsertNewClassParts(ncpPublishedProcs);

  Result:=true;
end;

function TCodeCompletionCodeTool.InsertClassHeaderComment: boolean;
var
  ClassNode: TCodeTreeNode;
  ClassIdentifierNode: TCodeTreeNode;
  Code: String;
  InsertPos: LongInt;
  Indent: LongInt;
  StartPos, CommentStart, CommentEnd: TCodeXYPosition;
begin
  Result:=true;
  if not ASourceChangeCache.BeautifyCodeOptions.ClassHeaderComments then exit;
  // check if there is already a comment in front of the class
  
  // find the start of the class (the position in front of the class name)
  ClassNode:=CodeCompleteClassNode;
  if ClassNode=nil then exit;
  ClassIdentifierNode:=
                   ClassNode.GetNodeOfTypes([ctnTypeDefinition,ctnGenericType]);
  if ClassIdentifierNode=nil then begin
    DebugLn('TCodeCompletionCodeTool.InsertClassHeaderComment WARNING: class without name');
    exit;
  end;
  if not CleanPosToCaret(ClassIdentifierNode.StartPos,StartPos) then exit;
  Code:=ExtractDefinitionName(ClassIdentifierNode);
  
  // check if there is already a comment in front
  if FindCommentInFront(StartPos,Code,false,true,false,false,true,true,
                        CommentStart,CommentEnd)
  then
    // comment already exists
    exit;
  if CommentStart.Code=nil then ;
  if CommentEnd.Code=nil then ;

  // insert comment in front
  InsertPos:=ClassIdentifierNode.StartPos;
  Indent:=GetLineIndent(Src,InsertPos);
  Code:=GetIndentStr(Indent)+'{ '+Code+' }';
  ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
                             InsertPos,InsertPos,Code);
end;

function TCodeCompletionCodeTool.InsertMissingClassSemicolons: boolean;
var
  ANode: TCodeTreeNode;
  ProcCode: String;
begin
  Result:=false;
  ANode:=FCompletingStartNode;
  while (ANode<>nil) do begin
    if ANode.Desc=ctnProcedure then begin
      ProcCode:=ExtractProcHead(ANode,[phpWithStart,
                  phpWithoutClassKeyword,
                  phpWithVarModifiers,phpWithParameterNames,phpWithResultType,
                  phpWithCallingSpecs,phpDoNotAddSemicolon]);
      if ProcCode[length(ProcCode)]<>';' then begin
        // add missing semicolon
        UndoReadNextAtom;
        if not ASourceChangeCache.Replace(gtNone,gtNone,
          CurPos.EndPos,CurPos.EndPos,';') then
            RaiseException('InsertMissingClassSemicolons: unable to insert semicolon');
      end;
    end;
    // next node
    if ANode.NextBrother<>nil then begin
      ANode:=ANode.NextBrother;
    end else begin
      ANode:=ANode.Parent.NextBrother;
      while (ANode<>nil) and (ANode.Desc in (AllCodeSections+AllClassSections))
      and (ANode.FirstChild=nil) do
        ANode:=ANode.NextBrother;
      if ANode<>nil then ANode:=ANode.FirstChild;
    end;
  end;
  Result:=true;
end;

function TCodeCompletionCodeTool.InsertAllNewUnitsToMainUsesSection: boolean;
var
  UsesNode: TCodeTreeNode;
  AVLNode: TAVLTreeNode;
  CurSourceName: String;
  SectionNode: TCodeTreeNode;
  NewUsesTerm: String;
  NewUnitName: String;
  InsertPos: LongInt;
begin
  Result:=true;
  if (fNewMainUsesSectionUnits=nil) then exit;
  //DebugLn(['TCodeCompletionCodeTool.InsertAllNewUnitsToMainUsesSection ']);
  UsesNode:=FindMainUsesSection;

  // remove units, that are already in the uses section
  CurSourceName:=GetSourceName(false);
  fNewMainUsesSectionUnits.Remove(PChar(CurSourceName)); // the unit itself
  if UsesNode<>nil then begin
    MoveCursorToNodeStart(UsesNode);
    ReadNextAtom; // read 'uses'
    repeat
      ReadNextAtom; // read name
      if AtomIsChar(';') then break;
      fNewMainUsesSectionUnits.Remove(@Src[CurPos.StartPos]);
      ReadNextAtom;
      if UpAtomIs('IN') then begin
        ReadNextAtom;
        ReadNextAtom;
      end;
      if AtomIsChar(';') then break;
      if not AtomIsChar(',') then break;
    until (CurPos.StartPos>SrcLen);;
    
    if (fNewMainUsesSectionUnits.Count=0) then exit;
  end;
  
  // add units
  NewUsesTerm:='';
  AVLNode:=fNewMainUsesSectionUnits.FindLowest;
  while AVLNode<>nil do begin
    if NewUsesTerm<>'' then
      NewUsesTerm:=NewUsesTerm+', ';
    NewUnitName:=GetIdentifier(PChar(AVLNode.Data));
    NewUsesTerm:=NewUsesTerm+NewUnitName;
    AVLNode:=fNewMainUsesSectionUnits.FindSuccessor(AVLNode);
  end;
  if UsesNode<>nil then begin
    // add unit to existing uses section
    MoveCursorToNodeStart(UsesNode); // for nice error position
    InsertPos:=UsesNode.EndPos-1; // position of semicolon at end of uses section
    NewUsesTerm:=', '+NewUsesTerm;
    if not ASourceChangeCache.Replace(gtNone,gtNone,InsertPos,InsertPos,
                                      NewUsesTerm) then exit;
  end else begin
    // create a new uses section
    if Tree.Root=nil then exit;
    SectionNode:=Tree.Root;
    MoveCursorToNodeStart(SectionNode);
    ReadNextAtom;
    if UpAtomIs('UNIT') then begin
      // search interface
      SectionNode:=SectionNode.NextBrother;
      if (SectionNode=nil) or (SectionNode.Desc<>ctnInterface) then exit;
      MoveCursorToNodeStart(SectionNode);
      ReadNextAtom;
    end;
    InsertPos:=CurPos.EndPos;
    NewUsesTerm:=ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('uses')
                 +' '+NewUsesTerm+';';
    if not ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
                                     InsertPos,InsertPos,NewUsesTerm) then exit;
  end;
end;

procedure TCodeCompletionCodeTool.AddNewPropertyAccessMethodsToClassProcs(
  ClassProcs: TAVLTree;  const TheClassName: string);
var ANodeExt: TCodeTreeNodeExtension;
  NewNodeExt: TCodeTreeNodeExtension;
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeCompletionCodeTool.AddNewPropertyAccessMethodsToClassProcs]');
  {$ENDIF}
  // add new property access methods to ClassProcs
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if not NodeExtIsVariable(ANodeExt) then begin
      if FindNodeInTree(ClassProcs,ANodeExt.Txt)=nil then begin
        NewNodeExt:=TCodeTreeNodeExtension.Create;
        with NewNodeExt do begin
          Txt:=UpperCaseStr(TheClassName)+'.'
                +ANodeExt.Txt;       // Name+ParamTypeList
          ExtTxt1:=ASourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
             ANodeExt.ExtTxt1,TheClassName,''); // complete proc head code
          ExtTxt3:=ANodeExt.ExtTxt3;
          Position:=ANodeExt.Position;
          {$IFDEF CTDEBUG}
          DebugLn('  Txt="',Txt,'"');
          DebugLn('  ExtTxt1="',ExtTxt1,'"');
          DebugLn('  ExtTxt3="',ExtTxt3,'"');
          {$ENDIF}
        end;
        ClassProcs.Add(NewNodeExt);
      end;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
end;

procedure TCodeCompletionCodeTool.CheckForOverrideAndAddInheritedCode(
  ClassProcs: TAVLTree);
// check for 'override' directive and add 'inherited' code to body
var AnAVLNode: TAVLTreeNode;
  ANodeExt: TCodeTreeNodeExtension;
  ProcCode, ProcCall: string;
  ProcNode: TCodeTreeNode;
  i: integer;
  BeautifyCodeOptions: TBeautifyCodeOptions;
begin
  if not AddInheritedCodeToOverrideMethod then exit;
  {$IFDEF CTDEBUG}
  DebugLn('[TCodeCompletionCodeTool.CheckForOverrideAndAddInheritedCode]');
  {$ENDIF}
  BeautifyCodeOptions:=ASourceChangeCache.BeautifyCodeOptions;
  AnAVLNode:=ClassProcs.FindLowest;
  while AnAVLNode<>nil do begin
    ANodeExt:=TCodeTreeNodeExtension(AnAVLNode.Data);
    ProcNode:=ANodeExt.Node;
    if (ProcNode<>nil) and (ANodeExt.ExtTxt3='')
    and (ProcNodeHasSpecifier(ProcNode,psOVERRIDE)) then begin
      ProcCode:=ExtractProcHead(ProcNode,[phpWithStart,
                      phpAddClassname,phpWithVarModifiers,phpWithParameterNames,
                      phpWithResultType,phpWithCallingSpecs]);
      ProcCall:='inherited '+ExtractProcHead(ProcNode,[phpWithoutClassName,
                                   phpWithParameterNames,phpWithoutParamTypes]);
      for i:=1 to length(ProcCall)-1 do
        if ProcCall[i]=';' then ProcCall[i]:=',';
      if ProcCall[length(ProcCall)]<>';' then
        ProcCall:=ProcCall+';';
      if NodeIsFunction(ProcNode) then
        ProcCall:=BeautifyCodeOptions.BeautifyIdentifier('Result')
                  +':='+ProcCall;
      ProcCode:=ProcCode+BeautifyCodeOptions.LineEnd
                  +'begin'+BeautifyCodeOptions.LineEnd
                  +GetIndentStr(BeautifyCodeOptions.Indent)
                    +ProcCall+BeautifyCodeOptions.LineEnd
                  +'end;';
      ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                 ProcCode,0,false);
      ANodeExt.ExtTxt3:=ProcCode;
    end;
    AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
  end;
end;

function TCodeCompletionCodeTool.CreateMissingProcBodies: boolean;
var
  Indent, InsertPos: integer;
  TheClassName: string;
   
  procedure InsertProcBody(ANodeExt: TCodeTreeNodeExtension);
  var ProcCode: string;
  begin
    if ANodeExt.ExtTxt3<>'' then
      ProcCode:=ANodeExt.ExtTxt3
    else
      ProcCode:=ANodeExt.ExtTxt1;
    ProcCode:=ASourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
                 ProcCode,TheClassName,'');
    {$IFDEF CTDEBUG}
    DebugLn('CreateMissingProcBodies InsertProcBody ',TheClassName,' "',ProcCode,'"');
    {$ENDIF}
    ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                 ProcCode,Indent,ANodeExt.ExtTxt3='');
    ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
      ProcCode);
    if JumpToProcName='' then begin
      // remember one proc body to jump to after the completion
      JumpToProcName:=ANodeExt.Txt;
      if System.Pos('.',JumpToProcName)<1 then
        JumpToProcName:=UpperCaseStr(TheClassName)+'.'+JumpToProcName;
      {$IFDEF CTDEBUG}
      DebugLn('CreateMissingProcBodies JumpToProcName="',JumpToProcName,'"');
      {$ENDIF}
    end;
  end;

  procedure CreateCodeForMissingProcBody(TheNodeExt: TCodeTreeNodeExtension);
  var
    ANode: TCodeTreeNode;
    ProcCode: string;
  begin
    if (TheNodeExt.ExtTxt1='') and (TheNodeExt.ExtTxt3='') then begin
      ANode:=TheNodeExt.Node;
      if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
        ProcCode:=ExtractProcHead(ANode,[phpWithStart,
             phpAddClassname,phpWithVarModifiers,
             phpWithParameterNames,phpWithResultType,phpWithCallingSpecs]);
        TheNodeExt.ExtTxt3:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                     ProcCode,Indent,true);
      end;
    end;
  end;

var
  ProcBodyNodes, ClassProcs: TAVLTree;
  ANodeExt, ANodeExt2: TCodeTreeNodeExtension;
  ExistingNode, MissingNode, AnAVLNode, NextAVLNode,
  NearestAVLNode: TAVLTreeNode;
  cmp, MissingNodePosition: integer;
  FirstExistingProcBody, LastExistingProcBody, ImplementationNode,
  ANode, ANode2, TypeSectionNode: TCodeTreeNode;
  ClassStartComment, s: string;
  Caret1, Caret2: TCodeXYPosition;
  MethodInsertPolicy: TMethodInsertPolicy;
  NearestNodeValid: boolean;
  
  procedure GatherExistingClassProcBodies;
  begin
    TypeSectionNode:=FCodeCompleteClassNode.GetNodeOfType(ctnTypeSection);
    ClassProcs:=nil;
    ProcBodyNodes:=GatherProcNodes(TypeSectionNode,
                        [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
                         ExtractClassName(FCodeCompleteClassNode,true));
  end;
  
  procedure FindTopMostAndBottomMostProcBodies;
  begin
    ExistingNode:=ProcBodyNodes.FindLowest;
    if ExistingNode<>nil then
      LastExistingProcBody:=TCodeTreeNodeExtension(ExistingNode.Data).Node
    else
      LastExistingProcBody:=nil;
    FirstExistingProcBody:=LastExistingProcBody;
    while ExistingNode<>nil do begin
      ANode:=TCodeTreeNodeExtension(ExistingNode.Data).Node;
      if ANode.StartPos<FirstExistingProcBody.StartPos then
        FirstExistingProcBody:=ANode;
      if ANode.StartPos>LastExistingProcBody.StartPos then
        LastExistingProcBody:=ANode;
      ExistingNode:=ProcBodyNodes.FindSuccessor(ExistingNode);
    end;
  end;
  
  procedure CheckForDoubleDefinedMethods;
  begin
    AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      NextAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
      if NextAVLNode<>nil then begin
        ANodeExt:=TCodeTreeNodeExtension(AnAVLNode.Data);
        ANodeExt2:=TCodeTreeNodeExtension(NextAVLNode.Data);
        if CompareTextIgnoringSpace(ANodeExt.Txt,ANodeExt2.Txt,false)=0 then
        begin
          // proc redefined -> error
          if ANodeExt.Node.StartPos>ANodeExt2.Node.StartPos then begin
            ANode:=ANodeExt.Node;
            ANode2:=ANodeExt2.Node;
          end else begin
            ANode:=ANodeExt2.Node;
            ANode2:=ANodeExt.Node;
          end;
          CleanPosToCaret(ANode.FirstChild.StartPos,Caret1);
          CleanPosToCaret(ANode2.FirstChild.StartPos,Caret2);
          s:=IntToStr(Caret2.Y)+','+IntToStr(Caret2.X);
          if Caret1.Code<>Caret2.Code then
            s:=s+' in '+Caret2.Code.Filename;
          MoveCursorToNodeStart(ANode.FirstChild);
          RaiseException('procedure redefined (first at '+s+')');
        end;
      end;
      AnAVLNode:=NextAVLNode;
    end;
  end;
  
  procedure RemoveAbstractMethods;
  begin
    AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      NextAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
      ANodeExt:=TCodeTreeNodeExtension(AnAVLNode.Data);
      ANode:=ANodeExt.Node;
      if (ANode<>nil) and (ANode.Desc=ctnProcedure)
      and ProcNodeHasSpecifier(ANode,psABSTRACT) then begin
        ClassProcs.Delete(AnAVLNode);
        ANodeExt.Free;
      end;
      AnAVLNode:=NextAVLNode;
    end;
  end;
  
  procedure FindInsertPointForNewClass;
  begin
    if NodeHasParentOfType(FCodeCompleteClassNode,ctnInterface) then begin
      // class is in interface section
      // -> insert at the end of the implementation section
      ImplementationNode:=FindImplementationNode;
      if ImplementationNode=nil then
        RaiseException(ctsImplementationNodeNotFound);
      Indent:=GetLineIndent(Src,ImplementationNode.StartPos);
      if (ImplementationNode.LastChild=nil)
      or (ImplementationNode.LastChild.Desc<>ctnBeginBlock) then
        InsertPos:=ImplementationNode.EndPos
      else begin
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(
           ImplementationNode.LastChild.StartPos);
      end;
    end else begin
      // class is not in interface section
      // -> insert at the end of the type section
      ANode:=FCodeCompleteClassNode.GetNodeOfTypes(
                                            [ctnTypeDefinition,ctnGenericType]);
      if ANode=nil then
        RaiseException(ctsClassNodeWithoutParentNode);
      if ANode.Parent.Desc=ctnTypeSection then
        ANode:=ANode.Parent; // type section
      if ANode=nil then
        RaiseException(ctsTypeSectionOfClassNotFound);
      Indent:=GetLineIndent(Src,ANode.StartPos);
      InsertPos:=ANode.EndPos;
    end;
  end;
  
  procedure InsertClassMethodsComment;
  var
    Code: String;
    InsertXYPos, CommentStart, CommentEnd: TCodeXYPosition;
  begin
    // insert class comment
    if ClassProcs.Count=0 then exit;
    // find the start of the class (the position in front of the class name)
    if not CleanPosToCaret(InsertPos,InsertXYPos) then exit;
    
    Code:=ExtractClassName(CodeCompleteClassNode,false);
    // check if there is already a comment in front
    if FindCommentInFront(InsertXYPos,Code,false,true,false,false,true,true,
                          CommentStart,CommentEnd)
    then begin
      // comment already exists
      exit;
    end;
    if CommentStart.Code=nil then ;
    if CommentEnd.Code=nil then ;

    ClassStartComment:=GetIndentStr(Indent)
                       +'{ '+ExtractClassName(CodeCompleteClassNode,false)+' }';
    ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
                               ClassStartComment);
  end;
  
begin
  {$IFDEF CTDEBUG}
  DebugLn('TCodeCompletionCodeTool.CreateMissingProcBodies Gather existing method bodies ... ');
  {$ENDIF}
  Result:=false;
  MethodInsertPolicy:=ASourceChangeCache.BeautifyCodeOptions.MethodInsertPolicy;
  // gather existing class proc bodies
  GatherExistingClassProcBodies;
  try
    // find topmost and bottommost proc body
    FindTopMostAndBottomMostProcBodies;

    {$IFDEF CTDEBUG}
    DebugLn('TCodeCompletionCodeTool.CreateMissingProcBodies Gather existing method declarations ... ');
    {$ENDIF}
    TheClassName:=ExtractClassName(FCodeCompleteClassNode,false);

    // gather existing proc definitions in the class
    ClassProcs:=GatherProcNodes(FCompletingStartNode,
       [phpInUpperCase,phpAddClassName],
       ExtractClassName(FCodeCompleteClassNode,true));

    // check for double defined methods in ClassProcs
    CheckForDoubleDefinedMethods;

    // remove abstract methods
    RemoveAbstractMethods;

    CurNode:=FirstExistingProcBody;
    
    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      DebugLn(' AAA ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}
    
    AddNewPropertyAccessMethodsToClassProcs(ClassProcs,TheClassName);

    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      DebugLn(' BBB ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}

    CheckForOverrideAndAddInheritedCode(ClassProcs);

    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      DebugLn(' BBB ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}

    if MethodInsertPolicy=mipClassOrder then begin
      // insert in ClassOrder -> get a definition position for every method
      AnAVLNode:=ClassProcs.FindLowest;
      while AnAVLNode<>nil do begin
        ANodeExt:=TCodeTreeNodeExtension(AnAVLNode.Data);
        if ANodeExt.Position<1 then
          // position not set => this proc was already there => there is a node
          ANodeExt.Position:=ANodeExt.Node.StartPos;
        // find corresponding proc body
        NextAVLNode:=ProcBodyNodes.Find(ANodeExt);
        if NextAVLNode<>nil then begin
          // NextAVLNode.Data is the TCodeTreeNodeExtension for the method body
          // (note 1)
          ANodeExt.Data:=NextAVLNode.Data;
        end;
        AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
      end;
      // sort the method definitions with the definition position
      ClassProcs.OnCompare:=@CompareCodeTreeNodeExtWithPos;
    end;

    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      DebugLn(' CCC ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}

    // search for missing proc bodies
    if (ProcBodyNodes.Count=0) then begin
      // there were no old proc bodies of the class -> start class
      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.CreateMissingProcBodies Starting class in implementation ');
      {$ENDIF}
      FindInsertPointForNewClass;
      InsertClassMethodsComment;

      // insert all proc bodies
      MissingNode:=ClassProcs.FindHighest;
      while (MissingNode<>nil) do begin
        ANodeExt:=TCodeTreeNodeExtension(MissingNode.Data);
        CreateCodeForMissingProcBody(ANodeExt);
        InsertProcBody(ANodeExt);
        MissingNode:=ProcBodyNodes.FindPrecessor(MissingNode);
      end;
      
    end else begin
      // there were old class procs already
      // -> search a good Insert Position behind or in front of
      //    another proc body of this class
      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.CreateMissingProcBodies  Insert missing bodies between existing ... ClassProcs.Count=',dbgs(ClassProcs.Count));
      {$ENDIF}

      // set default insert position
      Indent:=GetLineIndent(Src,LastExistingProcBody.StartPos);
      InsertPos:=FindLineEndOrCodeAfterPosition(LastExistingProcBody.EndPos);
                        
      // check for all defined class methods (MissingNode), if there is a body
      MissingNode:=ClassProcs.FindHighest;
      NearestNodeValid:=false;
      while (MissingNode<>nil) do begin
        ExistingNode:=ProcBodyNodes.Find(MissingNode.Data);
        if ExistingNode=nil then begin
          ANodeExt:=TCodeTreeNodeExtension(MissingNode.Data);
          // MissingNode does not have a body -> insert proc body
          case MethodInsertPolicy of
          mipAlphabetically:
            begin
              // search alphabetically nearest proc body
              ExistingNode:=ProcBodyNodes.FindNearest(MissingNode.Data);
              cmp:=CompareCodeTreeNodeExt(ExistingNode.Data,MissingNode.Data);
              if (cmp<0) then begin
                AnAVLNode:=ProcBodyNodes.FindSuccessor(ExistingNode);
                if AnAVLNode<>nil then begin
                  ExistingNode:=AnAVLNode;
                  cmp:=1;
                end;
              end;
              ANodeExt2:=TCodeTreeNodeExtension(ExistingNode.Data);
              ANode:=ANodeExt2.Node;
              Indent:=GetLineIndent(Src,ANode.StartPos);
              if cmp>0 then begin
                // insert behind ExistingNode
                InsertPos:=FindLineEndOrCodeAfterPosition(ANode.EndPos);
              end else begin
                // insert in front of ExistingNode
                InsertPos:=FindLineEndOrCodeInFrontOfPosition(ANode.StartPos);
              end;
            end;

          mipClassOrder:
            begin
              // search definition-position nearest proc node
              MissingNodePosition:=ANodeExt.Position;
              if not NearestNodeValid then begin
                // search NearestAVLNode method with body in front of MissingNode
                // and NextAVLNode method with body behind MissingNode
                NearestAVLNode:=nil;
                NextAVLNode:=ClassProcs.FindHighest;
                NearestNodeValid:=true;
              end;
              while (NextAVLNode<>nil) do begin
                ANodeExt2:=TCodeTreeNodeExtension(NextAVLNode.Data);
                if ANodeExt2.Data<>nil then begin
                  // method has body
                  if ANodeExt2.Position>MissingNodePosition then
                    break;
                  NearestAVLNode:=NextAVLNode;
                end;
                NextAVLNode:=ClassProcs.FindPrecessor(NextAVLNode);
              end;
              if NearestAVLNode<>nil then begin
                // there is a NearestAVLNode in front -> insert behind body
                ANodeExt2:=TCodeTreeNodeExtension(NearestAVLNode.Data);
                // see above (note 1) for ANodeExt2.Data
                ANode:=TCodeTreeNodeExtension(ANodeExt2.Data).Node;
                Indent:=GetLineIndent(Src,ANode.StartPos);
                InsertPos:=FindLineEndOrCodeAfterPosition(ANode.EndPos);
              end else if NextAVLNode<>nil then begin
                // there is a NextAVLNode behind -> insert in front of body
                ANodeExt2:=TCodeTreeNodeExtension(NextAVLNode.Data);
                // see above (note 1) for ANodeExt2.Data
                ANode:=TCodeTreeNodeExtension(ANodeExt2.Data).Node;
                Indent:=GetLineIndent(Src,ANode.StartPos);
                InsertPos:=FindLineEndOrCodeInFrontOfPosition(ANode.StartPos);
              end;
            end;
          end;
          CreateCodeForMissingProcBody(ANodeExt);
          InsertProcBody(ANodeExt);
        end;
        MissingNode:=ProcBodyNodes.FindPrecessor(MissingNode);
      end;
    end;
    Result:=true;
  finally
    if ClassProcs<>nil then begin
      ClassProcs.FreeAndClear;
      ClassProcs.Free;
    end;
    ProcBodyNodes.FreeAndClear;
    ProcBodyNodes.Free;
  end;
end;

function TCodeCompletionCodeTool.CompleteCode(CursorPos: TCodeXYPosition;
  OldTopLine: integer; out NewPos: TCodeXYPosition; out NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var CleanCursorPos, Indent, insertPos: integer;
  CursorNode, ProcNode, ImplementationNode, SectionNode, AClassNode,
  ANode: TCodeTreeNode;
  OldCleanCursorPos: LongInt;

  procedure CompleteClass;
  var
    OldCodePos: TCodePosition;
    CurClassName: String;
  begin
    {$IFDEF CTDEBUG}
    DebugLn('TCodeCompletionCodeTool.CompleteCode In-a-class ',NodeDescriptionAsString(AClassNode.Desc));
    {$ENDIF}
    // cursor is in class/object definition
    if (CursorNode.SubDesc and ctnsForwardDeclaration)>0 then exit;
    // parse class and build CodeTreeNodes for all properties/methods
    {$IFDEF CTDEBUG}
    DebugLn('TCodeCompletionCodeTool.CompleteCode C ',dbgs(CleanCursorPos),', |',copy(Src,CleanCursorPos,8));
    {$ENDIF}
    CodeCompleteClassNode:=AClassNode;
    CurClassName:=ExtractClassName(AClassNode,false);
    try
      // go through all properties and procs
      //  insert read + write prop specifiers
      //  demand Variables + Procs + Proc Bodies
      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.CompleteCode Complete Properties ... ');
      {$ENDIF}
      SectionNode:=FCodeCompleteClassNode.FirstChild;
      while SectionNode<>nil do begin
        ANode:=SectionNode.FirstChild;
        while ANode<>nil do begin
          if ANode.Desc=ctnProperty then begin
            // check if property is complete
            if not CompleteProperty(ANode) then
              RaiseException(ctsUnableToCompleteProperty);
          end;
          ANode:=ANode.NextBrother;
        end;
        SectionNode:=SectionNode.NextBrother;
      end;

      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.CompleteCode Insert new variables and methods ... ');
      {$ENDIF}
      // insert all new variables and procs definitions
      if not InsertAllNewClassParts then
        RaiseException(ctsErrorDuringInsertingNewClassParts);

      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.CompleteCode Insert new method bodies ... ');
      {$ENDIF}
      // insert all missing proc bodies
      if not CreateMissingProcBodies then
        RaiseException(ctsErrorDuringCreationOfNewProcBodies);

      {$IFDEF CTDEBUG}
      DebugLn('TCodeCompletionCodeTool.CompleteCode Apply ... ');
      {$ENDIF}
      // apply the changes and jump to first new proc body
      if not CleanPosToCodePos(CleanCursorPos,OldCodePos) then
        RaiseException('TCodeCompletionCodeTool.CompleteCode Internal Error CleanPosToCodePos');
      if not SourceChangeCache.Apply then
        RaiseException(ctsUnableToApplyChanges);

      if JumpToProcName<>'' then begin
        {$IFDEF CTDEBUG}
        DebugLn('TCodeCompletionCodeTool.CompleteCode Jump to new proc body ... "',JumpToProcName,'"');
        {$ENDIF}
        // there was a new proc body
        // -> find it and jump to

        // reparse code
        BuildTreeAndGetCleanPos(trAll,CursorPos,CleanCursorPos,[]);
        // find CodeTreeNode at cursor
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
        // due to insertions in front of the class, the cursor position could
        // have changed
        while (CursorNode<>nil) do begin
          if (CursorNode.Desc=ctnTypeSection)
          or ((CursorNode.Parent<>nil) and (CursorNode.Parent.Desc=ctnTypeSection))
          then break;
          CursorNode:=CursorNode.Parent;
        end;
        FCodeCompleteClassNode:=
                              FindClassNode(CursorNode,CurClassName,true,false);
        if FCodeCompleteClassNode=nil then
          RaiseException('oops, I lost your class');
        ANode:=FCodeCompleteClassNode.GetNodeOfTypes(
                                            [ctnTypeDefinition,ctnGenericType]);
        if ANode=nil then
          RaiseException(ctsClassNodeWithoutParentNode);
        if (ANode.Parent<>nil) and (ANode.Parent.Desc=ctnTypeSection) then
          ANode:=ANode.Parent;
        ProcNode:=FindProcNode(ANode,JumpToProcName,
                   [phpInUpperCase,phpIgnoreForwards]);
        if ProcNode=nil then
          RaiseException(ctsNewProcBodyNotFound);
        Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
      end else begin
        {$IFDEF CTDEBUG}
        DebugLn('TCodeCompletionCodeTool.CompleteCode Adjust Cursor ... ');
        {$ENDIF}
        // there was no new proc body
        // -> adjust cursor
        AdjustCursor(OldCodePos,OldTopLine,NewPos,NewTopLine);
        Result:=true;
      end;

    finally
      FreeClassInsertionList;
    end;
  end;
  
  procedure CompleteForwardProcs;
  // add proc bodies for forward procs
  var
    RevertableJump: boolean;
    ProcBodyNodes: TAVLTree;
    StartProcNode: TCodeTreeNode;
    CurProcNode: TCodeTreeNode;
    EndProcNode: TCodeTreeNode;
    ProcCode: String;
  begin
    {$IFDEF CTDEBUG}
    DebugLn('TCodeCompletionCodeTool.CompleteCode in a forward procedure ... ');
    {$ENDIF}

    // gather all proc bodies
    ProcBodyNodes:=GatherProcNodes(FindNextNodeOnSameLvl(ProcNode),
                        [phpInUpperCase,phpIgnoreForwards,phpIgnoreMethods],'');
    try
      // find first forward proc without body
      StartProcNode:=ProcNode;
      CurProcNode:=StartProcNode;
      repeat
        ProcCode:=ExtractProcHead(CurProcNode,[phpInUpperCase]);
        if (FindNodeInTree(ProcBodyNodes,ProcCode)<>nil)
        or (ProcNodeHasSpecifier(CurProcNode,psEXTERNAL)) then begin
          // node is already completed
          if CurProcNode=ProcNode then begin
            // cursor node is already completed -> stop completion
            exit;
          end;
          break;
        end;
        StartProcNode:=CurProcNode;
        CurProcNode:=FindPrevNodeOnSameLvl(CurProcNode);
      until (CurProcNode=nil) or (CurProcNode.Desc<>ctnProcedure)
      or ((CurProcNode.SubDesc and ctnsForwardDeclaration)=0);

      // find last forward proc without body
      EndProcNode:=ProcNode;
      CurProcNode:=EndProcNode;
      repeat
        ProcCode:=ExtractProcHead(CurProcNode,[phpInUpperCase]);
        if (FindNodeInTree(ProcBodyNodes,ProcCode)<>nil)
        or (ProcNodeHasSpecifier(CurProcNode,psEXTERNAL)) then begin
          // node is already completed
          if CurProcNode=ProcNode then begin
            // cursor node is already completed -> stop completion
            exit;
          end;
          break;
        end;
        EndProcNode:=CurProcNode;
        CurProcNode:=FindNextNodeOnSameLvl(CurProcNode);
      until (CurProcNode=nil) or (CurProcNode.Desc<>ctnProcedure)
      or ((CurProcNode.SubDesc and ctnsForwardDeclaration)=0);

      // find a nice insert position
      FindInsertPositionForForwardProc(SourceChangeCache,StartProcNode,
                                       Indent,InsertPos);

      // build nice procs
      CurProcNode:=StartProcNode;
      repeat
        ProcCode:=ExtractProcHead(CurProcNode,[phpWithStart,
                    phpWithoutClassKeyword,
                    phpWithVarModifiers,phpWithParameterNames,phpWithResultType,
                    phpWithCallingSpecs,phpDoNotAddSemicolon]);
        if ProcCode='' then
          RaiseException('CompleteForwardProcs: unable to parse forward proc node');
        if ProcCode[length(ProcCode)]<>';' then begin
          // add missing semicolon
          ProcCode:=ProcCode+';';
          UndoReadNextAtom;
          if not SourceChangeCache.Replace(gtNone,gtNone,
            CurPos.EndPos,CurPos.EndPos,';') then
              RaiseException('CompleteForwardProcs: unable to insert semicolon');
        end;
        ProcCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyProc(ProcCode,
                                                                   Indent,true);
        if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
          InsertPos,InsertPos,ProcCode) then
            RaiseException('CompleteForwardProcs: unable to insert new proc body');
        // next
        if CurProcNode=EndProcNode then break;
        CurProcNode:=FindNextNodeOnSameLvl(CurProcNode);
      until false;
      if not SourceChangeCache.Apply then
        RaiseException('CompleteForwardProcs: unable to apply changes');

      // reparse code and find jump point into new proc
      Result:=FindJumpPoint(CursorPos,NewPos,NewTopLine,RevertableJump);
    finally
      if ProcBodyNodes<>nil then begin
        ProcBodyNodes.FreeAndClear;
        ProcBodyNodes.Free;
      end;
    end;
  end;

  function CompleteEventAssignment: boolean;
  var SearchedClassName: string;
  { examples:
      Button1.OnClick:=|
      OnClick:=@AnEve|nt
      with Button1 do OnMouseDown:=@|

    If OnClick is a method then it will be completed to
      Button1.OnClick:=@Button1Click;
    and a 'procedure Button1Click(Sender: TObject);' with a method body will
    be added to the published section of the class of the Begin..End Block.
  }
  
    function CheckEventAssignmentSyntax(var PropertyAtom: TAtomPosition;
      var AssignmentOperator, AddrOperatorPos: integer;
      var UserEventAtom: TAtomPosition;
      var SemicolonPos: integer): boolean;
    begin
      Result:=false;

      // check if in begin..end block
      if not ((CursorNode.Desc=ctnBeginBlock)
              or CursorNode.HasParentOfType(ctnBeginBlock)) then exit;
      if CursorNode.Desc=ctnBeginBlock then
        BuildSubTreeForBeginBlock(CursorNode);
      CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
      // read event name (optional)
      
      while (CleanCursorPos<SrcLen)
      and (Src[CleanCursorPos] in [':','=',' ',#9]) do
        inc(CleanCursorPos);
      GetIdentStartEndAtPosition(Src,CleanCursorPos,
                                 UserEventAtom.StartPos,UserEventAtom.EndPos);
      MoveCursorToAtomPos(UserEventAtom);
      if AtomIsKeyWord then exit;
      ReadPriorAtom;
      // check @ operator (optional)
      if AtomIsChar('@') then begin
        AddrOperatorPos:=CurPos.StartPos;
        ReadPriorAtom;
      end else
        AddrOperatorPos:=-1;
      // check assignment operator :=
      if not AtomIs(':=') then exit;
      AssignmentOperator:=CurPos.StartPos;
      ReadPriorAtom;
      // check event name
      if not AtomIsIdentifier(false) then exit;
      PropertyAtom:=CurPos;
      
      // check for semicolon at end of statement
      MoveCursorToCleanPos(UserEventAtom.EndPos);
      ReadNextAtom;
      if AtomIsChar(';') then
        SemicolonPos:=CurPos.StartPos
      else
        SemicolonPos:=-1;
      
      {$IFDEF CTDEBUG}
      DebugLn('  CheckEventAssignmentSyntax: "',copy(Src,PropertyAtom.StartPos,
            UserEventAtom.EndPos-PropertyAtom.StartPos),'"');
      {$ENDIF}
      
      Result:=true;
    end;
    
    function FindEventTypeAtCursor(PropertyAtom: TAtomPosition;
      var PropertyContext, ProcContext: TFindContext;
      Params: TFindDeclarationParams): boolean;
    begin
      Result:=false;
      // find declaration of property identifier
      Params.ContextNode:=CursorNode;
      MoveCursorToCleanPos(PropertyAtom.StartPos);
      Params.SetIdentifier(Self,@Src[CurPos.StartPos],nil);
      fFullTopLvlName:='';
      Params.OnTopLvlIdentifierFound:=@OnTopLvlIdentifierFound;
      Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                     fdfTopLvlResolving,fdfFindVariable];
      if (not FindDeclarationOfIdentAtParam(Params))
      or (Params.NewNode.Desc<>ctnProperty) then begin
        {$IFDEF CTDEBUG}
        DebugLn('FindEventTypeAtCursor not a property');
        {$ENDIF}
        exit;
      end;
      PropertyContext:=CreateFindContext(Params);
      // identifier is property
      // -> check type of property
      Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors];
      ProcContext:=PropertyContext.Tool.FindBaseTypeOfNode(
                                                 Params,PropertyContext.Node);
      if (ProcContext.Node=nil) or (ProcContext.Node.Desc<>ctnProcedureType)
      then begin
        {$IFDEF CTDEBUG}
        DebugLn('FindEventTypeAtCursor not a procedure type');
        {$ENDIF}
        exit;
      end;
      // identifier is property of type proc => this is an event
      Result:=true;
    end;
    
    function CreateEventFullName(UserEventAtom,
      PropertyAtom: TAtomPosition): string;
    var PropertyName, AClassName: string;
      l: integer;
    begin
      if UserEventAtom.StartPos=UserEventAtom.EndPos then begin
        Result:=fFullTopLvlName;
        l:=PropertyAtom.EndPos-PropertyAtom.StartPos;
        PropertyName:=copy(Src,PropertyAtom.StartPos,l);
        if AnsiCompareText(PropertyName,RightStr(Result,l))<>0 then
          Result:=Result+PropertyName;
        if AnsiCompareText(PropertyName,Result)=0 then begin
          // this is an event of the class (not event of published objects)
          // -> add form name
          MoveCursorToNodeStart(AClassNode.Parent);
          ReadNextAtom;
          AClassName:=GetAtom;
          if (length(AClassName)>1) and (AClassName[1] in ['t','T']) then
            System.Delete(AClassName,1,1);
          Result:=AClassName+Result;
        end;
        // convert OnClick to Click
        if (UpperCaseStr(LeftStr(PropertyName,2))='ON')
        and (AnsiCompareText(RightStr(Result,l),PropertyName)=0)
        then
          Result:=LeftStr(Result,length(Result)-l)+RightStr(Result,l-2);
      end else begin
        Result:=copy(Src,UserEventAtom.StartPos,
                            UserEventAtom.EndPos-UserEventAtom.StartPos);
      end;
      {$IFDEF CTDEBUG}
      DebugLn('CreateEventFullName "',Result,'"');
      {$ENDIF}
    end;
    
    function FindClassAndProcNode: boolean;
    begin
      Result:=false;
      ProcNode:=CursorNode;
      while (ProcNode<>nil) do begin
        if (ProcNode.Desc=ctnProcedure) then begin
          SearchedClassname:=ExtractClassNameOfProcNode(ProcNode);
          if SearchedClassName<>'' then break;
        end;
        ProcNode:=ProcNode.Parent;
      end;
      if (ProcNode=nil) then exit;
      ANode:=FindFirstNodeOnSameLvl(ProcNode);
      if (ANode=nil) then exit;
      // search class node
      AClassNode:=FindClassNode(ANode,UpperCaseStr(SearchedClassName),
                                true,false);
      if AClassNode=nil then exit;
      Result:=true;
    end;
    
    function AddEventAndCompleteAssignment(const AnEventName: string;
      ProcContext: TFindContext;
      AssignmentOperator, AddrOperatorPos, SemicolonPos: integer;
      UserEventAtom: TAtomPosition;
      var MethodDefinition: string; var MethodAttr: TProcHeadAttributes
      ): boolean;
    var RValue, CleanMethodDefinition: string;
      StartInsertPos, EndInsertPos: integer;
    begin
      Result:=false;
      
      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: Extract method param list...');
      {$ENDIF}
      // extract method param list and result type
      CleanMethodDefinition:=UpperCaseStr(AnEventName)
                    +ProcContext.Tool.ExtractProcHead(ProcContext.Node,
                         [phpWithoutClassName, phpWithoutName, phpInUpperCase]);

      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: Initializing CodeCompletion...');
      {$ENDIF}
      // initialize class for code completion
      CodeCompleteClassNode:=AClassNode;
      CodeCompleteSrcChgCache:=SourceChangeCache;

      // insert new published method to class
      MethodAttr:=[phpWithStart, phpWithoutClassKeyword, phpWithVarModifiers,
                   phpWithParameterNames,phpWithDefaultValues,phpWithResultType];
      MethodDefinition:=TrimCodeSpace(ProcContext.Tool.ExtractProcHead(
                           ProcContext.Node,
                           MethodAttr+[phpWithoutClassName,phpWithoutName]));
      MethodDefinition:=SourceChangeCache.BeautifyCodeOptions.
                     AddClassAndNameToProc(MethodDefinition, '', AnEventName);
      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: Add Method To Class...');
      {$ENDIF}
      if not ProcExistsInCodeCompleteClass(CleanMethodDefinition) then begin
        // insert method definition into class
        AddClassInsertion(CleanMethodDefinition, MethodDefinition,
                          AnEventName, ncpPublishedProcs);
      end;
      MethodDefinition:=SourceChangeCache.BeautifyCodeOptions.
                     AddClassAndNameToProc(MethodDefinition,
                       ExtractClassName(AClassNode,false), AnEventName);
      if not InsertAllNewClassParts then
        RaiseException(ctsErrorDuringInsertingNewClassParts);

      // insert all missing proc bodies
      if not CreateMissingProcBodies then
        RaiseException(ctsErrorDuringCreationOfNewProcBodies);

      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: Changing right side of assignment...');
      {$ENDIF}
      // add new event name as right value of assignment
      // add address operator @ if needed or user provided it himself
      RValue:=AnEventName+';';
      if (AddrOperatorPos>0)
      or ((Scanner.PascalCompiler=pcFPC) and (Scanner.CompilerMode<>cmDelphi))
      then
        RValue:='@'+RValue;
      RValue:=':='+RValue;
      SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(RValue,0);
      StartInsertPos:=AssignmentOperator;
      EndInsertPos:=SemicolonPos+1;
      if EndInsertPos<1 then
        EndInsertPos:=UserEventAtom.EndPos;
      if EndInsertPos<1 then
        EndInsertPos:=AddrOperatorPos;
      if EndInsertPos<1 then
        EndInsertPos:=AssignmentOperator+2;
      SourceChangeCache.Replace(gtNone,gtNewLine,StartInsertPos,EndInsertPos,
                                RValue);
        
      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: Applying changes...');
      {$ENDIF}
      // apply the changes
      if not SourceChangeCache.Apply then
        RaiseException(ctsUnableToApplyChanges);
      Result:=true;
    end;
        
  // function CompleteEventAssignment: boolean
  var
    UserEventAtom, PropertyAtom: TAtomPosition;
    AssignmentOperator, AddrOperatorPos, SemicolonPos: integer;
    Params: TFindDeclarationParams;
    PropertyContext, ProcContext: TFindContext;
    FullEventName, AMethodDefinition: string;
    AMethodAttr: TProcHeadAttributes;
  begin
    Result:=false;

    {$IFDEF CTDEBUG}
    DebugLn('  CompleteEventAssignment: CheckEventAssignmentSyntax...');
    {$ENDIF}
    // check assigment syntax
    if not CheckEventAssignmentSyntax(PropertyAtom, AssignmentOperator,
                                   AddrOperatorPos, UserEventAtom, SemicolonPos)
    then
      exit;

    {$IFDEF CTDEBUG}
    DebugLn('  CompleteEventAssignment: find class of method...');
    {$ENDIF}
    if not FindClassAndProcNode then exit;

    ActivateGlobalWriteLock;
    Params:=TFindDeclarationParams.Create;
    try
      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: FindEventTypeAtCursor...');
      {$ENDIF}
      // check if identifier is event property and build
      Result:=FindEventTypeAtCursor(PropertyAtom,PropertyContext,ProcContext,
                                    Params);
      if not Result then exit;
      
      {$IFDEF CTDEBUG}
      DebugLn('  CompleteEventAssignment: CreateEventFullName... UserEventAtom.StartPos=',dbgs(UserEventAtom.StartPos));
      {$ENDIF}
      // create a nice event name
      FullEventName:=CreateEventFullName(UserEventAtom,PropertyAtom);
      if FullEventName='' then exit;
      
    finally
      Params.Free;
      DeactivateGlobalWriteLock;
    end;

    // add published method and method body and right side of assignment
    if not AddEventAndCompleteAssignment(FullEventName,ProcContext,
      AssignmentOperator,AddrOperatorPos,SemicolonPos,UserEventAtom,
      AMethodDefinition, AMethodAttr)
    then
      RaiseException('CompleteEventAssignment Internal Error 1');
      
    {$IFDEF CTDEBUG}
    DebugLn('  CompleteEventAssignment: jumping to new method body...');
    {$ENDIF}
    // jump to new method body
    if not JumpToMethod(AMethodDefinition,AMethodAttr,NewPos,NewTopLine,false)
    then
      RaiseException('CompleteEventAssignment Internal Error 2');
      
    CompleteCode:=true;
  end;
  
// function CompleteCode(CursorPos: TCodeXYPosition;
//        var NewPos: TCodeXYPosition; var NewTopLine: integer;
//        SourceChangeCache: TSourceChangeCache): boolean;
begin
  //DebugLn(['TCodeCompletionCodeTool.CompleteCode CursorPos=',DbgsCXY(CursorPos),' OldTopLine=',OldTopLine]);

  Result:=false;
  if (SourceChangeCache=nil) then 
    RaiseException('need a SourceChangeCache');
  BuildTreeAndGetCleanPos(trAll,CursorPos, CleanCursorPos,[]);
  OldCleanCursorPos:=CleanCursorPos;
  NewPos:=CleanCodeXYPosition;
  NewTopLine:=0;

  // find CodeTreeNode at cursor
  // skip newline chars
  while (CleanCursorPos>1) and (Src[CleanCursorPos] in [#10,#13]) do
    dec(CleanCursorPos);
  // skip space (first try left)
  while (CleanCursorPos>1) and (Src[CleanCursorPos] in [' ',#9,';']) do
    dec(CleanCursorPos);
  if (CleanCursorPos>0) and (CleanCursorPos<SrcLen)
  and (Src[CleanCursorPos] in [#10,#13]) then begin
    // then try right
    repeat
      inc(CleanCursorPos);
    until (CleanCursorPos>=SrcLen) or (not (Src[CleanCursorPos] in [' ',#9]));
  end;
  
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  CodeCompleteSrcChgCache:=SourceChangeCache;

  {$IFDEF CTDEBUG}
  DebugLn('TCodeCompletionCodeTool.CompleteCode A CleanCursorPos=',dbgs(CleanCursorPos),' NodeDesc=',NodeDescriptionAsString(CursorNode.Desc));
  {$ENDIF}
  ImplementationNode:=FindImplementationNode;
  if ImplementationNode=nil then ImplementationNode:=Tree.Root;

  // test if in a class
  AClassNode:=CursorNode.GetNodeOfType(ctnClass);
  if AClassNode<>nil then begin
    CompleteClass;
    exit;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TCodeCompletionCodeTool.CompleteCode not in-a-class ... ');
  {$ENDIF}
  
  // test if forward proc
  //debugln('TCodeCompletionCodeTool.CompleteCode ',CursorNode.DescAsString);
  ProcNode:=CursorNode.GetNodeOfType(ctnProcedure);
  if (ProcNode=nil) and (CursorNode.Desc=ctnProcedure) then
    ProcNode:=CursorNode;
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure)
  and ((ProcNode.SubDesc and ctnsForwardDeclaration)>0) then begin
    // Node is forward Proc
    CompleteForwardProcs;
    exit;
  end;
  
  // test if Event assignment
  Result:=CompleteEventAssignment;
  if Result then exit;
  
  // test if Local variable assignment
  Result:=CompleteLocalVariableAssignment(CleanCursorPos,OldTopLine,
                                CursorNode,NewPos,NewTopLine,SourceChangeCache);
  if Result then exit;
  
  // test if undeclared local variable as parameter
  Result:=CompleteLocalVariableAsParameter(CleanCursorPos,OldTopLine,
                                CursorNode,NewPos,NewTopLine,SourceChangeCache);
  if Result then exit;

  // test if method body
  Result:=CompleteMethod(OldCleanCursorPos,OldTopLine,CursorNode,
                         NewPos,NewTopLine,SourceChangeCache);
  if Result then exit;

  {$IFDEF CTDEBUG}
  DebugLn('TCodeCompletionCodeTool.CompleteCode  nothing to complete ... ');
  {$ENDIF}
end;

constructor TCodeCompletionCodeTool.Create;
begin
  inherited Create;
  FSetPropertyVariablename:='AValue';
  FCompleteProperties:=true;
  FAddInheritedCodeToOverrideMethod:=true;
end;


end.

