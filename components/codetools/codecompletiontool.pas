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
      - complete event assignments
      - complete local variables
      - complete local variables as parameter
      - insert header comment for classes

  ToDo:
    -add code for index properties (TList, TFPList, array of, Pointer array)
      TList:
        property Items[Index: integer]: AType accesstlist;
        -> creates
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
  PascalParserTool, MethodJumpTool, FindDeclarationTool, KeywordFuncLists,
  CodeToolsStructs, BasicCodeTools, LinkScanner, SourceChanger,
  AVL_Tree;

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

    // custom class completion
    function InitClassCompletion(const UpperClassName: string;
                                 SourceChangeCache: TSourceChangeCache): boolean;
    function ApplyClassCompletion: boolean;
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
var TrimmedIdentifier: string;
begin
  if not (fdfTopLvlResolving in Params.Flags) then exit;
  with Params do begin
    case NewNode.Desc of
    ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition:
      TrimmedIdentifier:=NewCodeTool.ExtractIdentifier(NewNode.StartPos);
    ctnProperty:
      begin
        NewCodeTool.MoveCursorToNodeStart(NewNode);
        NewCodeTool.ReadNextAtom; // 'property'
        NewCodeTool.ReadNextAtom; // name
        TrimmedIdentifier:=NewCodeTool.GetAtom;
      end;
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
  DebugLn('[TCodeCompletionCodeTool.AddClassInsertion] ',CleanDef,',',Def,',',Identifiername);
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
  if VarExistsInCodeCompleteClass(UpperCaseStr(VarName)) then begin

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
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
  if (ClassNode=nil) then exit;
  CodeCompleteClassNode:=ClassNode;
  CodeCompleteSrcChgCache:=SourceChangeCache;
  FreeClassInsertionList;
  Result:=true;
end;

function TCodeCompletionCodeTool.ApplyClassCompletion: boolean;
begin
  Result:=false;
  try
    // insert all new class parts
    if not InsertAllNewClassParts then
      RaiseException(ctsErrorDuringInsertingNewClassParts);
    // insert all missing proc bodies
    if not CreateMissingProcBodies then
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
            InsertPos:=FindFirstLineEndAfterInCode(InsertNode.EndPos);
          end else begin
            // insert in front of InsertNode
            InsertPos:=InsertNode.StartPos;
          end;
        end else begin
          // insert as first variable/proc
          Indent:=GetLineIndent(Src,ClassSectionNode.StartPos)
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
          InsertPos:=FindFirstLineEndAfterInCode(ClassSectionNode.StartPos);
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
  ClassIdentifierNode:=ClassNode.GetNodeOfType(ctnTypeDefinition);
  if ClassIdentifierNode=nil then begin
    DebugLn('TCodeCompletionCodeTool.InsertClassHeaderComment WARNING: class without name');
    exit;
  end;
  if not CleanPosToCaret(ClassIdentifierNode.StartPos,StartPos) then exit;
  Code:=ExtractIdentifier(ClassIdentifierNode.StartPos);
  
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
      ProcCode:=ExtractProcHead(ProcNode,[phpWithStart,phpWithoutClassKeyword,
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
  
  procedure FindTopMostAndBottomMostProcCodies;
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
      ANode:=FCodeCompleteClassNode.GetNodeOfType(ctnTypeDefinition);
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
    FindTopMostAndBottomMostProcCodies;

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
  ProcCode: string;

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
        // due to insertions in fron of the class, the cursor position could
        // have changed
        while (CursorNode<>nil) do begin
          if (CursorNode.Desc in [ctnTypeSection,ctnTypeDefinition])
          then break;
          CursorNode:=CursorNode.Parent;
        end;
        FCodeCompleteClassNode:=
                              FindClassNode(CursorNode,CurClassName,true,false);
        if FCodeCompleteClassNode=nil then
          RaiseException('oops, I lost your class');
        ANode:=FCodeCompleteClassNode.GetNodeOfType(ctnTypeDefinition);
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
  
  procedure CompleteMethod;
  begin
    // ToDo
  end;
  
  procedure CompleteForwardProcs;
  // add proc bodies for forward procs
  var
    RevertableJump: boolean;
    ProcBodyNodes: TAVLTree;
    StartProcNode: TCodeTreeNode;
    CurProcNode: TCodeTreeNode;
    EndProcNode: TCodeTreeNode;
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
                    phpWithCallingSpecs]);
        if ProcCode='' then
          RaiseException('CompleteForwardProcs: unable to parse forward proc node');
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
  Result:=false;
  if (SourceChangeCache=nil) then 
    RaiseException('need a SourceChangeCache');
  BuildTreeAndGetCleanPos(trAll,CursorPos, CleanCursorPos,[]);
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
  Result:=CompleteLocalVariableAssignment(CleanCursorPos,OldTopLine,CursorNode,
                                          NewPos,NewTopLine,SourceChangeCache);
  if Result then exit;
  
  // test if undeclared local variable as parameter
  Result:=CompleteLocalVariableAsParameter(CleanCursorPos,OldTopLine,CursorNode,
                                           NewPos,NewTopLine,SourceChangeCache);
  if Result then exit;

  // test if method body
  ProcNode:=CursorNode.GetNodeOfType(ctnProcedure);
  if (ProcNode=nil) and (CursorNode.Desc=ctnProcedure) then
    ProcNode:=CursorNode;
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure)
  and ((ProcNode.SubDesc and ctnsForwardDeclaration)=0)
  and NodeIsMethodBody(ProcNode) then begin
    CompleteMethod;
    exit;
  end;


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

