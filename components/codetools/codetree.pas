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
   A TCodeTree is the product of a code tool. Every TCodeTreeNode describes a
   logical block in the code (e.g. a class, a procedure or an identifier).

   This unit defines also all valid CodeTree-Node-Descriptors, constants for
   TCodeTreeNode types.

}
unit CodeTree;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, BasicCodeTools, AVL_Tree,
  CodeToolMemManager;

//-----------------------------------------------------------------------------

type
  TCodeTreeNodeDesc = word;
  TCodeTreeNodeSubDesc = word;

const
  // CodeTreeNodeDescriptors
  ctnNone            = 0;

  ctnProgram         = 1;
  ctnPackage         = 2;
  ctnLibrary         = 3;
  ctnUnit            = 4;
  ctnInterface       = 5;
  ctnImplementation  = 6;
  ctnInitialization  = 7;
  ctnFinalization    = 8;

  ctnTypeSection     = 10;
  ctnVarSection      = 11;
  ctnConstSection    = 12;
  ctnResStrSection   = 13;
  ctnLabelSection    = 14;
  ctnUsesSection     = 15;

  ctnTypeDefinition  = 20;
  ctnVarDefinition   = 21;
  ctnConstDefinition = 22;

  ctnClass           = 30;
  ctnClassInterface  = 31;
  ctnClassPublished  = 32;
  ctnClassPrivate    = 33;
  ctnClassProtected  = 34;
  ctnClassPublic     = 35;
  ctnClassGUID       = 36;

  ctnProperty        = 40;
  ctnMethodMap       = 41;
  
  ctnProcedure       = 50;
  ctnProcedureHead   = 51;
  ctnParameterList   = 52;

  ctnIdentifier      = 60;
  ctnArrayType       = 61;
  ctnOfConstType     = 62;
  ctnRecordType      = 63;
  ctnRecordCase      = 64;
  ctnRecordVariant   = 65;
  ctnProcedureType   = 66;
  ctnSetType         = 67;
  ctnRangeType       = 68;
  ctnEnumerationType = 69;
  ctnEnumIdentifier  = 70;
  ctnLabelType       = 71;
  ctnTypeType        = 72;
  ctnFileType        = 73;
  ctnPointerType     = 74;
  ctnClassOfType     = 75;
  ctnVariantType     = 76;

  ctnBeginBlock      = 80;
  ctnAsmBlock        = 81;

  ctnWithVariable    = 90;
  ctnWithStatement   = 91;
  ctnCaseBlock       = 92;
  ctnCaseVariable    = 93;
  ctnCaseStatement   = 94;


  // combined values
  AllCodeSections =
     [ctnProgram, ctnPackage, ctnLibrary, ctnUnit, ctnInterface,
      ctnImplementation, ctnInitialization, ctnFinalization];
  AllClassSections =
     [ctnClassPublic,ctnClassPublished,ctnClassPrivate,ctnClassProtected];
  AllDefinitionSections =
     [ctnTypeSection,ctnVarSection,ctnConstSection,ctnResStrSection,
      ctnLabelSection];
  AllIdentifierDefinitions =
     [ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition];
  AllPascalTypes =
     [ctnClass,ctnClassInterface,
      ctnIdentifier,ctnArrayType,ctnRecordType,ctnRecordCase,ctnRecordVariant,
      ctnProcedureType,ctnSetType,ctnRangeType,ctnEnumerationType,
      ctnEnumIdentifier,ctnLabelType,ctnTypeType,ctnFileType,ctnPointerType,
      ctnClassOfType,ctnVariantType];
  AllPascalStatements = [ctnBeginBlock,ctnWithStatement,ctnWithVariable,
                         ctnCaseBlock,ctnCaseStatement,ctnCaseVariable];
  AllSourceTypes =
     [ctnProgram,ctnPackage,ctnLibrary,ctnUnit];
  AllUsableSourceTypes =
     [ctnUnit];
  AllFindContextDescs = AllIdentifierDefinitions + AllCodeSections +
     [ctnClass,ctnClassInterface,ctnProcedure];


  // CodeTreeNodeSubDescriptors
  ctnsNone                = 0;
  ctnsForwardDeclaration  = 1 shl 0;
  ctnsNeedJITParsing      = 1 shl 1;
  ctnsHasDefaultValue     = 1 shl 2;

  
type
  // Procedure Specifiers
  TProcedureSpecifier = (
    psSTDCALL, psREGISTER, psPOPSTACK, psVIRTUAL, psABSTRACT, psDYNAMIC,
    psOVERLOAD, psOVERRIDE, psREINTRODUCE, psCDECL, psINLINE, psMESSAGE,
    psEXTERNAL, psFORWARD, psPASCAL, psASSEMBLER, psSAVEREGISTERS,
    psFAR, psNEAR, psEdgedBracket);
  TAllProcedureSpecifiers = set of TProcedureSpecifier;

const
  ProcedureSpecifierNames: array[TProcedureSpecifier] of shortstring = (
      'STDCALL', 'REGISTER', 'POPSTACK', 'VIRTUAL', 'ABSTRACT', 'DYNAMIC',
      'OVERLOAD', 'OVERRIDE', 'REINTRODUCE', 'CDECL', 'INLINE', 'MESSAGE',
      'EXTERNAL', 'FORWARD', 'PASCAL', 'ASSEMBLER', 'SAVEREGISTERS',
      'FAR', 'NEAR', '['
    );

  
type
  TCodeTreeNode = class
  public
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    Parent, NextBrother, PriorBrother, FirstChild, LastChild: TCodeTreeNode;
    StartPos, EndPos: integer;
    Cache: TObject;
    function Next: TCodeTreeNode;
    function Prior: TCodeTreeNode;
    function HasAsParent(Node: TCodeTreeNode): boolean;
    function HasAsChild(Node: TCodeTreeNode): boolean;
    function HasParentOfType(ParentDesc: TCodeTreeNodeDesc): boolean;
    function GetNodeOfType(ADesc: TCodeTreeNodeDesc): TCodeTreeNode;
    function GetFindContextParent: TCodeTreeNode;
    function GetLevel: integer;
    function DescAsString: string;
    function GetRoot: TCodeTreeNode;
    function FindOwner: TObject;
    procedure Clear;
    constructor Create;
    function ConsistencyCheck: integer; // 0 = ok
  end;

  TCodeTree = class
  private
    FNodeCount: integer;
  public
    Root: TCodeTreeNode;
    property NodeCount: integer read FNodeCount;
    procedure DeleteNode(ANode: TCodeTreeNode);
    procedure AddNodeAsLastChild(ParentNode, ANode: TCodeTreeNode);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;

  TCodeTreeNodeExtension = class
  public
    Node: TCodeTreeNode;
    Txt: string;
    ExtTxt1, ExtTxt2, ExtTxt3: string;
    Position: integer;
    Data: Pointer;
    Flags: cardinal;
    Next: TCodeTreeNodeExtension;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;

  // memory system for TCodeTreeNode(s)
  TCodeTreeNodeMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeNode(ANode: TCodeTreeNode);
    function NewNode: TCodeTreeNode;
  end;

  // memory system for TCodeTreeNodeExtension(s)
  TCodeTreeNodeExtMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeNode(ANode: TCodeTreeNodeExtension);
    procedure DisposeAVLTree(TheTree: TAVLTree);
    function NewNode: TCodeTreeNodeExtension;
  end;


var
  NodeExtMemManager: TCodeTreeNodeExtMemManager;
  NodeMemManager: TCodeTreeNodeMemManager;

//-----------------------------------------------------------------------------
// useful functions
function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
function CompareCodeTreeNodeExt(NodeData1, NodeData2: pointer): integer;
function CompareCodeTreeNodeExtWithPos(NodeData1, NodeData2: pointer): integer;
function CompareCodeTreeNodeExtWithNodeStartPos(
  NodeData1, NodeData2: pointer): integer;

type
  TOnFindOwnerOfCodeTreeNode = function (ANode: TCodeTreeNode): TObject;
  
var
  OnFindOwnerOfCodeTreeNode: TOnFindOwnerOfCodeTreeNode;
  
function FindOwnerOfCodeTreeNode(ANode: TCodeTreeNode): TObject;


implementation


function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
begin
  case Desc of
  ctnNone: Result:='None';

  ctnClass: Result:='Class';
  ctnClassInterface: Result:='Class Interface';
  ctnClassPublished: Result:='Published';
  ctnClassPrivate: Result:='Private';
  ctnClassProtected: Result:='Protected';
  ctnClassPublic: Result:='Public';

  ctnProcedure: Result:='Procedure';
  ctnProcedureHead: Result:='ProcedureHead';
  ctnParameterList: Result:='ParameterList';

  ctnBeginBlock: Result:='BeginBlock';
  ctnAsmBlock: Result:='AsmBlock';

  ctnProgram: Result:='Program';
  ctnPackage: Result:='Package';
  ctnLibrary: Result:='Library';
  ctnUnit: Result:='Unit';
  ctnInterface: Result:='Interface Section';
  ctnImplementation: Result:='Implementation';
  ctnInitialization: Result:='Initialization';
  ctnFinalization: Result:='Finalization';

  ctnTypeSection: Result:='Type Section';
  ctnVarSection: Result:='Var Section';
  ctnConstSection: Result:='Const Section';
  ctnResStrSection: Result:='Resource String Section';
  ctnUsesSection: Result:='Uses Section';

  ctnTypeDefinition: Result:='Type';
  ctnVarDefinition: Result:='Var';
  ctnConstDefinition: Result:='Const';

  ctnProperty: Result:='Property';
  ctnMethodMap: Result:='Method Map';

  ctnIdentifier: Result:='Identifier';
  ctnArrayType: Result:='Array Type';
  ctnRecordType: Result:='Record Type';
  ctnRecordCase: Result:='Record Case';
  ctnRecordVariant: Result:='Record Variant';
  ctnProcedureType: Result:='Procedure Type';
  ctnSetType: Result:='Set Type';
  ctnRangeType: Result:='Subrange Type';
  ctnEnumerationType: Result:='Enumeration Type';
  ctnEnumIdentifier: Result:='Enumeration Identifier';
  ctnLabelType: Result:='Label Type';
  ctnTypeType: Result:='''Type'' Type';
  ctnFileType: Result:='File Type';
  ctnPointerType: Result:='Pointer ^ Type';
  ctnClassOfType: Result:='Class Of Type';

  ctnWithVariable: Result:='With Variable';
  ctnWithStatement: Result:='With Statement';
  ctnCaseBlock: Result:='Case Block';
  ctnCaseVariable: Result:='Case Variable';
  ctnCaseStatement: Result:='Case Statement';

  else
    Result:='invalid descriptor';
  end;
end;

function CompareCodeTreeNodeExt(NodeData1, NodeData2: pointer): integer;
var NodeExt1, NodeExt2: TCodeTreeNodeExtension;
begin
  NodeExt1:=TCodeTreeNodeExtension(NodeData1);
  NodeExt2:=TCodeTreeNodeExtension(NodeData2);
  Result:=CompareTextIgnoringSpace(NodeExt1.Txt,NodeExt2.Txt,false);
end;

function CompareCodeTreeNodeExtWithPos(NodeData1, NodeData2: pointer): integer;
var NodeExt1Pos, NodeExt2Pos: integer;
begin
  NodeExt1Pos:=TCodeTreeNodeExtension(NodeData1).Position;
  NodeExt2Pos:=TCodeTreeNodeExtension(NodeData2).Position;
  if NodeExt1Pos<NodeExt2Pos then
    Result:=1
  else if NodeExt1Pos>NodeExt2Pos then
    Result:=-1
  else
    Result:=0;
end;

function CompareCodeTreeNodeExtWithNodeStartPos(
  NodeData1, NodeData2: pointer): integer;
var NodeExt1Pos, NodeExt2Pos: integer;
begin
  NodeExt1Pos:=TCodeTreeNodeExtension(NodeData1).Node.StartPos;
  NodeExt2Pos:=TCodeTreeNodeExtension(NodeData2).Node.StartPos;
  if NodeExt1Pos<NodeExt2Pos then
    Result:=1
  else if NodeExt1Pos>NodeExt2Pos then
    Result:=-1
  else
    Result:=0;
end;

function FindOwnerOfCodeTreeNode(ANode: TCodeTreeNode): TObject;
begin
  if Assigned(OnFindOwnerOfCodeTreeNode) then
    Result:=OnFindOwnerOfCodeTreeNode(ANode)
  else
    Result:=nil;
end;

{ TCodeTreeNode }

constructor TCodeTreeNode.Create;
begin
  Clear;
end;

procedure TCodeTreeNode.Clear;
begin
  Desc:=ctnNone;
  SubDesc:=ctnsNone;
  Parent:=nil;
  NextBrother:=nil;
  PriorBrother:=nil;
  FirstChild:=nil;
  LastChild:=nil;
  StartPos:=-1;
  EndPos:=-1;
end;

function TCodeTreeNode.Next: TCodeTreeNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.NextBrother=nil) do
    Result:=Result.Parent;
  if Result<>nil then Result:=Result.NextBrother;
end;

function TCodeTreeNode.Prior: TCodeTreeNode;
begin
  if PriorBrother<>nil then
    Result:=PriorBrother
  else
    Result:=Parent;
end;

function TCodeTreeNode.ConsistencyCheck: integer;
// 0 = ok
begin
  if (EndPos>0) and (StartPos>EndPos) then begin
    Result:=-1;  exit;
  end;
  if (Parent<>nil) then begin
    if (PriorBrother=nil) and (Parent.FirstChild<>Self) then begin
      Result:=-2;  exit;
    end;
    if (NextBrother=nil) and (Parent.LastChild<>Self) then begin
      Result:=-3;  exit;
    end;
  end;
  if (NextBrother<>nil) and (NextBrother.PriorBrother<>Self) then begin
    Result:=-4;  exit;
  end;
  if (PriorBrother<>nil) and (PriorBrother.NextBrother<>Self) then begin
    Result:=-5;  exit;
  end;
  if (FirstChild<>nil) then begin
    Result:=FirstChild.ConsistencyCheck;
    if Result<>0 then exit;
  end;
  if NextBrother<>nil then begin
    Result:=NextBrother.ConsistencyCheck;
    if Result<>0 then exit;
  end;
  Result:=0;
end;

function TCodeTreeNode.HasAsParent(Node: TCodeTreeNode): boolean;
var CurNode: TCodeTreeNode;
begin
  Result:=false;
  if Node=nil then exit;
  CurNode:=Parent;
  while (CurNode<>nil) do begin
    if CurNode=Node then begin
      Result:=true;
      exit;
    end;
    CurNode:=CurNode.Parent;
  end;
end;

function TCodeTreeNode.HasAsChild(Node: TCodeTreeNode): boolean;
begin
  Result:=false;
  if Node=nil then exit;
  Result:=Node.HasAsParent(Self);
end;

function TCodeTreeNode.HasParentOfType(ParentDesc: TCodeTreeNodeDesc): boolean;
var ANode: TCodeTreeNode;
begin
  ANode:=Parent;
  while (ANode<>nil) and (ANode.Desc<>ParentDesc) do
    ANode:=ANode.Parent;
  Result:=ANode<>nil;
end;

function TCodeTreeNode.GetNodeOfType(ADesc: TCodeTreeNodeDesc
  ): TCodeTreeNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.Desc<>ADesc) do
    Result:=Result.Parent;
end;

function TCodeTreeNode.GetFindContextParent: TCodeTreeNode;
begin
  Result:=Parent;
  while (Result<>nil) and (not (Result.Desc in AllFindContextDescs)) do
    Result:=Result.Parent;
end;

function TCodeTreeNode.GetLevel: integer;
var ANode: TCodeTreeNode;
begin
  Result:=0;
  ANode:=Parent;
  while ANode<>nil do begin
    inc(Result);
    ANode:=ANode.Parent;
  end;
end;

function TCodeTreeNode.DescAsString: string;
begin
  Result:=NodeDescriptionAsString(Desc);
end;

function TCodeTreeNode.GetRoot: TCodeTreeNode;
begin
  Result:=Self;
  while (Result.Parent<>nil) do Result:=Result.Parent;
  while (Result.PriorBrother<>nil) do Result:=Result.PriorBrother;
end;

function TCodeTreeNode.FindOwner: TObject;
begin
  Result:=FindOwnerOfCodeTreeNode(Self);
end;

{ TCodeTree }

constructor TCodeTree.Create;
begin
  inherited Create;
  Root:=nil;
  FNodeCount:=0;
end;

destructor TCodeTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeTree.Clear;
var ANode: TCodeTreeNode;
begin
  while Root<>nil do begin
    ANode:=Root;
    Root:=ANode.NextBrother;
    DeleteNode(ANode);
  end;
end;

procedure TCodeTree.DeleteNode(ANode: TCodeTreeNode);
begin
  if ANode=nil then exit;
  while (ANode.FirstChild<>nil) do DeleteNode(ANode.FirstChild);
  with ANode do begin
    if (Parent<>nil) then begin
      if (Parent.FirstChild=ANode) then
        Parent.FirstChild:=NextBrother;
      if (Parent.LastChild=ANode) then
        Parent.LastChild:=PriorBrother;
      Parent:=nil;
    end;
    if NextBrother<>nil then NextBrother.PriorBrother:=PriorBrother;
    if PriorBrother<>nil then PriorBrother.NextBrother:=NextBrother;
    NextBrother:=nil;
    PriorBrother:=nil;
  end;
  if ANode=Root then Root:=nil;
  dec(FNodeCount);
  NodeMemManager.DisposeNode(ANode);
end;

procedure TCodeTree.AddNodeAsLastChild(ParentNode, ANode: TCodeTreeNode);
var TopNode: TCodeTreeNode;
begin
  ANode.Parent:=ParentNode;
  if Root=nil then begin
    // set as root
    Root:=ANode;
    while Root.Parent<>nil do Root:=Root.Parent;
  end else if ParentNode<>nil then begin
    if ParentNode.FirstChild=nil then begin
      // add as first child
      ParentNode.FirstChild:=ANode;
      ParentNode.LastChild:=ANode;
    end else begin
      // add as last child
      ANode.PriorBrother:=ParentNode.LastChild;
      ParentNode.LastChild:=ANode;
      if ANode.PriorBrother<>nil then ANode.PriorBrother.NextBrother:=ANode;
    end;
  end else begin
    // add as last brother of top nodes
    TopNode:=Root;
    while (TopNode.NextBrother<>nil) do TopNode:=TopNode.NextBrother;
    ANode.PriorBrother:=TopNode;
    ANode.PriorBrother.NextBrother:=ANode;
  end;
  inc(FNodeCount);
end;

function TCodeTree.ConsistencyCheck: integer;
// 0 = ok
var RealNodeCount: integer;

  procedure CountNodes(ANode: TCodeTreeNode);
  begin
    if ANode=nil then exit;
    inc(RealNodeCount);
    CountNodes(ANode.FirstChild);
    CountNodes(ANode.NextBrother);
  end;

begin
  if Root<>nil then begin
    Result:=Root.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);  exit;
    end;
    if Root.Parent<>nil then begin
      Result:=-1;  exit;
    end;
  end;
  RealNodeCount:=0;
  CountNodes(Root);
  if RealNodeCount<>FNodeCount then begin
    Result:=-2;  exit;
  end;
  Result:=0;
end;

procedure TCodeTree.WriteDebugReport;
begin
  writeln('[TCodeTree.WriteDebugReport] Consistency=',ConsistencyCheck,
    ' Root=',Root<>nil);
end;

{ TCodeTreeNodeExtension }

procedure TCodeTreeNodeExtension.Clear;
begin
  Next:=nil;
  Txt:='';
  ExtTxt1:='';
  ExtTxt2:='';
  ExtTxt3:='';
  Node:=nil;
  Position:=-1;
  Data:=nil;
  Flags:=0;
end;

constructor TCodeTreeNodeExtension.Create;
begin
  inherited Create;
  Clear;
end;

destructor TCodeTreeNodeExtension.Destroy;
begin
  inherited Destroy;
end;

function TCodeTreeNodeExtension.ConsistencyCheck: integer;
// 0 = ok
begin
  Result:=0;
end;

procedure TCodeTreeNodeExtension.WriteDebugReport;
begin
  // nothing special
end;

{ TCodeTreeNodeMemManager }

function TCodeTreeNodeMemManager.NewNode: TCodeTreeNode;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TCodeTreeNode(FFirstFree);
    TCodeTreeNode(FFirstFree):=Result.NextBrother;
    Result.NextBrother:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    Result:=TCodeTreeNode.Create;
    inc(FAllocatedCount);
  end;
  inc(FCount);
end;

procedure TCodeTreeNodeMemManager.DisposeNode(ANode: TCodeTreeNode);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ANode to Free list
    ANode.Clear;
    ANode.NextBrother:=TCodeTreeNode(FFirstFree);
    TCodeTreeNode(FFirstFree):=ANode;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    ANode.Free;
    inc(FFreedCount);
  end;
  dec(FCount);
end;

procedure TCodeTreeNodeMemManager.FreeFirstItem;
var ANode: TCodeTreeNode;
begin
  ANode:=TCodeTreeNode(FFirstFree);
  TCodeTreeNode(FFirstFree):=ANode.NextBrother;
  ANode.Free;
end;

{ TCodeTreeNodeExtMemManager }

function TCodeTreeNodeExtMemManager.NewNode: TCodeTreeNodeExtension;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TCodeTreeNodeExtension(FFirstFree);
    TCodeTreeNodeExtension(FFirstFree):=Result.Next;
    Result.Next:=nil;
  end else begin
    // free list empty -> create new node
    Result:=TCodeTreeNodeExtension.Create;
  end;
  inc(FCount);
end;

procedure TCodeTreeNodeExtMemManager.DisposeNode(ANode: TCodeTreeNodeExtension);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ANode to Free list
    ANode.Clear;
    ANode.Next:=TCodeTreeNodeExtension(FFirstFree);
    TCodeTreeNodeExtension(FFirstFree):=ANode;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    ANode.Free;
  end;
  dec(FCount);
end;

procedure TCodeTreeNodeExtMemManager.DisposeAVLTree(TheTree: TAVLTree);
var ANode: TAVLTreeNode;
begin
  if TheTree=nil then exit;
  ANode:=TheTree.FindLowest;
  while ANode<>nil do begin
    DisposeNode(TCodeTreeNodeExtension(ANode.Data));
    ANode:=TheTree.FindSuccessor(ANode);
  end;
  TheTree.Free;
end;

procedure TCodeTreeNodeExtMemManager.FreeFirstItem;
var ANode: TCodeTreeNodeExtension;
begin
  ANode:=TCodeTreeNodeExtension(FFirstFree);
  TCodeTreeNodeExtension(FFirstFree):=ANode.Next;
  ANode.Free;
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
begin
  NodeMemManager:=TCodeTreeNodeMemManager.Create;
  NodeExtMemManager:=TCodeTreeNodeExtMemManager.Create;
end;

procedure InternalFinal;
begin
  NodeExtMemManager.Free;
  NodeMemManager.Free;
end;


initialization
  InternalInit;

finalization
{$IFDEF CTDEBUG}
writeln('codetree.pp - finalization');
{$ENDIF}
{$IFDEF MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  InternalFinal;

end.

