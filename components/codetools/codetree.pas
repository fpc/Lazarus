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
  Classes, SysUtils, BasicCodeTools, AVL_Tree;

//-----------------------------------------------------------------------------

type
  TCodeTreeNodeDesc = word;
  TCodeTreeNodeSubDesc = word;

const
  // CodeTreeNodeDescriptors
  ctnNone            = 0;

  ctnClass           = 1;
  ctnClassPublished  = 2;
  ctnClassPrivate    = 3;
  ctnClassProtected  = 4;
  ctnClassPublic     = 5;

  ctnProcedure       = 10;
  ctnProcedureHead   = 11;
  ctnParameterList   = 12;

  ctnBeginBlock      = 20;
  ctnAsmBlock        = 21;

  ctnProgram         = 30;
  ctnPackage         = 31;
  ctnLibrary         = 32;
  ctnUnit            = 33;
  ctnInterface       = 34;
  ctnImplementation  = 35;
  ctnInitialization  = 36;
  ctnFinalization    = 37;

  ctnTypeSection     = 40;
  ctnVarSection      = 41;
  ctnConstSection    = 42;
  ctnResStrSection   = 43;
  ctnUsesSection     = 44;

  ctnTypeDefinition  = 50;
  ctnVarDefinition   = 51;
  ctnConstDefinition = 52;

  ctnProperty        = 60;
  
  ctnIdentifier      = 70;
  ctnArrayType       = 71;
  ctnRecordType      = 72;
  ctnRecordCase      = 73;
  ctnRecordVariant   = 74;
  ctnProcedureType   = 75;
  ctnSetType         = 76;
  ctnRangeType       = 77;
  ctnEnumType        = 78;
  ctnLabelType       = 79;
  ctnTypeType        = 80;
  ctnFileType        = 81;
  ctnPointerType     = 82;
  ctnClassOfType     = 83;
  
  ctnWithVariable    = 90;
  ctnWithStatement   = 91;


  // combined values
  AllCodeSections =
     [ctnProgram, ctnPackage, ctnLibrary, ctnUnit, ctnInterface,
      ctnImplementation, ctnInitialization, ctnFinalization];
  AllClassSections =
     [ctnClassPublic,ctnClassPublished,ctnClassPrivate,ctnClassProtected];
  AllDefinitionSections =
     [ctnTypeSection,ctnVarSection,ctnConstSection,ctnResStrSection];
  AllIdentifierDefinitions =
     [ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition];
  AllPascalTypes =
     [ctnClass,
      ctnIdentifier,ctnArrayType,ctnRecordType,ctnRecordCase,ctnRecordVariant,
      ctnProcedureType,ctnSetType,ctnRangeType,ctnEnumType,ctnLabelType,
      ctnTypeType,ctnFileType,ctnPointerType,ctnClassOfType];


  // CodeTreeNodeSubDescriptors
  ctnsNone               = 0;
  ctnsForwardDeclaration = 1;
  
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
    function DescAsString: string;
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
    ExtTxt1, ExtTxt2: string;
    Next: TCodeTreeNodeExtension;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;

  // memory system for TCodeTreeNode(s)
  TCodeTreeNodeMemManager = class
  private
    FFirstFree: TCodeTreeNode;
    FFreeCount: integer;
    FCount: integer;
    FMinFree: integer;
    FMaxFreeRatio: integer;
    FAllocatedNodes: integer;
    FFreedNodes: integer;
    procedure SetMaxFreeRatio(NewValue: integer);
    procedure SetMinFree(NewValue: integer);
  public
    procedure DisposeNode(ANode: TCodeTreeNode);
    function NewNode: TCodeTreeNode;
    property MinimumFreeNode: integer read FMinFree write SetMinFree;
    property MaximumFreeNodeRatio: integer
        read FMaxFreeRatio write SetMaxFreeRatio; // in one eighth steps
    property Count: integer read FCount;
    property FreeCount: integer read FFreeCount;
    property AllocatedNodes: integer read FAllocatedNodes;
    property FreedNodes: integer read FFreedNodes;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  // memory system for TCodeTreeNodeExtension(s)
  TCodeTreeNodeExtMemManager = class
  private
    FFirstFree: TCodeTreeNodeExtension;
    FFreeCount: integer;
    FCount: integer;
    FMinFree: integer;
    FMaxFreeRatio: integer;
    procedure SetMaxFreeRatio(NewValue: integer);
    procedure SetMinFree(NewValue: integer);
  public
    procedure DisposeNode(ANode: TCodeTreeNodeExtension);
    procedure DisposeAVLTree(TheTree: TAVLTree);
    function NewNode: TCodeTreeNodeExtension;
    property MinimumFreeNode: integer read FMinFree write SetMinFree;
    property MaximumFreeNodeRatio: integer
        read FMaxFreeRatio write SetMaxFreeRatio; // in one eighth steps
    property Count: integer read FCount;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;


var
  NodeExtMemManager: TCodeTreeNodeExtMemManager;
  NodeMemManager: TCodeTreeNodeMemManager;

//-----------------------------------------------------------------------------
// useful functions
function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
function CompareCodeTreeNodeExt(NodeData1, NodeData2: pointer): integer;

implementation


function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
begin
  case Desc of
  ctnNone: Result:='None';

  ctnClass: Result:='Class';
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

  ctnIdentifier: Result:='Identifier';
  ctnArrayType: Result:='Array Type';
  ctnRecordType: Result:='Record Type';
  ctnRecordCase: Result:='Record Case';
  ctnRecordVariant: Result:='Record Variant';
  ctnProcedureType: Result:='Procedure Type';
  ctnSetType: Result:='Set Type';
  ctnRangeType: Result:='Subrange Type';
  ctnEnumType: Result:='Enumeration Type';
  ctnLabelType: Result:='Label Type';
  ctnTypeType: Result:='''Type'' Type';
  ctnFileType: Result:='File Type';
  ctnPointerType: Result:='Pointer ''^'' Type';
  ctnClassOfType: Result:='Class Of Type';

  ctnWithVariable: Result:='With Variable';
  ctnWithStatement: Result:='With Statement'

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
  Cache.Free;
  Cache:=nil;
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

function TCodeTreeNode.DescAsString: string;
begin
  Result:=NodeDescriptionAsString(Desc);
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
  Node:=nil;
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

constructor TCodeTreeNodeMemManager.Create;
begin
  inherited Create;
  FFirstFree:=nil;
  FFreeCount:=0;
  FCount:=0;
  FAllocatedNodes:=0;
  FFreedNodes:=0;
  FMinFree:=10000;
  FMaxFreeRatio:=8; // 1:1
end;

destructor TCodeTreeNodeMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCodeTreeNodeMemManager.NewNode: TCodeTreeNode;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=FFirstFree;
    FFirstFree:=FFirstFree.NextBrother;
    Result.NextBrother:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    Result:=TCodeTreeNode.Create;
    inc(FAllocatedNodes);
  end;
  inc(FCount);
end;

procedure TCodeTreeNodeMemManager.DisposeNode(ANode: TCodeTreeNode);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ANode to Free list
    ANode.Clear;
    ANode.NextBrother:=FFirstFree;
    FFirstFree:=ANode;
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    ANode.Free;
    inc(FFreedNodes);
  end;
  dec(FCount);
end;

procedure TCodeTreeNodeMemManager.Clear;
var ANode: TCodeTreeNode;
begin
  while FFirstFree<>nil do begin
    ANode:=FFirstFree;
    FFirstFree:=FFirstFree.NextBrother;
    ANode.NextBrother:=nil;
    ANode.Free;
    inc(FFreedNodes);
  end;
  FFreeCount:=0;
end;

procedure TCodeTreeNodeMemManager.SetMaxFreeRatio(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMaxFreeRatio then exit;
  FMaxFreeRatio:=NewValue;
end;

procedure TCodeTreeNodeMemManager.SetMinFree(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMinFree then exit;
  FMinFree:=NewValue;
end;

{ TCodeTreeNodeExtMemManager }

constructor TCodeTreeNodeExtMemManager.Create;
begin
  inherited Create;
  FFirstFree:=nil;
  FFreeCount:=0;
  FCount:=0;
  FMinFree:=10000;
  FMaxFreeRatio:=8; // 1:1
end;

destructor TCodeTreeNodeExtMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCodeTreeNodeExtMemManager.NewNode: TCodeTreeNodeExtension;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=FFirstFree;
    FFirstFree:=FFirstFree.Next;
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
    ANode.Next:=FFirstFree;
    FFirstFree:=ANode;
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
    TCodeTreeNodeExtension(ANode.Data).Free;
    ANode:=TheTree.FindSuccessor(ANode);
  end;
  TheTree.Free;
end;

procedure TCodeTreeNodeExtMemManager.Clear;
var ANode: TCodeTreeNodeExtension;
begin
  while FFirstFree<>nil do begin
    ANode:=FFirstFree;
    FFirstFree:=FFirstFree.Next;
    ANode.Next:=nil;
    ANode.Free;
  end;
  FFreeCount:=0;
end;

procedure TCodeTreeNodeExtMemManager.SetMaxFreeRatio(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMaxFreeRatio then exit;
  FMaxFreeRatio:=NewValue;
end;

procedure TCodeTreeNodeExtMemManager.SetMinFree(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMinFree then exit;
  FMinFree:=NewValue;
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

