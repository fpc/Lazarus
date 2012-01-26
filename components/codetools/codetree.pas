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

{$IMPLICITEXCEPTIONS OFF} // no automatic try..finally (exceptions in all functions are fatal)

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStructs, BasicCodeTools,
  AVL_Tree, CodeToolMemManager;

//-----------------------------------------------------------------------------

type
  TCodeTreeNodeDesc = word;
  TCodeTreeNodeSubDesc = word;

const
  // CodeTreeNodeDescriptors
  ctnNone               = 0;

  ctnProgram            = 1;
  ctnPackage            = 2;
  ctnLibrary            = 3;
  ctnUnit               = 4;
  ctnInterface          = 5;
  ctnImplementation     = 6;
  ctnInitialization     = 7;
  ctnFinalization       = 8;
  ctnEndPoint           = 9;

  ctnTypeSection        = 10;
  ctnVarSection         = 11;
  ctnConstSection       = 12;
  ctnResStrSection      = 13;
  ctnLabelSection       = 14;
  ctnPropertySection    = 15;
  ctnUsesSection        = 16;
  ctnRequiresSection    = 17;
  ctnContainsSection    = 18;
  ctnExportsSection     = 19;

  ctnTypeDefinition     = 20;
  ctnVarDefinition      = 21;
  ctnConstDefinition    = 22;
  ctnGlobalProperty     = 23;
  ctnUseUnit            = 24; // StartPos=unitname, EndPos=unitname+inFilename
  ctnVarArgs            = 25;

  ctnClass              = 30;
  ctnClassInterface     = 31;
  ctnObject             = 32;
  ctnObjCClass          = 33;
  ctnObjCCategory       = 34;
  ctnObjCProtocol       = 35;
  ctnCPPClass           = 36;
  ctnDispinterface      = 37;

  ctnClassAbstract      = 40;
  ctnClassSealed        = 41;
  ctnClassExternal      = 42;
  ctnClassInheritance   = 43;
  ctnClassGUID          = 44;
  ctnClassClassVar      = 45; // child of visibility section
  ctnClassPrivate       = 46; // child of AllClassObjects
  ctnClassProtected     = 47;
  ctnClassPublic        = 48;
  ctnClassPublished     = 49;
  ctnClassRequired      = 50;
  ctnClassOptional      = 51;
  ctnProperty           = 52; // child of visibility section or AllClassInterfaces
  ctnMethodMap          = 53; // child of visibility section or AllClassInterfaces
  
  ctnProcedure          = 60;  // children: ctnProcedureHead, sections, ctnBeginBlock/ctnAsmBlock
  ctnProcedureHead      = 61;  // children: ctnParameterList, operator: ctnVarDefinition, operator/function: ctnIdentifier
  ctnParameterList      = 62;  // children: ctnVarDefinition

  ctnIdentifier         = 70;
  ctnRangedArrayType    = 71;
  ctnOpenArrayType      = 72;
  ctnOfConstType        = 73;
  ctnRecordType         = 74;
  ctnRecordCase         = 75; // children: ctnVarDefinition plus 0..n ctnRecordVariant
  ctnRecordVariant      = 76; // children: 0..n ctnVarDefinition plus may be a ctnRecordCase
  ctnProcedureType      = 77;
  ctnSetType            = 78;
  ctnRangeType          = 79;
  ctnEnumerationType    = 80;
  ctnEnumIdentifier     = 81;
  ctnLabelType          = 82;
  ctnTypeType           = 83;
  ctnFileType           = 84;
  ctnPointerType        = 85;
  ctnClassOfType        = 86;
  ctnVariantType        = 87;
  ctnSpecialize         = 88;
  ctnSpecializeType     = 89;
  ctnSpecializeParams   = 90;
  ctnGenericType        = 91;// 1. child = ctnGenericName, 2. child = ctnGenericParams, 3. child = type
  ctnGenericName        = 92;
  ctnGenericParams      = 93;
  ctnGenericParameter   = 94;
  ctnConstant           = 95;
  ctnHintModifier       = 96; // deprecated, platform, unimplemented, library, experimental

  ctnBeginBlock         =100;
  ctnAsmBlock           =101;

  ctnWithVariable       =110;
  ctnWithStatement      =111;
  ctnOnBlock            =112;// childs: ctnOnIdentifier+ctnOnStatement, or ctnVarDefinition(with child ctnIdentifier)+ctnOnStatement
  ctnOnIdentifier       =113;// e.g. 'on Exception', Note: on E:Exception creates a ctnVarDefinition
  ctnOnStatement        =114;

  // combined values
  AllSourceTypes =
     [ctnProgram,ctnPackage,ctnLibrary,ctnUnit];
  AllUsableSourceTypes =
     [ctnUnit];
  AllCodeSections = AllSourceTypes
     + [ctnInterface, ctnImplementation, ctnInitialization, ctnFinalization];
  AllClassBaseSections =
     [ctnClassPublic,ctnClassPublished,ctnClassPrivate,ctnClassProtected,
      ctnClassRequired,ctnClassOptional];
  AllClassSubSections =
     [ctnConstSection, ctnTypeSection, ctnVarSection, ctnClassClassVar];
  AllClassSections =
    AllClassBaseSections+AllClassSubSections;
  AllClassInterfaces = [ctnClassInterface,ctnDispinterface,ctnObjCProtocol];
  AllClassObjects = [ctnClass,ctnObject,ctnRecordType,
                     ctnObjCClass,ctnObjCCategory,ctnCPPClass];
  AllClasses = AllClassObjects+AllClassInterfaces;
  AllClassModifiers = [ctnClassAbstract, ctnClassSealed, ctnClassExternal];
  AllDefinitionSections =
     [ctnTypeSection,ctnVarSection,ctnConstSection,ctnResStrSection,
      ctnLabelSection,ctnPropertySection];
  AllSimpleIdentifierDefinitions =
     [ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition];
  AllIdentifierDefinitions = AllSimpleIdentifierDefinitions
     +[ctnGenericType,ctnGlobalProperty];
  AllPascalTypes =
     AllClasses+
     [ctnGenericType,ctnSpecialize,
      ctnIdentifier,ctnOpenArrayType,ctnRangedArrayType,
      ctnRecordCase,ctnRecordVariant,
      ctnProcedureType,ctnSetType,ctnRangeType,ctnEnumerationType,
      ctnEnumIdentifier,ctnLabelType,ctnTypeType,ctnFileType,ctnPointerType,
      ctnClassOfType,ctnVariantType,ctnConstant];
  AllPascalStatements = [ctnBeginBlock,ctnWithStatement,ctnWithVariable,
                         ctnOnBlock,ctnOnIdentifier,ctnOnStatement];
  AllFindContextDescs = AllIdentifierDefinitions + AllCodeSections + AllClasses +
     [ctnProcedure];
  AllPointContexts = AllClasses+AllSourceTypes+[ctnEnumerationType,ctnInterface];


  // CodeTreeNodeSubDescriptors
  ctnsNone                = 0;
  ctnsNeedJITParsing      = 1 shl 1;
  ctnsHasParseError       = 1 shl 2;
  ctnsForwardDeclaration  = 1 shl 3;
  ctnsHasDefaultValue     = 1 shl 4;

  ClassSectionNodeType: array[TPascalClassSection] of TCodeTreeNodeDesc = (
    ctnClassPrivate,
    ctnClassProtected,
    ctnClassPublic,
    ctnClassPublished
    );


type
  // Procedure Specifiers
  TProcedureSpecifier = (
    psSTDCALL, psREGISTER, psPOPSTACK, psVIRTUAL, psABSTRACT, psDYNAMIC,
    psOVERLOAD, psOVERRIDE, psREINTRODUCE, psCDECL, psINLINE, psMESSAGE,
    psEXTERNAL, psFORWARD, psPASCAL, psASSEMBLER, psSAVEREGISTERS,
    psFAR, psNEAR, psFINAL, psSTATIC, psMWPASCAL, psNOSTACKFRAME,
    psDEPRECATED, psDISPID, psPLATFORM, psSAFECALL, psUNIMPLEMENTED,
    psEXPERIMENTAL, psLIBRARY, psENUMERATOR, psVARARGS,
    psEdgedBracket);
  TAllProcedureSpecifiers = set of TProcedureSpecifier;

const
  ProcedureSpecifierNames: array[TProcedureSpecifier] of shortstring = (
      'STDCALL', 'REGISTER', 'POPSTACK', 'VIRTUAL', 'ABSTRACT', 'DYNAMIC',
      'OVERLOAD', 'OVERRIDE', 'REINTRODUCE', 'CDECL', 'INLINE', 'MESSAGE',
      'EXTERNAL', 'FORWARD', 'PASCAL', 'ASSEMBLER', 'SAVEREGISTERS',
      'FAR', 'NEAR', 'FINAL', 'STATIC', 'MWPASCAL', 'NOSTACKFRAME',
      'DEPRECATED', 'DISPID', 'PLATFORM', 'SAFECALL', 'UNIMPLEMENTED',
      'EXPERIMENTAL', 'LIBRARY', 'ENUMERATOR', 'VARARGS',
      '['
    );

  
type

  { TCodeTreeNode }

  TCodeTreeNode = packed class
  public
    Parent, NextBrother, PriorBrother, FirstChild, LastChild: TCodeTreeNode;
    Cache: TObject;
    StartPos, EndPos: integer;
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    function Next: TCodeTreeNode;
    function NextSkipChilds: TCodeTreeNode;
    function Prior: TCodeTreeNode;
    function GetNodeInFrontOfPos(p: integer): TCodeTreeNode;
    function GetRoot: TCodeTreeNode;
    function ChildCount: integer;
    function HasAsParent(Node: TCodeTreeNode): boolean;
    function HasAsChild(Node: TCodeTreeNode): boolean;
    function HasParentOfType(ParentDesc: TCodeTreeNodeDesc): boolean;
    function HasAsRoot(RootNode: TCodeTreeNode): boolean;
    function GetNodeOfType(ADesc: TCodeTreeNodeDesc): TCodeTreeNode;
    function GetNodeOfTypes(Descriptors: array of TCodeTreeNodeDesc): TCodeTreeNode;
    function GetTopMostNodeOfType(ADesc: TCodeTreeNodeDesc): TCodeTreeNode;
    function GetFindContextParent: TCodeTreeNode;
    function GetLevel: integer;
    function DescAsString: string;
    function FindOwner: TObject;
    procedure Clear;
    constructor Create;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(const Prefix: string; WithChilds: boolean);
  end;

  { TCodeTree }

  TCodeTree = class
  private
    FNodeCount: integer;
  public
    Root: TCodeTreeNode;
    property NodeCount: integer read FNodeCount;
    procedure DeleteNode(ANode: TCodeTreeNode);
    procedure AddNodeAsLastChild(ParentNode, ANode: TCodeTreeNode);
    procedure AddNodeInFrontOf(NextBrotherNode, ANode: TCodeTreeNode);
    function FindFirstPosition: integer;
    function FindLastPosition: integer;
    function ContainsNode(ANode: TCodeTreeNode): boolean;
    function FindRootNode(Desc: TCodeTreeNodeDesc): TCodeTreeNode;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(WithChilds: boolean);
  end;


  { TCodeTreeNodeExtension }

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
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
    function CalcMemSize: PtrUInt;
  end;


//-----------------------------------------------------------------------------
// useful functions
function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
procedure WriteNodeExtTree(Tree: TAVLTree);
function FindCodeTreeNodeExt(Tree: TAVLTree; const Txt: string
                             ): TCodeTreeNodeExtension;
function FindCodeTreeNodeExtAVLNode(Tree: TAVLTree; const Txt: string
                                    ): TAVLTreeNode;
function FindCodeTreeNodeExtWithIdentifier(Tree: TAVLTree; Identifier: PChar
                             ): TCodeTreeNodeExtension;
function FindCodeTreeNodeExtAVLNodeWithIdentifier(Tree: TAVLTree;
                                               Identifier: PChar): TAVLTreeNode;
procedure AddNodeExtToTree(var TreeOfNodeExt: TAVLTree;
  DefNodeExt: TCodeTreeNodeExtension);
procedure ClearNodeExtData(TreeOfNodeExt: TAVLTree);
procedure DisposeAVLTree(var Tree: TAVLTree);
function CompareTxtWithCodeTreeNodeExt(p: Pointer;
                                       NodeData: pointer): integer;
function CompareIdentifierWithCodeTreeNodeExt(p: Pointer;
                                              NodeData: pointer): integer;
function CompareCodeTreeNodeExt(NodeData1, NodeData2: pointer): integer; // Txt
function CompareCodeTreeNodeExtWithPos(NodeData1, NodeData2: pointer): integer; // Position
function CompareCodeTreeNodeExtWithNodeStartPos(
  NodeData1, NodeData2: pointer): integer; // Node.StartPos
function CompareCodeTreeNodeExtTxtAndPos(NodeData1, NodeData2: pointer): integer; // Txt, then Position
function CompareCodeTreeNodeExtWithNode(NodeData1, NodeData2: pointer): integer;
function ComparePointerWithCodeTreeNodeExtNode(p: Pointer;
                                               NodeExt: pointer): integer;

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
  ctnDispinterface: Result:='Dispinterface';
  ctnObject: Result:='Object';
  ctnObjCClass: Result:='ObjCClass';
  ctnObjCCategory: Result:='ObjCCategory';
  ctnObjCProtocol: Result:='ObjCProtocol';
  ctnCPPClass: Result:='CPPClass';

  ctnClassInheritance: Result:='Class inheritance';
  ctnClassGUID: Result:='GUID';
  ctnClassPrivate: Result:='Private';
  ctnClassProtected: Result:='Protected';
  ctnClassPublic: Result:='Public';
  ctnClassPublished: Result:='Published';
  ctnClassRequired: Result:='Required section';
  ctnClassOptional: Result:='Optional section';
  ctnClassClassVar: Result:='Class Var';
  ctnClassAbstract: Result:='abstract';
  ctnClassSealed: Result:='sealed';
  ctnClassExternal: Result:='external';

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
  ctnEndPoint: Result:='End.';

  ctnTypeSection: Result:='Type Section';
  ctnVarSection: Result:='Var Section';
  ctnConstSection: Result:='Const Section';
  ctnResStrSection: Result:='Resource String Section';
  ctnPropertySection: Result:='Property Section';
  ctnUsesSection: Result:='Uses Section';
  ctnRequiresSection: Result:='Requires Section';
  ctnContainsSection: Result:='Contains Section';
  ctnExportsSection: Result:='Exports Section';

  ctnTypeDefinition: Result:='Type';
  ctnVarDefinition: Result:='Var';
  ctnConstDefinition: Result:='Const';
  ctnGlobalProperty: Result:='Global Property';
  ctnUseUnit: Result:='use unit';
  ctnVarArgs: Result:='VarArgs';

  ctnProperty: Result:='Property'; // can start with 'class property'
  ctnMethodMap: Result:='Method Map';

  ctnIdentifier: Result:='Identifier';
  ctnOpenArrayType: Result:='Open Array Type';
  ctnOfConstType: Result:='Of Const';
  ctnRangedArrayType: Result:='Ranged Array Type';
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
  ctnVariantType: Result:='Variant Type';
  ctnSpecialize: Result:='Specialize Type';
  ctnSpecializeType: Result:='Specialize Typename';
  ctnSpecializeParams: Result:='Specialize Parameterlist';
  ctnGenericType: Result:='Generic Type';
  ctnGenericName: Result:='Generic Type Name';
  ctnGenericParams: Result:='Generic Type Params';
  ctnGenericParameter: Result:='Generic Type Parameter';
  ctnConstant: Result:='Constant';
  ctnHintModifier: Result:='Hint Modifier';

  ctnWithVariable: Result:='With Variable';
  ctnWithStatement: Result:='With Statement';
  ctnOnBlock: Result:='On Block';
  ctnOnIdentifier: Result:='On Identifier';
  ctnOnStatement: Result:='On Statement';

  else
    Result:='invalid descriptor ('+IntToStr(Desc)+')';
  end;
end;

procedure WriteNodeExtTree(Tree: TAVLTree);
var
  Node: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
begin
  if Tree=nil then begin
    DebugLn(['WriteNodeExtTree Tree=nil']);
    exit;
  end;
  DebugLn(['WriteNodeExtTree ']);
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    NodeExt:=TCodeTreeNodeExtension(Node.Data);
    if NodeExt=nil then
      DebugLn(['  NodeExt=nil'])
    else
      NodeExt.WriteDebugReport;
    Node:=Tree.FindSuccessor(Node);
  end;
end;

function FindCodeTreeNodeExt(Tree: TAVLTree; const Txt: string
  ): TCodeTreeNodeExtension;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindCodeTreeNodeExtAVLNode(Tree,Txt);
  if AVLNode<>nil then
    Result:=TCodeTreeNodeExtension(AVLNode.Data)
  else
    Result:=nil;
end;

function FindCodeTreeNodeExtAVLNode(Tree: TAVLTree; const Txt: string
  ): TAVLTreeNode;
begin
  Result:=Tree.FindKey(Pointer(Txt),@CompareTxtWithCodeTreeNodeExt);
end;

function FindCodeTreeNodeExtWithIdentifier(Tree: TAVLTree; Identifier: PChar
  ): TCodeTreeNodeExtension;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindCodeTreeNodeExtAVLNodeWithIdentifier(Tree,Identifier);
  if AVLNode<>nil then
    Result:=TCodeTreeNodeExtension(AVLNode.Data)
  else
    Result:=nil;
end;

function FindCodeTreeNodeExtAVLNodeWithIdentifier(Tree: TAVLTree;
  Identifier: PChar): TAVLTreeNode;
begin
  Result:=Tree.FindKey(Identifier,@CompareIdentifierWithCodeTreeNodeExt);
end;

procedure AddNodeExtToTree(var TreeOfNodeExt: TAVLTree;
  DefNodeExt: TCodeTreeNodeExtension);
begin
  if TreeOfNodeExt=nil then
    TreeOfNodeExt:=TAVLTree.Create(@CompareCodeTreeNodeExt);
  TreeOfNodeExt.Add(DefNodeExt);
end;

procedure ClearNodeExtData(TreeOfNodeExt: TAVLTree);
var
  AVLNode: TAVLTreeNode;
begin
  if TreeOfNodeExt=nil then exit;
  AVLNode:=TreeOfNodeExt.FindLowest;
  while AVLNode<>nil do begin
    TCodeTreeNodeExtension(AVLNode.Data).Data:=nil;
    AVLNode:=TreeOfNodeExt.FindSuccessor(AVLNode);
  end;
end;

procedure DisposeAVLTree(var Tree: TAVLTree);
begin
  if Tree=nil then exit;
  Tree.FreeAndClear;
  Tree.Free;
  Tree:=nil;
end;

function CompareTxtWithCodeTreeNodeExt(p: Pointer; NodeData: pointer
  ): integer;
var
  NodeExt: TCodeTreeNodeExtension absolute NodeData;
begin
  Result:=CompareTextIgnoringSpace(Ansistring(p),NodeExt.Txt,false);
end;

function CompareIdentifierWithCodeTreeNodeExt(p: Pointer; NodeData: pointer
  ): integer;
var
  NodeExt: TCodeTreeNodeExtension absolute NodeData;
begin
  NodeExt:=TCodeTreeNodeExtension(NodeData);
  Result:=CompareIdentifierPtrs(p,Pointer(NodeExt.Txt));
end;

function CompareCodeTreeNodeExt(NodeData1, NodeData2: pointer): integer;
var
  NodeExt1: TCodeTreeNodeExtension absolute NodeData1;
  NodeExt2: TCodeTreeNodeExtension absolute NodeData2;
begin
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

function CompareCodeTreeNodeExtTxtAndPos(NodeData1, NodeData2: pointer
  ): integer;
var
  NodeExt1: TCodeTreeNodeExtension absolute NodeData1;
  NodeExt2: TCodeTreeNodeExtension absolute NodeData2;
begin
  Result:=CompareTextIgnoringSpace(NodeExt1.Txt,NodeExt2.Txt,false);
  if Result<>0 then exit;
  if NodeExt1.Position<NodeExt2.Position then
    Result:=1
  else if NodeExt1.Position>NodeExt2.Position then
    Result:=-1
  else
    Result:=0;
end;

function CompareCodeTreeNodeExtWithNode(NodeData1, NodeData2: pointer): integer;
var
  Node1: TCodeTreeNode;
  Node2: TCodeTreeNode;
begin
  Node1:=TCodeTreeNodeExtension(NodeData1).Node;
  Node2:=TCodeTreeNodeExtension(NodeData2).Node;
  if Pointer(Node1)>Pointer(Node2) then
    Result:=1
  else if Pointer(Node1)<Pointer(Node2) then
    Result:=-1
  else
    Result:=0;
end;

function ComparePointerWithCodeTreeNodeExtNode(p: Pointer; NodeExt: pointer
  ): integer;
var
  Node: TCodeTreeNode;
begin
  Node:=TCodeTreeNodeExtension(NodeExt).Node;
  if p>Pointer(Node) then
    Result:=1
  else if p<Pointer(Node) then
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
  StartPos:=-1;
  EndPos:=-1;
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
  Cache:=nil;
end;

function TCodeTreeNode.Next: TCodeTreeNode;
begin
  if FirstChild<>nil then begin
    Result:=FirstChild;
  end else begin
    Result:=Self;
    while (Result<>nil) and (Result.NextBrother=nil) do
      Result:=Result.Parent;
    if Result<>nil then Result:=Result.NextBrother;
  end;
end;

function TCodeTreeNode.NextSkipChilds: TCodeTreeNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.NextBrother=nil) do
    Result:=Result.Parent;
  if Result<>nil then Result:=Result.NextBrother;
end;

function TCodeTreeNode.Prior: TCodeTreeNode;
begin
  if PriorBrother<>nil then begin
    Result:=PriorBrother;
    while Result.LastChild<>nil do
      Result:=Result.LastChild;
  end else
    Result:=Parent;
end;

function TCodeTreeNode.GetNodeInFrontOfPos(p: integer): TCodeTreeNode;
// if p<=StartPos then next node with Node.StartPos<=p
// else returns the next child node with Node.EndPos<=p
begin
  if p<=StartPos then begin
    if (Parent<>nil) and (p<Parent.StartPos) then begin
      // p is in front of parent
      Result:=Parent;
      while (Result<>nil) and (p<Result.StartPos) do
        Result:=Result.Parent;
    end else begin
      // p is in parent and in front of node => prior brothers
      Result:=PriorBrother;
      while (Result<>nil) and (p<Result.StartPos) do
        Result:=Result.PriorBrother;
    end;
    if Result=nil then exit;
    // p is in Result => search in children
    Result:=Result.GetNodeInFrontOfPos(p);
  end else begin
    Result:=LastChild;
    while (Result<>nil) and (Result.EndPos>p) do
      Result:=Result.PriorBrother;
    if Result=nil then exit;
    while (Result.LastChild<>nil) and (Result.LastChild.EndPos=Result.EndPos) do
      Result:=Result.LastChild;
  end;
end;

procedure TCodeTreeNode.ConsistencyCheck;
begin
  if (EndPos>0) and (StartPos>EndPos) then
    raise Exception.Create('');
  if (Parent<>nil) then begin
    if (PriorBrother=nil) and (Parent.FirstChild<>Self) then
      raise Exception.Create('');
    if (NextBrother=nil) and (Parent.LastChild<>Self) then
      raise Exception.Create('');
  end;
  if (NextBrother<>nil) and (NextBrother.Parent<>Parent) then
    raise Exception.Create('');
  if (PriorBrother<>nil) and (PriorBrother.Parent<>Parent) then
    raise Exception.Create('');
  if (FirstChild<>nil) and (FirstChild.Parent<>Self) then
    raise Exception.Create('');
  if (FirstChild=nil) <> (LastChild=nil) then
    raise Exception.Create('');
  if (NextBrother<>nil) and (NextBrother.PriorBrother<>Self) then
    raise Exception.Create('');
  if (PriorBrother<>nil) and (PriorBrother.NextBrother<>Self) then
    raise Exception.Create('');
  if (FirstChild<>nil) then
    FirstChild.ConsistencyCheck;
  if NextBrother<>nil then
    NextBrother.ConsistencyCheck;
end;

procedure TCodeTreeNode.WriteDebugReport(const Prefix: string;
  WithChilds: boolean);
var
  Node: TCodeTreeNode;
begin
  DebugLn([Prefix,DescAsString,' Range=',StartPos,'..',EndPos,' Cache=',DbgSName(Cache)]);
  if WithChilds then begin
    Node:=FirstChild;
    while Node<>nil do begin
      Node.WriteDebugReport(Prefix+'  ',true);
      Node:=Node.NextBrother;
    end;
  end;
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

function TCodeTreeNode.HasAsRoot(RootNode: TCodeTreeNode): boolean;
begin
  Result:=(RootNode<>nil) and (RootNode=GetRoot);
end;

function TCodeTreeNode.GetNodeOfType(ADesc: TCodeTreeNodeDesc
  ): TCodeTreeNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.Desc<>ADesc) do
    Result:=Result.Parent;
end;

function TCodeTreeNode.GetNodeOfTypes(Descriptors: array of TCodeTreeNodeDesc
  ): TCodeTreeNode;
var
  i: Integer;
begin
  Result:=Self;
  while (Result<>nil) do begin
    for i:=Low(Descriptors) to High(Descriptors) do
      if Result.Desc=Descriptors[i] then exit;
    Result:=Result.Parent;
  end;
end;

function TCodeTreeNode.GetTopMostNodeOfType(ADesc: TCodeTreeNodeDesc
  ): TCodeTreeNode;
var
  Node: TCodeTreeNode;
begin
  Result:=nil;
  Node:=Self;
  while Node<>nil do begin
    if Node.Desc=ADesc then
      Result:=Node;
    Node:=Node.Parent;
  end;
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
  if Self=nil then
    Result:='nil'
  else
    Result:=NodeDescriptionAsString(Desc);
end;

function TCodeTreeNode.GetRoot: TCodeTreeNode;
begin
  Result:=Self;
  while (Result.Parent<>nil) do Result:=Result.Parent;
  while (Result.PriorBrother<>nil) do Result:=Result.PriorBrother;
end;

function TCodeTreeNode.ChildCount: integer;
var
  Node: TCodeTreeNode;
begin
  Result:=0;
  Node:=FirstChild;
  while Node<>nil do begin
    inc(Result);
    Node:=Node.NextBrother;
  end;
end;

function TCodeTreeNode.FindOwner: TObject;
begin
  Result:=FindOwnerOfCodeTreeNode(Self);
end;

{ TCodeTree }

constructor TCodeTree.Create;
begin
  Root:=nil;
  FNodeCount:=0;
end;

destructor TCodeTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeTree.Clear;
begin
  while Root<>nil do
    DeleteNode(Root);
end;

procedure TCodeTree.DeleteNode(ANode: TCodeTreeNode);
begin
  if ANode=nil then exit;
  if ANode=Root then Root:=ANode.NextBrother;
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
  dec(FNodeCount);
  ANode.Clear; // clear to spot dangling pointers early
  ANode.Free;
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

procedure TCodeTree.AddNodeInFrontOf(NextBrotherNode, ANode: TCodeTreeNode);
begin
  ANode.Parent:=NextBrotherNode.Parent;
  ANode.NextBrother:=NextBrotherNode;
  ANode.PriorBrother:=NextBrotherNode.PriorBrother;
  NextBrotherNode.PriorBrother:=ANode;
  if ANode.PriorBrother<>nil then
    ANode.PriorBrother.NextBrother:=ANode;
end;

function TCodeTree.FindFirstPosition: integer;
begin
  Result:=-1;
  if Root=nil then exit;
  Result:=Root.StartPos;
end;

function TCodeTree.FindLastPosition: integer;
var
  ANode: TCodeTreeNode;
begin
  Result:=-1;
  if Root=nil then exit;
  ANode:=Root;
  while (ANode.NextBrother<>nil) do ANode:=ANode.NextBrother;
  //debugln('TCodeTree.FindLastPosition A ',Anode.DescAsString,' ANode.StartPos=',dbgs(ANode.StartPos),' ANode.EndPos=',dbgs(ANode.EndPos));
  Result:=ANode.EndPos;
end;

function TCodeTree.ContainsNode(ANode: TCodeTreeNode): boolean;
begin
  if ANode=nil then exit(false);
  while ANode.Parent<>nil do ANode:=ANode.Parent;
  while ANode.PriorBrother<>nil do ANode:=ANode.PriorBrother;
  Result:=ANode=Root;
end;

function TCodeTree.FindRootNode(Desc: TCodeTreeNodeDesc): TCodeTreeNode;
begin
  Result:=Root;
  while (Result<>nil) and (Result.Desc<>Desc) do
    Result:=Result.NextBrother;
end;

procedure TCodeTree.ConsistencyCheck;
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
    if Root.Parent<>nil then
      raise Exception.Create('');
    Root.ConsistencyCheck;
  end;
  RealNodeCount:=0;
  CountNodes(Root);
  if RealNodeCount<>FNodeCount then
    raise Exception.Create('');
end;

procedure TCodeTree.WriteDebugReport(WithChilds: boolean);
begin
  DebugLn('[TCodeTree.WriteDebugReport] Root=',dbgs(Root<>nil));
  if Root<>nil then
    Root.WriteDebugReport(' ',true);
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
  Position:=-1;
end;

function TCodeTreeNodeExtension.ConsistencyCheck: integer;
// 0 = ok
begin
  Result:=0;
end;

procedure TCodeTreeNodeExtension.WriteDebugReport;
begin
  // nothing special
  DbgOut('  ');
  if Node<>nil then
    DbgOut('Node=',NodeDescriptionAsString(Node.Desc))
  else
    DbgOut('Node=nil');
  DbgOut(' Position=',dbgs(Position),' Txt="'+Txt+'" ExtTxt1="'+ExtTxt1+'" ExtTxt2="'+ExtTxt2+'" ExtTxt3="'+ExtTxt3+'"');
  debugln;
end;

function TCodeTreeNodeExtension.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(Txt)
    +MemSizeString(ExtTxt1)
    +MemSizeString(ExtTxt2)
    +MemSizeString(ExtTxt3);
end;

end.

