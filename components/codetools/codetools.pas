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
    Functions for automated code editing.

  ToDo:
    -Split unit, it is far too big
    -Parsing of GUID
    -Parsing of With
    -Parsing of proc modifier alias [Alias: ''];
    -Find Declaration
    -GetCompatibleMethods: search ancestor methods
    -GetCompatibleMethods: ParamType comparison by Find Declaration
    -Insert class method body in pipClassOrder
    -Parameter List Hints
    -Open Filename At Cursor
    -Mouse Hints
    -Code Explorer functions
    -CompleteCode.ProcExists: search procs in ancestors too
    -CompleteCode.VarExists: search vars in ancestors too
    -CompleteCode  pipClassOrder
    -CompleteCode  proc body -> add proc definition
}
unit CodeTools;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, AVL_Tree, TypInfo, SourceChanger;

type
  TGetStringProc = procedure(const s: string) of object;

  TCodePosition = record
    P: integer;
    Code: TCodeBuffer;
  end;

  TCodeXYPosition = record
    X, Y: integer;
    Code: TCodeBuffer;
  end;

//-----------------------------------------------------------------------------
// An Atom is the smallest unit for a parser. Usually a word or a symbol.
type
  TAtomPosition = record
    StartPos: integer; // first char of Atom
    EndPos: integer;   // char behind Atom
  end;

  TAtomRing = class
  private
    FSize: integer;
    FItems: {$ifdef FPC}^{$else}array of {$endif}TAtomPosition;
       // ring of TAtomPosition
    FStart, FLast: integer;
    procedure SetSize(NewSize: integer);
  public
    procedure Add(NewAtom: TAtomPosition);
    procedure UndoLastAdd;
    function GetValueAt(
        RelativePos:integer):TAtomPosition;  // 0=current 1=prior current ...
    function Count: integer;
    property Size: integer read FSize write SetSize;
    procedure Clear;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------------
// a TCodeTree is the product of a code tool. Every TCodeTreeNode describes a
// logical block in the code (e.g. a class or a procedure).

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
  ctnWithBlock       = 22;

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


  // combined values
  AllCodeSections =
     [ctnProgram, ctnPackage, ctnLibrary, ctnUnit, ctnInterface,
      ctnImplementation, ctnInitialization, ctnFinalization];
  AllClassSections =
     [ctnClassPublic,ctnClassPublished,ctnClassPrivate,ctnClassProtected];
  AllDefinitionSections =
     [ctnTypeSection,ctnVarSection,ctnConstSection,ctnResStrSection];

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
    function Next: TCodeTreeNode;
    function Prior: TCodeTreeNode;
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


//-----------------------------------------------------------------------------
// TCustomCodeTool is the ancestor class for code tools which parses code
// beginning with the Main Source code. It can parse atoms, the smallest code
// elements in source code, create new code tree nodes and provides several
// useful functions for parsing and changing code.
type
  TCustomCodeTool = class(TObject)
  private
    //FIgnoreMissingIncludeFiles: boolean;
    FLastScannerChangeStep: integer;
    FScanner: TLinkScanner;
  protected
    KeyWordFuncList: TKeyWordFunctionList;
    FForceUpdateNeeded: boolean;
    function DefaultKeyWordFunc: boolean;
    procedure BuildDefaultKeyWordFunctions; virtual;
    procedure SetScanner(NewScanner: TLinkScanner); virtual;
    procedure RaiseException(const AMessage: string); virtual;
  public
    Tree: TCodeTree;

    // current Values, Position, Node ...
    CurPos: TAtomPosition;
    Src: string;
    UpperSrc: string;
    SrcLen: integer;
    CurNode: TCodeTreeNode;
    LastAtoms: TAtomRing;
    
    CheckFilesOnDisk: boolean;
    IndentSize: integer;
    VisibleEditorLines: integer;
    JumpCentered: boolean;
    CursorBeyondEOL: boolean;
    
    property Scanner: TLinkScanner read FScanner write SetScanner;
    
    function FindDeepestNodeAtPos(P: integer): TCodeTreeNode;
    function CaretToCleanPos(Caret: TCodeXYPosition;
        var CleanPos: integer): integer;  // 0=valid CleanPos
              //-1=CursorPos was skipped, CleanPos between two links
              // 1=CursorPos beyond scanned code
              //-2=X,Y beyond source
    function CleanPosToCaret(CleanPos: integer;
        var Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
    procedure GetLineInfo(ACleanPos: integer;
        var ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);

    function UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
    procedure BeginParsing(DeleteNodes, OnlyInterfaceNeeded: boolean); virtual;
    procedure MoveCursorToNodeStart(ANode: TCodeTreeNode); virtual;
    procedure MoveCursorToCleanPos(ACleanPos: integer); virtual;
    function ReadTilSection(SectionType: TCodeTreeNodeDesc): boolean;
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function DoAtom: boolean; virtual;
    procedure ReadNextAtom; virtual;
    procedure UndoReadNextAtom; virtual;
    function AtomIs(const AnAtom: shortstring): boolean;
    function UpAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextUpAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextAtomIsChar(const c: char): boolean;
    function AtomIsChar(const c: char): boolean;
    function AtomIsWord: boolean;
    function AtomIsKeyWord: boolean;
    function AtomIsNumber: boolean;
    function AtomIsStringConstant: boolean;
    function AtomIsIdentifier(ExceptionOnNotFound: boolean): boolean;
    function LastAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function LastUpAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function GetAtom: string;
    function GetUpAtom: string;
    function CompareNodeSrc(ANode: TCodeTreeNode;
        const ASource: string): integer;
    function CompareNodeUpSrc(ANode: TCodeTreeNode;
        const ASource: string): integer;

    procedure CreateChildNode; virtual;
    procedure EndChildNode; virtual;
    
    procedure Clear; virtual;
    function NodeDescToStr(Desc: integer): string;
    function NodeSubDescToStr(Desc, SubDesc: integer): string;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugTreeReport;
    constructor Create;
    destructor Destroy; override;
  end;

  TMultiKeyWordListCodeTool = class(TCustomCodeTool)
  private
    FKeyWordLists: TList; // list of TKeyWordFunctionList
    FCurKeyWordListID: integer;
    procedure SetCurKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList);
  protected
    procedure SetKeyWordListID(NewID: integer);
  public
    DefaultKeyWordFuncList: TKeyWordFunctionList;
    property KeyWordListID: integer read FCurKeyWordListID write SetKeyWordListID;
    property CurKeyWordFuncList: TKeyWordFunctionList
       read KeyWordFuncList write SetCurKeyWordFuncList;
    function AddKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList): integer;
    procedure ClearKeyWordFuncLists;

    constructor Create;
    destructor Destroy; override;
  end;

  TProcHeadAttribute = (phpWithStart, phpAddClassname, phpWithoutClassName,
      phpWithoutName, phpWithVarModifiers, phpWithParameterNames,
      phpWithDefaultValues, phpWithResultType, phpWithComments, phpInUpperCase,
      phpWithoutBrackets, phpIgnoreForwards, phpIgnoreProcsWithBody,
      phpOnlyWithClassname, phpFindCleanPosition);
  TProcHeadAttributes = set of TProcHeadAttribute;

  TPascalParserTool = class(TMultiKeyWordListCodeTool)
  private
  protected
    EndKeyWordFuncList: TKeyWordFunctionList;
    TypeKeyWordFuncList: TKeyWordFunctionList;
    PackedTypesKeyWordFuncList: TKeyWordFunctionList;
    InnerClassKeyWordFuncList: TKeyWordFunctionList;
    ClassVarTypeKeyWordFuncList: TKeyWordFunctionList;
    ExtractMemStream: TMemoryStream;
    ExtractSearchPos: integer;
    ExtractFoundPos: integer;
    procedure InitExtraction;
    function GetExtraction: string;
    procedure ExtractNextAtom(AddAtom: boolean; Attr: TProcHeadAttributes);
    // sections
    function KeyWordFuncSection: boolean;
    function KeyWordFuncEnd: boolean;
    // type/var/const/resourcestring
    function KeyWordFuncType: boolean;
    function KeyWordFuncVar: boolean;
    function KeyWordFuncConst: boolean;
    function KeyWordFuncResourceString: boolean;
    // types
    function KeyWordFuncClass: boolean;
    function KeyWordFuncTypePacked: boolean;
    function KeyWordFuncTypeArray: boolean;
    function KeyWordFuncTypeProc: boolean;
    function KeyWordFuncTypeSet: boolean;
    function KeyWordFuncTypeLabel: boolean;
    function KeyWordFuncTypeType: boolean;
    function KeyWordFuncTypeFile: boolean;
    function KeyWordFuncTypePointer: boolean;
    function KeyWordFuncTypeRecord: boolean;
    function KeyWordFuncTypeDefault: boolean;
    // procedures/functions/methods
    function KeyWordFuncMethod: boolean;
    function KeyWordFuncBeginEnd: boolean;
    // class/object elements
    function KeyWordFuncClassSection: boolean;
    function KeyWordFuncClassMethod: boolean;
    function KeyWordFuncClassProperty: boolean;
    function KeyWordFuncClassReadTilEnd: boolean;
    function KeyWordFuncClassIdentifier: boolean;
    function KeyWordFuncClassVarTypeClass: boolean;
    function KeyWordFuncClassVarTypePacked: boolean;
    function KeyWordFuncClassVarTypeRecord: boolean;
    function KeyWordFuncClassVarTypeArray: boolean;
    function KeyWordFuncClassVarTypeSet: boolean;
    function KeyWordFuncClassVarTypeProc: boolean;
    function KeyWordFuncClassVarTypeIdent: boolean;
    // keyword lists
    procedure BuildDefaultKeyWordFunctions; override;
    procedure BuildEndKeyWordFunctions; virtual;
    procedure BuildTypeKeyWordFunctions; virtual;
    procedure BuildPackedTypesKeyWordFunctions; virtual;
    procedure BuildInnerClassKeyWordFunctions; virtual;
    procedure BuildClassVarTypeKeyWordFunctions; virtual;
    function UnexpectedKeyWord: boolean;
    // read functions
    function ReadTilProcedureHeadEnd(IsMethod, IsFunction, IsType: boolean;
        var HasForwardModifier: boolean): boolean;
    function ReadConstant(ExceptionOnError, Extract: boolean;
        Attr: TProcHeadAttributes): boolean;
    function ReadParamType(ExceptionOnError, Extract: boolean;
        Attr: TProcHeadAttributes): boolean;
    function ReadParamList(ExceptionOnError, Extract: boolean;
        Attr: TProcHeadAttributes): boolean;
    function ReadUsesSection(ExceptionOnError: boolean): boolean;
    function ReadSubRange(ExceptionOnError: boolean): boolean;
  public
    CurSection: TCodeTreeNodeDesc;

    InterfaceSectionFound: boolean;
    ImplementationSectionFound: boolean;
    EndOfSourceFound: boolean;

    function DoAtom: boolean; override;
    procedure BuildTree(OnlyInterfaceNeeded: boolean); virtual;
    procedure BuildSubTreeForClass(ClassNode: TCodeTreeNode); virtual;
    function GetSourceType: TCodeTreeNodeDesc;
    function ExtractPropName(PropNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractProcName(ProcNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractProcHead(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractClassName(ClassNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode): string;
    function FindProcNode(StartNode: TCodeTreeNode; const ProcName: string;
        Attr: TProcHeadAttributes): TCodeTreeNode;
    function FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
    function FindVarNode(StartNode: TCodeTreeNode;
        const UpperVarName: string): TCodeTreeNode;
    function FindFirstNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindNextNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindClassNode(StartNode: TCodeTreeNode;
        const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindClassNodeInInterface(const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function FindInterfaceNode: TCodeTreeNode;
    function FindImplementationNode: TCodeTreeNode;
    function FindInitializationNode: TCodeTreeNode;
    function FindMainBeginEndNode: TCodeTreeNode;
    function NodeHasParentOfType(ANode: TCodeTreeNode;
        NodeDesc: TCodeTreeNodeDesc): boolean;
    constructor Create;
    destructor Destroy; override;
  end;
  
  TBasicCodeTool = class(TPascalParserTool)
  public
    // source name  e.g. 'unit UnitName;'
    function GetSourceNamePos(var NamePos: TAtomPosition): boolean;
    function GetSourceName: string;
    function RenameSource(const NewName: string;
        SourceChangeCache: TSourceChangeCache): boolean;

    // uses sections
    function FindUnitInUsesSection(UsesNode: TCodeTreeNode;
          const UpperUnitName: string;
          var NamePos, InPos: TAtomPosition): boolean;
    function FindUnitInAllUsesSections(const UpperUnitName: string;
          var NamePos, InPos: TAtomPosition): boolean;
    function FindMainUsesSection: TCodeTreeNode;
    function FindImplementationUsesSection: TCodeTreeNode;
    function RenameUsedUnit(const OldUpperUnitName, NewUnitName,
          NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function AddUnitToUsesSection(UsesNode: TCodeTreeNode;
          const NewUnitName, NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function AddUnitToMainUsesSection(const NewUnitName, NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveUnitFromUsesSection(UsesNode: TCodeTreeNode;
          const UpperUnitName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveUnitFromAllUsesSections(const UpperUnitName: string;
          SourceChangeCache: TSourceChangeCache): boolean;

    // resources
    function FindNextIncludeInInitialization(
          var LinkIndex: integer): TCodeBuffer;
    function FindLazarusResourceInBuffer(ResourceCode: TCodeBuffer;
          const ResourceName: string): TAtomPosition;
    function FindLazarusResource(const ResourceName: string): TAtomPosition;
    function AddLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName, ResourceData: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RenameInclude(LinkIndex: integer; const NewFilename: string;
          KeepPath: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;

    // createform
    function FindCreateFormStatement(StartPos: integer;
          const UpperClassName, UpperVarName: string;
          var Position: TAtomPosition): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(const AClassName,
          AVarName: string; SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveCreateFormStatement(const UpperVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function ListAllCreateFormStatements: TStrings;
    function SetAllCreateFromStatements(List: TStrings;
          SourceChangeCache: TSourceChangeCache): boolean;    

    // form components
    function FindPublishedVariable(const UpperClassName,
          UpperVarName: string): TCodeTreeNode;
    function AddPublishedVariable(const UpperClassName,VarName, VarType: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemovePublishedVariable(const UpperClassName, UpperVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
  end;

  TMethodJumpingCodeTool = class(TBasicCodeTool)
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

  // TEventsCodeTool provides functions to work with published methods in the
  // source. It can gather a list of compatible methods, test if method exists,
  // jump to the method body, create a method
  TEventsCodeTool = class(TMethodJumpingCodeTool)
  protected
    function InsertNewMethodToClass(ClassSectionNode: TCodeTreeNode;
        const AMethodName,NewMethod: string;
        SourceChangeCache: TSourceChangeCache): boolean;
  public
    procedure GetCompatiblePublishedMethods(const UpperClassName: string;
        TypeData: PTypeData; Proc: TGetStringProc);
    procedure GetCompatiblePublishedMethods(ClassNode: TCodeTreeNode;
        TypeData: PTypeData; Proc: TGetStringProc);
    function PublishedMethodExists(const UpperClassName,
        UpperMethodName: string; TypeData: PTypeData): boolean;
    function PublishedMethodExists(ClassNode: TCodeTreeNode;
        const UpperMethodName: string; TypeData: PTypeData): boolean;
    function JumpToPublishedMethodBody(const UpperClassName,
        UpperMethodName: string; TypeData: PTypeData;
        var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function RenamePublishedMethod(const UpperClassName, UpperOldMethodName,
        NewMethodName: string; TypeData: PTypeData;
        SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedMethod(ClassNode: TCodeTreeNode;
        const UpperOldMethodName, NewMethodName: string; TypeData: PTypeData;
        SourceChangeCache: TSourceChangeCache): boolean;
    function CreatePublishedMethod(const UpperClassName,
        AMethodName: string; TypeData: PTypeData;
        SourceChangeCache: TSourceChangeCache): boolean;
    function CreatePublishedMethod(ClassNode: TCodeTreeNode;
        const AMethodName: string; TypeData: PTypeData;
        SourceChangeCache: TSourceChangeCache): boolean;
        
    function MethodTypeDataToStr(TypeData: PTypeData;
        Attr: TProcHeadAttributes): string;
    function FindPublishedMethodNodeInClass(ClassNode: TCodeTreeNode;
        const UpperMethodName: string; TypeData: PTypeData): TCodeTreeNode;
    function FindProcNodeInImplementation(const UpperClassName,
        UpperMethodName: string; TypeData: PTypeData;
        BuildTreeBefore: boolean): TCodeTreeNode;
  end;


  NewClassPart = (ncpProcs, ncpVars);

  // TCodeCompletionCodeTool
  TCodeCompletionCodeTool = class(TEventsCodeTool)
  private
    ClassNode, StartNode: TCodeTreeNode;
    FirstInsert: TCodeTreeNodeExtension;
    JumpToProc: string;
    ASourceChangeCache: TSourceChangeCache;
    NewPrivatSectionIndent, NewPrivatSectionInsertPos: integer;
    function ProcExists(const NameAndParams: string): boolean;
    function VarExists(const UpperName: string): boolean;
    procedure AddInsert(PosNode: TCodeTreeNode;
        const CleanDef, Def, IdentifierName: string);
    function NodeExtIsVariable(ANodeExt: TCodeTreeNodeExtension): boolean;
    function CompleteProperty(PropNode: TCodeTreeNode): boolean;
    procedure InsertNewClassParts(PartType: NewClassPart);
    function InsertAllNewClassParts: boolean;
    function CreateMissingProcBodies: boolean;
  public
    function CompleteCode(CursorPos: TCodeXYPosition;
        var NewPos: TCodeXYPosition; var NewTopLine: integer;
        SourceChangeCache: TSourceChangeCache): boolean;
  end;

  ECodeToolError = class(Exception);


//-----------------------------------------------------------------------------
// useful functions
function AtomPosition(StartPos, EndPos: integer): TAtomPosition;
function CodePosition(P: integer; Code: TCodeBuffer): TCodePosition;
function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;

//=============================================================================

implementation

const
  MethodKindAsString: array[TMethodKind] of shortstring = (
        'procedure', 'function', 'constructor', 'destructor',
        'class procedure', 'class function'
      );

type
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

var
  NodeMemManager: TCodeTreeNodeMemManager;

type
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


{ useful functions }

function AtomPosition(StartPos, EndPos: integer): TAtomPosition;
begin
  Result.StartPos:=StartPos;
  Result.EndPos:=EndPos;
end;

function CodePosition(P: integer; Code: TCodeBuffer): TCodePosition;
begin
  Result.P:=P;
  Result.Code:=Code;
end;

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
  ctnWithBlock: Result:='WithBlock';

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


{ TAtomRing }

constructor TAtomRing.Create;
begin
  inherited Create;
  FItems:=nil;
  Size:=5;
end;

destructor TAtomRing.Destroy;
begin
  if FItems<>nil then FreeMem(FItems);
  inherited Destroy;
end;

procedure TAtomRing.SetSize(NewSize: integer);
var i: integer;
begin
  Clear;
  if NewSize<2 then NewSize:=2;
  if NewSize>$FFFFFFF then NewSize:=$FFFFFFF;
  i:=0;
  while (i<30) and (NewSize>=(1 shl i)) do inc(i);
  NewSize:=(1 shl i)-1;
  if FSize=NewSize then exit;
  if FItems<>nil then FreeMem(FItems);
  FSize:=NewSize;
  GetMem(FItems,(FSize+1) * SizeOf(TAtomPosition));
end;

procedure TAtomRing.Add(NewAtom: TAtomPosition);
begin
  FItems[FStart]:=NewAtom;
  FStart:=(FStart+1) and FSize;
  if (FStart=FLast) then
    FLast:=(FLast+1) and FSize;
end;

procedure TAtomRing.UndoLastAdd;
begin
  if FStart=FLast then exit;
  FStart:=(FStart-1) and FSize;
end;

function TAtomRing.GetValueAt(RelativePos:integer):TAtomPosition;
// 0=current 1=prior current ...
begin
  if RelativePos<Count then
    Result:=FItems[(FStart-RelativePos-1) and FSize]
  else begin
    Result.StartPos:=1;
    Result.EndPos:=1;
  end;
end;

procedure TAtomRing.Clear;
begin
  FStart:=0;
  FLast:=0;
end;

function TAtomRing.Count: integer;
begin
  Result:=FStart-FLast;
  if Result<0 then inc(Result,FSize);
end;

procedure TAtomRing.WriteDebugReport;
var i: integer;
  p: TAtomPosition;
begin
  writeln('[TAtomRing.WriteDebugReport] Size=',FSize
    ,' Start=',FStart,' Last=',FLast,' Count=',Count);
  write('ValuesAt: ');
  for i:=0 to Count-1 do begin
    p:=GetValueAt(i);
    write(' ',i,'=',p.StartPos,'-',p.EndPos);
  end;
  writeln('');
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

{ TCustomCodeTool }

constructor TCustomCodeTool.Create;
begin
  inherited Create;
  Tree:=TCodeTree.Create;
  KeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildDefaultKeyWordFunctions;
  LastAtoms:=TAtomRing.Create;
  IndentSize:=2;
  VisibleEditorLines:=20;
  CursorBeyondEOL:=true;
  FForceUpdateNeeded:=false;
  Clear;
end;

destructor TCustomCodeTool.Destroy;
begin
  Clear;
  LastAtoms.Free;
  Tree.Free;
  KeyWordFuncList.Free;
  inherited Destroy;
end;

procedure TCustomCodeTool.Clear;
begin
  Tree.Clear;
  CurPos.StartPos:=1;
  CurPos.EndPos:=-1;
  LastAtoms.Clear;
end;

procedure TCustomCodeTool.RaiseException(const AMessage: string);
var CaretXY: TCodeXYPosition;
begin
  if (CleanPosToCaret(CurPos.StartPos,CaretXY))
  and (CaretXY.Code<>nil) then begin
    raise ECodeToolError.Create('"'+CaretXY.Code.Filename+'"'
      +' at Y:'+IntToStr(CaretXY.Y)+',X:'+IntToStr(CaretXY.X)+' '+AMessage);
  end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then
    raise ECodeToolError.Create('"'+TCodeBuffer(Scanner.MainCode).Filename+'" '
      +AMessage)
  else
    raise ECodeToolError.Create(AMessage);
end;

procedure TCustomCodeTool.SetScanner(NewScanner: TLinkScanner);
begin
  if NewScanner=FScanner then exit;
  Clear;
  FScanner:=NewScanner;
  if FScanner<>nil then
    FLastScannerChangeStep:=Scanner.ChangeStep;
  FForceUpdateNeeded:=true;
end;

function TCustomCodeTool.NodeDescToStr(Desc: integer): string;
begin
  case Desc of
  // CodeTreeNodeDescriptors
  ctnNone            : Result:='None';

  ctnClass           : Result:='Class';
  ctnClassPublished  : Result:='Published';
  ctnClassPrivate    : Result:='Private';
  ctnClassProtected  : Result:='Protected';
  ctnClassPublic     : Result:='Public';

  ctnProcedure       : Result:='Method';
  ctnProcedureHead   : Result:='Method Head';
  ctnParameterList   : Result:='Param List';

  ctnBeginBlock      : Result:='Begin';
  ctnAsmBlock        : Result:='Asm';
  ctnWithBlock        : Result:='With';

  ctnProgram         : Result:='Program';
  ctnPackage         : Result:='Package';
  ctnLibrary         : Result:='Library';
  ctnUnit            : Result:='Unit';
  ctnInterface       : Result:='Interface';
  ctnImplementation  : Result:='Implementation';
  ctnInitialization  : Result:='Initialization';
  ctnFinalization    : Result:='Finalization';

  ctnTypeSection     : Result:='Type Section';
  ctnVarSection      : Result:='Var Section';
  ctnConstSection    : Result:='Const Section';
  ctnResStrSection   : Result:='Resource String Section';
  ctnUsesSection     : Result:='Uses Section';

  ctnTypeDefinition  : Result:='Type Definition';
  ctnVarDefinition   : Result:='Variable Definition';
  ctnConstDefinition : Result:='Const Definition';
  
  ctnProperty        : Result:='Property';
  
  ctnIdentifier      : Result:='Identifier';
  ctnArrayType       : Result:='Array Type';
  ctnRecordType      : Result:='Record Type';
  ctnRecordCase      : Result:='Record Case';
  ctnRecordVariant   : Result:='Record Variant';
  ctnProcedureType   : Result:='Procedure Type';
  ctnSetType         : Result:='Set Type';
  ctnRangeType       : Result:='Subrange Type';
  ctnEnumType        : Result:='Enumeration Type';
  ctnLabelType       : Result:='Label Type';
  ctnTypeType        : Result:='''Type'' Type';
  ctnFileType        : Result:='File Type';
  ctnPointerType     : Result:='Pointer ''^'' Type';
  ctnClassOfType     : Result:='Class Of Type';

  else
    Result:='(unknown descriptor '+IntToStr(Desc)+')';
  end;
end;

function TCustomCodeTool.NodeSubDescToStr(Desc, SubDesc: integer): string;
begin
  if SubDesc<>0 then
    Result:='(unknown subdescriptor '+IntToStr(SubDesc)+')'
  else
    Result:='';
  case Desc of
  ctnProcedure:
    case SubDesc of
    // CodeTreeNodeSubDescriptors
    ctnsForwardDeclaration : Result:='Forward';
    end;
  ctnClass:
    case SubDesc of
    // CodeTreeNodeSubDescriptors
    ctnsForwardDeclaration : Result:='Forward';
    end;
  end;
end;

function TCustomCodeTool.AtomIs(const AnAtom: shortstring): boolean;
var AnAtomLen,i : integer;
begin
  Result:=false;
  if (CurPos.StartPos<=SrcLen) and (CurPos.EndPos<=SrcLen+1)
  and (CurPos.StartPos>=1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen=CurPos.EndPos-CurPos.StartPos then begin
      for i:=1 to AnAtomLen do
        if AnAtom[i]<>Src[CurPos.StartPos-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TCustomCodeTool.UpAtomIs(const AnAtom: shortstring): boolean;
var AnAtomLen,i : integer;
begin
  Result:=false;
  if (CurPos.StartPos<SrcLen) and (CurPos.EndPos<=SrcLen+1)
  and (CurPos.StartPos>=1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen=CurPos.EndPos-CurPos.StartPos then begin
      for i:=1 to AnAtomLen do
        if AnAtom[i]<>UpperSrc[CurPos.StartPos-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TCustomCodeTool.ReadNextAtomIs(const AnAtom: shortstring): boolean;
begin
  ReadNextAtom;
  Result:=AtomIs(AnAtom);
end;

function TCustomCodeTool.ReadNextAtomIsChar(const c: char): boolean;
begin
  ReadNextAtom;
  Result:=AtomIsChar(c);
end;

function TCustomCodeTool.ReadNextUpAtomIs(const AnAtom: shortstring): boolean;
begin
  ReadNextAtom;
  Result:=UpAtomIs(AnAtom);
end;

function TCustomCodeTool.CompareNodeSrc(ANode: TCodeTreeNode;
  const ASource: string): integer;
var ASrcLen, i, NodeSrcLen : integer;
begin
  if (ANode.StartPos<=SrcLen) and (ANode.EndPos<=SrcLen+1)
  and (ANode.StartPos>=1) then begin
    ASrcLen:=length(ASource);
    NodeSrcLen:=ANode.EndPos-ANode.StartPos;
    if ASrcLen=NodeSrcLen then begin
      for i:=1 to ASrcLen do
        if ASource[i]<>Src[ANode.StartPos-1+i] then begin
          if ASource[i]>Src[ANode.StartPos-1+i] then
            Result:=1
          else
            Result:=-1;
          exit;
        end;
      Result:=0;
    end else if ASrcLen<NodeSrcLen then
      Result:=1
    else
      Result:=-1;
  end else
    Result:=-1;
end;

function TCustomCodeTool.CompareNodeUpSrc(ANode: TCodeTreeNode;
  const ASource: string): integer;
var ASrcLen, i, NodeSrcLen : integer;
begin
  if (ANode.StartPos<=SrcLen) and (ANode.EndPos<=SrcLen+1)
  and (ANode.StartPos>=1) then begin
    ASrcLen:=length(ASource);
    NodeSrcLen:=ANode.EndPos-ANode.StartPos;
    if ASrcLen<=NodeSrcLen then begin
      i:=1;
      while (i<=ASrcLen) and (IsIdentChar[Src[ANode.StartPos-1+i]]) do begin
        if ASource[i]<>UpperSrc[ANode.StartPos-1+i] then begin
          if ASource[i]>UpperSrc[ANode.StartPos-1+i] then
            Result:=1
          else
            Result:=-1;
          exit;
        end;
        inc(i);
      end;
      Result:=0;
    end else
      Result:=-1;
  end else
    Result:=-1;
end;

function TCustomCodeTool.AtomIsChar(const c: char): boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (CurPos.EndPos-CurPos.StartPos=1)
      and (Src[CurPos.StartPos]=c);
end;

function TCustomCodeTool.AtomIsWord: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (IsIdentStartChar[UpperSrc[CurPos.StartPos]])
end;

function TCustomCodeTool.AtomIsKeyWord: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (IsIdentStartChar[UpperSrc[CurPos.StartPos]])
      and (WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos));
end;

function TCustomCodeTool.AtomIsIdentifier(ExceptionOnNotFound: boolean):boolean;
begin
  if CurPos.StartPos<=SrcLen then begin
    if IsIdentStartChar[UpperSrc[CurPos.StartPos]] then begin
      if not WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos) then
        Result:=true
      else begin
        if ExceptionOnNotFound then
          RaiseException(
            'syntax error: identifier expected, but keyword '+GetAtom+' found')
        else
          Result:=false;
      end;
    end else begin
      if ExceptionOnNotFound then
        RaiseException(
          'syntax error: identifier expected, but '+GetAtom+' found')
      else
        Result:=false;
    end;
  end else begin
    if ExceptionOnNotFound then
      RaiseException('unexpected end of file (identifier expected)')
    else
      Result:=false;
  end;
end;

function TCustomCodeTool.AtomIsNumber: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (Src[CurPos.StartPos] in ['0'..'9','%','$']);
end;

function TCustomCodeTool.AtomIsStringConstant: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (Src[CurPos.StartPos] in ['''','#']);
end;

function TCustomCodeTool.LastAtomIs(BackIndex: integer;
  const AnAtom: shortstring): boolean;
var ap: TAtomPosition;
  AnAtomLen: integer;
  i: integer;
begin
  Result:=false;
  if (BackIndex>=0) and (BackIndex<LastAtoms.Count) then begin
    ap:=LastAtoms.GetValueAt(BackIndex);
    Result:=false;
    if (ap.StartPos<SrcLen) and (ap.EndPos<=SrcLen+1)
    and (ap.StartPos>=1) then begin
      AnAtomLen:=length(AnAtom);
      if AnAtomLen=ap.EndPos-ap.StartPos then begin
        for i:=1 to AnAtomLen do
          if AnAtom[i]<>Src[ap.StartPos-1+i] then exit;
        Result:=true;
      end;
    end;
  end;
end;

function TCustomCodeTool.LastUpAtomIs(BackIndex: integer;
  const AnAtom: shortstring): boolean;
var ap: TAtomPosition;
  AnAtomLen: integer;
  i: integer;
begin
  Result:=false;
  if (BackIndex>=0) and (BackIndex<LastAtoms.Count) then begin
    ap:=LastAtoms.GetValueAt(BackIndex);
    Result:=false;
    if (ap.StartPos<SrcLen) and (ap.EndPos<=SrcLen+1)
    and (ap.StartPos>=1) then begin
      AnAtomLen:=length(AnAtom);
      if AnAtomLen=ap.EndPos-ap.StartPos then begin
        for i:=1 to AnAtomLen do
          if AnAtom[i]<>UpperSrc[ap.StartPos-1+i] then exit;
        Result:=true;
      end;
    end;
  end;
end;

function TCustomCodeTool.GetAtom: string;
begin
  Result:=copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TCustomCodeTool.GetUpAtom: string;
begin
  Result:=copy(UpperSrc,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

procedure TCustomCodeTool.ReadNextAtom;
var c1, c2: char;
  CommentLvl: integer;
begin
  // Skip all spaces and comments
  CommentLvl:=0;
  if (CurPos.StartPos<CurPos.EndPos) and (CurPos.StartPos>=1) then
    LastAtoms.Add(CurPos);
  CurPos.StartPos:=CurPos.EndPos;
  //if CurPos.StartPos<1 then CurPos.StartPos:=SrcLen+1;
  while CurPos.StartPos<=SrcLen do begin
    if IsCommentStartChar[Src[CurPos.StartPos]] then begin
      case Src[CurPos.StartPos] of
      '{': // pascal comment
        begin
          CommentLvl:=1;
          inc(CurPos.StartPos);
          while (CurPos.StartPos<=SrcLen) and (CommentLvl>0) do begin
            case Src[CurPos.StartPos] of
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}': dec(CommentLvl);
            end;
            inc(CurPos.StartPos);
          end;
        end;
      '/':  // Delphi comment
        if (CurPos.StartPos<SrcLen) and (Src[CurPos.StartPos+1]='/') then begin
          inc(CurPos.StartPos,2);
          while (CurPos.StartPos<=SrcLen)
          and (not (Src[CurPos.StartPos] in [#10,#13])) do
            inc(CurPos.StartPos);
          inc(CurPos.StartPos);
          if (CurPos.StartPos<=SrcLen) and (Src[CurPos.StartPos] in [#10,#13])
          and (Src[CurPos.StartPos-1]<>Src[CurPos.StartPos]) then
            inc(CurPos.StartPos);
        end else
          break;
      '(': // old turbo pascal comment
        if (CurPos.StartPos<SrcLen) and (Src[CurPos.StartPos+1]='*') then begin
          inc(CurPos.StartPos,3);
          while (CurPos.StartPos<=SrcLen)
          and ((Src[CurPos.StartPos-1]<>'*') or (Src[CurPos.StartPos]<>')')) do
            inc(CurPos.StartPos);
          inc(CurPos.StartPos);  
        end else
          break;
      end;
    end else if IsSpaceChar[Src[CurPos.StartPos]] then begin
      repeat
        inc(CurPos.StartPos);
      until (CurPos.StartPos>SrcLen)
      or (not (IsSpaceChar[Src[CurPos.StartPos]]));
    end else begin
      break;
    end;
  end;
  CurPos.EndPos:=CurPos.StartPos;
  if CurPos.StartPos>SrcLen then
    exit;
  // read atom
  c1:=UpperSrc[CurPos.EndPos];
  case c1 of
    '_','A'..'Z':
      begin
        inc(CurPos.EndPos);
        while (CurPos.EndPos<=SrcLen)
        and (IsIdentChar[UpperSrc[CurPos.EndPos]]) do
          inc(CurPos.EndPos);
      end;
    '''','#':
      begin
        while (CurPos.EndPos<=SrcLen) do begin
          case (Src[CurPos.EndPos]) of
          '#':
            begin
              inc(CurPos.EndPos);
              while (CurPos.EndPos<=SrcLen)
              and (IsNumberChar[Src[CurPos.EndPos]]) do
                inc(CurPos.EndPos);
            end;
          '''':
            begin
              inc(CurPos.EndPos);
              while (CurPos.EndPos<=SrcLen)
              and (Src[CurPos.EndPos]<>'''') do
                inc(CurPos.EndPos);
              inc(CurPos.EndPos);
            end;
          else
            break;
          end;
        end;
      end;
    '0'..'9':
      begin
        inc(CurPos.EndPos);
        while (CurPos.EndPos<=SrcLen) and (IsNumberChar[Src[CurPos.EndPos]]) do
          inc(CurPos.EndPos);
        if (CurPos.EndPos<SrcLen)
        and (Src[CurPos.EndPos]='.') and (Src[CurPos.EndPos+1]<>'.') then begin
          // real type number
          inc(CurPos.EndPos);
          while (CurPos.EndPos<=SrcLen) and (IsNumberChar[Src[CurPos.EndPos]])
          do
            inc(CurPos.EndPos);
          if (CurPos.EndPos<=SrcLen) and (UpperSrc[CurPos.EndPos]='E') then
          begin
            // read exponent
            inc(CurPos.EndPos);
            if (CurPos.EndPos<=SrcLen) and (Src[CurPos.EndPos] in ['-','+'])
            then inc(CurPos.EndPos);
            while (CurPos.EndPos<=SrcLen) and (IsNumberChar[Src[CurPos.EndPos]])
            do
              inc(CurPos.EndPos);
          end;
        end;
      end;
    '%':
      begin
        inc(CurPos.EndPos);
        while (CurPos.EndPos<=SrcLen) and (Src[CurPos.EndPos] in ['0'..'1']) do
          inc(CurPos.EndPos);
      end;
    '$':
      begin
        inc(CurPos.EndPos);
        while (CurPos.EndPos<=SrcLen)
        and (IsHexNumberChar[UpperSrc[CurPos.EndPos]]) do
          inc(CurPos.EndPos);
      end;
    else
      inc(CurPos.EndPos);
      if CurPos.EndPos<=SrcLen then begin
        c2:=Src[CurPos.EndPos];
        // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
        if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
        or ((c1='<') and (c2='>'))
        or ((c1='>') and (c2='<'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        then inc(CurPos.EndPos);
      end;
  end;
end;

procedure TCustomCodeTool.UndoReadNextAtom;
begin
  if LastAtoms.Count>0 then begin
    CurPos:=LastAtoms.GetValueAt(0);
    LastAtoms.UndoLastAdd;
  end else
    RaiseException('TCustomCodeTool.UndoReadNextAtom impossible');
end;

function TCustomCodeTool.ReadTilSection(
  SectionType: TCodeTreeNodeDesc): boolean;
var SectionID: TCodeTreeNodeDesc;
begin
  Result:=false;
  if not (SectionType in AllCodeSections) then exit;
  Result:=false;
  repeat
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) then break;
    if IsKeyWordSection.DoItUppercase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos)
    and (not LastAtomIs(1,'=')) then begin
      if UpAtomIs('UNIT') then
        SectionID:=ctnUnit
      else if UpAtomIs('PROGRAM') then
        SectionID:=ctnProgram
      else if UpAtomIs('PACKAGE') then
        SectionID:=ctnPackage
      else if UpAtomIs('LIBRARY') then
        SectionID:=ctnLibrary
      else if UpAtomIs('INTERFACE') then
        SectionID:=ctnInterface
      else if UpAtomIs('IMPLEMENTATION') then
        SectionID:=ctnImplementation
      else if UpAtomIs('INITIALIZATION') then
        SectionID:=ctnInitialization
      else if UpAtomIs('FINALIZATION') then
        SectionID:=ctnFinalization
      else
        SectionID:=ctnNone;
      if (SectionType=SectionID)
      or ((SectionType=ctnInterface)
      and (SectionID in [ctnProgram,ctnPackage,ctnLibrary])) then begin
        Result:=true;  exit;
      end;
      if SectionID>SectionType then
        exit;
    end;
  until false;
end;

function TCustomCodeTool.ReadTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
var CloseBracket, AntiCloseBracket: char;
  Start: TAtomPosition;
begin
  Result:=false;
  if AtomIsChar('(') then begin
    CloseBracket:=')';
    AntiCloseBracket:=']';
  end else if AtomIsChar('[') then begin
    CloseBracket:=']';
    AntiCloseBracket:=')';
  end else begin
    if ExceptionOnNotFound then
      RaiseException(
        'syntax error: bracket open expected, but '+GetAtom+' found');
    exit;
  end;
  Start:=CurPos;
  repeat
    ReadNextAtom;
    if (AtomIsChar(CloseBracket)) then break;
    if (CurPos.StartPos>SrcLen) or AtomIsChar(AntiCloseBracket)
    or UpAtomIs('END') then begin
      CurPos:=Start;
      if ExceptionOnNotFound then
        RaiseException(
          'syntax error: bracket '+CloseBracket+' not found');
      exit;
    end;
    if (AtomIsChar('(')) or (AtomIsChar('[')) then begin
      if not ReadTilBracketClose then exit;
    end;
  until false;
  Result:=true;
end;

procedure TCustomCodeTool.BeginParsing(DeleteNodes,
  OnlyInterfaceNeeded: boolean);
begin
  Scanner.Scan(OnlyInterfaceNeeded,CheckFilesOnDisk);
  Src:=Scanner.CleanedSrc;
  FLastScannerChangeStep:=Scanner.ChangeStep;
  UpperSrc:=UpperCaseStr(Src);
  SrcLen:=length(Src);
  CurPos.StartPos:=1;
  CurPos.EndPos:=1;
  LastAtoms.Clear;
  CurNode:=nil;
  if DeleteNodes then Tree.Clear;
end;

procedure TCustomCodeTool.MoveCursorToNodeStart(ANode: TCodeTreeNode);
begin
  CurPos.StartPos:=ANode.StartPos;
  CurPos.EndPos:=ANode.StartPos;
  LastAtoms.Clear;
  CurNode:=ANode;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: integer);
begin
  CurPos.StartPos:=ACleanPos;
  CurPos.EndPos:=ACleanPos;
  LastAtoms.Clear;
  CurNode:=nil;
end;

procedure TCustomCodeTool.CreateChildNode;
var NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.NewNode;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  CurNode:=NewNode;
  CurNode.StartPos:=CurPos.StartPos;
end;

procedure TCustomCodeTool.EndChildNode;
begin
  CurNode:=CurNode.Parent;
end;

procedure TCustomCodeTool.BuildDefaultKeyWordFunctions;
begin
  KeyWordFuncList.Clear;
  KeyWordFuncList.DefaultKeyWordFunction:=
    {$ifdef FPC}@{$endif}DefaultKeyWordFunc;
end;

function TCustomCodeTool.DoAtom: boolean;
begin
  if (CurPos.StartPos>SrcLen) or (CurPos.EndPos<=CurPos.StartPos) then
    Result:=false
  else if IsIdentStartChar[Src[CurPos.StartPos]] then
    Result:=KeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
  else
    Result:=true;
end;

function TCustomCodeTool.DefaultKeyWordFunc: boolean;
begin
  Result:=true;
end;

function TCustomCodeTool.ConsistencyCheck: integer;
// 0 = ok
begin
  Result:=Tree.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,100);  exit;
  end;
  Result:=0;
end;

procedure TCustomCodeTool.WriteDebugTreeReport;

  procedure WriteSrcSubString(A,Len: integer);
  var i: integer;
  begin
    write('"');
    for i:=A to A+Len-1 do begin
      if (i>0) and (i<SrcLen) and (ord(Src[i])>31) then
        write(Src[i]);
    end;
    write('"');
  end;

  procedure WriteSubTree(RootNode: TCodeTreeNode; Indent: string);
  begin
    while RootNode<>nil do begin
      write(Indent);
      with RootNode do begin
        write(NodeDescToStr(Desc),'(',NodeSubDescToStr(Desc,SubDesc),')  ');
        write(' Start=',StartPos,' ');
        WriteSrcSubString(StartPos,5);
        write(' End=',EndPos,' ');
        WriteSrcSubString(EndPos-5,5);
{$ifdef fpc}
        write(' Self=',HexStr(Cardinal(RootNode),8));
        write(' P=',HexStr(Cardinal(Parent),8));
        write(' NB=',HexStr(Cardinal(NextBrother),8));
        //write(' PB=',HexStr(Cardinal(PriorBrother),8));
        //write(' FC=',HexStr(Cardinal(FirstChild),8));
        //write(' LC=',HexStr(Cardinal(LastChild),8));
{$endif}
      end;
      writeln('');
      WriteSubTree(RootNode.FirstChild,Indent+'  ');
      RootNode:=RootNode.NextBrother;
    end;
  end;

begin
  writeln('[TCustomCodeTool.WriteDebugTreeReport] Consistency=',
     ConsistencyCheck);
  WriteSubTree(Tree.Root,'  ');
end;

function TCustomCodeTool.FindDeepestNodeAtPos(P: integer): TCodeTreeNode;

  function SearchInNode(ANode: TCodeTreeNode): TCodeTreeNode;
  begin
    if ANode<>nil then begin
//writeln('SearchInNode ',NodeDescriptionAsString(ANode.Desc),
//',',ANode.StartPos,',',ANode.EndPos,', p=',p,
//' "',copy(Src,ANode.StartPos,20),'"');
      if (ANode.StartPos<=P) and ((ANode.EndPos>P) or (ANode.EndPos<1)) then
      begin
        // first search in childs
        Result:=SearchInNode(ANode.FirstChild);
        if Result=nil then
          // no child found -> take this node
          Result:=ANode;
      end else
        // search in next node
        Result:=SearchInNode(ANode.NextBrother);
    end else
      Result:=nil;
  end;

// TCustomCodeTool.FindDeepestNodeAtPos
begin
  Result:=SearchInNode(Tree.Root);
end;

function TCustomCodeTool.CaretToCleanPos(Caret: TCodeXYPosition;
  var CleanPos: integer): integer;
begin
  Caret.Code.LineColToPosition(Caret.Y,Caret.X,CleanPos);
  if (CleanPos>=1) then
    Result:=Scanner.CursorToCleanPos(CleanPos,Caret.Code,CleanPos)
  else
    Result:=-2; // x,y beyond source
end;

function TCustomCodeTool.CleanPosToCaret(CleanPos: integer;
  var Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
var p: integer;
  Code: Pointer;
begin
  Result:=Scanner.CleanedPosToCursor(CleanPos,p,Code);
  if Result then begin
    Caret.Code:=TCodeBuffer(Code);
    TCodeBuffer(Code).AbsoluteToLineCol(p,Caret.Y,Caret.X);
    Result:=(Caret.Y>=0);
  end;
end;

procedure TCustomCodeTool.GetLineInfo(ACleanPos: integer;
  var ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);
begin
  if ACleanPos>=1 then begin
    if ACleanPos<=SrcLen then begin
      // search line start
      ALineStart:=ACleanPos;
      while (ALineStart>=1) and (not (Src[ALineStart] in [#10,#13])) do
        dec(ALineStart);
      inc(ALineStart);
      // search line end
      ALineEnd:=ACleanPos;
      while (ALineEnd>=1) and (not (Src[ALineEnd] in [#10,#13])) do
        inc(ALineEnd);
      // search first atom in line
      CurPos.StartPos:=ALineStart;
      CurPos.EndPos:=ALineStart;
      ReadNextAtom;
      AFirstAtomStart:=CurPos.StartPos;
      // search last atom in line
      repeat
        ALastAtomEnd:=CurPos.EndPos;
        ReadNextAtom;
      until CurPos.EndPos>ALineEnd;
    end else begin
      ALineStart:=Srclen+1;
      ALineEnd:=Srclen+1;
      AFirstAtomStart:=Srclen+1;
      ALastAtomEnd:=Srclen+1;
    end;
  end else begin
    ALineStart:=1;
    ALineEnd:=1;
    AFirstAtomStart:=1;
    ALastAtomEnd:=1;
  end;
end;

function TCustomCodeTool.UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
begin
{$IFDEF CTDEBUG}
writeln('TCustomCodeTool.UpdateNeeded A ',Scanner<>nil);
{$ENDIF}
  if FForceUpdateNeeded then begin
    Result:=true;
    exit;
  end;
  Result:=(FLastScannerChangeStep<>Scanner.ChangeStep)
           or (Scanner.UpdateNeeded(OnlyInterfaceNeeded, CheckFilesOnDisk));
  FForceUpdateNeeded:=Result;
{$IFDEF CTDEBUG}
writeln('TCustomCodeTool.UpdateNeeded END');
{$ENDIF}
end;

{ TMultiKeyWordListCodeTool }

constructor TMultiKeyWordListCodeTool.Create;
begin
  inherited Create;
  FKeyWordLists:=TList.Create; // list of TKeyWordFunctionList
  AddKeyWordFuncList(KeyWordFuncList);
  FCurKeyWordListID:=0;
  DefaultKeyWordFuncList:=KeyWordFuncList;
end;

destructor TMultiKeyWordListCodeTool.Destroy;
begin
  ClearKeyWordFuncLists;
  FKeyWordLists.Free;
  inherited Destroy;
end;

procedure TMultiKeyWordListCodeTool.SetKeyWordListID(NewID: integer);
begin
  if FCurKeyWordListID=NewID then exit;
  FCurKeyWordListID:=NewID;
  KeyWordFuncList:=TKeyWordFunctionList(FKeyWordLists[NewID]);
end;

procedure TMultiKeyWordListCodeTool.SetCurKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList);
var i: integer;
begin
  i:=0;
  while i<FKeyWordLists.Count do begin
    if TKeyWordFunctionList(FKeyWordLists[i])=AKeyWordFuncList then begin
      SetKeyWordListID(i);
      exit;
    end;
    inc(i);
  end;
  RaiseException(
    '[TMultiKeyWordListCodeTool.SetCurKeyWordFuncList] unknown list');
end;

function TMultiKeyWordListCodeTool.AddKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList): integer;
begin
  Result:=FKeyWordLists.Add(AKeyWordFuncList);
end;

procedure TMultiKeyWordListCodeTool.ClearKeyWordFuncLists;
var i: integer;
begin
  KeyWordListID:=0;
  for i:=FKeyWordLists.Count-1 downto 1 do begin
    TKeyWordFunctionList(FKeyWordLists[i]).Free;
    FKeyWordLists.Delete(i);
  end;
  KeyWordFuncList.Clear;
end;


{ TPascalParserTool }

constructor TPascalParserTool.Create;
begin
  inherited Create;
  // KeyWord functions for parsing blocks (e.g. begin..end)
  EndKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildEndKeyWordFunctions;
  AddKeyWordFuncList(EndKeyWordFuncList);
  // keywords for parsing types
  TypeKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildTypeKeyWordFunctions;
  AddKeyWordFuncList(TypeKeyWordFuncList);
  PackedTypesKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildPackedTypesKeyWordFunctions;
  AddKeyWordFuncList(PackedTypesKeyWordFuncList);
  // KeyWord functions for parsing in a class
  InnerClassKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildInnerClassKeyWordFunctions;
  AddKeyWordFuncList(InnerClassKeyWordFuncList);
  ClassVarTypeKeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildClassVarTypeKeyWordFunctions;
  AddKeyWordFuncList(ClassVarTypeKeyWordFuncList);
end;

destructor TPascalParserTool.Destroy;
begin
  if ExtractMemStream<>nil then
    ExtractMemStream.Free;
  inherited Destroy;
end;

procedure TPascalParserTool.BuildDefaultKeyWordFunctions;
begin
  inherited BuildDefaultKeyWordFunctions;
  with KeyWordFuncList do begin
    Add('PROGRAM',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('LIBRARY',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('PACKAGE',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('UNIT',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('INTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}KeyWordFuncSection);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}KeyWordFuncSection);

    Add('END',{$ifdef FPC}@{$endif}KeyWordFuncEnd);

    Add('TYPE',{$ifdef FPC}@{$endif}KeyWordFuncType);
    Add('VAR',{$ifdef FPC}@{$endif}KeyWordFuncVar);
    Add('CONST',{$ifdef FPC}@{$endif}KeyWordFuncConst);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}KeyWordFuncResourceString);
    
    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncMethod);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncMethod);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncMethod);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncMethod);
    Add('OPERATOR',{$ifdef FPC}@{$endif}KeyWordFuncMethod);
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncMethod);

    Add('BEGIN',{$ifdef FPC}@{$endif}KeyWordFuncBeginEnd);
    Add('ASM',{$ifdef FPC}@{$endif}KeyWordFuncBeginEnd);
    
    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}UnexpectedKeyWord;
  end;
end;

procedure TPascalParserTool.BuildEndKeyWordFunctions;
// KeyWordFunctions for parsing end - blocks
begin
  with EndKeyWordFuncList do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;

procedure TPascalParserTool.BuildTypeKeyWordFunctions;
// KeyWordFunctions for parsing types
begin
  with TypeKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('OBJECT',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}KeyWordFuncClass);
    Add('PACKED',{$ifdef FPC}@{$endif}KeyWordFuncTypePacked);
    Add('ARRAY',{$ifdef FPC}@{$endif}KeyWordFuncTypeArray);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncTypeProc);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncTypeProc);
    Add('SET',{$ifdef FPC}@{$endif}KeyWordFuncTypeSet);
    Add('LABEL',{$ifdef FPC}@{$endif}KeyWordFuncTypeLabel);
    Add('TYPE',{$ifdef FPC}@{$endif}KeyWordFuncTypeType);
    Add('FILE',{$ifdef FPC}@{$endif}KeyWordFuncTypeFile);
    Add('RECORD',{$ifdef FPC}@{$endif}KeyWordFuncTypeRecord);
    Add('^',{$ifdef FPC}@{$endif}KeyWordFuncTypePointer);
    
    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}KeyWordFuncTypeDefault;
  end;
end;

procedure TPascalParserTool.BuildPackedTypesKeyWordFunctions;
// KeyWordFunctions for valid packed types
begin
  with PackedTypesKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;

procedure TPascalParserTool.BuildInnerClassKeyWordFunctions;
// KeyWordFunctions for parsing in a class/object
begin
  with InnerClassKeyWordFuncList do begin
    Add('PUBLIC',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);
    Add('PRIVATE',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);
    Add('PUBLISHED',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);
    Add('PROTECTED',{$ifdef FPC}@{$endif}KeyWordFuncClassSection);

    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);
    Add('STATIC',{$ifdef FPC}@{$endif}KeyWordFuncClassMethod);

    Add('PROPERTY',{$ifdef FPC}@{$endif}KeyWordFuncClassProperty);
    
    Add('END',{$ifdef FPC}@{$endif}AllwaysFalse);

    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}KeyWordFuncClassIdentifier;
  end;
end;

procedure TPascalParserTool.BuildClassVarTypeKeyWordFunctions;
// KeywordFunctions for parsing the type of a variable in a class/object
begin
  with ClassVarTypeKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeClass);
    Add('OBJECT',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeClass);
    Add('PACKED',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypePacked);
    Add('RECORD',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeRecord);
    Add('ARRAY',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeArray);
    Add('SET',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeSet);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeProc);
    Add('FUNCTION',{$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeProc);

    DefaultKeyWordFunction:={$ifdef FPC}@{$endif}KeyWordFuncClassVarTypeIdent;
  end;
end;

function TPascalParserTool.UnexpectedKeyWord: boolean;
begin
  Result:=false;
  RaiseException('syntax error: unexpected word "'+GetAtom+'"');
end;

procedure TPascalParserTool.BuildTree(OnlyInterfaceNeeded: boolean);
begin
writeln('TPascalParserTool.BuildTree A OnlyInterfaceNeeded=',OnlyInterfaceNeeded);
{$IFDEF MEM_CHECK}
CheckHeap('TBasicCodeTool.BuildTree A '+IntToStr(GetMem_Cnt));
{$ENDIF}
  if not UpdateNeeded(OnlyInterfaceNeeded) then exit;
{$IFDEF CTDEBUG}
writeln('TPascalParserTool.BuildTree B');
{$ENDIF}
//CheckHeap('TBasicCodeTool.BuildTree B '+IntToStr(GetMem_Cnt));
  BeginParsing(true,OnlyInterfaceNeeded);
  InterfaceSectionFound:=false;
  ImplementationSectionFound:=false;
  EndOfSourceFound:=false;
  ReadNextAtom;
  if UpAtomIs('UNIT') then
    CurSection:=ctnUnit
  else if UpAtomIs('PROGRAM') then
    CurSection:=ctnProgram
  else if UpAtomIs('PACKAGE') then
    CurSection:=ctnPackage
  else if UpAtomIs('LIBRARY') then
    CurSection:=ctnLibrary
  else
    RaiseException(
      'syntax error: no pascal code found (first token is '+GetAtom+')');
  CreateChildNode;
  CurNode.Desc:=CurSection;
  ReadNextAtom; // read source name
  AtomIsIdentifier(true);
  ReadNextAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseException('syntax error: ; expected, but '+GetAtom+' found');
  if CurSection=ctnUnit then begin
    ReadNextAtom;
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
    if not UpAtomIs('INTERFACE') then
      RaiseException(
        'syntax error: ''interface'' expected, but '+GetAtom+' found');
    CreateChildNode;
    CurSection:=ctnInterface;
    CurNode.Desc:=CurSection;
  end;
  InterfaceSectionFound:=true;
  ReadNextAtom;
  if UpAtomIs('USES') then
    ReadUsesSection(true);
  repeat
//writeln('[TPascalParserTool.BuildTree] ALL '+GetAtom);
    if not DoAtom then break;
    if CurSection=ctnNone then begin
      EndOfSourceFound:=true;
      break;
    end;
    ReadNextAtom;
  until (CurPos.StartPos>SrcLen);
  FForceUpdateNeeded:=false;
{$IFDEF CTDEBUG}
writeln('[TPascalParserTool.BuildTree] END');
{$ENDIF}
{$IFDEF MEM_CHECK}
CheckHeap('TBasicCodeTool.BuildTree END '+IntToStr(GetMem_Cnt));
{$ENDIF}
end;

procedure TPascalParserTool.BuildSubTreeForClass(ClassNode: TCodeTreeNode);
// reparse a quick parsed class and build the child nodes
begin
  if ClassNode=nil then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForClass: Classnode=nil');
  if ClassNode.FirstChild<>nil then
    // class already parsed
    exit;
  // set CursorPos after class head
  MoveCursorToNodeStart(ClassNode);
  // parse
  //   - inheritage
  //   - class sections (public, published, private, protected)
  //   - methods (procedures, functions, constructors, destructors)

  // first parse the inheritage
  // read the "class"/"object" keyword
  ReadNextAtom;
  if UpAtomIs('PACKED') then ReadNextAtom;
  if (not UpAtomIs('CLASS')) and (not UpAtomIs('OBJECT')) then
    RaiseException(
        'TPascalParserTool.BuildSubTreeForClass:'
       +' class/object keyword expected, but '+GetAtom+' found');
  ReadNextAtom;
  if AtomIsChar('(') then
    // read inheritage
    ReadTilBracketClose(true)
  else
    UndoReadNextAtom;
  // clear the last atoms
  LastAtoms.Clear;
  // start the first class section
  CreateChildNode;
  CurNode.StartPos:=CurPos.EndPos;
  if UpAtomIs('PUBLIC') then
    CurNode.Desc:=ctnClassPublic
  else if UpAtomIs('PRIVATE') then
    CurNode.Desc:=ctnClassPrivate
  else if UpAtomIs('PROTECTED') then
    CurNode.Desc:=ctnClassProtected
  else
    CurNode.Desc:=ctnClassPublished;
  // parse till "end" of class/object
  CurKeyWordFuncList:=InnerClassKeyWordFuncList;
  try
    repeat
      ReadNextAtom;
      if CurPos.StartPos>=ClassNode.EndPos then break;
      if not DoAtom then break;
    until false;
    // end last class section (public, private, ...)
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
  finally
    CurKeyWordFuncList:=DefaultKeyWordFuncList;
  end;
end;

function TPascalParserTool.GetSourceType: TCodeTreeNodeDesc;
begin
  if Tree.Root<>nil then
    Result:=Tree.Root.Desc
  else
    Result:=ctnNone;
end;

function TPascalParserTool.KeyWordFuncClassReadTilEnd: boolean;
// read til atom after next 'end'
begin
  repeat
    ReadNextAtom;
  until (CurPos.StartPos>SrcLen) or UpAtomIs('END');
  ReadNextAtom;
  Result:=(CurPos.StartPos<SrcLen);
end;

function TPascalParserTool.KeyWordFuncClassIdentifier: boolean;
{ parse class variable

  examples:
    Name: TypeName;
    Name: UnitName.TypeName;
    i, j: integer;
    MyArray: array of array[EnumType] of array [Range] of TypeName;
    MyRecord: record
              i: packed record
                   j: integer;
                   k: record end;
                   case integer of
                     0: (a: integer);
                     1,2,3: (b: array[char] of char; c: char);
                     3: ( d: record
                               case byte of
                                 10: (i: integer; );
                                 11: (y: byte);
                             end;
                 end;
            end;
    MyPointer: ^integer;
    MyEnum: (MyEnumm1, MyEnumm2 := 2, MyEnummy3);
    MySet: set of (MyEnummy4 := 4 , MyEnummy5);
    MyRange: 3..5;
}
begin
  // create variable definition node
  CreateChildNode;
  CurNode.Desc:=ctnVarDefinition;
  ReadNextAtom;
  while AtomIsChar(',') do begin
    // end variable definition
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
    // read next variable name
    ReadNextAtom;
    AtomIsIdentifier(true);
    // create variable definition node
    CreateChildNode;
    CurNode.Desc:=ctnVarDefinition;
    ReadNextAtom;
  end;
  if not AtomIsChar(':') then
    RaiseException('syntax error: : expected, but '+GetAtom+' found');
  ReadNextAtom;
  if (CurPos.StartPos>SrcLen) then
    RaiseException('syntax error: variable type definition not found');
  // create type body node
  CreateChildNode;
  CurNode.Desc:=ctnTypeDefinition;
  // parse type body
  if AtomIsChar('^') then begin
    // parse pointer type
    ReadNextAtom;
    AtomIsIdentifier(true);
  end else if (Src[CurPos.StartPos] in ['(','-','+']) or AtomIsNumber then begin
    // parse enum or range type
    while (CurPos.StartPos<=SrcLen) do begin
      if Src[CurPos.StartPos] in ['(','['] then
        ReadTilBracketClose(true);
      if AtomIsChar(';') or UpAtomIs('END') then begin
        UndoReadNextAtom;
        break;
      end;
      ReadNextAtom;
    end;
  end else
    Result:=ClassVarTypeKeyWordFuncList.DoItUpperCase(UpperSrc,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
  ReadNextAtom;
  if (UpAtomIs('END')) then
    UndoReadNextAtom
  else if not AtomIsChar(';') then
    RaiseException('syntax error: ; expected, but '+GetAtom+' found');
  // end type body
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  // end variable definition
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeClass: boolean;
// class and object as type are not allowed, because they would have no name
begin
  RaiseException(
    'syntax error: Anonym '+GetAtom+' definitions are not allowed');
  Result:=false;
end;

function TPascalParserTool.KeyWordFuncClassVarTypePacked: boolean;
// 'packed' record
begin
  ReadNextAtom;
  if UpAtomIs('RECORD') then
    Result:=KeyWordFuncClassVarTypeRecord
  else begin
    RaiseException('syntax error: ''record'' expected, but '+GetAtom+' found');
    Result:=true;
  end;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeRecord: boolean;
{ read variable type 'record'

  examples:
    record
      i: packed record
           j: integer;
           k: record end;
           case integer of
             0: (a: integer);
             1,2,3: (b: array[char] of char; c: char);
             3: ( d: record
                       case byte of
                         10: (i: integer; );
                         11: (y: byte);
                     end;
         end;
    end;
}
var Level: integer;
begin
  Level:=1;
  while (CurPos.StartPos<=SrcLen) and (Level>0) do begin
    ReadNextAtom;
    if UpAtomIs('RECORD') then inc(Level)
    else if UpAtomIs('END') then dec(Level);
  end;
  if CurPos.StartPos>SrcLen then
    RaiseException('syntax error: end for record not found.');
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeArray: boolean;
{ read variable type 'array'

  examples:
    array of array[EnumType] of array [Range] of TypeName;
}
begin
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // array[Range]
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseException('syntax error: [ expected, but '+GetAtom+' found');
  ReadNextAtom;
//writeln('TPascalParserTool.KeyWordFuncClassVarTypeArray ',GetAtom);
  Result:=ClassVarTypeKeyWordFuncList.DoItUpperCase(UpperSrc,
    CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncClassVarTypeSet: boolean;
{ read variable type 'set of'

  examples:
    set of Name
    set of (MyEnummy4 := 4 , MyEnummy5);
}
begin
  ReadNextAtom;
  if not UpAtomIs('OF') then
    RaiseException('syntax error: ''of'' expected, but '+GetAtom+' found');
  ReadNextAtom;
  if CurPos.StartPos>SrcLen then
    RaiseException('syntax error: missing enum list');
  if UpperSrc[CurPos.StartPos] in ['A'..'Z','_'] then
    // set of identifier
  else if AtomIsChar('(') then
    // set of ()
    ReadTilBracketClose(true);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeProc: boolean;
{ read variable type 'procedure ...' or 'function ... : ...'

  examples:
    procedure
    function : integer;
    procedure (a: char) of object;
}
var IsFunction, HasForwardModifier: boolean;
begin
//writeln('[TPascalParserTool.KeyWordFuncClassVarTypeProc]');
  IsFunction:=UpAtomIs('FUNCTION');
  ReadNextAtom;
  HasForwardModifier:=false;
  ReadTilProcedureHeadEnd(true,IsFunction,true,HasForwardModifier);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeIdent: boolean;
// read variable type <identfier>
begin
  if CurPos.StartPos>SrcLen then
    RaiseException('syntax error: missing type identifier');
  if UpperSrc[CurPos.StartPos] in ['A'..'Z','_'] then
    // identifier
  else
    RaiseException('syntax error: missing type identifier');
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassSection: boolean;
// change section in a class (public, private, protected, published)
begin
  // end last section
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  // start new section
  CreateChildNode;
  if UpAtomIs('PUBLIC') then
    CurNode.Desc:=ctnClassPublic
  else if UpAtomIs('PRIVATE') then
    CurNode.Desc:=ctnClassPrivate
  else if UpAtomIs('PROTECTED') then
    CurNode.Desc:=ctnClassProtected
  else
    CurNode.Desc:=ctnClassPublished;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassMethod: boolean;
{ parse class method

 examples:
   procedure ProcName;  virtual; abstract;
   function FuncName(Parameter1: Type1; Parameter2: Type2): ResultType;
   constructor Create;
   destructor Destroy;  override;
   class function X: integer;
   static function X: integer;

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>
}
var IsFunction, HasForwardModifier: boolean;
begin
  HasForwardModifier:=false;
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProcedure;
  // read method keyword
  if UpAtomIs('CLASS') or (UpAtomIs('STATIC')) then begin
    ReadNextAtom;
    if (not UpAtomIs('PROCEDURE')) and (not UpAtomIs('FUNCTION')) then begin
      RaiseException(
        'syntax error: procedure or function expected, but '+GetAtom+' found');
    end;
  end;
  IsFunction:=UpAtomIs('FUNCTION');
  // read procedure head
  // read name
  ReadNextAtom;
  if (CurPos.StartPos>SrcLen)
  or (not (UpperSrc[CurPos.StartPos] in ['A'..'Z','_']))
  then
    RaiseException('syntax error: method name expected, but '+GetAtom+' found');
  // create node for procedure head
  CreateChildNode;
  CurNode.Desc:=ctnProcedureHead;
  // read rest
  ReadNextAtom;
  ReadTilProcedureHeadEnd(true,IsFunction,false,HasForwardModifier);
  // close procedure header
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  // close procedure
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.ReadParamList(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
var CloseBracket: char;
begin
  Result:=false;
  if AtomIsChar('(') or AtomIsChar('[') then begin
    if AtomIsChar('(') then
      CloseBracket:=')'
    else
      CloseBracket:=']';
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutBrackets in Attr),Attr);
  end else
    CloseBracket:=#0;
  repeat
    // read parameter prefix modifier
    if (UpAtomIs('VAR')) or (UpAtomIs('CONST')) or (UpAtomIs('OUT')) then
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(phpWithVarModifiers in Attr,Attr);
    // read parameter name(s)
    repeat
      AtomIsIdentifier(ExceptionOnError);
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(phpWithParameterNames in Attr,Attr);
      if not AtomIsChar(',') then
        break
      else
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    until false;
    // read type
    if (AtomIsChar(':')) then begin
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      if not ReadParamType(ExceptionOnError,Extract,Attr) then exit;
      if AtomIsChar('=') then begin
        if not Extract then
          ReadNextAtom
        else
          ExtractNextAtom(phpWithDefaultValues in Attr,Attr);
        ReadConstant(ExceptionOnError,
          Extract and (phpWithDefaultValues in Attr),Attr);
      end;
    end;
    // read next parameter
    if (CurPos.StartPos>SrcLen) then
      if ExceptionOnError then
        RaiseException(
          'syntax error: '+CloseBracket+' expected, but '+GetAtom+' found')
      else exit;
    if (Src[CurPos.StartPos] in [')',']']) then break;
    if (Src[CurPos.StartPos]<>';') then
      if ExceptionOnError then
        RaiseException(
          'syntax error: '+CloseBracket+' expected, but '+GetAtom+' found')
      else exit;
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
  until false;
  if (CloseBracket<>#0) then begin
    if Src[CurPos.StartPos]<>CloseBracket then
      if ExceptionOnError then
        RaiseException(
          'syntax error: '+CloseBracket+' expected, but '+GetAtom+' found')
      else exit;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutBrackets in Attr),Attr);
  end;
  Result:=true;
end;

function TPascalParserTool.ReadParamType(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
begin
  Result:=false;
  if AtomIsWord then begin
    if UpAtomIs('ARRAY') then begin
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      if not UpAtomIs('OF') then
        if ExceptionOnError then
          RaiseException('syntax error: ''of'' expected, but '+GetAtom+' found')
        else exit;
      ReadNextAtom;
      if UpAtomIs('CONST') then begin
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
        Result:=true;
        exit;
      end;
    end;
    if not AtomIsIdentifier(ExceptionOnError) then exit;
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
  end else begin
    if ExceptionOnError then
      RaiseException(
        'syntax error: identifier expected, but '+GetAtom+' found')
    else exit;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadTilProcedureHeadEnd(
  IsMethod, IsFunction, IsType: boolean;
  var HasForwardModifier: boolean): boolean;
{ parse parameter list, result type, of object, method specifiers

  IsMethod: true if parsing in a class/object
  IsFunction: 'function'
  IsType: parsing type definition. e.g. 'Event: procedure of object'


 examples:
   procedure ProcName;  virtual; abstract;
   function FuncName(Parameter1: Type1; Parameter2: Type2): ResultType;
   constructor Create;
   destructor Destroy;  override;
   class function X: integer;

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>
   external <id or number> name <id>
}
var IsSpecifier: boolean;
begin
//writeln('[TPascalParserTool.ReadTilProcedureHeadEnd] ',
//'Method=',IsMethod,', Function=',IsFunction,', Type=',IsType);
  Result:=true;
  HasForwardModifier:=false;
  if AtomIsChar('(') then
    ReadParamList(true,false,[]);
  if IsFunction then begin
    // read function result type
    if not AtomIsChar(':') then
      RaiseException('syntax error: : expected, but '+GetAtom+' found');
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen)
    or (not (UpperSrc[CurPos.StartPos] in ['A'..'Z','_']))
    then
      RaiseException(
        'syntax error: method result type expected but '+GetAtom+' found');
    ReadNextAtom;
  end;
  if UpAtomIs('OF') then begin
    // read 'of object'
    if not IsType then
      RaiseException(
        'syntax error: expected ;, but '+GetAtom+' found');
    ReadNextAtom;
    if not UpAtomIs('OBJECT') then
      RaiseException('syntax error: "object" expected, but '+GetAtom+' found');
    ReadNextAtom;
  end;
  // read procedures/method specifiers
  if UpAtomIs('END') then begin
    UndoReadNextAtom;
    exit;
  end;
  if not AtomIsChar(';') then
    RaiseException('syntax error: ; expected, but '+GetAtom+' found');
  if (CurPos.StartPos>SrcLen) then
    RaiseException('syntax error: semicolon not found');
  repeat
    ReadNextAtom;
    if IsMethod then
      IsSpecifier:=IsKeyWordMethodSpecifier.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    else
      IsSpecifier:=IsKeyWordProcedureSpecifier.DoItUppercase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
    if IsSpecifier then begin
      // read specifier
      if UpAtomIs('MESSAGE') or UpAtomIs('EXTERNAL') then begin
        repeat
          ReadNextAtom;
          if UpAtomIs('END') then begin
            UndoReadNextAtom;
            exit;
          end;
        until (CurPos.Startpos>SrcLen) or AtomIsChar(';');
      end else begin
        if UpAtomIs('FORWARD') then HasForwardModifier:=true;
        ReadNextAtom;
        if UpAtomIs('END') then begin
          UndoReadNextAtom;
          exit;
        end;
      end;
      if not AtomIsChar(';') then
        RaiseException('syntax error: ; expected, but '+GetAtom+' found');
    end else begin
      // current atom does not belong to procedure/method declaration
      UndoReadNextAtom;
      UndoReadNextAtom;
      break;
    end;
  until false;
end;

function TPascalParserTool.ReadConstant(ExceptionOnError, Extract: boolean;
  Attr: TProcHeadAttributes): boolean;
var c: char;
begin
  Result:=false;
  if AtomIsWord then begin
    // identifier
    if AtomIsKeyWord then
      if ExceptionOnError then
        RaiseException('syntax error: unexpected keyword '+GetAtom+' found')
      else exit;
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    if WordIsTermOperator.DoItUpperCase(UpperSrc,
         CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // identifier + operator + ?
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      Result:=ReadConstant(ExceptionOnError,Extract,Attr);
      exit;
    end else if AtomIsChar('(') or AtomIsChar('[') then begin
      // type cast or constant array
      c:=Src[CurPos.StartPos];
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
      if (c='(') and (not AtomIsChar(')')) then
        if ExceptionOnError then
          RaiseException('syntax error: ( expected, but '+GetAtom+' found')
        else exit;
      if (c='[') and (not AtomIsChar(']')) then
        if ExceptionOnError then
          RaiseException('syntax error: [ expected, but '+GetAtom+' found')
        else exit;
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    end;
  end else if AtomIsNumber or AtomIsStringConstant then begin
    // number or '...' or #...
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    if WordIsTermOperator.DoItUpperCase(UpperSrc,
         CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // number + operator + ?
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      Result:=ReadConstant(ExceptionOnError,Extract,Attr);
      exit;
    end;
  end else begin
    if CurPos.EndPos-CurPos.StartPos=1 then begin
      c:=Src[CurPos.StartPos];
      case c of
        '(','[':
          begin
            // open bracket + ? + close bracket
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
            if (c='(') and (not AtomIsChar(')')) then
              if ExceptionOnError then
                RaiseException(
                  'syntax error: ( expected, but '+GetAtom+' found')
              else exit;
            if (c='[') and (not AtomIsChar(']')) then
              if ExceptionOnError then
                RaiseException(
                  'syntax error: [ expected, but '+GetAtom+' found')
              else exit;
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if WordIsTermOperator.DoItUpperCase(UpperSrc,
                 CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
            then begin
              // open bracket + ? + close bracket + operator + ?
              if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
              Result:=ReadConstant(ExceptionOnError,Extract,Attr);
              exit;
            end;
          end;
        '+','-':
          begin
            // sign
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
            if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
          end;
      else
        if ExceptionOnError then
          RaiseException(
            'syntax error: constant expected, but '+GetAtom+' found')
        else exit;
      end;
    end else
      // syntax error
      if ExceptionOnError then
        RaiseException(
          'syntax error: constant expected, but '+GetAtom+' found')
      else exit;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadUsesSection(
  ExceptionOnError: boolean): boolean;
{ parse uses section

  examples:
    uses name1, name2 in '', name3;

}
begin
  CreateChildNode;
  CurNode.Desc:=ctnUsesSection;
  repeat
    ReadNextAtom;  // read name
    if AtomIsChar(';') then break;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        if ExceptionOnError then
          RaiseException(
            'syntax error: string constant expected, but '+GetAtom+' found')
        else exit;
      ReadNextAtom;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then
      if ExceptionOnError then
        RaiseException(
          'syntax error: ; expected, but '+GetAtom+' found')
      else exit;
  until (CurPos.StartPos>SrcLen);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.ReadSubRange(ExceptionOnError: boolean): boolean;
{ parse subrange till ',' ';' ':' ']' or ')'

  examples:
    number..number
    identifier
    Low(identifier)..High(identifier)
    Pred(identifier)..Succ(identfier)
}
var RangeOpFound: boolean;
begin
  RangeOpFound:=false;
  repeat
    if AtomIsChar(';') or AtomIsChar(')') or AtomIsChar(']') or AtomIsChar(',')
    or AtomIsChar(':') then break;
    if AtomIs('..') then begin
      if RangeOpFound then
        RaiseException('syntax error: ; expected, but '+GetAtom+' found');
      RangeOpFound:=true;
    end else if AtomIsChar('(') or AtomIsChar('[') then
      ReadTilBracketClose(ExceptionOnError);
    ReadNextAtom;
  until false;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassProperty: boolean;
{ parse class/object property

 examples:
   property Visible;
   property Count: integer;
   property Color: TColor read FColor write SetColor;
   property Items[Index1, Index2: integer]: integer read GetItems; default;
   property X: integer index 1 read GetCoords write SetCoords stored IsStored;
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8;

 property specifiers without parameters:
   default, nodefault

 property specifiers with parameters:
   index <id or number>, read <id>, write <id>, implements <id>, stored <id>
}
begin
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProperty;
  // read property Name
  ReadNextAtom;
  AtomIsIdentifier(true);
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  while (CurPos.StartPos<=SrcLen) and (not AtomIsChar(';')) do
    ReadNextAtom;
  ReadNextAtom;
  if UpAtomIs('DEFAULT') then begin
    if not ReadNextAtomIsChar(';') then
      RaiseException('syntax error: ; expected after "default" property '
          +'specifier, but '+GetAtom+' found');
  end else if UpAtomIs('NODEFAULT') then begin
    if not ReadNextAtomIsChar(';') then
      RaiseException('syntax error: ; expected after "nodefault" property '
          +'specifier, but '+GetAtom+' found');
  end else
    UndoReadNextAtom;
  // close property
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.DoAtom: boolean;
begin
//writeln('[TPascalParserTool.DoAtom] A ',HexStr(Cardinal(CurKeyWordFuncList),8));
  if (CurPos.StartPos>SrcLen) or (CurPos.EndPos<=CurPos.StartPos) then
    Result:=false
  else if IsIdentStartChar[Src[CurPos.StartPos]] then
    Result:=CurKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
                                    CurPos.EndPos-CurPos.StartPos)
  else begin
    if Src[CurPos.StartPos] in ['(','['] then
      ReadTilBracketClose(true);
    Result:=true;
  end;
end;

function TPascalParserTool.KeyWordFuncSection: boolean;
// parse section keywords (program, unit, interface, implementation, ...)
begin
  case CurSection of
   ctnInterface, ctnProgram, ctnPackage, ctnLibrary, ctnUnit:
    begin
      if (UpAtomIs('INTERFACE')) and (LastAtomIs(1,'=')) then begin
        Result:=KeyWordFuncClass();
        exit;
      end;
      if not ((CurSection=ctnInterface) and UpAtomIs('IMPLEMENTATION')) then
        RaiseException('syntax error: unexpected keyword '+GetAtom+' found');
      // close interface section node
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
      ImplementationSectionFound:=true;
      // start implementation section node
      CreateChildNode;
      CurNode.Desc:=ctnImplementation;
      CurSection:=ctnImplementation;
      ReadNextAtom;
      if UpAtomIs('USES') then
        ReadUsesSection(true);
      UndoReadNextAtom;
      Result:=true;
    end;
   ctnImplementation:
    begin
      if not (UpAtomIs('INITIALIZATION') or UpAtomIs('FINALIZATION')) then
        RaiseException('syntax error: unexpected keyword '+GetAtom+' found');
      // close implementation section node
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
      // start initialization / finalization section node
      CreateChildNode;
      if UpAtomIs('INITIALIZATION') then begin
        CurNode.Desc:=ctnInitialization;
      end else
        CurNode.Desc:=ctnFinalization;
      CurSection:=CurNode.Desc;
      repeat
        ReadNextAtom;
        if (CurSection=ctnInitialization) and UpAtomIs('FINALIZATION') then
        begin
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          CreateChildNode;
          CurNode.Desc:=ctnFinalization;
          CurSection:=CurNode.Desc;
        end else if UpAtomIs('END') then begin
          Result:=KeyWordFuncEnd;
          break;
        end;
      until (CurPos.StartPos>SrcLen);
      Result:=true;
    end;
  else
    begin
      RaiseException('syntax error: unexpected keyword '+GetAtom+' found');
      Result:=false;
    end;
  end;
end;

function TPascalParserTool.KeyWordFuncEnd: boolean;
// end  (parse end of block, e.g. begin..end)
begin
  if CurNode.Desc in [ctnImplementation,ctnInterface] then
    CurNode.EndPos:=CurPos.StartPos
  else
    CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  if AtomIsChar('.') then
    CurSection:=ctnNone
  else
    UndoReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncMethod: boolean;
// procedure, function, constructor, destructor, operator
var ChildCreated: boolean;
  IsFunction, HasForwardModifier, IsClassProc: boolean;
  ProcNode: TCodeTreeNode;
begin
  if UpAtomIs('CLASS') then begin
    if CurSection<>ctnImplementation then
      RaiseException(
        'syntax error: identifier expected, but '+GetAtom+' found');
    ReadNextAtom;
    if UpAtomIs('PROCEDURE') or UpAtomIs('FUNCTION') then
     IsClassProc:=true
    else
      RaiseException(
        'syntax error: "procedure" expected, but '+GetAtom+' found');
  end else
    IsClassProc:=false;
  ChildCreated:=true;
  if ChildCreated then begin
    // create node for procedure
    CreateChildNode;
    if IsClassProc then
      CurNode.StartPos:=LastAtoms.GetValueAt(0).StartPos;
    ProcNode:=CurNode;
    ProcNode.Desc:=ctnProcedure;
    if CurSection=ctnInterface then
      ProcNode.SubDesc:=ctnsForwardDeclaration;
  end;
  IsFunction:=UpAtomIs('FUNCTION');
  ReadNextAtom;// read first atom of head (= name + parameterlist + resulttype;)
  AtomIsIdentifier(true);
  if ChildCreated then begin
    // create node for procedure head
    CreateChildNode;
    CurNode.Desc:=ctnProcedureHead;
  end;
  ReadNextAtom;
  if (CurSection<>ctnInterface) and (AtomIsChar('.')) then begin
    // read procedure name of a class method (the name after the . )
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  // read rest of procedure head
  HasForwardModifier:=false;
  ReadTilProcedureHeadEnd(false,IsFunction,false,HasForwardModifier);
  if ChildCreated then begin
    if HasForwardModifier then
      ProcNode.SubDesc:=ctnsForwardDeclaration;
    // close head
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if ChildCreated and (ProcNode.SubDesc=ctnsForwardDeclaration) then begin
    // close method
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncBeginEnd: boolean;
// Keyword: begin, asm
var BeginKeyWord: shortstring;
  ChildNodeCreated: boolean;
  Level: integer;
begin
  BeginKeyWord:=GetUpAtom;
  ChildNodeCreated:=(BeginKeyWord='BEGIN') or (BeginKeyWord='ASM');
  if ChildNodeCreated then begin
    CreateChildNode;
    if BeginKeyWord='BEGIN' then
      CurNode.Desc:=ctnBeginBlock
    else
      CurNode.Desc:=ctnAsmBlock;
  end;
  // search "end"
  Level:=1;
  repeat
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) then begin
      RaiseException('syntax error: "end" not found.')
    end else if EndKeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then begin
      inc(Level);
    end else if (UpAtomIs('END')) then begin
      dec(Level);
    end;
  until Level<=0;
  // close node
  if ChildNodeCreated then begin
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if (CurSection<>ctnInterface)
  and (CurNode<>nil) and (CurNode.Desc=ctnProcedure) then begin
    // close procedure
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end else if (CurNode.Desc in [ctnProgram]) then begin
    ReadNextAtom;
    if not AtomIsChar('.') then
      RaiseException('syntax error: missing . after program end');
    // close program
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
    CurSection:=ctnNone;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncType: boolean;
{ The 'type' keyword is the start of a type section.
  examples:

    interface
      type  a=b;
      
    implementation
    
    procedure c;
    type d=e;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseException('syntax error: unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnTypeSection;
  // read all type definitions  Name = Type;
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnTypeDefinition;
      if not ReadNextAtomIsChar('=') then
        RaiseException('syntax error: = expected, but '+GetAtom+' found');
      // read type
      ReadNextAtom;
      TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
      // read ;
      if not AtomIsChar(';') then
        RaiseException('syntax error: ; expected, but '+GetAtom+' found');
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncVar: boolean;
{
  examples:

    interface
      var a:b;
        a:b; cvar;

    implementation

    procedure c;
    var d:e;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseException('syntax error: unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnVarSection;
  // read all variable definitions  Name : Type; [cvar;]
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      CurNode.EndPos:=CurPos.EndPos;
      ReadNextAtom;
      while AtomIsChar(',') do begin
        EndChildNode; // close variable definition
        ReadNextAtom;
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end;
      if not AtomIsChar(':') then
        RaiseException('syntax error: : expected, but '+GetAtom+' found');
      // read type
      ReadNextAtom;
      TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
      // read ;
      if not AtomIsChar(';') then
        RaiseException('syntax error: ; expected, but '+GetAtom+' found');
      if not ReadNextUpAtomIs('CVAR') then
        UndoReadNextAtom
      else
        if not ReadNextAtomIsChar(';') then
          RaiseException('syntax error: ; expected, but '+GetAtom+' found');
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncConst: boolean;
{
  examples:

    interface
      const a:b=3;

    implementation

    procedure c;
    const d=2;
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseException('syntax error: unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnConstSection;
  // read all constants  Name = <Const>; or Name : type = <Const>;
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      ReadNextAtom;
      if AtomIsChar(':') then begin
        // read type
        ReadNextAtom;
        TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
      end;
      if not AtomIsChar('=') then
        RaiseException('syntax error: = expected, but '+GetAtom+' found');
      // read constant
      repeat
        ReadNextAtom;
        if AtomIsChar('(') or AtomIsChar('[') then
          ReadTilBracketClose(true);
        if AtomIsWord and (not IsKeyWordInConstAllowed.DoItUppercase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
        and (UpAtomIs('END') or AtomIsKeyWord) then
          RaiseException('syntax error: ; expected, but '+GetAtom+' found');
      until AtomIsChar(';');
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncResourceString: boolean;
{
  examples:

    interface
      ResourceString a='';

    implementation

    procedure c;
    ResourceString b='';
}
begin
  if not (CurSection in [ctnProgram,ctnInterface,ctnImplementation]) then
    RaiseException('syntax error: unexpected keyword '+GetAtom);
  CreateChildNode;
  CurNode.Desc:=ctnResStrSection;
  // read all string constants Name = 'abc';
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      if not ReadNextAtomIsChar('=') then
        RaiseException('syntax error: = expected, but '+GetAtom+' found');
      // read string constant
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseException(
          'syntax error: string constant expected, but '+GetAtom+' found');
      // read ;
      if not ReadNextAtomIsChar(';') then
        RaiseException('syntax error: ; expected, but '+GetAtom+' found');
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypePacked: boolean;
begin
  ReadNextAtom;
  if not PackedTypesKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos) then
    RaiseException('syntax error: ''record'' expected, but '+GetAtom+' found');
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncClass: boolean;
// class, object, interface (type, not section), dispinterface
//   this is a quick parser, which will only create one node for each class
//   the nodes for the methods and properties are created in a second
//   parsing phase (in KeyWordFuncClassMethod)
var
  ChildCreated: boolean;
  ClassAtomPos: TAtomPosition;
  Level: integer;
begin
  if CurNode.Desc<>ctnTypeDefinition then
    RaiseException('syntax error: anonym classes are forbidden');
  if (LastUpAtomIs(0,'PACKED')) then begin
    if not LastAtomIs(1,'=') then
      RaiseException('syntax error: anonym classes are not allowed');
    ClassAtomPos:=LastAtoms.GetValueAt(1);
  end else begin
    if not LastAtomIs(0,'=') then
      RaiseException('syntax error: anonym classes are not allowed');
    ClassAtomPos:=CurPos;
  end;
  // class start found
  ChildCreated:=(UpAtomIs('CLASS')) or (UpAtomIs('OBJECT'));
  if ChildCreated then begin
    CreateChildNode;
    CurNode.Desc:=ctnClass;
    CurNode.StartPos:=ClassAtomPos.StartPos;
  end;
  // find end of class
  ReadNextAtom;
  if UpAtomIs('OF') then begin
    ReadNextAtom;
    AtomIsIdentifier(true);
    if not ReadNextAtomIsChar(';') then
      RaiseException('syntax error: ; expected, but '+GetAtom+' found');
    if ChildCreated then CurNode.Desc:=ctnClassOfType;
  end else if AtomIsChar('(') then begin
    // read inheritage brackets
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if AtomIsChar(';') then begin
    if ChildCreated and (CurNode.Desc=ctnClass) then begin
      // forward class definition found
      CurNode.SubDesc:=ctnsForwardDeclaration;
    end;
  end else begin
    Level:=1;
    while (CurPos.StartPos<=SrcLen) do begin
      if UpAtomIs('END') then begin
        dec(Level);
        if Level=0 then break;
      end else if UpAtomIs('RECORD') then inc(Level);
      ReadNextAtom;
    end;
    if (CurPos.StartPos>SrcLen) then
      RaiseException('syntax error: "end" for class/object not found');
  end;
  if ChildCreated then begin
    // close class
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if UpAtomIs('END') then ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeArray: boolean;
{
  examples:
    array of ...
    array[SubRange] of ...
    array[SubRange,SubRange,...] of ...
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnArrayType;
  if ReadNextAtomIsChar('[') then begin
    repeat
      ReadNextAtom;
      CreateChildNode;
      CurNode.Desc:=ctnRangeType;
      ReadSubRange(true);
      CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
      EndChildNode;
      if AtomIsChar(']') then break;
      if not AtomIsChar(',') then
        RaiseException('syntax error: ] expected, but '+GetAtom+' found');
    until false;
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseException('syntax error: ''of'' expected, but '+GetAtom+' found');
  ReadNextAtom;
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeProc: boolean;
{
  examples:
    procedure;
    procedure of object;
    procedure(ParmList) of object;
    function(ParmList):SimpleType of object;
    procedure; cdecl; popstack; register; pascal; stdcall;
}
var IsFunction: boolean;
begin
  IsFunction:=UpAtomIs('FUNCTION');
  CreateChildNode;
  CurNode.Desc:=ctnProcedure;
  ReadNextAtom;
  if AtomIsChar('(') then begin
    // read parameter list
    ReadParamList(true,false,[]);
  end;
  if IsFunction then begin
    if not AtomIsChar(':') then
      RaiseException('syntax error: : expected, but '+GetAtom+' found');
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  if UpAtomIs('OF') then begin
    if not ReadNextUpAtomIs('OBJECT') then
      RaiseException(
        'syntax error: ''object'' expected, but '+GetAtom+' found');
    ReadNextAtom;
  end;
  if not AtomIsChar(';') then
    RaiseException('syntax error: ; expected, but '+GetAtom+' found');
  // read modifiers
  repeat
    ReadNextAtom;
    if not IsKeyWordProcedureTypeSpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then begin
      UndoReadNextAtom;
      break;
    end else begin
      if not ReadNextAtomIsChar(';') then
        RaiseException('syntax error: ; expected, but '+GetAtom+' found');
    end;
  until false;
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeSet: boolean;
{
  examples:
    set of Identifier;
    set of SubRange;
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnSetType;
  if not ReadNextUpAtomIs('OF') then
    RaiseException('syntax error: ''of'' expected, but '+GetAtom+' found');
  ReadNextAtom;
  Result:=KeyWordFuncTypeDefault;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeLabel: boolean;
// 'label;'
begin
  CreateChildNode;
  CurNode.Desc:=ctnLabelType;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeType: boolean;
// 'type identifier'
begin
  if not LastAtomIs(0,'=') then
    RaiseException('syntax error: identfier expected, but ''type'' found');
  CreateChildNode;
  CurNode.Desc:=ctnTypeType;
  ReadNextAtom;
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncTypeFile: boolean;
// 'file' or 'file of <type>'
begin
  CreateChildNode;
  CurNode.Desc:=ctnFileType;
  if ReadNextUpAtomIs('OF') then begin
    ReadNextAtom;
    Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
    if not Result then exit;
  end;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypePointer: boolean;
// '^Identfier'
begin
  if not (LastAtomIs(0,'=') or LastAtomIs(0,':')) then
    RaiseException('syntax error: identifier expected, but ^ found');
  CreateChildNode;
  CurNode.Desc:=ctnPointerType;
  ReadNextAtom;
  Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncTypeDefault: boolean;
{ check for enumeration, subrange and identifier types

  examples:
    integer
    1..3
    (a,b:=3,c)
    (a)..4
    Low(integer)..High(integer)
    'a'..'z'
}
var SubRangeOperatorFound: boolean;

  procedure ReadTillTypeEnd;
  begin
    // read till ';', ':', ')', '=', 'end'
    while (CurPos.StartPos<=SrcLen)
    and (not (Src[CurPos.StartPos] in [';',':',')',']','=']))
    and (not AtomIsKeyWord) do begin
      if AtomIsChar('(') or AtomIsChar('[') then
        ReadTilBracketClose(true)
      else if AtomIs('..') then begin
        if SubRangeOperatorFound then
          RaiseException(
            'syntax error: unexpected subrange operator ''..'' found');
        SubRangeOperatorFound:=true;
      end;
      ReadNextAtom;
    end;
  end;

// TPascalParserTool.KeyWordFuncTypeDefault: boolean
begin
  CreateChildNode;
  SubRangeOperatorFound:=false;
  if AtomIsIdentifier(false) then begin
    ReadNextAtom;
    if not AtomIs('..') then begin
      // an identifier
      CurNode.Desc:=ctnIdentifier;
      CurNode.EndPos:=CurPos.StartPos;
    end else begin
      // a subrange
      CurNode.Desc:=ctnRangeType;
      ReadTillTypeEnd;
      if not SubRangeOperatorFound then
        RaiseException('syntax error: invalid subrange');
      CurNode.EndPos:=CurPos.StartPos;
    end;
  end else begin
    // enum or subrange
    ReadTillTypeEnd;
    if SubRangeOperatorFound then begin
      // a subrange
      CurNode.Desc:=ctnRangeType;
      CurNode.EndPos:=CurPos.StartPos;
    end else begin
      MoveCursorToNodeStart(CurNode);
      ReadNextAtom;
      if AtomIsChar('(') then begin
        // an enumeration -> read all enums
        repeat
          ReadNextAtom; // read enum name
          if AtomIsChar(')') then break;
          CreateChildNode;
          CurNode.Desc:=ctnEnumType;
          CurNode.EndPos:=CurPos.EndPos;
          AtomIsIdentifier(true);
          ReadNextAtom;
          if AtomIs(':=') then begin
            // read ordinal value
            repeat
              ReadNextAtom;
              if AtomIsChar('(') or AtomIsChar('[') then
                ReadTilBracketClose(true)
              else if AtomIsChar(')') or AtomIsChar(',') then
                break
              else if AtomIsKeyWord then
                RaiseException(
                  'syntax error: unexpected keyword '+GetAtom+' found');
            until CurPos.StartPos>SrcLen;
            CurNode.EndPos:=CurPos.StartPos;
          end;
          EndChildNode; // close enum node
          if AtomIsChar(')') then break;
          if not AtomIsChar(',') then
            RaiseException('syntax error: ) expected, but '+GetAtom+' found');
        until false;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end else
        RaiseException('syntax error: invalid type');
    end;
  end;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeRecord: boolean;
{ read variable type 'record'

  examples:
    record
      i: packed record
           j: integer;
           k: record end;
           case integer of
             0: (a: integer);
             1,2,3: (b: array[char] of char; c: char);
             3: ( d: record
                       case byte of
                         10: (i: integer; );
                         11: (y: byte);
                     end; );
         end;
    end;
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnRecordType;
  if LastUpAtomIs(0,'PACKED') then
    CurNode.StartPos:=LastAtoms.GetValueAt(0).StartPos;
  // read all variables
  repeat
    ReadNextAtom;
    if UpAtomIs('END') then break;
    if UpAtomIs('CASE') then begin
      CreateChildNode;
      CurNode.Desc:=ctnRecordCase;
      ReadNextAtom; // read ordinal type
      AtomIsIdentifier(true);
      if not ReadNextUpAtomIs('OF') then // read 'of'
        RaiseException('syntax error: ''of'' expected, but '+GetAtom+' found');
      // read all variants
      repeat
        ReadNextAtom;  // read constant (variant identifier)
        if UpAtomIs('END') then break;
        CreateChildNode;
        CurNode.Desc:=ctnRecordVariant;
        repeat
          ReadNextAtom;  // read till ':'
          if AtomIsChar(':') then break
          else if AtomIsChar('(') or AtomIsChar('[') then
            ReadTilBracketClose(true)
          else if UpAtomIs('END') or AtomIsChar(')') or AtomIsKeyWord then
            RaiseException('syntax error: : expected, but '+GetAtom+' found');
        until false;
        ReadNextAtom;  // read '('
        if not AtomIsChar('(') then
          RaiseException('syntax error: ( expected, but '+GetAtom+' found');
        // read all variables
        ReadNextAtom; // read first variable name
        repeat
          if AtomIsChar(')') then break;
          repeat
            AtomIsIdentifier(true);
            CreateChildNode;
            CurNode.Desc:=ctnVarDefinition;
            CurNode.EndPos:=CurPos.EndPos;
            ReadNextAtom;
            if AtomIsChar(':') then break;
            if not AtomIsChar(',') then
              RaiseException(
                'syntax error: '','' expected, but '+GetAtom+' found');
            EndChildNode;
            ReadNextAtom; // read next variable name
          until false;
          ReadNextAtom; // read type
          Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos);
          if not Result then exit;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode; // close variable definition
          if AtomIsChar(')') then break;
          if not AtomIsChar(';') then
            RaiseException('syntax error: ; expected, but '+GetAtom+' found');
          ReadNextAtom;
        until false;
        ReadNextAtom;
        if UpAtomIs('END') then begin
          CurNode.EndPos:=CurPos.StartPos;
          EndChildNode; // close variant
          break;
        end;
        if not AtomIsChar(';') then
          RaiseException('syntax error: ; expected, but '+GetAtom+' found');
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode; // close variant
      until false;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode; // close case
      break;
    end else begin
      // read variable names
      repeat
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
        if AtomIsChar(':') then break;
        if not AtomIsChar(',') then
          RaiseException('syntax error: : expected, but '+GetAtom+' found');
        EndChildNode; // close variable
        ReadNextAtom; // read next variable name
      until false;
      ReadNextAtom;
      Result:=TypeKeyWordFuncList.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos);
      if not Result then exit;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode; // close variable
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode; // close record
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.ExtractPropName(PropNode: TCodeTreeNode;
  InUpperCase: boolean): string;
begin
  Result:='';
  if (PropNode=nil) or (PropNode.Desc<>ctnProperty) then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if not UpAtomIs('PROPERTY') then exit;
  ReadNextAtom;
  AtomIsIdentifier(true);
  if InUpperCase then
    Result:=copy(UpperSrc,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
  else
    Result:=copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.ExtractProcName(ProcNode: TCodeTreeNode;
  InUpperCase: boolean): string;
var ProcHeadNode: TCodeTreeNode;
begin
  Result:='';
  while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then exit;
  ProcHeadNode:=ProcNode.FirstChild;
  while (ProcHeadNode<>nil) and (ProcHeadNode.Desc<>ctnProcedureHead) do
    ProcHeadNode:=ProcHeadNode.NextBrother;
  if (ProcHeadNode=nil) or (ProcHeadNode.StartPos<1) then exit;
  MoveCursorToNodeStart(ProcHeadNode);
  repeat
    ReadNextAtom;
    if (CurPos.StartPos<=SrcLen)
    and (UpperSrc[CurPos.StartPos] in ['.','_','A'..'Z']) then begin
      if InUpperCase then
        Result:=Result+GetUpAtom
      else
        Result:=Result+GetAtom;
    end else
      break;
  until false;
end;

procedure TPascalParserTool.InitExtraction;
begin
  if ExtractMemStream=nil then
    ExtractMemStream:=TMemoryStream.Create;
  ExtractMemStream.Position:=0;
  ExtractMemStream.Size:=0;
end;

function TPascalParserTool.GetExtraction: string;
begin
  SetLength(Result,ExtractMemStream.Size);
  ExtractMemStream.Position:=0;
  ExtractMemStream.Read(Result[1],length(Result));
end;

procedure TPascalParserTool.ExtractNextAtom(AddAtom: boolean;
  Attr: TProcHeadAttributes);
// add current atom and text before, then read next atom
// if not phpWithComments in Attr then the text before will be shortened
var LastAtomEndPos, LastStreamPos: integer;
begin
  LastStreamPos:=ExtractMemStream.Position;
  if LastAtoms.Count>0 then begin
    LastAtomEndPos:=LastAtoms.GetValueAt(0).EndPos;
    if phpWithComments in Attr then begin
      if phpInUpperCase in Attr then
        ExtractMemStream.Write(UpperSrc[LastAtomEndPos],
             CurPos.StartPos-LastAtomEndPos)
      else
        ExtractMemStream.Write(Src[LastAtomEndPos],
             CurPos.StartPos-LastAtomEndPos)
    end else if (CurPos.StartPos>LastAtomEndPos) 
    and (ExtractMemStream.Position>0) then begin
      ExtractMemStream.Write(' ',1);
    end;
  end;
  if AddAtom then begin
    if phpInUpperCase in Attr then
      ExtractMemStream.Write(UpperSrc[CurPos.StartPos],
          CurPos.EndPos-CurPos.StartPos)
    else
      ExtractMemStream.Write(Src[CurPos.StartPos],
          CurPos.EndPos-CurPos.StartPos);
  end;
  if (ExtractSearchPos>0)
  and (ExtractSearchPos<=ExtractMemStream.Position)
  then begin
    ExtractFoundPos:=ExtractSearchPos-1-LastStreamPos+CurPos.StartPos;
    ExtractSearchPos:=-1;
  end;
  ReadNextAtom;
end;

function TPascalParserTool.ExtractProcHead(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  GrandPaNode: TCodeTreeNode;
  TheClassName, s: string;
  HasClassName: boolean;
// function TPascalParserTool.ExtractProcHead(ProcNode: TCodeTreeNode;
//   Attr: TProcHeadAttributes): string;
begin
  Result:='';
  if (ProcNode=nil) or (ProcNode.StartPos<1) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then exit;
  if ProcNode.Desc<>ctnProcedure then exit;
  if phpAddClassname in Attr then begin
    GrandPaNode:=ProcNode.Parent;
    if GrandPaNode=nil then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if (GrandPaNode=nil) or (GrandPaNode.Desc<>ctnClass) then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if GrandPaNode.Desc<>ctnTypeDefinition then exit;
    CurPos.StartPos:=GrandPaNode.StartPos;
    CurPos.EndPos:=CurPos.StartPos;
    ReadNextAtom;
    if not AtomIsWord then exit;
    TheClassName:=GetAtom;
  end;
  InitExtraction;
  // reparse the clean source
  MoveCursorToNodeStart(ProcNode);
  // parse procedure head = start + name + parameterlist + result type ;
  ExtractNextAtom(false,Attr);
  // read procedure start keyword
  if (UpAtomIs('CLASS') or UpAtomIs('STATIC')) then
    ExtractNextAtom(phpWithStart in Attr,Attr);
  if (UpAtomIs('PROCEDURE')) or (UpAtomIs('FUNCTION'))
  or (UpAtomIs('CONSTRUCTOR')) or (UpAtomIs('DESTRUCTOR'))
  or (UpAtomIs('OPERATOR')) then
    ExtractNextAtom(phpWithStart in Attr,Attr)
  else
    exit;
  // read name
  if (not AtomIsWord) or AtomIsKeyWord then exit;
  ReadNextAtom;
  HasClassName:=AtomIsChar('.');
  UndoReadNextAtom;
  if HasClassName then begin
    // read class name
    ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
    // read '.'
    ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
    // read name
    if (not AtomIsWord) or AtomIsKeyWord then exit;
    ExtractNextAtom(not (phpWithoutName in Attr),Attr);
  end else begin
    // read name
    if not (phpAddClassname in Attr) then begin
      ExtractNextAtom(not (phpWithoutName in Attr),Attr);
    end else begin
      // add class name
      s:=TheClassName+'.';
      if not (phpWithoutName in Attr) then
        s:=s+GetAtom;
      if phpInUpperCase in Attr then s:=UpperCaseStr(s);
      ExtractNextAtom(false,Attr);
      ExtractMemStream.Write(s[1],length(s));
    end;
  end;
  // read parameter list
  if AtomIsChar('(') then
    ReadParamList(false,true,Attr);
  // read result type
  while not AtomIsChar(';') do
    ExtractNextAtom(phpWithResultType in Attr,Attr);
  if AtomIsChar(';') then
    ExtractNextAtom(true,Attr);
  
  // copy memorystream to Result string
  Result:=GetExtraction;
end;

function TPascalParserTool.ExtractClassName(ClassNode: TCodeTreeNode;
  InUpperCase: boolean): string;
var Len: integer;
begin
  if ClassNode<>nil then begin
    if ClassNode.Desc=ctnClass then begin
      ClassNode:=ClassNode.Parent;
      if ClassNode=nil then begin
        Result:='';
        exit;
      end;
    end;
    Len:=1;
    while (ClassNode.StartPos+Len<=SrcLen)
    and (IsIdentChar[Src[ClassNode.StartPos+Len]]) do
      inc(Len);
    if InUpperCase then
      Result:=copy(UpperSrc,ClassNode.StartPos,Len)
    else
      Result:=copy(Src,ClassNode.StartPos,Len);
  end else
    Result:='';
end;

function TPascalParserTool.FindProcNode(StartNode: TCodeTreeNode;
  const ProcName: string; Attr: TProcHeadAttributes): TCodeTreeNode;
// search in all next brothers for a Procedure Node with the Name ProcName
// if there are no further brothers and the parent is a section node
// ( e.g. 'interface', 'implementation', ...) or a class visibility node
// (e.g. 'public', 'private', ...) then the search will continue in the next
// section
var CurProcName: string;
begin
  Result:=StartNode;
  while (Result<>nil) do begin
//writeln('TPascalParserTool.FindProcNode A "',NodeDescriptionAsString(Result.Desc),'"');
    if Result.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
               and (Result.SubDesc=ctnsForwardDeclaration)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(Result)<>nil))) then begin
        CurProcName:=ExtractProcHead(Result,Attr);
//writeln('TPascalParserTool.FindProcNode B "',CurProcName,'" =? "',ProcName,'"');
        if (CurProcName<>'')
        and (CompareTextIgnoringSpace(CurProcName,ProcName,false)=0) then
          exit;
      end;
    end;
    // next node
    Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalParserTool.FindProcBody(
  ProcNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=ProcNode;
  if Result=nil then exit;
  Result:=Result.FirstChild;
  while Result<>nil do begin
    if Result.Desc in [ctnBeginBlock,ctnAsmBlock] then
      exit;
    Result:=Result.NextBrother;
  end;
end;

function TPascalParserTool.FindVarNode(StartNode: TCodeTreeNode;
  const UpperVarName: string): TCodeTreeNode;
begin
  Result:=StartNode;
  while Result<>nil do begin
    if (Result.Desc=ctnVarDefinition)
    and (CompareNodeUpSrc(Result,UpperVarName)=0) then
      exit;
    Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalParserTool.ExtractClassNameOfProcNode(
  ProcNode: TCodeTreeNode): string;
var TheClassName: string;
begin
  Result:='';
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if not AtomIsWord then exit;
  TheClassName:=GetAtom;
  ReadNextAtom;
  if not AtomIsChar('.') then exit;
  ReadNextAtom;
  if not AtomIsWord then exit;
  Result:=TheClassName;
end;

function TPascalParserTool.FindFirstNodeOnSameLvl(
  StartNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=StartNode;
  if Result=nil then exit;
  Result:=Result.Parent;
  if Result=nil then exit;
  while (Result.Desc in AllCodeSections) and (Result.PriorBrother<>nil) do
    Result:=Result.PriorBrother;
  while (Result<>nil) and (Result.FirstChild=nil) do
    Result:=Result.NextBrother;
  Result:=Result.FirstChild;
end;

function TPascalParserTool.FindNextNodeOnSameLvl(
  StartNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=StartNode;
  if Result=nil then exit;
  if Result.NextBrother<>nil then
    Result:=Result.NextBrother
  else begin
    Result:=Result.Parent;
    if Result=nil then exit;
    Result:=Result.NextBrother;
    while (Result<>nil) and (Result.FirstChild=nil) do
      Result:=Result.NextBrother;
    if Result=nil then exit;
    Result:=Result.FirstChild;
  end;
end;

function TPascalParserTool.FindClassNode(StartNode: TCodeTreeNode;
  const UpperClassName: string;
  IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
// search for types on same level,
// with type class and classname = SearchedClassName
var CurClassName: string;
  ANode, CurClassNode: TCodeTreeNode;
begin
  ANode:=StartNode;
  Result:=nil;
  while (ANode<>nil) do begin
    if ANode.Desc=ctnTypeSection then begin
      Result:=FindClassNode(ANode.FirstChild,UpperClassName,IgnoreForwards,
                     IgnoreNonForwards);
      if Result<>nil then exit;
    end else if ANode.Desc=ctnTypeDefinition then begin
      CurClassNode:=ANode.FirstChild;
      if (CurClassNode<>nil) and (CurClassNode.Desc=ctnClass) then begin
        if (not (IgnoreForwards
                 and (CurClassNode.SubDesc=ctnsForwardDeclaration)))
        and (not (IgnoreNonForwards
                 and (CurClassNode.SubDesc<>ctnsForwardDeclaration))) then begin
          MoveCursorToNodeStart(ANode);
          ReadNextAtom;
          CurClassName:=GetUpAtom;
          if UpperClassName=CurClassName then begin
            Result:=CurClassNode;
            exit;
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

function TPascalParserTool.FindClassNodeInInterface(
  const UpperClassName: string;
  IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  if Result.Desc=ctnUnit then begin
    Result:=Result.NextBrother;
    if Result=nil then exit;
  end;
  Result:=FindClassNode(Result.FirstChild,UpperClassName,
               IgnoreForwards, IgnoreNonForwards);
end;

function TPascalParserTool.FindFirstIdentNodeInClass(
  ClassNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=nil;
  if (ClassNode=nil) then exit;
  BuildSubTreeForClass(ClassNode);
  Result:=ClassNode.FirstChild;
  while (Result<>nil) and (Result.FirstChild=nil) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
end;

function TPascalParserTool.FindInterfaceNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnInterface) do
    Result:=Result.NextBrother;
end;

function TPascalParserTool.FindImplementationNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
end;

function TPascalParserTool.FindInitializationNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnInitialization) do
    Result:=Result.NextBrother;
end;

function TPascalParserTool.FindMainBeginEndNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if (Result=nil) then exit;
  if (Result.Desc<>ctnProgram) then begin
    Result:=nil;
    exit;
  end;
  Result:=Result.LastChild;
  if Result=nil then exit;
  if Result.Desc<>ctnBeginBlock then Result:=nil;
end;

function TPascalParserTool.NodeHasParentOfType(ANode: TCodeTreeNode;
  NodeDesc: TCodeTreeNodeDesc): boolean;
begin
  if ANode<>nil then begin
    repeat
      ANode:=ANode.Parent;
    until (ANode=nil) or (ANode.Desc=NodeDesc);
  end;
  Result:=(ANode<>nil);
end;

{ TBasicCodeTool }

function TBasicCodeTool.GetSourceNamePos(var NamePos: TAtomPosition): boolean;
begin
  Result:=false;
  BuildTree(true);
  NamePos.StartPos:=-1;
  if Tree.Root=nil then exit;
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read source type 'program', 'unit' ...
  ReadNextAtom; // read name
  NamePos:=CurPos;
  Result:=(NamePos.StartPos<SrcLen);
end;

function TBasicCodeTool.GetSourceName: string;
var NamePos: TAtomPosition;
begin
  Result:='';
  if not GetSourceNamePos(NamePos) then exit;
  Result:=copy(Src,NamePos.StartPos,NamePos.EndPos-NamePos.StartPos);
end;

function TBasicCodeTool.RenameSource(const NewName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var NamePos: TAtomPosition;
begin
  Result:=false;
  if (not GetSourceNamePos(NamePos)) or (NamePos.StartPos<1) or (NewName='')
  or (Length(NewName)>255) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.Replace(gtNone,gtNone,NamePos.StartPos,NamePos.EndPos,
    NewName);
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TBasicCodeTool.FindUnitInUsesSection(UsesNode: TCodeTreeNode;
  const UpperUnitName: string;
  var NamePos, InPos: TAtomPosition): boolean;
begin
  Result:=false;
  if (UsesNode=nil) or (UpperUnitName='') or (length(UpperUnitName)>255)
  or (UsesNode.Desc<>ctnUsesSection) then exit;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read 'uses'
  repeat
    ReadNextAtom; // read name
    if AtomIsChar(';') then break;
    if UpAtomIs(UpperUnitName) then begin
      NamePos:=CurPos;
      InPos.StartPos:=-1;
      ReadNextAtom;
      if UpAtomIs('IN') then begin
        ReadNextAtom;
        InPos:=CurPos;
      end;
      Result:=true;
      exit;
    end;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      ReadNextAtom;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then break;
  until (CurPos.StartPos>SrcLen);;
end;

function TBasicCodeTool.FindUnitInAllUsesSections(const UpperUnitName: string;
  var NamePos, InPos: TAtomPosition): boolean;
var SectionNode, UsesNode: TCodeTreeNode;
begin
  Result:=false;
  if (UpperUnitName='') or (length(UpperUnitName)>255) then exit;
  BuildTree(false);
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) and (SectionNode.Desc in [ctnProgram, ctnUnit,
    ctnPackage,ctnLibrary,ctnInterface,ctnImplementation]) do begin
    if SectionNode.Desc in [ctnProgram, ctnPackage,ctnLibrary, ctnInterface,
       ctnImplementation] then
    begin
      UsesNode:=SectionNode.FirstChild;
      if FindUnitInUsesSection(UsesNode,UpperUnitName,NamePos,InPos) then begin
        Result:=true;
        exit;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TBasicCodeTool.FindMainUsesSection: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  if Result.Desc=ctnUnit then begin
    Result:=Result.NextBrother;
    if Result=nil then exit;
  end;
  Result:=Result.FirstChild;
  if (Result<>nil) and (Result.Desc<>ctnUsesSection) then Result:=nil;
end;

function TBasicCodeTool.FindImplementationUsesSection: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
  if (Result=nil) or (Result.Desc<>ctnUsesSection) then exit;
end;

function TBasicCodeTool.RenameUsedUnit(const OldUpperUnitName,
  NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var UnitPos, InPos: TAtomPosition;
  NewUnitTerm: string;
begin
  Result:=false;
  if (OldUpperUnitName='') or (length(OldUpperUnitName)>255) or (NewUnitName='')
  or (length(NewUnitName)>255) then exit;
  if not FindUnitInAllUsesSections(OldUpperUnitName,UnitPos,InPos) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  if InPos.StartPos>0 then UnitPos.EndPos:=InPos.EndPos;
  NewUnitTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUnitTerm:=NewUnitTerm+' in '''+NewUnitInFile+'''';
  if ReplacementNeedsLineEnd(Src,UnitPos.StartPos,UnitPos.EndPos,
    length(NewUnitTerm),SourceChangeCache.BeautifyCodeOptions.LineLength) then
  begin
    if not SourceChangeCache.Replace(gtNewLine,gtNone,
      UnitPos.StartPos,UnitPos.EndPos,NewUnitTerm) then exit;
  end else begin
    if not SourceChangeCache.Replace(gtSpace,gtNone,
      UnitPos.StartPos,UnitPos.EndPos,NewUnitTerm) then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TBasicCodeTool.AddUnitToUsesSection(UsesNode: TCodeTreeNode;
  const NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var LineStart, LineEnd, Indent, InsertPos: integer;
  NewUnitTerm: string;
begin
  Result:=false;
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) or (NewUnitName='')
  or (length(NewUnitName)>255) or (UsesNode.StartPos<1)
  or (UsesNode.EndPos<1) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read first name
  Indent:=GetLineIndent(Src,CurPos.StartPos);
  if Indent<SourceChangeCache.BeautifyCodeOptions.Indent then
    Indent:=SourceChangeCache.BeautifyCodeOptions.Indent;
  InsertPos:=UsesNode.EndPos-1;
  NewUnitTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUnitTerm:=NewUnitTerm+' in '''+NewUnitInFile+'''';
  GetLineStartEndAtPosition(Src,InsertPos,LineStart,LineEnd);
  if InsertPos-LineStart+length(NewUnitTerm)+2>=
    SourceChangeCache.BeautifyCodeOptions.LineLength then begin
    NewUnitTerm:=','+SourceChangeCache.BeautifyCodeOptions.LineEnd+
      GetIndentStr(Indent)+NewUnitTerm;
  end else
    NewUnitTerm:=', '+NewUnitTerm;
  if not SourceChangeCache.Replace(gtNone,gtNone,InsertPos,InsertPos,
                                    NewUnitTerm) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TBasicCodeTool.AddUnitToMainUsesSection(const NewUnitName,
  NewUnitInFile: string; SourceChangeCache: TSourceChangeCache): boolean;
var UsesNode, SectionNode: TCodeTreeNode;
  NewUnitTerm: string;
  InsertPos: integer;
  Junk     : TAtomPosition;
begin
  Result:=false;
  if (NewUnitName='') or (length(NewUnitName)>255) then exit;
  BuildTree(true);
  SourceChangeCache.MainScanner:=Scanner;
  UsesNode:=FindMainUsesSection;
  if UsesNode<>nil then begin
    // add unit to existing uses section
    if not(FindUnitInUsesSection(UsesNode,Uppercase(NewUnitName),Junk,Junk)) then
       Result:=AddUnitToUsesSection(UsesNode,NewUnitName, NewUnitInFile,
                                 SourceChangeCache);
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
    NewUnitTerm:=SourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('uses')
         +' '+NewUnitName;
    if NewUnitInFile<>'' then
      NewUnitTerm:=NewUnitTerm+' in '''+NewUnitInFile+''';'
    else
      NewUnitTerm:=NewUnitTerm+';';
    InsertPos:=CurPos.EndPos;
    if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
      NewUnitTerm) then exit;
    if not SourceChangeCache.Apply then exit;
    Result:=true;
  end;
end;

function TBasicCodeTool.RemoveUnitFromUsesSection(UsesNode: TCodeTreeNode;
  const UpperUnitName: string; SourceChangeCache: TSourceChangeCache): boolean;
var UnitCount, StartPos, EndPos: integer;
begin
  Result:=false;
  if (UsesNode=nil) or (UpperUnitName='') or (length(UpperUnitName)>255) then
    exit;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read 'uses'
  UnitCount:=0;
  repeat
    EndPos:=CurPos.StartPos;
    ReadNextAtom; // read name
    if not AtomIsWord then exit;
    inc(UnitCount);
    if UpAtomIs(UpperUnitName) then begin
      // unit found
      SourceChangeCache.MainScanner:=Scanner;
      StartPos:=CurPos.StartPos;
      ReadNextAtom;
      if UpAtomIs('IN') then begin
        ReadNextAtom;
        ReadNextAtom;
      end;
      if UnitCount=1 then begin
        // first unit in uses section
        if AtomIsChar(';') then begin
          // last unit in uses section -> delete whole uses section
          if not SourceChangeCache.Replace(gtNone,gtNone,
            UsesNode.StartPos,UsesNode.EndPos,'') then exit;
        end else begin
          // not last unit -> delete with comma behind
          if not SourceChangeCache.Replace(gtNone,gtNone,
            StartPos,CurPos.EndPos,'') then exit;
        end;
      end else begin
        // not first unit in uses section -> delete with comma in front
        if not SourceChangeCache.Replace(gtNone,gtNone,
          EndPos,CurPos.StartPos,'') then exit;
      end;
     if not SourceChangeCache.Apply then exit;
      Result:=true;
      exit;
    end;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      ReadNextAtom;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then break;
  until (CurPos.StartPos>UsesNode.EndPos) or (CurPos.StartPos>SrcLen);
end;

function TBasicCodeTool.RemoveUnitFromAllUsesSections(
  const UpperUnitName: string; SourceChangeCache: TSourceChangeCache): boolean;
var SectionNode: TCodeTreeNode;
begin
  Result:=false;
  if (UpperUnitName='') or (length(UpperUnitName)>255)
  or (SourceChangeCache=nil) then exit;
  BuildTree(false);
  Result:=true;
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) do begin
    if (SectionNode.Desc in [ctnProgram,ctnPackage,ctnLibrary,ctnInterface,
         ctnImplementation]) then begin
      if RemoveUnitFromUsesSection(SectionNode.FirstChild,UpperUnitName,
         SourceChangeCache) then begin
        Result:=RemoveUnitFromAllUsesSections(UpperUnitName,SourceChangeCache);
        exit;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TBasicCodeTool.FindNextIncludeInInitialization(
  var LinkIndex: integer): TCodeBuffer;
// LinkIndex < 0  ->  search first
var
  InitializationNode: TCodeTreeNode;
  StartCode: TCodeBuffer;
begin
  Result:=nil;
  if LinkIndex<0 then begin
    BuildTree(false);
    InitializationNode:=FindInitializationNode;
    if InitializationNode=nil then exit;
    LinkIndex:=Scanner.LinkIndexAtCleanPos(InitializationNode.StartPos);
  end else
    inc(LinkIndex);
  if (LinkIndex<0) or (LinkIndex>=Scanner.LinkCount) then exit;
  StartCode:=TCodeBuffer(Scanner.Links[LinkIndex].Code);
  while (LinkIndex<Scanner.LinkCount)
  and (Scanner.Links[LinkIndex].CleanedPos<InitializationNode.EndPos) do begin
    Result:=TCodeBuffer(Scanner.Links[LinkIndex].Code);
    if (Result<>StartCode) then
      exit;
    inc(LinkIndex);
  end;
  Result:=nil;
end;

function TBasicCodeTool.FindLazarusResourceInBuffer(ResourceCode: TCodeBuffer;
  const ResourceName: string): TAtomPosition;
var ResNameCode: string;

  function ReadLazResource: boolean;
  begin
    Result:=false;
    if not ReadNextAtomIsChar('.') then exit;
    if not ReadNextUpAtomIs('ADD') then exit;
    if not ReadNextAtomIsChar('(') then exit;
    ReadNextAtom;
    if not AtomIsStringConstant then exit;
    if UpAtomIs(ResNameCode) then
      Result:=true;
    repeat
      ReadNextAtom;
    until (CurPos.StartPos>SrcLen) or (AtomIsChar(')'));
    ReadNextAtom; // read ';'
  end;
  
var CleanPos, MaxCleanPos: integer;
begin
  Result.StartPos:=-1;
  if (ResourceCode=nil) or (ResourceName='') or (length(ResourceName)>255) then
    exit;
  if Scanner.CursorToCleanPos(1,ResourceCode,CleanPos)<>0 then exit;
  if Scanner.CursorToCleanPos(ResourceCode.SourceLength,ResourceCode,
    MaxCleanPos)<>0 then
    MaxCleanPos:=-1;
  MoveCursorToCleanPos(CleanPos);
  ResNameCode:=''''+UpperCaseStr(ResourceName)+'''';
  // search "LazarusResources.Add('<ResourceName>',"
  repeat
    ReadNextAtom; // read 'LazarusResources'
    if UpAtomIs('LAZARUSRESOURCES') then begin
      Result.StartPos:=CurPos.StartPos;
      if ReadLazResource then begin
        Result.EndPos:=CurPos.EndPos;
        exit;
      end;
    end;
  until (CurPos.StartPos>SrcLen) or UpAtomIs('END')
  or ((MaxCleanPos>0) and (CurPos.StartPos>MaxCleanPos));
  Result.StartPos:=-1;
end;

function TBasicCodeTool.FindLazarusResource(
  const ResourceName: string): TAtomPosition;
// search Resource in all include files
var LinkIndex: integer;
  CurCode: TCodeBuffer;
begin
  Result.StartPos:=-1;
  LinkIndex:=-1;
  CurCode:=FindNextIncludeInInitialization(LinkIndex);
  while (CurCode<>nil) do begin
    Result:=FindLazarusResourceInBuffer(CurCode,ResourceName);
    if Result.StartPos>0 then exit;
    CurCode:=FindNextIncludeInInitialization(LinkIndex);
  end;
end;

function TBasicCodeTool.AddLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName, ResourceData: string;
  SourceChangeCache: TSourceChangeCache): boolean;
// ResoureData is the complete LazarusResource Statement
var FromPos, ToPos, i: integer;
  OldPosition: TAtomPosition;
begin
  Result:=false;
  if (ResourceCode=nil) or (ResourceName='') or (length(ResourceName)>255)
  or (ResourceData='') or (SourceChangeCache=nil) then exit;
  BuildTree(false);
  SourceChangeCache.MainScanner:=Scanner;
  OldPosition:=FindLazarusResourceInBuffer(ResourceCode,ResourceName);
  if OldPosition.StartPos>0 then begin
    // replace old resource
    FromPos:=OldPosition.StartPos;
    ToPos:=OldPosition.EndPos;
    if not SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
      ResourceData) then exit;
  end else begin
    // insert new resource
    if ResourceCode.SourceLength>0 then begin
      if Scanner.CursorToCleanPos(ResourceCode.SourceLength,ResourceCode,
        FromPos)<>0 then exit;
      inc(FromPos);
    end else begin
      // resource code empty -> can not be found in cleaned code
      // special replace
      i:=0;
      while (i<Scanner.LinkCount) 
      and (Scanner.Links[i].Code<>Pointer(ResourceCode)) do
        inc(i);
      if i>=Scanner.LinkCount then exit;
      FromPos:=Scanner.Links[i].CleanedPos;
    end;
    if not SourceChangeCache.ReplaceEx(gtNewLine,gtNewLine,FromPos,FromPos,
      ResourceCode,ResourceCode.SourceLength+1,ResourceData) then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TBasicCodeTool.RemoveLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var OldPosition: TAtomPosition;
begin
  Result:=false;
  if (ResourceCode=nil) or (ResourceName='') or (length(ResourceName)>255)
  or (SourceChangeCache=nil) then exit;
  BuildTree(false);
  SourceChangeCache.MainScanner:=Scanner;
  OldPosition:=FindLazarusResourceInBuffer(ResourceCode,ResourceName);
  if OldPosition.StartPos>0 then begin
    OldPosition.StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,
         OldPosition.StartPos,Scanner.NestedComments);
    OldPosition.EndPos:=FindFirstLineEndAfterInCode(Src,OldPosition.EndPos,
         Scanner.NestedComments);
    if not SourceChangeCache.Replace(gtNone,gtNone,
      OldPosition.StartPos,OldPosition.EndPos,'') then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TBasicCodeTool.RenameInclude(LinkIndex: integer;
  const NewFilename: string; KeepPath: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
var IncludeStart, IncludeEnd, FileStart, FileNameStart, FileEnd: integer;
begin
  Result:=false;
  if (LinkIndex<0) or (LinkIndex>=Scanner.LinkCount) or (NewFileName='')
  or (KeepPath and (length(NewFilename)>255))
  or (SourceChangeCache=nil) then exit;
  // find include directive
  IncludeEnd:=Scanner.Links[LinkIndex].CleanedPos;
  IncludeStart:=IncludeEnd-1;
  if IncludeStart<1 then exit;
  case Src[IncludeStart] of
    '}':
      begin
        FileEnd:=IncludeStart;
        dec(IncludeStart);
        while (IncludeStart>0) and (Src[IncludeStart]<>'{') do
          dec(IncludeStart);
      end;
    ')':
      begin
        dec(IncludeStart);
        FileEnd:=IncludeStart;
        while (IncludeStart>1)
        and ((Src[IncludeStart]<>'*') or (Src[IncludeStart-1]<>'(')) do
          dec(IncludeStart);
      end;
    #13,#10:
      begin
        FileEnd:=IncludeStart;
        if (FileEnd>0) and (IsLineEndChar[Src[FileEnd]]) then dec(FileEnd);
        dec(IncludeStart);
        while (IncludeStart>1)
        and ((Src[IncludeStart]<>'/') or (Src[IncludeStart-1]<>'/')) do
          dec(IncludeStart);
      end;
  end;
  if IncludeStart<1 then exit;
  FileStart:=IncludeStart;
  while (FileStart<IncludeEnd) and (Src[FileStart]<>'$') do
    inc(FileStart);
  while (FileStart<IncludeEnd) and (not (IsSpaceChar[Src[FileStart]])) do
    inc(FileStart);
  while (FileStart<IncludeEnd) and (IsSpaceChar[Src[FileStart]]) do
    inc(FileStart);
  if FileStart>=IncludeEnd then exit;
  SourceChangeCache.MainScanner:=Scanner;
  if KeepPath then begin
    FileNameStart:=FileEnd;
    while (FileNameStart>FileStart) and (Src[FileNameStart]<>OSDirSeparator) do
      dec(FileNameStart);
    if Src[FileNameStart]=OSDirSeparator then
      FileStart:=FileNameStart+1;
  end;
  if not SourceChangeCache.Replace(gtNone,GtNone,FileStart,FileEnd,
    NewFilename) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TBasicCodeTool.FindCreateFormStatement(StartPos: integer;
  const UpperClassName, UpperVarName: string;
  var Position: TAtomPosition): integer;
// 0=found, -1=not found, 1=found, but wrong classname
var MainBeginNode: TCodeTreeNode;
  ClassNameFits: boolean;
begin
  Result:=-1;
  if (UpperClassName='') or (UpperVarName='') or (length(UpperClassName)>255)
  or (length(UpperVarName)>255) then exit;
  if StartPos<1 then begin
    BuildTree(false);
    MainBeginNode:=FindMainBeginEndNode;
    if MainBeginNode=nil then exit;
    StartPos:=MainBeginNode.StartPos;
    if StartPos<1 then exit;
  end;
  MoveCursorToCleanPos(StartPos);
  repeat
    ReadNextAtom;
    if UpAtomIs('APPLICATION') then begin
      Position.StartPos:=CurPos.StartPos;
      if ReadNextAtomIsChar('.') and ReadNextUpAtomIs('CREATEFORM')
      and ReadNextAtomIsChar('(') then begin
        ReadNextAtom;
        ClassNameFits:=UpAtomIs(UpperClassName);
        if ReadNextAtomIsChar(',')
        and (ReadNextUpAtomIs(UpperVarName) or (UpperVarName='*')) then begin
          if ReadNextAtomIsChar(')') then ReadNextAtomIsChar(';');
          Position.EndPos:=CurPos.EndPos;
          if ClassNameFits then
            Result:=0
          else
            Result:=1;
          exit;
        end;
      end;
    end;
  until (CurPos.StartPos>SrcLen);
  Result:=-1;
end;

function TBasicCodeTool.AddCreateFormStatement(const AClassName,
  AVarName: string; SourceChangeCache: TSourceChangeCache): boolean;
var MainBeginNode: TCodeTreeNode;
  OldPosition: TAtomPosition;
  FromPos, ToPos, Indent: integer;
begin
  Result:=false;
  if (AClassName='') or (length(AClassName)>255) or (AVarName='')
  or (length(AVarName)>255) then exit;
  BuildTree(false);
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode=nil then exit;
  FromPos:=-1;
  if FindCreateFormStatement(MainBeginNode.StartPos,UpperCaseStr(AClassName),
    UpperCaseStr(AVarName),OldPosition)=-1 then begin
    // does not exists -> create as last in front of 'Application.Run'
    MoveCursorToCleanPos(MainBeginNode.StartPos);
    repeat
      if ReadNextUpAtomIs('APPLICATION') then begin
        FromPos:=CurPos.StartPos;
        if ReadNextAtomIsChar('.') and ReadNextUpAtomIs('RUN') then begin
          break;
        end;
        FromPos:=-1;
      end;
    until (CurPos.StartPos>SrcLen);
    if FromPos<1 then exit;
    SourceChangeCache.MainScanner:=Scanner;
    Indent:=GetLineIndent(Src,FromPos);
    FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,FromPos,
                    Scanner.NestedComments);
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,FromPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+');',Indent));
  end else begin
    FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,OldPosition.StartPos,
                                         Scanner.NestedComments);
    ToPos:=FindFirstLineEndAfterInCode(Src,OldPosition.EndPos,
                                       Scanner.NestedComments);
    SourceChangeCache.MainScanner:=Scanner;
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+')',2));
  end;
  Result:=SourceChangeCache.Apply;
end;

function TBasicCodeTool.RemoveCreateFormStatement(const UpperVarName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var Position: TAtomPosition;
  FromPos, ToPos: integer;
begin
  Result:=false;
  if FindCreateFormStatement(-1,'*',UpperVarName,Position)=-1 then
    exit;
  FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,Position.StartPos,
                                       Scanner.NestedComments);
  ToPos:=FindFirstLineEndAfterInCode(Src,Position.EndPos,
                                     Scanner.NestedComments);
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');
  Result:=SourceChangeCache.Apply;
end;

function TBasicCodeTool.ListAllCreateFormStatements: TStrings;
// list format: VarName:ClassName
var Position: integer;
  StatementPos: TAtomPosition;
  s:string;
  var MainBeginNode: TCodeTreeNode;
begin
  Result:=TStringList.Create;
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode=nil then exit;
  Position:=MainBeginNode.StartPos;
  repeat
    if FindCreateFormStatement(Position,'*','*',StatementPos)=-1 then
      exit;
    Position:=StatementPos.EndPos;
    MoveCursorToCleanPos(StatementPos.StartPos);
    ReadNextAtom; // read 'Application'
    ReadNextAtom; // read '.'
    ReadNextAtom; // read 'CreateForm'
    ReadNextAtom; // read '('
    ReadNextAtom; // read class name
    s:=GetAtom;
    ReadNextAtom; // read ','
    ReadNextAtom; // read variable name
    s:=GetAtom+':'+s;
    Result.Add(s);
  until false;
end;

function TBasicCodeTool.SetAllCreateFromStatements(List: TStrings;
  SourceChangeCache: TSourceChangeCache): boolean;
{ every string in the list has the format VarName:ClassName
  or simply VarName In the latter case it will be automatically expanded
  to VarName:TVarName
  
  ToDo: do it less destructable
}
var Position, InsertPos, i, ColonPos, Indent: integer;
  StatementPos: TAtomPosition;
  var MainBeginNode: TCodeTreeNode;
  AClassName, AVarName: string;
begin
  Result:=false;
  if (List=nil) or (SourceChangeCache=nil) then exit;
  // first delete all CreateForm Statements
  SourceChangeCache.MainScanner:=Scanner;
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode=nil then exit;
  Position:=MainBeginNode.StartPos;
  InsertPos:=-1;
  repeat
    if FindCreateFormStatement(Position,'*','*',StatementPos)=-1 then
      break;
    Position:=StatementPos.EndPos;
    StatementPos.StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,
       StatementPos.StartPos,Scanner.NestedComments);
    InsertPos:=StatementPos.StartPos;
    StatementPos.EndPos:=FindFirstLineEndAfterInCode(Src,
       StatementPos.EndPos,Scanner.NestedComments);
    SourceChangeCache.Replace(gtNone,gtNone,
       StatementPos.StartPos,StatementPos.EndPos,'');
  until false;
  // then add all CreateForm Statements
  if InsertPos<1 then begin
    // there was no createform statement -> insert in front of Application.Run
    MoveCursorToCleanPos(MainBeginNode.StartPos);
    repeat
      if ReadNextUpAtomIs('APPLICATION') then begin
        InsertPos:=CurPos.StartPos;
        if ReadNextAtomIsChar('.') and ReadNextUpAtomIs('RUN') then begin
          break;
        end;
        InsertPos:=-1;
      end;
    until (CurPos.StartPos>SrcLen);
    if InsertPos<1 then exit;
  end;
  for i:=0 to List.Count-1 do begin
    ColonPos:=1;
    while (ColonPos<=length(List[i])) and (List[i][ColonPos]<>':') do
      inc(ColonPos);
    AVarName:=copy(List[i],1,ColonPos);
    if AVarName<>'' then begin
      AClassName:=copy(List[i],ColonPos+1,length(List[i])-ColonPos);
      if AClassName='' then AClassName:='T'+AVarName;
      Indent:=GetLineIndent(Src,InsertPos);
      SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
        SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
          'Application.CreateForm('+AClassName+','+AVarName+');',Indent)
        );
    end;
  end;
  Result:=SourceChangeCache.Apply;
end;

function TBasicCodeTool.FindPublishedVariable(const UpperClassName,
  UpperVarName: string): TCodeTreeNode;
var ClassNode, SectionNode: TCodeTreeNode;
begin
  Result:=nil;
  if (UpperClassName='') or (length(UpperClassName)>255) then exit;
  BuildTree(true);
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  if ClassNode=nil then exit;
  BuildSubTreeForClass(ClassNode);
  SectionNode:=ClassNode.FirstChild;
  while (SectionNode<>nil) do begin
    if SectionNode.Desc=ctnClassPublished then begin
      Result:=SectionNode.FirstChild;
      while Result<>nil do begin
        if (Result.Desc=ctnVarDefinition) then begin
          MoveCursorToNodeStart(Result);
          if ReadNextUpAtomIs(UpperVarName) then
            exit;
        end;
        Result:=Result.NextBrother;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TBasicCodeTool.AddPublishedVariable(const UpperClassName,
  VarName, VarType: string; SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode, SectionNode: TCodeTreeNode;
  Indent, InsertPos: integer;
begin
  Result:=false;
  if (UpperClassName='') or (length(UpperClassName)>255)
  or (VarName='') or (length(VarName)>255) or (VarType='')
  or (length(VarType)>255) or (SourceChangeCache=nil) then exit;
  if FindPublishedVariable(UpperClassName,UpperCaseStr(VarName))<>nil then begin
    Result:=true;
    exit;
  end;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  if ClassNode=nil then exit;
  BuildSubTreeForClass(ClassNode);
  SectionNode:=ClassNode.FirstChild;
  if (SectionNode.NextBrother<>nil)
  and (SectionNode.NextBrother.Desc=ctnClassPublished) then
    SectionNode:=SectionNode.NextBrother;
  SourceChangeCache.MainScanner:=Scanner;
  if SectionNode.FirstChild<>nil then begin
    Indent:=GetLineIndent(Src,SectionNode.FirstChild.StartPos);
  end else begin
    Indent:=GetLineIndent(Src,SectionNode.StartPos)
              +SourceChangeCache.BeautifyCodeOptions.Indent;
  end;
  InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,SectionNode.EndPos,
               Scanner.NestedComments);
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
          SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                     VarName+':'+VarType+';',Indent)
       );
  Result:=SourceChangeCache.Apply;
end;

function TBasicCodeTool.RemovePublishedVariable(const UpperClassName,
  UpperVarName: string; SourceChangeCache: TSourceChangeCache): boolean;
var VarNode: TCodeTreeNode;
  FromPos, ToPos: integer;
begin
  Result:=false;
  VarNode:=FindPublishedVariable(UpperClassName,UpperVarName);
  if VarNode=nil then exit;
  if (VarNode.PriorBrother<>nil)
  and (VarNode.PriorBrother.Desc=ctnVarDefinition)
  and (VarNode.PriorBrother.FirstChild=nil) then begin
    // variable definition has the form  'PriorVarName, VarName: VarType;'
    // or 'PriorVarName, VarName, NextVarName: VarType'
    // -> delete only ', VarName'
    MoveCursorToNodeStart(VarNode.PriorBrother);
    ReadNextAtom; // read 'PriorVarName'
    ReadNextAtom; // read ','
    FromPos:=CurPos.StartPos;
    ReadNextAtom; // read 'VarName'
    ReadNextAtom; // read ':'
    ToPos:=CurPos.StartPos;
  end else begin
    if VarNode.FirstChild<>nil then begin
      // variable definition has the form  'VarName: VarType;'
      // -> delete whole line
      FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,VarNode.StartPos,
                      Scanner.NestedComments);
      ToPos:=FindFirstLineEndAfterInCode(Src,VarNode.EndPos,
                      Scanner.NestedComments);
    end else begin
      // variable definition has the form  'VarName, NextVarName: VarType;'
      // -> delete only 'VarName, '
      FromPos:=VarNode.StartPos;
      ToPos:=VarNode.NextBrother.StartPos;
    end;
  end;
  SourceChangeCache.MainScanner:=Scanner;
  if not SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'') then exit;
  Result:=SourceChangeCache.Apply;
end;

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
      // XXX ToDo strange: SizeOf(TParamFlags) is 4, but the data is only 1
      Len:=1; // SizeOf(TParamFlags)
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

procedure TEventsCodeTool.GetCompatiblePublishedMethods(
  const UpperClassName: string; TypeData: PTypeData; Proc: TGetStringProc);
var ClassNode: TCodeTreeNode;
begin
  BuildTree(true);
  if not InterfaceSectionFound then exit;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  GetCompatiblePublishedMethods(ClassNode,TypeData,Proc);
end;

procedure TEventsCodeTool.GetCompatiblePublishedMethods(
  ClassNode: TCodeTreeNode; TypeData: PTypeData; Proc: TGetStringProc);
var SearchedProc: string;
  SectionNode, ANode: TCodeTreeNode;
  CurProcHead, CurProcName: string;
begin
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (TypeData=nil)
  or (Proc=nil) then exit;
  BuildSubTreeForClass(ClassNode);
  SearchedProc:=MethodTypeDataToStr(TypeData,[phpInUpperCase]);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.GetCompatibleMethods] SearchedProc="',SearchedProc,'"');
{$ENDIF}
  SectionNode:=ClassNode.FirstChild;
  while (SectionNode<>nil) do begin
    while (SectionNode.Desc<>ctnClassPublished) or (SectionNode.FirstChild=nil)
    do begin
      SectionNode:=SectionNode.NextBrother;
      if SectionNode=nil then exit;
    end;
    ANode:=SectionNode.FirstChild;
    repeat
      if (ANode.Desc=ctnProcedure) then begin
        CurProcHead:=ExtractProcHead(ANode,[phpInUpperCase,phpWithoutName]);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.GetCompatibleMethods] CurProcName="',CurProcHead,'"');
{$ENDIF}
        if (CurProcHead<>'')
        and (CompareTextIgnoringSpace(CurProcHead,SearchedProc,true)=0) then
        begin
          CurProcName:=ExtractProcName(ANode,false);
          if (CurProcName<>'') and (length(CurProcName)<256) then
            Proc(CurProcName);
        end;
      end;
      ANode:=ANode.NextBrother;
    until ANode=nil;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TEventsCodeTool.FindPublishedMethodNodeInClass(
  ClassNode: TCodeTreeNode; const UpperMethodName: string;
  TypeData: PTypeData): TCodeTreeNode;
var
  SectionNode, ANode: TCodeTreeNode;
  SearchedProcHead, CurProcHead: string;
begin
  Result:=nil;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (UpperMethodName='')
  or (Scanner=nil) or (TypeData=nil) then exit;
  SearchedProcHead:=UpperMethodName+MethodTypeDataToStr(TypeData,
                       [phpInUpperCase]);
{$IFDEF CTDEBUG}
writeln('TEventsCodeTool.FindPublishedMethodNodeInClass A SearchedProcHead="',
SearchedProcHead,'"');
{$ENDIF}
  BuildSubTreeForClass(ClassNode);
  SectionNode:=ClassNode.FirstChild;
  while (SectionNode<>nil) do begin
    while (SectionNode.Desc<>ctnClassPublished) or (SectionNode.FirstChild=nil)
    do begin
      SectionNode:=SectionNode.NextBrother;
      if SectionNode=nil then exit;
    end;
    ANode:=SectionNode.FirstChild;
    repeat
      if (ANode.Desc=ctnProcedure) then begin
        CurProcHead:=ExtractProcHead(ANode,[phpInUpperCase]);
{$IFDEF CTDEBUG}
writeln('TEventsCodeTool.FindPublishedMethodNodeInClass "',SearchedProcHead,
'"="',CurProcHead,'"');
{$ENDIF}
        if (CurProcHead<>'')
        and (CompareTextIgnoringSpace(CurProcHead,SearchedProcHead,true)=0) then
        begin
          Result:=ANode;
          exit;
        end;
      end;
      ANode:=ANode.NextBrother;
    until ANode=nil;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TEventsCodeTool.FindProcNodeInImplementation(const UpperClassName,
  UpperMethodName: string; TypeData: PTypeData;
  BuildTreeBefore: boolean): TCodeTreeNode;
var SectionNode, ANode: TCodeTreeNode;
  SearchedProcHead, CurProcHead: string;
begin
  Result:=nil;
  if BuildTreeBefore then BuildTree(false);
  // find implementation node
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) and (SectionNode.Desc<>ctnImplementation) do
    SectionNode:=SectionNode.NextBrother;
  if SectionNode=nil then exit;
  ANode:=SectionNode.FirstChild;
  SearchedProcHead:=UpperClassName+'.'+UpperMethodName
       +MethodTypeDataToStr(TypeData,[phpInUpperCase,phpWithParameterNames]);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.FindProcNodeInImplementation] SearchedProcHead=',SearchedProcHead);
{$ENDIF}
  while (ANode<>nil) do begin
    if (ANode.Desc=ctnProcedure) then begin
      CurProcHead:=ExtractProcHead(ANode,[phpInUpperCase]);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.FindProcNodeInImplementation] CurProcHead=',CurProcHead);
{$ENDIF}
      if (CurProcHead<>'')
      and (CompareTextIgnoringSpace(CurProcHead,SearchedProcHead,true)=0) then
      begin
        Result:=ANode;
        exit;
      end;
    end;
    ANode:=ANode.NextBrother;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(const UpperClassName,
  UpperMethodName: string; TypeData: PTypeData): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(true);
  if not InterfaceSectionFound then exit;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  Result:=PublishedMethodExists(ClassNode,UpperMethodName,TypeData);
end;

function TEventsCodeTool.PublishedMethodExists(ClassNode: TCodeTreeNode;
  const UpperMethodName: string; TypeData: PTypeData): boolean;
begin
  Result:=(FindPublishedMethodNodeInClass(ClassNode,UpperMethodName,TypeData)
            <>nil);
end;

function TEventsCodeTool.JumpToPublishedMethodBody(const UpperClassName,
  UpperMethodName: string; TypeData: PTypeData;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var ANode: TCodeTreeNode;
begin
  Result:=false;
  ANode:=FindProcNodeInImplementation(UpperClassName,UpperMethodName,TypeData,
            true);
  Result:=FindJumpPointInProcNode(ANode,NewPos,NewTopLine);
end;

function TEventsCodeTool.RenamePublishedMethod(const UpperClassName,
  UpperOldMethodName, NewMethodName: string; TypeData: PTypeData;
  SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(false);
  if not EndOfSourceFound then exit;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  Result:=RenamePublishedMethod(ClassNode,UpperOldMethodName,NewMethodName,
                 TypeData,SourceChangeCache);
end;

function TEventsCodeTool.RenamePublishedMethod(ClassNode: TCodeTreeNode;
    const UpperOldMethodName, NewMethodName: string; TypeData: PTypeData;
    SourceChangeCache: TSourceChangeCache): boolean;
// rename published method in class and in procedure itself
var ANode, ProcHeadNode: TCodeTreeNode;
  NameStart, NameEnd: integer;
  UpperClassName: string;
begin
  Result:=false;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (UpperOldMethodName='')
  or (NewMethodName='') or (SourceChangeCache=nil) or (Scanner=nil)
  or (TypeData=nil) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  // rename in class
  ANode:=FindPublishedMethodNodeInClass(ClassNode,UpperOldMethodName,TypeData);
{$IFDEF CTDEBUG}
writeln('TEventsCodeTool.RenamePublishedMethod A ',ANode<>nil);
{$ENDIF}
  if ANode=nil then exit;
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
{$IFDEF CTDEBUG}
writeln('TEventsCodeTool.RenamePublishedMethod B ClassName=',UpperClassName);
{$ENDIF}
  ANode:=FindProcNodeInImplementation(UpperClassName,UpperOldMethodName,
              TypeData,false);
{$IFDEF CTDEBUG}
writeln('TEventsCodeTool.RenamePublishedMethod C ',ANode<>nil);
{$ENDIF}
  if ANode=nil then exit;
  ProcHeadNode:=ANode.FirstChild;
  if ProcHeadNode=nil then exit;
  MoveCursorToNodeStart(ProcHeadNode);
  ReadNextAtom; // read class name
  ReadNextAtom; // read '.'
  ReadNextAtom; // read method name
{$IFDEF CTDEBUG}
writeln('TEventsCodeTool.RenamePublishedMethod D "',GetAtom,'"');
{$ENDIF}
  Result:=SourceChangeCache.Replace(gtNone,gtNone,
      CurPos.StartPos,CurPos.EndPos,NewMethodName);
end;

function TEventsCodeTool.CreatePublishedMethod(const UpperClassName,
  AMethodName: string; TypeData: PTypeData;
  SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(false);
  if not EndOfSourceFound then exit;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  Result:=CreatePublishedMethod(ClassNode,AMethodName,TypeData,
               SourceChangeCache);
end;

function TEventsCodeTool.CreatePublishedMethod(ClassNode: TCodeTreeNode;
  const AMethodName: string; TypeData: PTypeData;
  SourceChangeCache: TSourceChangeCache): boolean;
var PublishedNode: TCodeTreeNode;
  NewMethod: string;
begin
  Result:=false;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (AMethodName='')
  or (TypeData=nil) or (SourceChangeCache=nil) or (Scanner=nil) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  // add method to published section
  BuildSubTreeForClass(ClassNode);
  PublishedNode:=ClassNode.FirstChild;
  if PublishedNode=nil then exit;
  if (PublishedNode.StartPos=PublishedNode.EndPos)
  and (PublishedNode.NextBrother<>nil)
  and (PublishedNode.NextBrother.Desc=ctnClassPublished) then
    PublishedNode:=PublishedNode.NextBrother;
  NewMethod:=MethodKindAsString[TypeData^.MethodKind]+' '+AMethodName+
      MethodTypeDataToStr(TypeData,[phpWithVarModifiers,phpWithParameterNames]);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreatePublishedMethod] A NewMethod="',NewMethod,'"');
{$ENDIF}
  Result:=InsertNewMethodToClass(PublishedNode,AMethodName,NewMethod,
              SourceChangeCache);
end;

function TEventsCodeTool.InsertNewMethodToClass(
  ClassSectionNode: TCodeTreeNode; const AMethodName,NewMethod: string;
  SourceChangeCache: TSourceChangeCache): boolean;
// NewMethod is for example 'class function Lol(c: char): char;'
var InsertNode, ClassNode, ImplementationNode, StartNode, ANode: TCodeTreeNode;
  Indent, InsertPos, cmp, WordStart, WordEnd: integer;
  UpperMethodName, CurProcName, ProcCode, UpperClassName, CurWord,
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
  ProcCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyProc(NewMethod,
                     Indent,false);
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
           ProcCode) then exit;
           
  // add method body to implementation section

  ImplementationNode:=Tree.Root;
  while (ImplementationNode<>nil)
  and (ImplementationNode.Desc<>ctnImplementation) do
    ImplementationNode:=ImplementationNode.NextBrother;
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
    StartClassCode:=(ClassBodyProcs.Count=0);
    UpperMethodName:=UpperClassName+'.'+UpperMethodName;
    if not StartClassCode then begin
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] H');
{$ENDIF}
      // find a nice insert position for the proc body
      case SourceChangeCache.BeautifyCodeOptions.ProcedureInsertPolicy of
        pipAlphabetically:
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
        else // pipLast:
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
  WordEnd:=1;
  repeat
    WordStart:=WordEnd;
    while (WordStart<=length(NewMethod)) and (IsSpaceChar[NewMethod[WordStart]])
    do inc(WordStart);
    WordEnd:=WordStart;
    while (WordEnd<=length(NewMethod)) and (IsIdentChar[NewMethod[WordEnd]])
    do inc(WordEnd);
    CurWord:=UpperCaseStr(copy(NewMethod,WordStart,WordEnd-WordStart));
  until (CurWord<>'PROCEDURE') and (CurWord<>'FUNCTION') and (CurWord<>'CLASS')
  and (CurWord<>'CONSTRUCTOR') and (CurWord<>'DESTRUCTOR');
  ProcCode:=copy(NewMethod,1,WordStart-1)+AClassName+'.'
            +copy(NewMethod,WordStart,length(NewMethod)-WordStart+1);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] L');
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


{ TCodeCompletionCodeTool }

function TCodeCompletionCodeTool.ProcExists(
  const NameAndParams: string): boolean;
// NameAndParams should be uppercase and contains the proc name and the
// parameter list without names and default values
// and should not contain any comments, result types
var ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  // search in new nodes, which will be inserted
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if CompareTextIgnoringSpace(ANodeExt.Txt,NameAndParams,true)=0 then begin
      Result:=true;
      exit;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
  if not Result then begin
    // ToDo: check ancestor procs too
    // search in current class
    Result:=(FindProcNode(StartNode,NameAndParams,[phpInUpperCase])<>nil);
  end;
end;

function TCodeCompletionCodeTool.VarExists(const UpperName: string): boolean;
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
    Result:=(FindVarNode(StartNode,UpperName)<>nil);
  end;
end;

procedure TCodeCompletionCodeTool.AddInsert(PosNode: TCodeTreeNode;
  const CleanDef, Def, IdentifierName: string);
var NewInsert, InsertPos, Last: TCodeTreeNodeExtension;
begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.AddInsert] ',CleanDef,',',Def,',',Identifiername);
{$ENDIF}
  NewInsert:=NodeExtMemManager.NewNode;
  with NewInsert do begin
    Node:=PosNode;
    Txt:=CleanDef;
    ExtTxt1:=Def;
    ExtTxt2:=IdentifierName;
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
    Last:=nil;
    while (InsertPos<>nil)
    and (CompareTextIgnoringSpace(InsertPos.Txt,CleanDef,true)<=0) do begin
      Last:=InsertPos;
      InsertPos:=InsertPos.Next;
    end;
    if (InsertPos=nil)
    or (CompareTextIgnoringSpace(InsertPos.Txt,CleanDef,true)>0) then begin
      if Last<>nil then begin
        // insert after last
        NewInsert.Next:=Last.Next;
        Last.Next:=NewInsert;
      end else begin
        NewInsert.Next:=InsertPos;
        FirstInsert:=NewInsert;
      end;
    end else begin
      // insert after InsertPos
      NewInsert.Next:=InsertPos.Next;
      InsertPos.Next:=NewInsert;
    end;
  end;
end;

function TCodeCompletionCodeTool.NodeExtIsVariable(
  ANodeExt: TCodeTreeNodeExtension): boolean;
// a variable has the form 'Name:Type;'
var APos, TxtLen: integer;
begin
  APos:=1;
  TxtLen:=length(ANodeExt.ExtTxt1);
  while (APos<=TxtLen) and (IsIdentChar[ANodeExt.ExtTxt1[APos]]) do
    inc(APos);
  while (APos<=TxtLen) and (IsSpaceChar[ANodeExt.ExtTxt1[APos]]) do
    inc(APos);
  Result:=(APos<=TxtLen) and (ANodeExt.ExtTxt1[APos]=':');
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
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8;

   property specifiers without parameters:
     ;nodefault, ;default

   property specifiers with parameters:
     index <constant>, read <id>, write <id>, implements <id>,
     stored <id>, default <constant>
}
type
  TPropPart = (ppName,ppParamList, ppType, ppIndexWord, ppIndex, ppReadWord,
               ppRead, ppWriteWord, ppWrite, ppStoredWord, ppStored,
               ppImplementsWord, ppImplements, ppDefaultWord, ppDefault,
               ppNoDefaultWord);
var Parts: array[TPropPart] of TAtomPosition;
  APart: TPropPart;
  
  function ReadSimpleSpec(SpecWord, SpecParam: TPropPart): boolean;
  begin
    if Parts[SpecWord].StartPos>=1 then begin
      Result:=false;
      exit;
    end;
    Parts[SpecWord]:=CurPos;
    ReadNextAtom;
    Result:=AtomIsWord;
    if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then exit;
    Parts[SpecParam]:=CurPos;
    ReadNextAtom;
  end;

var AccessParam, AccessParamPrefix, CleanAccessFunc, AccessFunc,
  CleanParamList, ParamList, PropType: string;
  InsertPos: integer;
begin
  Result:=false;
  for APart:=Low(TPropPart) to High(TPropPart) do
    Parts[APart].StartPos:=-1;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'property'
  ReadNextAtom; // read name
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] Checking Property ',GetAtom);
{$ENDIF}
  Parts[ppName]:=CurPos;
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // read parameter list '[ ... ]'
    Parts[ppParamList].StartPos:=CurPos.StartPos;
    InitExtraction;
    if not ReadParamList(false,true,[phpInUpperCase,phpWithoutBrackets])
    then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] error parsing param list');
{$ENDIF}
      exit;
    end;
    CleanParamList:=GetExtraction;
    Parts[ppParamList].EndPos:=CurPos.EndPos;
  end else
    CleanParamList:='';
  if not AtomIsChar(':') then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] no type : found -> ignore property');
{$ENDIF}
    // no type -> ignore this property
    Result:=true;
    exit;
  end;
  ReadNextAtom; // read type
  if (CurPos.StartPos>PropNode.EndPos)
  or UpAtomIs('END') or AtomIsChar(';') or (not AtomIsIdentifier(false))
  or AtomIsKeyWord then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] error: no type name found');
{$ENDIF}
    exit;
  end;
  Parts[ppType]:=CurPos;
  // read specifiers
  ReadNextAtom;
  if UpAtomIs('INDEX') then begin
    if Parts[ppIndexWord].StartPos>=1 then exit;
    Parts[ppIndexWord]:=CurPos;
    ReadNextAtom;
    if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then exit;
    Parts[ppIndex].StartPos:=CurPos.StartPos;
    if not ReadConstant(false,false,[]) then exit;
    Parts[ppIndex].EndPos:=LastAtoms.GetValueAt(0).EndPos;
  end;
  if UpAtomIs('READ') and not ReadSimpleSpec(ppReadWord,ppRead) then exit;
  if UpAtomIs('WRITE') and not ReadSimpleSpec(ppWriteWord,ppWrite) then
    exit;
  while (CurPos.StartPos<PropNode.EndPos) and (not AtomIsChar(';'))
  and (not UpAtomIs('END')) do begin
    if UpAtomIs('STORED') then begin
      if not ReadSimpleSpec(ppStoredWord,ppStored) then
        exit;
    end else if UpAtomIs('DEFAULT') then begin
      if Parts[ppDefaultWord].StartPos>=1 then exit;
      Parts[ppDefaultWord]:=CurPos;
      ReadNextAtom;
      if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos) then exit;
      Parts[ppDefault].StartPos:=CurPos.StartPos;
      if not ReadConstant(false,false,[]) then exit;
      Parts[ppDefault].EndPos:=LastAtoms.GetValueAt(0).EndPos;
    end else if UpAtomIs('IMPLEMENTS') then begin
      if not ReadSimpleSpec(ppImplementsWord,ppImplements) then exit;
    end else if UpAtomIs('NODEFAULT') then begin
      if Parts[ppNoDefaultWord].StartPos>=1 then exit;
      Parts[ppNoDefaultWord]:=CurPos;
      ReadNextAtom;
    end else
      exit;
  end;
  if (CurPos.StartPos>PropNode.EndPos) then exit;
  PropType:=copy(Src,Parts[ppType].StartPos,
               Parts[ppType].EndPos-Parts[ppType].StartPos);
  // check read specifier
  if (Parts[ppReadWord].StartPos>0) or (Parts[ppWriteWord].StartPos<1) then
  begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] read specifier needed');
{$ENDIF}
    AccessParamPrefix:=
      ASourceChangeCache.BeautifyCodeOptions.PropertyReadIdentPrefix;
    if Parts[ppRead].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppRead].StartPos,
          Parts[ppRead].EndPos-Parts[ppRead].StartPos)
    else
      AccessParam:='';
    if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
    or (AnsiCompareText(AccessParamPrefix,
            LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
    begin
      // the read identifier is a function
      if Parts[ppRead].StartPos<1 then
        AccessParam:=AccessParamPrefix+copy(Src,Parts[ppName].StartPos,
            Parts[ppName].EndPos-Parts[ppName].StartPos);
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
      // check if function exists
      if not ProcExists(CleanAccessFunc) then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] CleanAccessFunc ',CleanAccessFunc,' does not exist');
{$ENDIF}
        // add insert demand for function
        // build function code
        if (Parts[ppParamList].StartPos>0) then begin
          MoveCursorToCleanPos(Parts[ppParamList].StartPos);
          ReadNextAtom;
          InitExtraction;
          if not ReadParamList(false,true,[phpWithParameterNames,
                               phpWithoutBrackets,phpWithVarModifiers,
                               phpWithComments])
          then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] Error reading param list');
{$ENDIF}
            exit;
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
        AddInsert(PropNode,CleanAccessFunc,AccessFunc,AccessParam);
      end;
    end else begin
      if Parts[ppRead].StartPos<1 then
        AccessParam:=ASourceChangeCache.BeautifyCodeOptions.PrivatVariablePrefix
             +copy(Src,Parts[ppName].StartPos,
               Parts[ppName].EndPos-Parts[ppName].StartPos);
      // the read identifier is a variable
      if not VarExists(UpperCaseStr(AccessParam)) then begin
        // variable does not exist yet -> add insert demand for variable
        AddInsert(PropNode,UpperCaseStr(AccessParam),
                  AccessParam+':'+PropType+';',AccessParam);
      end;
    end;
    if Parts[ppRead].StartPos<0 then begin
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
           ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('read')
           +' '+AccessParam);
      end;
    end;
  end;
  // check write specifier
  if (Parts[ppWriteWord].StartPos>0) or (Parts[ppReadWord].StartPos<1) then
  begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] write specifier needed');
{$ENDIF}
    AccessParamPrefix:=
      ASourceChangeCache.BeautifyCodeOptions.PropertyWriteIdentPrefix;
    if Parts[ppWrite].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppWrite].StartPos,
            Parts[ppWrite].EndPos-Parts[ppWrite].StartPos)
    else
      AccessParam:=AccessParamPrefix+copy(Src,Parts[ppName].StartPos,
            Parts[ppName].EndPos-Parts[ppName].StartPos);
    if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
    or (AnsiCompareText(AccessParamPrefix,
            LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
    begin
      // the write identifier is a procedure
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
      // check if procedure exists
      if not ProcExists(CleanAccessFunc) then begin
        // add insert demand for function
        // build function code
        if (Parts[ppParamList].StartPos>0) then begin
          MoveCursorToCleanPos(Parts[ppParamList].StartPos);
          ReadNextAtom;
          InitExtraction;
          if not ReadParamList(false,true,[phpWithParameterNames,
                               phpWithoutBrackets,phpWithVarModifiers,
                               phpWithComments])
          then
            exit;
          ParamList:=GetExtraction;
          if (Parts[ppIndexWord].StartPos<1) then begin
            // param list, no index
            AccessFunc:='procedure '+AccessParam
                        +'('+ParamList+';const AValue: '+PropType+');';
          end else begin
            // index + param list
            AccessFunc:='procedure '+AccessParam
                        +'(Index:integer;'+ParamList+';'
                        +'const AValue: '+PropType+');';
          end;
        end else begin
          if (Parts[ppIndexWord].StartPos<1) then begin
            // no param list, no index
            AccessFunc:='procedure '+AccessParam
                        +'(const AValue: '+PropType+');';
          end else begin
            // index, no param list
            AccessFunc:='procedure '+AccessParam
                        +'(Index:integer; const AValue: '+PropType+');';
          end;
        end;
        // add new Insert Node
        AddInsert(PropNode,CleanAccessFunc,AccessFunc,AccessParam);
      end;
    end else begin
      // the write identifier is a variable
      if not VarExists(UpperCaseStr(AccessParam)) then begin
        // variable does not exist yet -> add insert demand for variable
        AddInsert(PropNode,UpperCaseStr(AccessParam),
                  AccessParam+':'+PropType+';',AccessParam);
      end;
    end;
    if Parts[ppWrite].StartPos<0 then begin
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
           ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('write')
           +' '+AccessParam);
      end;
    end;
  end;
  // check stored specifier
  if (Parts[ppStoredWord].StartPos>0) then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] stored specifier needed');
{$ENDIF}
    if Parts[ppStored].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppStored].StartPos,
            Parts[ppStored].EndPos-Parts[ppStored].StartPos)
    else
      AccessParam:=
        ASourceChangeCache.BeautifyCodeOptions.PropertyStoredFunction;
    CleanAccessFunc:=UpperCaseStr(AccessParam);
    // check if procedure exists
    if (not ProcExists(CleanAccessFunc)) and (not VarExists(CleanAccessFunc))
    then begin
      // add insert demand for function
      // build function code
      AccessFunc:='function '+AccessParam+':boolean;';
      // add new Insert Node
      AddInsert(PropNode,CleanAccessFunc,AccessFunc,AccessParam);
    end;
    if Parts[ppStored].StartPos<0 then begin
      // insert stored specifier
      InsertPos:=Parts[ppStoredWord].EndPos;
      ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           AccessParam);
    end;
  end;
  Result:=true;
end;

procedure TCodeCompletionCodeTool.InsertNewClassParts(PartType: NewClassPart);
var ANodeExt: TCodeTreeNodeExtension;
  PrivatNode, ANode, InsertNode: TCodeTreeNode;
  Indent, InsertPos: integer;
  CurCode: string;
begin
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if ((PartType=ncpVars)=NodeExtIsVariable(ANodeExt)) then begin
      // search a privat section in front of the node
      PrivatNode:=ANodeExt.Node.Parent.PriorBrother;
      while (PrivatNode<>nil) and (PrivatNode.Desc<>ctnClassPrivate) do
        PrivatNode:=PrivatNode.PriorBrother;
      if PrivatNode=nil then begin
        // there is no privat section node in front of the property
        if NewPrivatSectionInsertPos<1 then begin
          // -> insert one at the end of the first published node
          // Note: the first node is a fake published section, so the first
          // real section is the second
          ANode:=ClassNode.FirstChild.NextBrother;
          if ANode=nil then ANode:=ClassNode;
          NewPrivatSectionIndent:=GetLineIndent(Src,ANode.StartPos);
          ANode:=ClassNode.FirstChild;
          if (ANode.FirstChild=nil) and (ANode.NextBrother<>nil)
          and (ANode.NextBrother.Desc=ctnClassPublished) then
            ANode:=ANode.NextBrother;
          NewPrivatSectionInsertPos:=ANode.EndPos;
          ASourceChangeCache.Replace(gtNewLine,gtNewLine,
            NewPrivatSectionInsertPos,NewPrivatSectionInsertPos,
            ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord(
              'private'));
        end;
        Indent:=NewPrivatSectionIndent
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
        InsertPos:=NewPrivatSectionInsertPos;
      end else begin
        // there is a privat section in front of the property
        InsertNode:=nil;
        ANode:=PrivatNode.FirstChild;
        if PartType=ncpProcs then begin
          while (ANode<>nil) and (ANode.Desc=ctnVarDefinition) do begin
            InsertNode:=ANode;
            ANode:=ANode.NextBrother;
          end;
        end;
        case ASourceChangeCache.BeautifyCodeOptions.ClassPartInsertPolicy of
          cpipAlphabetically:
            begin
              while ANode<>nil do begin
                if (PartType=ncpVars) then begin
                  if (CompareNodeSrc(ANode,ANodeExt.Txt)>0) then
                    break;
                end else begin
                  case ANode.Desc of
                    ctnProcedure:
                      begin
                        CurCode:=ExtractProcName(ANode,false);
                        if AnsiCompareStr(CurCode,ANodeExt.ExtTxt2)>0 then
                          break;
                      end;
                    ctnProperty:
                      begin
                        CurCode:=ExtractPropName(ANode,false);
                        if AnsiCompareStr(CurCode,ANodeExt.ExtTxt2)>0 then
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
              if ANode.Desc<>ctnVarDefinition then break;
              InsertNode:=ANode;
              ANode:=ANode.NextBrother;
            end;
          end
        end;
        if InsertNode<>nil then begin
          // insert after InsertNode
          Indent:=GetLineIndent(Src,InsertNode.StartPos);
          InsertPos:=FindFirstLineEndAfterInCode(Src,InsertNode.EndPos,
                       Scanner.NestedComments);
        end else begin
          // insert as first variable
          Indent:=GetLineIndent(Src,PrivatNode.StartPos)
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
          InsertPos:=FindFirstLineEndAfterInCode(Src,PrivatNode.StartPos,
                       Scanner.NestedComments);
        end;
      end;
      CurCode:=ANodeExt.ExtTxt1;
      CurCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                          CurCode,0);
      ASourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
         GetIndentStr(Indent)+CurCode);
    end;
    ANodeExt:=ANodeExt.Next;
  end;
end;
  
function TCodeCompletionCodeTool.InsertAllNewClassParts: boolean;
begin
  if FirstInsert=nil then begin
    Result:=true;
    exit;
  end;
  NewPrivatSectionInsertPos:=-1;
  InsertNewClassParts(ncpVars);
  InsertNewClassParts(ncpProcs);
  Result:=true;
end;

function TCodeCompletionCodeTool.CreateMissingProcBodies: boolean;
var
  Indent, InsertPos: integer;
  TheClassName: string;
   
  procedure InsertProcBody(ANodeExt: TCodeTreeNodeExtension);
  var ProcCode: string;
  begin
    ProcCode:=ANodeExt.ExtTxt1;
    ProcCode:=ASourceChangeCache.BeautifyCodeOptions.AddClassNameToProc(
                 ProcCode,TheClassName);
writeln('>>> InsertProcBody ',TheClassName,' "',ProcCode,'"');
    ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                 ProcCode,Indent,true);
    ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
      ProcCode);
    if JumpToProc='' then begin
      // remember a proc body to set the cursor at
      JumpToProc:=UpperCaseStr(TheClassName)+'.'+ANodeExt.Txt;
    end;
  end;

var
  ProcBodyNodes, ClassProcs: TAVLTree;
  ANodeExt, NewNodeExt: TCodeTreeNodeExtension;
  ExistingNode, MissingNode: TAVLTreeNode;
  cmp: integer;
  FirstExistingProcBody, LastExistingProcBody, ImplementationNode,
  ANode, TypeSectionNode: TCodeTreeNode;
  ClassStartComment, ProcCode: string;
begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CreateMissingProcBodies Gather existing method bodies ... ');
{$ENDIF}
  // gather existing class proc bodies
  TypeSectionNode:=ClassNode.Parent;
  if (TypeSectionNode<>nil) and (TypeSectionNode.Parent<>nil)
  and (TypeSectionNode.Parent.Desc=ctnTypeSection) then
    TypeSectionNode:=TypeSectionNode.Parent;
  ClassProcs:=nil;
  ProcBodyNodes:=GatherProcNodes(TypeSectionNode,
     [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
     ExtractClassName(ClassNode,true));
  try
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

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CreateMissingProcBodies Gather existing method declarations ... ');
{$ENDIF}
    TheClassName:=ExtractClassName(ClassNode,false);

    // gather existing class proc definitions
    ClassProcs:=GatherProcNodes(StartNode,[phpInUpperCase,phpAddClassName],
       ExtractClassName(ClassNode,true));
    // add new class parts to ClassProcs
    CurNode:=FirstExistingProcBody;
    ANodeExt:=FirstInsert;
    while ANodeExt<>nil do begin
      if not NodeExtIsVariable(ANodeExt) then begin
        if FindNodeInTree(ClassProcs,ANodeExt.Txt)=nil then begin
          NewNodeExt:=TCodeTreeNodeExtension.Create;
          with NewNodeExt do begin
            Txt:=UpperCaseStr(TheClassName)+'.'
                  +ANodeExt.Txt;       // Name+ParamTypeList
            ExtTxt1:=ASourceChangeCache.BeautifyCodeOptions.AddClassNameToProc(
               ANodeExt.ExtTxt1,TheClassName); // complete proc head code
          end;
          ClassProcs.Add(NewNodeExt);
        end;
      end;
      ANodeExt:=ANodeExt.Next;
    end;


    // search for missing proc bodies
    ExistingNode:=ProcBodyNodes.FindHighest;
    MissingNode:=ClassProcs.FindHighest;
    if ExistingNode=nil then begin
      // there were no old proc bodies of the class
      if NodeHasParentOfType(ClassNode,ctnInterface) then begin
        // class is in interface section
        // -> insert at the end of the implementation section
        ImplementationNode:=FindImplementationNode;
        if ImplementationNode=nil then exit;
        Indent:=GetLineIndent(Src,ImplementationNode.StartPos);
        InsertPos:=ImplementationNode.EndPos;
      end else begin
        // class is not in interface section
        // -> insert at the end of the type section
        ANode:=ClassNode.Parent; // type definition
        if ANode=nil then exit;
        if ANode.Parent.Desc=ctnTypeSection then
          ANode:=ANode.Parent; // type section
        if ANode=nil then exit;
        Indent:=GetLineIndent(Src,ANode.StartPos);
        InsertPos:=ANode.EndPos;
      end;
      // insert class comment
      ClassStartComment:=GetIndentStr(Indent)
                          +'{ '+ExtractClassName(ClassNode,false)+' }';
      ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
         ClassStartComment);
      // insert all missing proc bodies
      while (MissingNode<>nil) do begin
        ANodeExt:=TCodeTreeNodeExtension(MissingNode.Data);
        ProcCode:=ANodeExt.ExtTxt1;
        if (ProcCode='') then begin
          ANode:=TCodeTreeNodeExtension(MissingNode.Data).Node;
          if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
            ProcCode:=ExtractProcHead(ANode,[phpWithStart,phpAddClassname,
                 phpWithParameterNames,phpWithResultType,phpWithVarModifiers]);
          end;
        end;
        if ProcCode<>'' then begin
          ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                     ProcCode,Indent,true);
          ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,
            InsertPos,ProcCode);
          if JumpToProc='' then begin
            // remember a proc body to set the cursor at
            JumpToProc:=ANodeExt.Txt;
          end;
        end;
        MissingNode:=ProcBodyNodes.FindPrecessor(MissingNode);
      end;
    end else begin
      // there were old class procs already
      // -> search a good Insert Position behind or in front of
      //    another proc body of this class
      if ASourceChangeCache.BeautifyCodeOptions.ProcedureInsertPolicy
        <>pipAlphabetically then 
      begin
        Indent:=GetLineIndent(Src,LastExistingProcBody.StartPos);
        InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                        LastExistingProcBody.EndPos,Scanner.NestedComments);
      end;
      while (MissingNode<>nil) do begin
        if ExistingNode<>nil then
          cmp:=CompareTextIgnoringSpace(
                 TCodeTreeNodeExtension(MissingNode.Data).Txt,
                 TCodeTreeNodeExtension(ExistingNode.Data).Txt,true)
        else
          cmp:=1;
        if cmp>0 then begin
          // MissingNode does not have a body -> insert proc body
          case ASourceChangeCache.BeautifyCodeOptions.ProcedureInsertPolicy of
          pipAlphabetically:
            if ExistingNode<>nil then begin
              // insert behind ExistingNode
              ANodeExt:=TCodeTreeNodeExtension(ExistingNode.Data);
              ANode:=ANodeExt.Node;
              Indent:=GetLineIndent(Src,ANode.StartPos);
              InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                            ANode.EndPos,Scanner.NestedComments);
            end else begin
              // insert behind last existing proc body
              Indent:=GetLineIndent(Src,LastExistingProcBody.StartPos);
              InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                          LastExistingProcBody.EndPos,Scanner.NestedComments);
            end;
          end;
          ANodeExt:=TCodeTreeNodeExtension(MissingNode.Data);
          ProcCode:=ANodeExt.ExtTxt1;
          if (ProcCode='') then begin
            ANode:=ANodeExt.Node;
            if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
              ProcCode:=ExtractProcHead(ANode,[phpWithStart,phpAddClassname,
               phpWithParameterNames,phpWithResultType,phpWithVarModifiers]);
            end;
          end;
          if (ProcCode<>'') then begin
            ProcCode:=
              ASourceChangeCache.BeautifyCodeOptions.AddClassNameToProc(
                ProcCode,TheClassName);
            ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                        ProcCode,Indent,true);
            ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
                  InsertPos,InsertPos,ProcCode);
            if JumpToProc='' then begin
              // remember a proc body to set the cursor at
              JumpToProc:=ANodeExt.Txt;
            end;
          end;
          MissingNode:=ProcBodyNodes.FindPrecessor(MissingNode);
        end else if cmp<0 then
          ExistingNode:=ProcBodyNodes.FindPrecessor(ExistingNode)
        else
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
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var CleanCursorPos, Dummy, Indent, insertPos: integer;
  CursorNode, ProcNode, ImplementationNode, SectionNode,
  ANode: TCodeTreeNode;
  ProcCode: string;
  ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  if (SourceChangeCache=nil) then exit;
  // in a class or in a forward proc?
  BuildTree(false);
  if not EndOfSourceFound then exit;
  ASourceChangeCache:=SourceChangeCache;
  SourceChangeCache.MainScanner:=Scanner;
  // find the CursorPos in cleaned source
  Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (Dummy<>0) and (Dummy<>-1) then exit;
  // find CodeTreeNode at cursor
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos);
  if CursorNode=nil then
    exit;
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode A ',NodeDescriptionAsString(CursorNode.Desc));
{$ENDIF}
  ImplementationNode:=FindImplementationNode;
  if ImplementationNode=nil then ImplementationNode:=Tree.Root;
  FirstInsert:=nil;

  // first test if in a class
  ClassNode:=CursorNode;
  while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
    ClassNode:=ClassNode.Parent;
  if ClassNode<>nil then begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode In-a-class ',NodeDescriptionAsString(ClassNode.Desc));
{$ENDIF}
    // cursor is in class/object definition
    if CursorNode.SubDesc=ctnsForwardDeclaration then exit;
    // parse class and build CodeTreeNodes for all properties/methods
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode C ',CleanCursorPos,', |',copy(Src,CleanCursorPos,8));
{$ENDIF}
    BuildSubTreeForClass(ClassNode);
    StartNode:=ClassNode.FirstChild;
    while (StartNode<>nil) and (StartNode.FirstChild=nil) do
      StartNode:=StartNode.NextBrother;
    if StartNode=nil then exit;
    StartNode:=StartNode.FirstChild;
    JumpToProc:='';
    try
      // go through all properties and procs
      //  insert read + write prop specifiers
      //  demand Variables + Procs + Proc Bodies
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Complete Properties ... ');
{$ENDIF}
      SectionNode:=ClassNode.FirstChild;
      while SectionNode<>nil do begin
        ANode:=SectionNode.FirstChild;
        while ANode<>nil do begin
          if ANode.Desc=ctnProperty then begin
            // check if property is complete
            if not CompleteProperty(ANode) then exit;
          end;
          ANode:=ANode.NextBrother;
        end;
        SectionNode:=SectionNode.NextBrother;
      end;

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Insert new variables and methods ... ');
{$ENDIF}
      // insert all new variables and procs definitions
      if not InsertAllNewClassParts then exit;

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Insert new method bodies ... ');
{$ENDIF}
      // insert all missing proc bodies
      if not CreateMissingProcBodies then exit;

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Apply ... ');
{$ENDIF}
      // apply the changes and jump to first new proc body
      if not SourceChangeCache.Apply then exit;

      if JumpToProc<>'' then begin
        // there was a new proc body
        // -> find it and jump to

        // reparse code
        BuildTree(false);
        if not EndOfSourceFound then exit;
        // find the CursorPos in cleaned source
        Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
        if (Dummy<>0) and (Dummy<>-1) then exit;
        // find CodeTreeNode at cursor
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos);
        if CursorNode=nil then exit;

        ClassNode:=CursorNode;
        while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
          ClassNode:=ClassNode.Parent;
        if ClassNode=nil then exit;
        ANode:=ClassNode.Parent;
        if ANode=nil then exit;
        if (ANode.Parent<>nil) and (ANode.Parent.Desc=ctnTypeSection) then
          ANode:=ANode.Parent;
        ProcNode:=FindProcNode(ANode,JumpToProc,
                   [phpInUpperCase,phpIgnoreForwards]);
        if ProcNode=nil then exit;
        Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
        exit;
      end else begin
        // there was no new proc body
        // -> adjust cursor
        NewPos:=CursorPos;
        NewPos.Code.AdjustCursor(NewPos.Y,NewPos.X);
        NewTopLine:=NewPos.Y-(VisibleEditorLines div 2);
        if NewTopLine<1 then NewTopLine:=1;
        Result:=true;
        exit;
      end;

    finally
      // dispose all new variables/procs definitions
      while FirstInsert<>nil do begin
        ANodeExt:=FirstInsert;
        FirstInsert:=FirstInsert.Next;
        NodeExtMemManager.DisposeNode(ANodeExt);
      end;
    end;
    
  end else begin
    // then test if forward proc
    ProcNode:=CursorNode;
    if ProcNode.Desc=ctnProcedureHead then ProcNode:=ProcNode.Parent;
    if (ProcNode.Desc=ctnProcedure)
    and (ProcNode.SubDesc=ctnsForwardDeclaration) then begin
      // Node is forward Proc
        
      // check if proc already exists
      ProcCode:=ExtractProcHead(ProcNode,[phpInUpperCase]);
      if FindProcNode(FindNextNodeOnSameLvl(ProcNode),ProcCode,
             [phpInUpperCase])<>nil
      then exit;
        
      // -> create proc body at end of implementation

      Indent:=GetLineIndent(Src,ImplementationNode.StartPos);
      if ImplementationNode.Desc=ctnImplementation then
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
           ImplementationNode.EndPos,Scanner.NestedComments)
      else begin
        // insert in front of main program begin..end.
        StartNode:=ImplementationNode.LastChild;
        if (StartNode=nil) or (StartNode.Desc<>ctnBeginBlock) then exit;
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,StartNode.StartPos,
           Scanner.NestedComments);
      end;

      // build nice proc
      ProcCode:=ExtractProcHead(ProcNode,[phpWithStart,phpWithVarModifiers,
                  phpWithParameterNames,phpWithResultType,phpWithComments]);
      if ProcCode='' then exit;
      ProcCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyProc(ProcCode,
                         Indent,true);
      if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
        InsertPos,InsertPos,ProcCode) then exit;
      if not SourceChangeCache.Apply then exit;
        
      // reparse code and find jump point into new proc
      Result:=FindJumpPoint(CursorPos,NewPos,NewTopLine);
      exit;
    end;
  end;
end;


//=============================================================================

procedure InternalInit;
begin
  NodeMemManager:=TCodeTreeNodeMemManager.Create;
  NodeExtMemManager:=TCodeTreeNodeExtMemManager.Create;
end;

procedure InternalFinal;
begin
//writeln('codetools.pp - InternalFinal Nodes: Count=',NodeMemManager.Count
//,' Free=',NodeMemManager.FreeCount
//,' Allocated=',NodeMemManager.AllocatedNodes
//,' Freed=',NodeMemManager.FreedNodes);
  NodeExtMemManager.Free;
  NodeMemManager.Free;
end;


initialization
  InternalInit;

finalization
{$IFDEF CTDEBUG}
writeln('codetools.pp - finalization');
{$ENDIF}
{$IFDEF MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  InternalFinal;

end.

