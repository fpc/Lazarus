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
    A tool to help converting C header files to pascal bindings.
    
    enum            ->  enum
    int i           ->  var i: integer
    struct          ->  var plus record
    union           ->  var plus record case
    typedef         ->  type
    void func()     -> procedure
    int func()      -> function
    implicit types  -> explicit types
    #ifdef,if,ifndef,undef,elif,else,endif
                    ->  $ifdef,if,ifndef,...
    #define macroname
                    -> $define macroname
    #define macroname constant
                    -> const macroname = constant

  ToDos:
    add comments for skipped items
    insert auto generated types in front of current node
    c comments
    const char a; -> const a: char;
    simplify conditional directives (e.g. #ifdef)
    #define name value  ->  alias  (const, var, type, proc)
    more complex expressions and statements
}
unit H2PasTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolsStructs, AVL_Tree,
  FileProcs, BasicCodeTools, CCodeParserTool,
  NonPascalCodeTools, KeywordFuncLists, CodeCache,
  CodeTree, CodeAtom;
  
const
  DefaultMaxPascalIdentLen = 70;
  
  h2pdnBase     = 1000;
  h2pdnNone     =  0+h2pdnBase;
  h2pdnRoot     =  1+h2pdnBase;

  h2pdnDefine   = 11+h2pdnBase;
  h2pdnUndefine = 12+h2pdnBase;

  h2pdnIf       = 21+h2pdnBase;
  h2pdnIfDef    = 22+h2pdnBase;
  h2pdnIfNDef   = 23+h2pdnBase;
  h2pdnElseIf   = 24+h2pdnBase;
  h2pdnElse     = 25+h2pdnBase;
  h2pdnEndIf    = 26+h2pdnBase;

  h2pdnError    = 31+h2pdnBase;

type
  { TH2PBaseNode }

  TH2PBaseNode = class
  public
    Parent, FirstChild, LastChild, NextBrother, PriorBrother: TH2PBaseNode;
    function Next: TH2PBaseNode;
    function NextSkipChilds: TH2PBaseNode;
    function Prior: TH2PBaseNode;
    function HasAsParent(Node: TH2PBaseNode): boolean;
    function HasAsChild(Node: TH2PBaseNode): boolean;
    function GetLevel: integer;
    function DescAsString(CTool: TCCodeParserTool = nil): string; virtual; abstract;
    procedure ConsistencyCheck; virtual;
    procedure WriteDebugReport(const Prefix: string; WithChilds: boolean;
                               CTool: TCCodeParserTool = nil); virtual;
  end;

  TH2PNode = class;

  { TH2PDirectiveNode }

  TH2PDirectiveNode = class(TH2PBaseNode)
  public
    H2PNode: TH2PNode;
    Desc: TCodeTreeNodeDesc;// e.g. h2pdnDefine
    MacroName: string; // ifdef, ifndef, undef, define
    MacroParams: string; // define
    Expression: string; // if, elseif, define, error
    function DescAsString(CTool: TCCodeParserTool = nil): string; override;
  end;
  

  { TH2PNode }

  TH2PNode = class(TH2PBaseNode)
  public
    PascalName: string;
    CName: string;
    CNode: TCodeTreeNode;
    PascalDesc: TCodeTreeNodeDesc;
    PascalCode: string;
    NormalizedPascalCode: string;
    Directive: TH2PDirectiveNode;
    function DescAsString(CTool: TCCodeParserTool = nil): string; override;
  end;
  
  
  { TH2PTree }

  TH2PTree = class
  private
    FNodeCount: integer;
    procedure Unbind(Node: TH2PBaseNode);
  public
    Root: TH2PBaseNode;
    LastRoot: TH2PBaseNode;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property NodeCount: integer read FNodeCount;
    procedure DeleteNode(ANode: TH2PBaseNode);
    procedure AddNodeAsLastChild(ParentNode, ANode: TH2PBaseNode);
    procedure AddNodeAsPreLastChild(ParentNode, ANode: TH2PBaseNode);
    procedure AddNodeInFrontOf(NextBrotherNode, ANode: TH2PBaseNode);
    procedure MoveChildsInFront(ANode: TH2PBaseNode);
    function ContainsNode(ANode: TH2PBaseNode): boolean;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(WithChilds: boolean);
  end;
  

  TH2PMacroStatus = (
    hmsUnknown,   // never seen
    hmsDefined,   // set to a specific value e.g. by $Define or by $IfDef
    hmsUndefined, // undefined e.g. by $Undef
    hmsComplex    // value depends on complex expressions. e.g. {$if A or B}.
    );

  TH2PMacroStats = class
  public
    Name: string;
    Value: string;
    Status: TH2PMacroStatus;
    LastDefineNode: TH2PNode;// define or undef node
    LastReadNode: TH2PNode;// if node
  end;


  TIgnoreCSourcePart = (
    icspInclude
    );
  TIgnoreCSourceParts = set of TIgnoreCSourcePart;

  { TH2PasTool }

  TH2PasTool = class
  private
    FCNames: TAVLTree;// tree of TH2PNode sorted for CName
    FCurDirectiveNode: TH2PDirectiveNode;
    FCurIndentStr: string;
    FCurPasSection: TCodeTreeNodeDesc;
    FCurPasStream: TStream;
    FDefines: TStringToStringTree;
    FDisableUnusedDefines: boolean;
    FIgnoreCParts: TIgnoreCSourceParts;
    FPascalNames: TAVLTree;// tree of TH2PNode sorted for PascalName
    FPredefinedCTypes: TStringToStringTree;
    FRemoveDisabledDirectives: boolean;
    FSimplifyExpressions: boolean;
    FSourceName: string;
    FUndefines: TStringToStringTree;
    // converting C nodes to H2P nodes
    procedure ConvertStruct(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertVariable(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertEnumBlock(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertFunction(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertFuncParameter(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertTypedef(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertDirective(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    function ConvertCToPascalDirectiveExpression(const CCode: string;
           StartPos, EndPos: integer; out PasExpr: string;
           out ErrorPos: integer; out ErrorMsg: string): boolean;

    // writing pascal
    procedure WriteStr(const Line: string);
    procedure WriteLnStr(const Line: string);
    procedure W(const aStr: string);// write indent + aStr + lineend
    procedure IncIndent;
    procedure DecIndent;
    procedure SetPasSection(NewSection: TCodeTreeNodeDesc);
    procedure WriteGlobalVarNode(H2PNode: TH2PNode);
    procedure WriteGlobalTypeNode(H2PNode: TH2PNode);
    procedure WriteGlobalConstNode(H2PNode: TH2PNode);
    procedure WriteGlobalProcedureNode(H2PNode: TH2PNode);
    procedure WriteGlobalEnumerationTypeNode(H2PNode: TH2PNode);
    procedure WriteGlobalRecordTypeNode(H2PNode: TH2PNode);
    procedure WriteDirectiveNode(DirNode: TH2PDirectiveNode);
    function CreateDirectiveValue(const s: string): string;

    // simplification
    procedure SimplifyUndefineDirective(Node: TH2PDirectiveNode;
                                        var NextNode: TH2PDirectiveNode;
                                        var Changed: boolean);
    procedure SimplifyDefineDirective(Node: TH2PDirectiveNode;
                                      var NextNode: TH2PDirectiveNode;
                                      var Changed: boolean);
    procedure SimplifyIfDirective(Node: TH2PDirectiveNode; Expression: string;
                                  var NextNode: TH2PDirectiveNode;
                                  var Changed: boolean);
    function SimplifyIfDirectiveExpression(var Expression: string): boolean;
    procedure SimplifyMacroRedefinition(var Node: TH2PDirectiveNode;
                         const NewValue: string; NewStatus: TH2PMacroStatus;
                         var NextNode: TH2PDirectiveNode; var Changed: boolean);
    procedure SimplifyUnusedDefines(Changed: boolean);
    function MacroValueIsConstant(Node: TH2PDirectiveNode;
                                  out PasType, PasExpression: string): boolean;
    procedure DeleteDirectiveNode(Node: TH2PDirectiveNode;
                                  DeleteChilds: boolean;
                                  AdaptNeighborhood: boolean);
    procedure DeleteH2PNode(Node: TH2PNode);
  public
    Tree: TH2PTree; // TH2PNode
    DirectivesTree: TH2PTree; // TH2PDirectiveNode
    CTool: TCCodeParserTool;
    Macros: TAVLTree;// tree of TH2PMacroStats
    function Convert(CCode, PascalCode: TCodeBuffer): boolean;
    procedure BuildH2PTree(ParentNode: TH2PNode = nil; StartNode: TCodeTreeNode = nil);
    function FindEnclosingIFNDEF(CCode: TCodeBuffer): TCodeTreeNode;
    procedure UndefineEnclosingIFNDEF(CCode: TCodeBuffer);
    procedure SimplifyDirectives;
    procedure WritePascal(PascalCode: TCodeBuffer);
    procedure WritePascalToStream(s: TStream);
    
    function GetSimplePascalTypeOfCVar(CVarNode: TCodeTreeNode): string;
    function GetSimplePascalTypeOfCParameter(CParamNode: TCodeTreeNode): string;
    function GetSimplePascalResultTypeOfCFunction(CFuncNode: TCodeTreeNode): string;
    function ConvertSimpleCTypeToPascalType(CType: string;
                  UseSingleIdentifierAsDefault: boolean): string;
    
    function CreateH2PNode(var PascalName: string; const CName: string;
       CNode: TCodeTreeNode;
       PascalDesc: TCodeTreeNodeDesc; const PascalCode: string;
       ParentNode: TH2PNode = nil; IsGlobal: boolean = true;
       InsertAsPreLast: boolean = false): TH2PNode;
    function CreateAutoGeneratedH2PNode(var PascalName: string; CNode: TCodeTreeNode;
       PascalDesc: TCodeTreeNodeDesc; const PascalCode: string;
       ParentNode: TH2PNode; IsGlobal: boolean;
       InsertAsPreLast: boolean): TH2PNode;
    function GetH2PNodeForComplexType(CNode: TCodeTreeNode;
                               CreateIfNotExists: boolean;
                               InsertAsPreLast: boolean): TH2PNode;
    function CreatePascalNameFromCCode(const CCode: string;
                                       StartPos: integer = 1;
                                       EndPos: integer = -1): string;
    function CreateUniquePascalName(const CName: string): string;
    function FindH2PNodeWithPascalName(const PascalName: string): TH2PNode;
    function FindH2PNodeWithCName(const CName: string): TH2PNode;

    function CreateH2PDirectiveNode(H2PNode: TH2PNode; Desc: TCodeTreeNodeDesc
                                    ): TH2PDirectiveNode;

    procedure WriteDebugReport;
    procedure WriteH2PNodeReport;
    procedure WriteH2PDirectivesNodeReport;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property PredefinedCTypes: TStringToStringTree read FPredefinedCTypes;
    property IgnoreCParts: TIgnoreCSourceParts read FIgnoreCParts write FIgnoreCParts;
    property SourceName: string read FSourceName write FSourceName;

    // directives
    property SimplifyExpressions: boolean read FSimplifyExpressions
                                          write FSimplifyExpressions;
    property DisableUnusedDefines: boolean read FDisableUnusedDefines
                                           write FDisableUnusedDefines;
    property RemoveDisabledDirectives: boolean read FRemoveDisabledDirectives
                                               write FRemoveDisabledDirectives;
    property Defines: TStringToStringTree read FDefines;
    property Undefines: TStringToStringTree read FUndefines;// undefines take precedence over defines
    procedure AddCommonCDefines;

    // macros - temporary values - use Defines and Undefines
    procedure ResetMacros;
    procedure ClearMacros;
    procedure InitMacros;
    function FindMacro(const MacroName: string;
                       CreateIfNotExists: boolean = false): TH2PMacroStats;
    function DefineMacro(const MacroName, AValue: string;
                         DefineNode: TH2PNode): TH2PMacroStats;// use Defines instead
    function UndefineMacro(const MacroName: string;
                           UndefineNode: TH2PNode): TH2PMacroStats;// use Undefines instead
    procedure MarkMacrosAsRead(Node: TH2PNode; const Src: string;
                               StartPos: integer = 1; EndPos: integer = -1);
    function MarkMacroAsRead(const MacroName: string;
                             Node: TH2PNode): TH2PMacroStats;// use Undefines instead
  end;
  
  
function DefaultPredefinedCTypes: TStringToStringTree;// types in unit ctypes

function CompareH2PNodePascalNames(Data1, Data2: Pointer): integer;
function CompareStringWithH2PNodePascalName(AString, ANode: Pointer): integer;
function CompareH2PNodeCNames(Data1, Data2: Pointer): integer;
function CompareStringWithH2PNodeCName(AString, ANode: Pointer): integer;
function CompareH2PMacroStats(Data1, Data2: Pointer): integer;
function ComparePCharWithH2PMacroStats(Name, MacroStats: Pointer): integer;

function H2PDirectiveNodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;


implementation


var
  InternalPredefinedCTypes: TStringToStringTree = nil;// types in unit ctypes

function DefaultPredefinedCTypes: TStringToStringTree;
begin
  if InternalPredefinedCTypes=nil then begin
    InternalPredefinedCTypes:=TStringToStringTree.Create(true);
    with InternalPredefinedCTypes do begin
      // int
      Add('int','cint');
      Add('int*','pcint');
      Add('signed int','csint');
      Add('signed int*','pcsint');
      Add('unsigned int','cuint');
      Add('unsigned int*','pcuint');
      // ToDo: signed -> cint
      // ToDo: unsigned -> cuint
      // short
      Add('short','cshort');
      Add('short*','pcshort');
      Add('signed short','csshort');
      Add('signed short*','pcsshort');
      Add('unsigned short','cushort');
      Add('unsigned short*','pcushort');
      Add('short int','cshort');
      Add('short int*','pcshort');
      Add('signed short int','csshort');
      Add('signed short int*','pcsshort');
      Add('short signed int','csshort');
      Add('short signed int*','pcsshort');
      Add('short unsigned int','cushort');
      Add('short unsigned int*','pcushort');
      // int8
      Add('int8','cint8');
      Add('int8*','pcint8');
      Add('int8_t','cint8');
      Add('int8_t*','pcint8');
      Add('unsigned int8','cuint8');
      Add('unsigned int8*','pcuint8');
      Add('uint8_t','cuint8');
      Add('uint8_t*','pcuint8');
      // int16
      Add('int16','cint16');
      Add('int16*','pcint16');
      Add('int16_t','cint16');
      Add('int16_t*','pcint16');
      Add('unsigned int16','cuint16');
      Add('unsigned int16*','pcuint16');
      Add('uint16_t','cuint16');
      Add('uint16_t*','pcuint16');
      // int32
      Add('int32','cint32');
      Add('int32*','pcint32');
      Add('int32_t','cint32');
      Add('int32_t*','pcint32');
      Add('unsigned int32','cuint32');
      Add('unsigned int32*','pcuint32');
      Add('uint32_t','cuint32');
      Add('uint32_t*','pcuint32');
      // int64
      Add('int64','cint64');
      Add('int64*','pcint64');
      Add('int64_t','cint64');
      Add('int64_T*','pcint64');
      Add('unsigned int64','cuint64');
      Add('unsigned int64*','pcuint64');
      Add('uint64_t','cuint64');
      Add('uint64_t*','pcuint64');
      // long
      Add('long','clong');
      Add('long*','pclong');
      Add('signed long','cslong');
      Add('signed long*','pcslong');
      Add('unsigned long','culong');
      Add('unsigned long*','pculong');
      Add('long int','clong');
      Add('long int*','pclong');
      Add('signed long int','cslong');
      Add('signed long int*','pcslong');
      Add('long signed int','cslong');
      Add('long signed int*','pcslong');
      Add('unsigned long int','culong');
      Add('unsigned long int*','pculong');
      Add('long unsigned int','culong');
      Add('long unsigned int*','pculong');
      // long long
      Add('long long','clonglong');
      Add('long long*','pclonglong');
      Add('signed long long','cslonglong');
      Add('signed long long*','pcslonglong');
      Add('unsigned long long','culonglong');
      Add('unsigned long long*','pculonglong');
      // bool
      Add('bool','cbool');
      Add('bool*','pcbool');
      // char
      Add('char','cchar');
      Add('char*','pcchar');
      Add('signed char','cschar');
      Add('signed char*','pcschar');
      Add('unsigned char','cuchar');
      Add('unsigned char*','pcuchar');
      // float
      Add('float','cfloat');
      Add('float*','pcfloat');
      // double
      Add('double','cdouble');
      Add('double*','pcdouble');
      Add('long double','clongdouble');
      Add('long double*','pclongdouble');
      // void
      Add('void*','pointer');
      // size_t
      Add('size_t','PtrUInt');
    end;
  end;
  Result:=InternalPredefinedCTypes;
end;

function CompareH2PNodePascalNames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TH2PNode(Data1).PascalName),
                                Pointer(TH2PNode(Data2).PascalName));
end;

function CompareStringWithH2PNodePascalName(AString, ANode: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(AString),
                                Pointer(TH2PNode(ANode).PascalName));
end;

function CompareH2PNodeCNames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifiersCaseSensitive(PChar(Pointer(TH2PNode(Data1).CName)),
                                          PChar(Pointer(TH2PNode(Data2).CName)));
end;

function CompareStringWithH2PNodeCName(AString, ANode: Pointer): integer;
begin
  Result:=CompareIdentifiersCaseSensitive(PChar(AString),
                                          PChar(Pointer(TH2PNode(ANode).CName)));
end;

function CompareH2PMacroStats(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TH2PMacroStats(Data1).Name),
                                Pointer(TH2PMacroStats(Data2).Name));
end;

function ComparePCharWithH2PMacroStats(Name, MacroStats: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Name,
                                Pointer(TH2PMacroStats(MacroStats).Name));
end;

function H2PDirectiveNodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
begin
  case Desc of
  h2pdnNone:   Result:='none';
  h2pdnRoot:   Result:='root';

  h2pdnDefine:   Result:='Define';
  h2pdnUndefine: Result:='Undef';

  h2pdnIf:      Result:='If';
  h2pdnIfDef:   Result:='IfDef';
  h2pdnIfNDef:  Result:='IfNDef';
  h2pdnElseIf:  Result:='ElseIf';
  h2pdnElse:    Result:='Else';
  h2pdnEndIf:   Result:='EndIf';

  h2pdnError:   Result:='Error';

  else          Result:='?('+IntToStr(Desc)+')';
  end;
end;

{ TH2PasTool }

procedure TH2PasTool.ConvertStruct(CNode: TCodeTreeNode; ParentNode: TH2PNode);
var
  CurName: String;
  TypeH2PNode: TH2PNode;
  CurCName: String;
begin
  CurName:=CTool.ExtractStructName(CNode);
  if CurName='' then begin
    // this is an anonymous struct -> ignore
    DebugLn(['TH2PasTool.ConvertStruct SKIPPING anonymous struct at ',CTool.CleanPosToStr(CNode.StartPos)]);
  end else begin
    // this struct has a name
    // create a type
    CurCName:=CurName;
    TypeH2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnRecordType,'',
                               nil,ParentNode=nil);
    DebugLn(['TH2PasTool.ConvertStruct ADDED ',TypeH2PNode.DescAsString(CTool)]);
    // build recursively
    BuildH2PTree(TypeH2PNode);
  end;
end;

procedure TH2PasTool.ConvertVariable(CNode: TCodeTreeNode; ParentNode: TH2PNode
  );
var
  CurName: String;
  TypeH2PNode: TH2PNode;
  CurType: String;
  SimpleType: String;
  H2PNode: TH2PNode;
  SubTypeName: String;
  CurCName: String;
begin
  if (CNode.FirstChild<>nil) and (CNode.FirstChild.Desc=ccnUnion)
  then begin
    CurName:=CTool.ExtractVariableName(CNode);
    if (ParentNode<>nil) and (ParentNode.PascalDesc=ctnRecordType)
    then begin
      // create a pascal 'record case'
      CurCName:=CurName;
      TypeH2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnRecordCase,'',
                                 ParentNode,false);
      DebugLn(['TH2PasTool.ConvertVariable added record case for nested union']);
      // build recursively the record cases
      if CNode.FirstChild.FirstChild<>nil then
        BuildH2PTree(TypeH2PNode,CNode.FirstChild.FirstChild);
    end else if (CurName<>'') and (ParentNode=nil) then begin
      // this union has a name
      // create a record type
      SubTypeName:='T'+CurName;
      TypeH2PNode:=CreateH2PNode(SubTypeName,'',CNode,ctnRecordCase,'',
                                 nil,true);
      DebugLn(['TH2PasTool.ConvertVariable added record type for union: ',TypeH2PNode.DescAsString(CTool)]);
      // build recursively
      if CNode.FirstChild.FirstChild<>nil then
        BuildH2PTree(TypeH2PNode,CNode.FirstChild.FirstChild);
      // create variable
      CurName:=CTool.ExtractUnionName(CNode);
      CurCName:=CurName;
      H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnVarDefinition,
                             TypeH2PNode.PascalName,
                             nil,ParentNode=nil);
      DebugLn(['TH2PasTool.ConvertVariable added variable for union: ',H2PNode.DescAsString(CTool)]);
    end else begin
      DebugLn(['TH2PasTool.ConvertVariable SKIPPING union variable at ',CTool.CleanPosToStr(CNode.StartPos)]);
    end;
  end else begin
    CurName:=CTool.ExtractVariableName(CNode);
    CurType:=CTool.ExtractVariableType(CNode);
    SimpleType:=GetSimplePascalTypeOfCVar(CNode);
    if SimpleType='' then begin
      // this variable has a complex type
      TypeH2PNode:=GetH2PNodeForComplexType(CNode,true,ParentNode<>nil);
      if TypeH2PNode<>nil then
        SimpleType:=TypeH2PNode.PascalName;
    end;
    if SimpleType<>'' then begin
      CurCName:=CurName;
      H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnVarDefinition,SimpleType,
                             ParentNode,ParentNode=nil);
      DebugLn(['TH2PasTool.ConvertVariable CurName=',CurName,' ',H2PNode.PascalName]);
      DebugLn(['TH2PasTool.ConvertVariable added: ',H2PNode.DescAsString(CTool)]);
    end else begin
      DebugLn(['TH2PasTool.ConvertVariable SKIPPING Variable Name="',CurName,'" Type="',CurType,'"']);
    end;
  end;
end;

procedure TH2PasTool.ConvertEnumBlock(CNode: TCodeTreeNode;
  ParentNode: TH2PNode);
var
  CurName: String;
  TypeH2PNode: TH2PNode;
  CurValue: String;
  H2PNode: TH2PNode;
  CurCName: String;
begin
  CurName:=CTool.ExtractEnumBlockName(CNode);
  if CurName='' then begin
    // this is an anonymous enum block => auto generate a name
    CurName:=CreatePascalNameFromCCode(CTool.Src,CNode.StartPos,CNode.EndPos);
    TypeH2PNode:=CreateAutoGeneratedH2PNode(CurName,CNode,ctnEnumerationType,'',
                                            nil,true,ParentNode<>nil);
  end else begin
    // this enum block has a name
    CurCName:=CurName;
    TypeH2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnEnumerationType,'',
                               nil,true);
  end;
  DebugLn(['TH2PasTool.ConvertEnumBlock added: ',TypeH2PNode.DescAsString(CTool)]);

  CNode:=CNode.FirstChild;
  while CNode<>nil do begin
    if CNode.Desc=ccnEnumID then begin
      CurName:=CTool.ExtractEnumIDName(CNode);
      CurValue:=CTool.ExtractEnumIDValue(CNode);
      CurCName:=CurName;
      H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnEnumIdentifier,CurValue,
                             TypeH2PNode,true);
      DebugLn(['TH2PasTool.ConvertEnumBlock added: ',H2PNode.DescAsString(CTool)]);
    end;
    CNode:=CNode.NextBrother;
  end;
end;

procedure TH2PasTool.ConvertFunction(CNode: TCodeTreeNode; ParentNode: TH2PNode
  );
var
  CurName: String;
  CurType: String;
  SimpleType: String;
  IsPointerToFunction: Boolean;
  Ok: Boolean;
  StatementNode: TCodeTreeNode;
  TypeH2PNode: TH2PNode;
  H2PNode: TH2PNode;
  SubTypeName: String;
  ParamsNode: TCodeTreeNode;
  CurCName: String;
begin
  CurName:=CTool.ExtractFunctionName(CNode);
  CurType:=CTool.ExtractFunctionResultType(CNode);
  SimpleType:=GetSimplePascalResultTypeOfCFunction(CNode);
  IsPointerToFunction:=CTool.IsPointerToFunction(CNode);
  StatementNode:=nil;
  Ok:=true;
  if (CNode.LastChild<>nil) and (CNode.LastChild.Desc=ccnStatementBlock) then
    StatementNode:=CNode.LastChild;
  DebugLn(['TH2PasTool.ConvertFunction Function Name="',CurName,'" ResultType="',CurType,'" SimpleType=',SimpleType,' HasStatements=',StatementNode<>nil,' IsPointer=',IsPointerToFunction,' ParentNode=',ParentNode<>nil]);
  if StatementNode<>nil then begin
    // this function has a body
    Ok:=false;
  end;
  if Ok and (SimpleType='') then begin
    // this function has a complex result type
    TypeH2PNode:=GetH2PNodeForComplexType(CNode,true,ParentNode<>nil);
    if TypeH2PNode<>nil then begin
      SimpleType:=TypeH2PNode.PascalName;
    end else
      Ok:=false;
  end;

  if Ok then begin
    if IsPointerToFunction then begin
      // create proc type
      ParamsNode:=CTool.GetFunctionParamListNode(CNode);
      SubTypeName:=CreatePascalNameFromCCode(CurName+CTool.ExtractFunctionParamList(CNode));
      TypeH2PNode:=CreateAutoGeneratedH2PNode(SubTypeName,ParamsNode,
                         ctnProcedureType,SimpleType,nil,true,ParentNode<>nil);
      DebugLn(['TH2PasTool.ConvertFunction function type added: ',TypeH2PNode.DescAsString(CTool)]);
      // create variable
      CurCName:=CurName;
      H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnVarDefinition,SubTypeName,
                             ParentNode,ParentNode=nil);
      DebugLn(['TH2PasTool.ConvertFunction variable added: ',H2PNode.DescAsString(CTool)]);
      // build parameters recursively
      if ParamsNode.FirstChild<>nil then
        BuildH2PTree(TypeH2PNode,ParamsNode.FirstChild);
    end else begin
      // create proc
      CurCName:=CurName;
      H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnProcedure,SimpleType,
                             nil,true);
      DebugLn(['TH2PasTool.ConvertFunction function added: ',H2PNode.DescAsString(CTool)]);
      // build parameters recursively
      if CNode.FirstChild<>nil then
        BuildH2PTree(H2PNode);
    end;
  end else begin
    DebugLn(['TH2PasTool.ConvertFunction SKIPPING Function Name="',CurName,'" Type="',CurType,'" at ',CTool.CleanPosToStr(CNode.StartPos)]);
  end;
end;

procedure TH2PasTool.ConvertFuncParameter(CNode: TCodeTreeNode;
  ParentNode: TH2PNode);
var
  CurName: String;
  CurType: String;
  SimpleType: String;
  TypeH2PNode: TH2PNode;
  H2PNode: TH2PNode;
  CurCName: String;
begin
  CurName:=CTool.ExtractParameterName(CNode);
  CurType:=CTool.ExtractParameterType(CNode);
  if CurType='void' then begin
    // for example int f(void) is a function without params
    exit;
  end;
  SimpleType:=GetSimplePascalTypeOfCParameter(CNode);
  DebugLn(['TH2PasTool.ConvertFuncParameter Parameter: Name="',CurName,'" Type="',CurType,'" SimpleType="',SimpleType,'"']);
  if SimpleType='' then begin
    // this variable has a complex type
    TypeH2PNode:=GetH2PNodeForComplexType(CNode,true,true);
    if TypeH2PNode<>nil then
      SimpleType:=TypeH2PNode.PascalName;
  end;
  if SimpleType<>'' then begin
    CurCName:=CurName;
    H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnVarDefinition,SimpleType,
                           ParentNode,false);
    DebugLn(['TH2PasTool.ConvertFuncParameter added: ',H2PNode.DescAsString(CTool)]);
  end else begin
    DebugLn(['TH2PasTool.ConvertFuncParameter SKIPPING parameter Name="',CurName,'" Type="',CurType,'" at ',CTool.CleanPosToStr(CNode.StartPos)]);
  end;
end;

procedure TH2PasTool.ConvertTypedef(CNode: TCodeTreeNode; ParentNode: TH2PNode
  );
var
  CurName: String;
  ChildNode: TCodeTreeNode;
  CurType: String;
  TypeH2PNode: TH2PNode;
  IsPointerToFunction: Boolean;
  SimpleType: String;
  H2PNode: TH2PNode;
  CurCName: String;
  CurValue: String;
  SubChildNode: TCodeTreeNode;
begin
  if CNode.FirstChild=nil then begin
    exit;
  end;
  CurName:=CTool.ExtractTypedefName(CNode);
  DebugLn(['TH2PasTool.ConvertTypedef Typedef name="',CurName,'"']);
  ChildNode:=CNode.FirstChild;
  case ChildNode.Desc of
  
  ccnVariable: // typedef variable
    begin
      CurName:=CTool.ExtractVariableName(ChildNode);
      CurType:=CTool.ExtractVariableType(ChildNode);
      SimpleType:=GetSimplePascalTypeOfCVar(ChildNode);
      if SimpleType='' then begin
        // this variable has a complex type
        TypeH2PNode:=GetH2PNodeForComplexType(ChildNode,true,ParentNode<>nil);
        if TypeH2PNode<>nil then
          SimpleType:=TypeH2PNode.PascalName;
      end;
      if SimpleType<>'' then begin
        CurCName:=CurName;
        H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnTypeDefinition,
                               SimpleType,nil,true);
        DebugLn(['TH2PasTool.ConvertTypedef added: ',H2PNode.DescAsString(CTool)]);
      end else begin
        DebugLn(['TH2PasTool.ConvertTypedef SKIPPING Typedef Variable Name="',CurName,'" Type="',CurType,'"']);
      end;
    end;

  ccnStruct: // typedef struct
    begin
      ChildNode:=CNode.FirstChild.FirstChild;
      if (ChildNode<>nil)
      and (ChildNode.Desc=ccnStructAlias) then begin
        // this is a struct alias
        CurType:=GetIdentifier(@CTool.Src[ChildNode.StartPos]);
        CurCName:=CurName;
        TypeH2PNode:=CreateH2PNode(CurName,CurCName,CNode,
                                   ctnTypeDefinition,CurType);
      end else begin
        // this is a new struct
        CurCName:=CurName;
        TypeH2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnRecordType,'');
        DebugLn(['TH2PasTool.ConvertTypedef added record: ',TypeH2PNode.DescAsString(CTool)]);
        // build recursively
        if ChildNode<>nil then
          BuildH2PTree(TypeH2PNode,ChildNode);
      end;
    end;

  ccnFunction: // typedef function
    begin
      CurName:=CTool.ExtractFunctionName(ChildNode);
      CurType:=CTool.ExtractFunctionResultType(ChildNode,false,false);
      IsPointerToFunction:=CTool.IsPointerToFunction(ChildNode);
      SimpleType:=GetSimplePascalResultTypeOfCFunction(ChildNode);
      if IsPointerToFunction and (SimpleType='') then begin
        // this function has a complex result type
        TypeH2PNode:=GetH2PNodeForComplexType(ChildNode,true,ParentNode<>nil);
        if TypeH2PNode<>nil then
          SimpleType:=TypeH2PNode.PascalName;
      end;
      if IsPointerToFunction and (SimpleType<>'') then begin
        CurCName:=CurName;
        H2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnProcedureType,SimpleType,
                               nil,true);
        DebugLn(['TH2PasTool.ConvertTypedef function type added: ',H2PNode.DescAsString(CTool)]);
        // build the param list
        if ChildNode.FirstChild<>nil then
          BuildH2PTree(H2PNode,ChildNode.FirstChild);
      end else begin
        DebugLn(['TH2PasTool.ConvertTypedef typdef function CurName=',CurName,' CurType=',CTool.ExtractFunctionResultType(ChildNode),' SimpleType=',SimpleType,' IsPointerToFunction=',IsPointerToFunction]);
        DebugLn(['TH2PasTool.ConvertTypedef SKIPPING typedef ',CCNodeDescAsString(ChildNode.Desc),' at ',CTool.CleanPosToStr(CNode.StartPos)]);
      end;
    end;
    
  ccnEnumBlock: // enum block
    begin
      // this enum block has a name
      CurCName:=CurName;
      TypeH2PNode:=CreateH2PNode(CurName,CurCName,CNode,ctnEnumerationType,'',
                                 nil,true);
      DebugLn(['TH2PasTool.ConvertTypedef added: ',TypeH2PNode.DescAsString(CTool)]);

      SubChildNode:=ChildNode.FirstChild;
      while SubChildNode<>nil do begin
        if SubChildNode.Desc=ccnEnumID then begin
          CurName:=CTool.ExtractEnumIDName(SubChildNode);
          CurValue:=CTool.ExtractEnumIDValue(SubChildNode);
          CurCName:=CurName;
          H2PNode:=CreateH2PNode(CurName,CurCName,SubChildNode,
                                 ctnEnumIdentifier,CurValue,
                                 TypeH2PNode,true);
          DebugLn(['TH2PasTool.ConvertTypedef added: ',H2PNode.DescAsString(CTool)]);
        end;
        SubChildNode:=SubChildNode.NextBrother;
      end;
    end;

  else // typedef
    DebugLn(['TH2PasTool.ConvertTypedef SKIPPING typedef ',CTool.NodeAsString(ChildNode)]);
  end;
end;

procedure TH2PasTool.ConvertDirective(CNode: TCodeTreeNode; ParentNode: TH2PNode
  );
var
  Directive: String;
  H2PNode: TH2PNode;
  CurName: String;
  PascalCode: String;
  ErrorPos: integer;
  ErrorMsg: string;
  StartPos: LongInt;
  EndPos: LongInt;
  MacroName,MacroParamList,MacroValue: string;
  DirNode: TH2PDirectiveNode;
  Desc: TCodeTreeNodeDesc;
begin
  Directive:=CTool.ExtractDirectiveAction(CNode);
  if Directive='include' then begin
    // #include <filename>  // search independent of source position
    // #include "filename"  // search dependent on source position
    if icspInclude in IgnoreCParts then
      exit;
  end else if Directive='define' then begin
    // #define macrofunction(a,b) a here, then b
    // #define simplemacro some text here
    if CTool.ExtractDefine(CNode,MacroName,MacroParamList,MacroValue)
    then begin
      CurName:='$'+Directive;
      H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                             MacroName,ParentNode,false);
      DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
      DirNode:=CreateH2PDirectiveNode(H2PNode,h2pdnDefine);
      DirNode.MacroName:=MacroName;
      DirNode.MacroParams:=MacroParamList;
      if MacroValue='__BYTE_ORDER' then
        MacroValue:='FPC';
      DirNode.Expression:=MacroValue;
      exit;
    end;
  end else if (Directive='undef') or (Directive='ifdef')
  or (Directive='ifndef') then begin
    // #undef NAME
    // #ifdef NAME
    // #ifndef NAME
    CurName:='$'+Directive;
    PascalCode:=CTool.ExtractDirectiveFirstAtom(CNode);
    H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                           PascalCode,ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
    if (Directive='ifdef') then
      Desc:=h2pdnIfDef
    else if (Directive='ifndef') then
      Desc:=h2pdnIfNDef
    else
      Desc:=h2pdnUndefine;
    DirNode:=CreateH2PDirectiveNode(H2PNode,Desc);
    DirNode.MacroName:=PascalCode;
    if (Desc=h2pdnIfDef) or (Desc=h2pdnIfNDef) then begin
      // start block
      FCurDirectiveNode:=DirNode;
    end;
    exit;
  end else if (Directive='if') or (Directive='elif') then begin
    // #if EXPRESSION
    // #elif EXPRESSION
    CTool.MoveCursorToPos(CNode.StartPos+1);
    // read action
    CTool.ReadRawNextAtom;
    // convert expression
    StartPos:=CTool.SrcPos;
    EndPos:=CNode.EndPos;
    if not ConvertCToPascalDirectiveExpression(CTool.Src,StartPos,EndPos,
      PascalCode,ErrorPos,ErrorMsg) then
    begin
      DebugLn(['TH2PasTool.ConvertDirective failed to convert expression at ',
        CTool.CleanPosToStr(ErrorPos)+': '+ErrorMsg]);
    end else begin
      CurName:='$'+Directive;
      H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                             PascalCode,ParentNode,false);
      DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
      if (Directive='if') then
        Desc:=h2pdnIf
      else begin
        Desc:=h2pdnElseIf;
        // end block
        FCurDirectiveNode:=TH2PDirectiveNode(FCurDirectiveNode.Parent);
      end;
      DirNode:=CreateH2PDirectiveNode(H2PNode,Desc);
      DirNode.Expression:=PascalCode;
      // start block
      FCurDirectiveNode:=DirNode;
      exit;
    end;
  end else if (Directive='else') then begin
    // #else
    CurName:='$'+Directive;
    H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                           '',ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
    // end block
    FCurDirectiveNode:=TH2PDirectiveNode(FCurDirectiveNode.Parent);
    DirNode:=CreateH2PDirectiveNode(H2PNode,h2pdnElse);
    // start block
    FCurDirectiveNode:=DirNode;
    exit;
  end else if (Directive='endif') then begin
    // #endif
    CurName:='$'+Directive;
    H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                           '',ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
    // end block
    FCurDirectiveNode:=TH2PDirectiveNode(FCurDirectiveNode.Parent);
    DirNode:=CreateH2PDirectiveNode(H2PNode,h2pdnEndIf);
    exit;
  end else if Directive='line' then begin
    // #line: set the current line number -> ignore
    exit;
  end else if Directive='error' then begin
    // #error
    PascalCode:=CTool.ExtractCode(CNode.StartPos+length('#error'),
                                  CNode.EndPos);
    CurName:='$'+Directive;
    H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                           PascalCode,ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added $error: ',H2PNode.DescAsString(CTool)]);
    DirNode:=CreateH2PDirectiveNode(H2PNode,h2pdnError);
    DirNode.Expression:=PascalCode;
    exit;
  end else if Directive='pragma' then begin
    // #pragma: implementation specifics
    exit;
  end else if Directive='' then begin
    // #  : null
    exit;
  end;
  DebugLn(['TH2PasTool.ConvertDirective SKIPPING directive at ',CTool.CleanPosToStr(CNode.StartPos),' Code="',dbgstr(CTool.ExtractCode(CNode.StartPos,CNode.EndPos)),'"']);
end;

function TH2PasTool.ConvertCToPascalDirectiveExpression(const CCode: string;
  StartPos, EndPos: integer; out PasExpr: string;
  out ErrorPos: integer; out ErrorMsg: string): boolean;
type
  TTokenType = (
    ttNone,
    ttValue,
    ttBinaryOperator,
    ttBracketOpen,
    ttBracketClose
    );
var
  p: LongInt;
  AtomStart: integer;
  BracketLvl: Integer;
  LastToken: TTokenType;
  NeedBracket: Boolean;

  function AtomIs(const s: shortstring): boolean;
  var
    len: Integer;
    i: Integer;
  begin
    len:=length(s);
    if (len<>p-AtomStart) then exit(false);
    if p>EndPos then exit(false);
    for i:=1 to len do
      if CCode[AtomStart+i-1]<>s[i] then exit(false);
    Result:=true;
  end;
  
  function GetAtom: string;
  begin
    Result:=copy(CCode,AtomStart,p-AtomStart);
  end;
  
  procedure ErrorExpectedButFound(const s: string);
  begin
    ErrorPos:=AtomStart;
    ErrorMsg:=s+' expected, but '+GetAtom+' found';
  end;
  
  procedure Add(NewToken: TTokenType; const s: string);
  begin
    LastToken:=NewToken;
    if s='' then exit;
    if (IsIdentChar[s[1]])
    and (PasExpr<>'') and IsIdentChar[PasExpr[length(PasExpr)]] then
      PasExpr:=PasExpr+' ';
    PasExpr:=PasExpr+s;
  end;
  
  procedure Add(NewToken: TTokenType);
  begin
    Add(NewToken,GetAtom);
  end;
  
  procedure Replace(const OldText,NewText: string);
  var
    l: Integer;
  begin
    p:=1;
    l:=length(OldText);
    repeat
      ReadRawNextCAtom(PasExpr,p,AtomStart);
      if AtomStart>length(PasExpr) then break;
      if CompareMem(@PasExpr[AtomStart],@OldText[1],l)
      and ((not IsIdentChar[OldText[l]])
           or (AtomStart+l>length(PasExpr))
           or (not IsIdentChar[PasExpr[AtomStart+l]]))
      then begin
        DebugLn(['TH2PasTool.ConvertCToPascalDirectiveExpression.Replace Old="',OldText,'" New="',NewText,'"']);
        PasExpr:=copy(PasExpr,1,AtomStart-1)
               +NewText+copy(PasExpr,AtomStart+length(OldText),length(PasExpr));
      end;
    until false;
  end;
  
begin
  Result:=false;
  PasExpr:='';
  ErrorMsg:='';
  ErrorPos:=StartPos;
  LastToken:=ttNone;
  BracketLvl:=0;
  p:=StartPos;
  repeat
    ReadRawNextCAtom(CCode,p,AtomStart);
    if (AtomStart>=EndPos) or (CCode[AtomStart] in [#10,#13]) then begin
      if BracketLvl>0 then begin
        ErrorPos:=EndPos;
        ErrorMsg:='missing closing bracket';
        exit;
      end else if LastToken in [ttNone,ttBinaryOperator] then begin
        ErrorPos:=EndPos;
        ErrorMsg:='missing value';
        exit;
      end;
      Result:=true;
      break;
    end;
    if IsIdentChar[CCode[AtomStart]] then begin
      // value
      if LastToken in [ttValue,ttBracketClose] then begin
        ErrorPos:=AtomStart;
        ErrorMsg:='missing operator';
        exit;
      end;
      if AtomIs('defined') then begin
        Add(ttValue);
        // read   defined(name)   or   defined name
        ReadRawNextCAtom(CCode,p,AtomStart);
        if AtomIs('(') then
          NeedBracket:=true
        else
          NeedBracket:=false;
        Add(ttBracketOpen);
        ReadRawNextCAtom(CCode,p,AtomStart);
        if (AtomStart>=EndPos) or (not IsIdentStartChar[CCode[AtomStart]])
        then begin
          ErrorExpectedButFound('identifier');
          exit;
        end;
        // convert defined(__BYTE_ORDER) to defined(FPC)
        if AtomIs('__BYTE_ORDER') then
          Add(ttValue,'FPC')
        else
          Add(ttValue);
        if NeedBracket then begin
          ReadRawNextCAtom(CCode,p,AtomStart);
          if not AtomIs(')') then begin
            ErrorExpectedButFound(')');
            exit;
          end;
        end;
        Add(ttBracketClose);
      end else begin
        Add(ttValue);
      end;
    end else if AtomIs('+') or AtomIs('-') or AtomIs('!') then begin
      if LastToken in [ttValue,ttBracketClose] then begin
        if AtomIs('!') then
          Add(ttBinaryOperator,'not')
        else
          Add(ttBinaryOperator);
      end else begin
        // just a modifier, not important for the type
      end;
    end else if AtomIs('*') or AtomIs('/') or AtomIs('!=') or AtomIs('==')
    then begin
      if LastToken in [ttValue,ttBracketClose] then begin
        if AtomIs('!=') then
          Add(ttBinaryOperator,'<>')
        else if AtomIs('==') then
          Add(ttBinaryOperator,'=')
        else
          Add(ttBinaryOperator);
      end else begin
        ErrorPos:=AtomStart;
        ErrorMsg:='value expected, but '+GetAtom+' found';
        exit;
      end;
    end else if AtomIs('(') then begin
      if LastToken in [ttNone,ttBinaryOperator] then begin
        Add(ttBracketOpen);
        inc(BracketLvl);
      end else begin
        ErrorPos:=AtomStart;
        ErrorMsg:='operator expected, but '+GetAtom+' found';
        exit;
      end;
    end else if AtomIs(')') then begin
      if BracketLvl=0 then begin
        ErrorPos:=AtomStart;
        ErrorMsg:='missing opening bracket';
        exit;
      end;
      if LastToken in [ttValue] then begin
        Add(ttBracketClose);
        dec(BracketLvl);
      end else begin
        ErrorPos:=AtomStart;
        ErrorMsg:='operator expected, but '+GetAtom+' found';
        exit;
      end;
    end else begin
      ErrorPos:=AtomStart;
      ErrorMsg:='invalid symbol '+GetAtom+' found';
      exit;
    end;
  until false;
  
  // now convert a few common things:
  Replace('__BYTE_ORDER=__LITTLE_ENDIAN','defined(ENDIAN_LITTLE)');
  Replace('__LITTLE_ENDIAN=__BYTE_ORDER','defined(ENDIAN_LITTLE)');
  Replace('__BYTE_ORDER=__BIG_ENDIAN','defined(ENDIAN_BIG)');
  Replace('__BIG_ENDIAN=__BYTE_ORDER','defined(ENDIAN_BIG)');
end;

procedure TH2PasTool.WriteStr(const Line: string);
begin
  if Line='' then exit;
  FCurPasStream.Write(Line[1],length(Line));
end;

procedure TH2PasTool.WriteLnStr(const Line: string);
begin
  WriteStr(Line+LineEnding);
end;

procedure TH2PasTool.W(const aStr: string);
begin
  WriteLnStr(FCurIndentStr+aStr);
end;

procedure TH2PasTool.IncIndent;
begin
  FCurIndentStr:=FCurIndentStr+'  ';
end;

procedure TH2PasTool.DecIndent;
begin
  FCurIndentStr:=copy(FCurIndentStr,1,length(FCurIndentStr)-2);
end;

procedure TH2PasTool.SetPasSection(NewSection: TCodeTreeNodeDesc);
begin
  if NewSection=FCurPasSection then exit;
  // close old section
  case FCurPasSection of
  ctnVarSection,ctnTypeSection,ctnConstSection:
    begin
      DecIndent;
    end;
  end;
  FCurPasSection:=NewSection;
  // start new section
  W('');
  case FCurPasSection of
  ctnVarSection,ctnTypeSection,ctnConstSection:
    begin
      case FCurPasSection of
      ctnVarSection: W('var');
      ctnTypeSection: W('type');
      ctnConstSection: W('const');
      end;
      IncIndent;
    end;
  end;
end;

procedure TH2PasTool.WriteGlobalVarNode(H2PNode: TH2PNode);
var
  PascalCode: String;
begin
  // global variable
  SetPasSection(ctnVarSection);
  PascalCode:=H2PNode.PascalCode+';';
  if H2PNode.CName<>'' then begin
    PascalCode:=PascalCode+' cvar; public';
    if H2PNode.PascalName<>H2PNode.CName then begin
      PascalCode:=PascalCode+' name '''+H2PNode.CName+'''';
    end;
    PascalCode:=PascalCode+';';
  end;
  W(H2PNode.PascalName+': '+PascalCode);
end;

procedure TH2PasTool.WriteGlobalTypeNode(H2PNode: TH2PNode);
begin
  // global type
  SetPasSection(ctnTypeSection);
  if H2PNode.FirstChild=nil then begin
    W(H2PNode.PascalName+' = '+H2PNode.PascalCode+';');
  end else begin
    DebugLn(['TH2PasTool.WriteGlobalTypeNode SKIPPING ',H2PNode.DescAsString(CTool)]);
  end;
end;

procedure TH2PasTool.WriteGlobalConstNode(H2PNode: TH2PNode);
begin
  // global const
  SetPasSection(ctnConstSection);
  if H2PNode.FirstChild=nil then begin
    W(H2PNode.PascalName+H2PNode.PascalCode+';');
  end else begin
    DebugLn(['TH2PasTool.WriteGlobalTypeNode SKIPPING ',H2PNode.DescAsString(CTool)]);
  end;
end;

procedure TH2PasTool.WriteGlobalProcedureNode(H2PNode: TH2PNode);
var
  PascalCode: String;
  ChildNode: TH2PNode;
  NoNameCount: Integer;
  CurName: String;
begin
  // global procedure or procedure type
  if H2PNode.PascalDesc=ctnProcedure then
    SetPasSection(ctnNone)
  else
    SetPasSection(ctnTypeSection);
  // create param list
  PascalCode:='';
  ChildNode:=TH2PNode(H2PNode.FirstChild);
  NoNameCount:=0;
  while ChildNode<>nil do begin
    if ChildNode.PascalDesc=ctnVarDefinition then begin
      if PascalCode<>'' then
        PascalCode:=PascalCode+'; ';
      CurName:=ChildNode.PascalName;
      if CurName='' then begin
        inc(NoNameCount);
        CurName:='param'+IntToStr(NoNameCount)
                +CreatePascalNameFromCCode(ChildNode.PascalCode);
      end;
      PascalCode:=PascalCode+CurName+': '+ChildNode.PascalCode;
    end else begin
      DebugLn(['TH2PasTool.WriteGlobalProcedureNode SKIPPING ',ChildNode.DescAsString(CTool)]);
    end;
    ChildNode:=TH2PNode(ChildNode.NextBrother);
  end;
  if PascalCode<>'' then
    PascalCode:='('+PascalCode+')';
  if H2PNode.PascalDesc=ctnProcedure then begin
    PascalCode:=H2PNode.PascalName+PascalCode;
    if H2PNode.PascalCode='void' then
      PascalCode:='procedure '+PascalCode
    else
      PascalCode:='function '+PascalCode+': '+H2PNode.PascalCode;
    PascalCode:=PascalCode+'; cdecl;';
    if H2PNode.CName<>'' then begin
      if H2PNode.CName<>H2PNode.PascalName then
        PascalCode:=PascalCode+' external name '''+H2PNode.CName+''';'
      else
        PascalCode:=PascalCode+' external;';
    end;
  end else begin
    if H2PNode.PascalCode='void' then
      PascalCode:='procedure'+PascalCode
    else
      PascalCode:='function'+PascalCode+': '+H2PNode.PascalCode;
    PascalCode:=PascalCode+'; cdecl;';
    PascalCode:=H2PNode.PascalName+' = '+PascalCode;
  end;
  W(PascalCode);
end;

procedure TH2PasTool.WriteGlobalEnumerationTypeNode(H2PNode: TH2PNode);
var
  PascalCode: String;
  ChildNode: TH2PNode;
begin
  { for example:
      e2 = (
        a = 3,
        b = 9
      );
  }
  SetPasSection(ctnTypeSection);
  // write start
  PascalCode:=H2PNode.PascalName+' = (';
  W(PascalCode);
  // write enums
  IncIndent;
  ChildNode:=TH2PNode(H2PNode.FirstChild);
  while ChildNode<>nil do begin
    PascalCode:=ChildNode.PascalName;
    if ChildNode.PascalCode<>'' then
      PascalCode:=PascalCode+' = '+ChildNode.PascalCode;
    if ChildNode.NextBrother<>nil then
      PascalCode:=PascalCode+',';
    W(PascalCode);
    ChildNode:=TH2PNode(ChildNode.NextBrother);
  end;
  DecIndent;
  // write end
  W(');');
end;

procedure TH2PasTool.WriteGlobalRecordTypeNode(H2PNode: TH2PNode);
var
  PascalCode: String;
  ChildNode: TH2PNode;
  NoNameCount: Integer;
  SubChildNode: TH2PNode;
begin
  { examples:
     TRecord = record
     end;
  }
  SetPasSection(ctnTypeSection);
  // write header
  PascalCode:=H2PNode.PascalName+' = record';
  W(PascalCode);
  // write sub variables
  IncIndent;
  ChildNode:=TH2PNode(H2PNode.FirstChild);
  while ChildNode<>nil do begin
    if ChildNode.PascalDesc=ctnVarDefinition then begin
      PascalCode:=ChildNode.PascalName+': '+ChildNode.PascalCode+';';
      W(PascalCode);
    end else if ChildNode.PascalDesc=ctnRecordCase then begin
      { record
          case longint of
          0: ( a: b );
          2: ( c: d );
        end;
      }
      // write header
      PascalCode:=ChildNode.PascalName+': record';
      W(PascalCode);
      IncIndent;
      // write children
      W('case longint of');
      IncIndent;
      NoNameCount:=0;
      SubChildNode:=TH2PNode(ChildNode.FirstChild);
      while SubChildNode<>nil do begin
        PascalCode:=IntToStr(NoNameCount)+': ('
           +SubChildNode.PascalName+': '+SubChildNode.PascalCode+' );';
        W(PascalCode);
        SubChildNode:=TH2PNode(SubChildNode.NextBrother);
        inc(NoNameCount);
      end;
      DecIndent;
      // write footer
      W('end;');
      DecIndent;
    end else
      DebugLn(['TH2PasTool.WriteGlobalRecordTypeNode SKIPPING record sub ',ChildNode.DescAsString(CTool)]);
    ChildNode:=TH2PNode(ChildNode.NextBrother);
  end;
  DecIndent;
  // write end
  W('end;');
end;

procedure TH2PasTool.WriteDirectiveNode(DirNode: TH2PDirectiveNode);
begin
  case DirNode.Desc of
  h2pdnIfDef:
    begin
      SetPasSection(ctnNone);
      W('{$IfDef '+DirNode.MacroName+'}');
      IncIndent;
    end;
  h2pdnIfNDef:
    begin
      SetPasSection(ctnNone);
      W('{$IfNDef '+DirNode.MacroName+'}');
      IncIndent;
    end;
  h2pdnIf:
    begin
      SetPasSection(ctnNone);
      W('{$If '+DirNode.Expression+'}');
      IncIndent;
    end;
  h2pdnElseIf:
    begin
      SetPasSection(ctnNone);
      DecIndent;
      W('{$ElseIf '+DirNode.Expression+'}');
      IncIndent;
    end;
  h2pdnElse:
    begin
      SetPasSection(ctnNone);
      DecIndent;
      W('{$Else}');
      IncIndent;
    end;
  h2pdnEndIf:
    begin
      SetPasSection(ctnNone);
      DecIndent;
      W('{$EndIf}');
    end;
  h2pdnError:
    begin
      SetPasSection(ctnNone);
      W('{$Error '+CreateDirectiveValue(DirNode.Expression)+'}');
    end;
  h2pdnUndefine:
    begin
      SetPasSection(ctnNone);
      W('{$UnDef '+DirNode.MacroName+'}');
    end;
  h2pdnDefine:
    if (DirNode.MacroParams='') then begin
      SetPasSection(ctnNone);
      if ExtractCCode(DirNode.Expression)='' then begin
        W('{$Define '+DirNode.MacroName+'}');
      end else begin
        W('{off $Define '+DirNode.MacroName+':='+CreateDirectiveValue(DirNode.Expression)+'}');
      end;
    end else begin
      DebugLn(['TH2PasTool.WriteDirectiveNode SKIPPING ',DirNode.DescAsString(CTool)]);
    end;
  else
    DebugLn(['TH2PasTool.WriteDirectiveNode SKIPPING ',DirNode.DescAsString(CTool)]);
  end;
end;

function TH2PasTool.CreateDirectiveValue(const s: string): string;
var
  p: Integer;
begin
  Result:=s;
  p:=length(Result);
  while p>=1 do begin
    if (Result[p] in [#0..#31,'{','}']) then begin
      Result:=copy(Result,1,p-1)+'#'+IntToStr(ord(Result[p]))+copy(Result,p+1,length(Result));
    end;
    dec(p);
  end;
end;

procedure TH2PasTool.SimplifyUndefineDirective(Node: TH2PDirectiveNode;
  var NextNode: TH2PDirectiveNode; var Changed: boolean);
begin
  SimplifyMacroRedefinition(Node,'',hmsUndefined,NextNode,Changed);
  if Node=nil then exit;
  UndefineMacro(Node.MacroName,Node.H2PNode);
end;

procedure TH2PasTool.SimplifyDefineDirective(Node: TH2PDirectiveNode;
  var NextNode: TH2PDirectiveNode; var Changed: boolean);
{ Examples:

  Macro flag:
    #define MPI_FILE_DEFINED
    =>  $Define MPI_FILE_DEFINED

  Simple constant:
    #define SOME_FLAG1   31
    =>  const SOME_FLAG1 = 31;

  null pointer
    #define MPI_BOTTOM      (void *)0
    =>  const MPI_BOTTOM = nil;

  Alias:
    #define SOME_FLAG2  SOME_FLAG1
    =>  const SOME_FLAG2 = SOME_FLAG1;
     OR type SOME_FLAG2 = SOME_FLAG1;

  Dummy function:
    #define htobs(d)  (d)
    =>  comment

  Function alias:
    #define htobs(d)  bswap_16(d)
    =>  comment

  Function without parameters:
    #define HIDPCONNADD     _IOW('H', 200, int)
    =>  comment
}
var
  PasType: string;
  PasExpr: string;
  H2PNode: TH2PNode;
begin
  if Node.H2PNode<>nil then
    MarkMacrosAsRead(Node.H2PNode,Node.Expression);

  if (Node.H2PNode<>nil) and (Node.H2PNode.Parent<>nil) then begin
    // this directive is in a C block
    // ToDo: try to make it global
    if (Node.Parent<>nil) and (TH2PDirectiveNode(Node.Parent).Desc<>h2pdnRoot)
    then begin
      // this define is in a conditional
    end;
    exit;
  end;
  
  if Node.MacroParams='' then begin
    // a macro without parameters
    if ExtractCCode(Node.Expression)='' then begin
      // example: #define MPI_FILE_DEFINED
      // => simple macro flag
      SimplifyMacroRedefinition(Node,'',hmsDefined,NextNode,Changed);
      if Node=nil then exit;
      DefineMacro(Node.MacroName,'',Node.H2PNode);
    end else if MacroValueIsConstant(Node,PasType,PasExpr) then begin
      // convert node to constant
      H2PNode:=Node.H2PNode;
      H2PNode.PascalName:=CreateUniquePascalName(Node.MacroName);
      H2PNode.CName:=Node.MacroName;
      H2PNode.PascalDesc:=ctnConstDefinition;
      H2PNode.PascalCode:=' = '+PasExpr;
      if PasType<>'' then
        H2PNode.PascalCode:=': '+PasType+H2PNode.PascalCode;
      FPascalNames.Add(H2PNode);
      FCNames.Add(H2PNode);
      DefineMacro(H2PNode.CName,PasExpr,nil);
      NextNode:=TH2PDirectiveNode(Node.NextSkipChilds);
      Node.H2PNode:=nil;
      H2PNode.Directive:=nil;
      DeleteDirectiveNode(Node,true,false);
      DebugLn(['TH2PasTool.SimplifyDefineDirective ADDED constant ',H2PNode.DescAsString(CTool)]);
      Changed:=true;
    end;
  end else begin
    DefineMacro(Node.MacroName,Node.Expression,Node.H2PNode);
  end;
end;

procedure TH2PasTool.SimplifyIfDirective(Node: TH2PDirectiveNode;
  Expression: string; var NextNode: TH2PDirectiveNode;
  var Changed: boolean);
begin
  if Node.H2PNode=nil then exit;
  MarkMacrosAsRead(Node.H2PNode,Expression);
  
  if (Node.FirstChild=nil) and (Node.H2PNode.FirstChild=nil)
  and ((Node.NextBrother=nil)
       or (TH2PDirectiveNode(Node.NextBrother).H2PNode=Node.H2PNode.NextBrother))
  then begin
    // no content
    DebugLn(['TH2PasTool.SimplifyIfDirective REMOVING empty if directive: ',Node.DescAsString(CTool)]);
    if NextNode.HasAsParent(Node)
    or ((NextNode=Node.NextBrother) and (NextNode.Desc=h2pdnEndIf)) then
      NextNode:=TH2PDirectiveNode(NextNode.NextSkipChilds);
    DeleteDirectiveNode(Node,true,true);
    Changed:=true;
    exit;
  end;

  Changed:=SimplifyIfDirectiveExpression(Expression);
  if Expression='0' then begin
    // always false
    DebugLn(['TH2PasTool.SimplifyIfDirective REMOVING directive, because always false: ',Node.DescAsString(CTool)]);
    if NextNode.HasAsParent(Node)
    or ((NextNode=Node.NextBrother) and (NextNode.Desc=h2pdnEndIf)) then
      NextNode:=TH2PDirectiveNode(NextNode.NextSkipChilds);
    DeleteDirectiveNode(Node,true,true);
    Changed:=true;
    exit;
  end;

  if Changed and ((Node.Desc=h2pdnIf) or (Node.Desc=h2pdnElseIf)) then begin
    Node.Expression:=Expression;
  end;
end;

function TH2PasTool.SimplifyIfDirectiveExpression(var Expression: string
  ): boolean;
// returns true, if changed
// uses current Undefines and Defines
var
  p: Integer;
  AtomStart: integer;
  CurAtom: String;
begin
  Result:=false;
  p:=1;
  repeat
    ReadRawNextCAtom(Expression,p,AtomStart);
    if AtomStart>length(Expression) then break;
    CurAtom:=copy(Expression,AtomStart,p-AtomStart);
    if CurAtom='' then ;
  until false;
end;

function TH2PasTool.MacroValueIsConstant(Node: TH2PDirectiveNode;
  out PasType, PasExpression: string): boolean;
  
  function TrimBrackets(const s: string): string;
  begin
    Result:=s;
  end;
  
var
  AtomStart: integer;
  p: Integer;

  procedure Replace(NewAtom: string);
  begin
    if IsIdentChar[NewAtom[1]]
    and (AtomStart>1) and (IsIdentChar[PasExpression[AtomStart-1]]) then
      NewAtom:=' '+NewAtom;
    if IsIdentChar[NewAtom[length(NewAtom)]]
    and (p<=length(PasExpression)) and (IsIdentChar[PasExpression[p]]) then
      NewAtom:=NewAtom+' ';
    PasExpression:=copy(PasExpression,1,AtomStart-1)+NewAtom
      +copy(PasExpression,p,length(PasExpression));
    p:=AtomStart+length(NewAtom);
  end;
  
var
  CurAtom: String;
  UsedNode: TH2PNode;
begin
  //DebugLn(['TH2PasTool.MacroValueIsConstant ',Node.MacroName,':=',Node.Expression]);
  Result:=false;
  PasType:='';
  PasExpression:=TrimBrackets(Node.Expression);
  
  // check for special constants
  if ExtractCCode(PasExpression)='(void*)0' then begin
    PasExpression:='nil';
    exit(true);
  end;
  
  p:=1;
  repeat
    ReadRawNextCAtom(PasExpression,p,AtomStart);
    if AtomStart>length(PasExpression) then break;
    //DebugLn(['TH2PasTool.MacroValueIsConstant Atom=',copy(PasExpression,AtomStart,p-AtomStart)]);
    if IsIdentStartChar[PasExpression[AtomStart]] then begin
      CurAtom:=copy(PasExpression,AtomStart,p-AtomStart);
      if CurAtom='sizeof' then begin
        // the sizeof(type) function is a C compiler built in function
        // read (
        ReadRawNextCAtom(PasExpression,p,AtomStart);
        if (AtomStart>length(PasExpression))
        or (PasExpression[AtomStart]<>'(') then break;
        // skip bracket content
        p:=AtomStart;
        ReadTilCBracketClose(PasExpression,p);
        AtomStart:=p-1;
      end else begin
        UsedNode:=FindH2PNodeWithCName(CurAtom);
        if (UsedNode<>nil) and (UsedNode.PascalDesc=ctnConstDefinition)
        then begin
          if UsedNode.PascalName<>CurAtom then
            Replace(UsedNode.PascalName);
        end else begin
          //
          DebugLn(['TH2PasTool.MacroValueIsConstant NO, because this is not a constant: ',CurAtom]);
          exit;
        end;
      end;
    end else if IsCHexNumber(PasExpression,AtomStart) then begin
      // hex number
      // replace 0x with $
      PasExpression:=copy(PasExpression,1,AtomStart-1)
        +'$'+copy(PasExpression,AtomStart+2,length(PasExpression));
      dec(p);
      if p-AtomStart>17 then begin
        // out of bounds
        DebugLn(['TH2PasTool.MacroValueIsConstant hex number out of bounds: "',PasExpression,'"']);
        exit;
      end;
    end else if IsCOctalNumber(PasExpression,AtomStart) then begin
      // octal number
      // replace 0 with &
      PasExpression[AtomStart]:='&';
    end else if IsCDecimalNumber(PasExpression,AtomStart) then begin
      // decimal number
    end else if PasExpression[AtomStart]='"' then begin
      PasExpression[AtomStart]:='''';
      PasExpression[p-1]:='''';
    end else begin
      CurAtom:=copy(PasExpression,AtomStart,p-AtomStart);
      if (CurAtom='(') or (CurAtom=')')
      or (CurAtom='+') or (CurAtom='-') then begin
        // same in pascal
      end else if (CurAtom='*') then begin
        // can be multiplication or dereference or pointer type
        if (AtomStart>1) and (IsNumberChar[PasExpression[AtomStart-1]]) then
        begin
          // is multiplication
        end else begin
          // don't know
          // At the moment all constants are allowed,
          // so it is most probable a multiplication
        end;
      end else if (CurAtom='|') or (CurAtom='||') then begin
        Replace('or');
      end else if (CurAtom='&') or (CurAtom='&&') then begin
        Replace('and');
      end else begin
        DebugLn(['TH2PasTool.MacroValueIsConstant NO ',CurAtom]);
        // unknown
        exit;
      end;
    end;
  until false;
  Result:=true;
end;

procedure TH2PasTool.SimplifyMacroRedefinition(var Node: TH2PDirectiveNode;
  const NewValue: string; NewStatus: TH2PMacroStatus;
  var NextNode: TH2PDirectiveNode; var Changed: boolean);
var
  Macro: TH2PMacroStats;
  Parent: TH2PBaseNode;
begin
  if Node.MacroName='' then exit;
  Macro:=FindMacro(Node.MacroName);
  if Macro=nil then exit;
  if Macro.LastDefineNode=nil then exit;
  if Macro.LastReadNode=nil then begin
    // macro was read, so last define is needed
    if (Node.H2PNode<>nil)
    and (Macro.LastDefineNode.Parent=Node.H2PNode.Parent)
    and (Macro.Status=NewStatus) and (Macro.Value=NewValue) then
    begin
      // value is kept => the new Node is a redefinition
      if (NextNode=Node) or (Node.HasAsChild(NextNode)) then
        NextNode:=TH2PDirectiveNode(Node.NextSkipChilds);
      DebugLn(['TH2PasTool.SimplifyMacroRedefinition DELETE redefinition ',Node.DescAsString(CTool)]);
      DeleteDirectiveNode(Node,false,false);
      Node:=nil;
      Changed:=true;
    end;
  end else begin
    // macro was not read since last write
    Parent:=Macro.LastDefineNode.Parent;
    repeat
      if Parent=Node.Parent then begin
        // last write was on same or lower level
        // => last write is not needed
        DebugLn(['TH2PasTool.SimplifyMacroRedefinition DELETE unused ',Macro.LastDefineNode.DescAsString(CTool)]);
        DeleteH2PNode(Macro.LastDefineNode);
        Changed:=true;
      end;
      if Parent=nil then break;
      Parent:=Parent.Parent;
    until false;
  end;
end;

procedure TH2PasTool.SimplifyUnusedDefines(Changed: boolean);
var
  AVLNode: TAVLTreeNode;
  Macro: TH2PMacroStats;
begin
  if Macros=nil then exit;
  AVLNode:=Macros.FindLowest;
  while AVLNode<>nil do begin
    Macro:=TH2PMacroStats(AVLNode.Data);
    if (Macro.LastDefineNode<>nil)
    and (Macro.LastReadNode=nil) then begin
      DebugLn(['TH2PasTool.SimplifyUnusedDefines DELETE unused ',Macro.LastDefineNode.DescAsString(CTool)]);
      DeleteH2PNode(Macro.LastDefineNode);
      Changed:=true;
    end;
    AVLNode:=Macros.FindSuccessor(AVLNode);
  end;
  if Changed then ;
end;

procedure TH2PasTool.DeleteDirectiveNode(Node: TH2PDirectiveNode;
  DeleteChilds: boolean; AdaptNeighborhood: boolean);
var
  Expression: String;
  Sibling: TH2PDirectiveNode;
  H2PNode: TH2PNode;
  EndIfNode: TH2PDirectiveNode;
begin
  if (Node.H2PNode<>nil) and (Node.H2PNode.FirstChild<>nil) then begin
    raise Exception.Create('TH2PasTool.DeleteDirectiveNode: inconsistency: a directive can not have H2P children');
  end;
  DebugLn(['TH2PasTool.DeleteDirectiveNode ',Node.DescAsString(CTool)]);

  if AdaptNeighborhood then begin
    // adapt following Else and ElseIf directives
    Expression:='';
    case Node.Desc of
    h2pdnIf,h2pdnElseIf: Expression:='not ('+Node.Expression+')';
    h2pdnIfDef: Expression:='not defined('+Node.MacroName+')';
    h2pdnIfNDef: Expression:='defined('+Node.MacroName+')';
    end;
    if Expression<>'' then begin
      Sibling:=TH2PDirectiveNode(Node.NextBrother);
      while Sibling<>nil do begin
        case Sibling.Desc of
        h2pdnElseIf:
          begin
            Sibling.Expression:='('+Sibling.Expression+') and '+Expression;
            if (Sibling.PriorBrother=Node) and (Node.Desc<>h2pdnElseIf) then
              Sibling.Desc:=h2pdnIf;
            DebugLn(['TH2PasTool.DeleteDirectiveNode ADAPTED neighbour: ',Sibling.DescAsString(CTool)]);
          end;
        h2pdnElse:
          begin
            Sibling.Expression:=Expression;
            if (Sibling.PriorBrother=Node) and (Node.Desc<>h2pdnElseIf) then
              Sibling.Desc:=h2pdnIf
            else
              Sibling.Desc:=h2pdnElseIf;
            DebugLn(['TH2PasTool.DeleteDirectiveNode ADAPTED neighbour: ',Sibling.DescAsString(CTool)]);
          end;
        else break;
        end;
        Sibling:=TH2PDirectiveNode(Sibling.NextBrother);
      end;
    end;
  end;
  
  // delete or move children
  if Node.FirstChild<>nil then begin
    if DeleteChilds then begin
      // delete directive children
      while Node.FirstChild<>nil do begin
        DeleteDirectiveNode(TH2PDirectiveNode(Node.FirstChild),true,false);
      end;
    end else begin
      // keep children
      // => move directive children one level up (in front of Node)
      if (Node.Desc<>h2pdnIf) and (Node.Desc<>h2pdnIfDef) and (Node.Desc<>h2pdnIfNDef)
      then
        raise Exception.Create('TH2PasTool.DeleteDirectiveNode: inconsistency: can not move children in front');
      DirectivesTree.MoveChildsInFront(Node);
    end;
  end;
  
  H2PNode:=Node.H2PNode;
  if H2PNode<>nil then begin
    H2PNode.Directive:=nil; // avoid circle between DeleteH2PNode and DeleteDirectiveNode
    Node.H2PNode:=nil;
    DeleteH2PNode(H2PNode);
  end;

  EndIfNode:=TH2PDirectiveNode(Node.NextBrother);
  if (EndIfNode<>nil) and (EndIfNode.Desc<>h2pdnEndIf) then
    EndIfNode:=nil;

  DirectivesTree.DeleteNode(Node);
  if AdaptNeighborhood and (EndIfNode<>nil) then
    DeleteDirectiveNode(EndIfNode,true,false);
end;

procedure TH2PasTool.DeleteH2PNode(Node: TH2PNode);
var
  DirNode: TH2PDirectiveNode;
  AVLNode: TAVLTreeNode;
  Macro: TH2PMacroStats;
begin
  DebugLn(['TH2PasTool.DeleteH2PNode ',Node.DescAsString(CTool)]);
  if Node.PascalName<>'' then
    FPascalNames.Remove(Node);
  if Node.CName<>'' then
    FCNames.Remove(Node);
  // delete children
  while Node.FirstChild<>nil do
    DeleteH2PNode(TH2PNode(Node.FirstChild));
  // delete directives
  DirNode:=Node.Directive;
  if DirNode<>nil then begin
    Node.Directive:=nil; // avoid circle between DeleteH2PNode and DeleteDirectiveNode
    DirNode.H2PNode:=nil;
    DeleteDirectiveNode(DirNode,false,true);
  end;
  // check references
  if Macros<>nil then begin
     AVLNode:=Macros.FindLowest;
     while AVLNode<>nil do begin
       Macro:=TH2PMacroStats(AVLNode.Data);
       if Macro.LastDefineNode=Node then
         Macro.LastDefineNode:=nil;
       if Macro.LastReadNode=Node then
         Macro.LastReadNode:=nil;
       AVLNode:=Macros.FindSuccessor(AVLNode);
     end;
  end;
  Tree.DeleteNode(Node);
end;

function TH2PasTool.Convert(CCode, PascalCode: TCodeBuffer): boolean;
begin
  Result:=false;

  if CTool=nil then
    CTool:=TCCodeParserTool.Create;
  // parse C header file
  CTool.Parse(CCode);
  CTool.WriteDebugReport;

  BuildH2PTree;
  
  SimplifyDirectives;
  
  WritePascal(PascalCode);

  Result:=true;
end;

procedure TH2PasTool.BuildH2PTree(ParentNode: TH2PNode;
  StartNode: TCodeTreeNode);
var
  CNode: TCodeTreeNode;
  NextCNode: TCodeTreeNode;
begin
  //DebugLn(['TH2PasTool.BuildH2PTree ParentNode=',ParentNode.DescAsString(CTool)]);
  if ParentNode<>nil then begin
    if StartNode=nil then
      StartNode:=ParentNode.CNode.FirstChild;
  end else begin
    Tree.Clear;
    if StartNode=nil then
      StartNode:=CTool.Tree.Root;
    DirectivesTree.Clear;
    FCurDirectiveNode:=TH2PDirectiveNode.Create;
    FCurDirectiveNode.Desc:=h2pdnRoot;
    DirectivesTree.AddNodeAsLastChild(nil,FCurDirectiveNode);
  end;
  CNode:=StartNode;
  while CNode<>nil do begin
    //DebugLn(['TH2PasTool.BuildH2PTree Current ParentNode=',ParentNode.DescAsString(CTool),' CNode=',CCNodeDescAsString(CNode.Desc)]);
    NextCNode:=CNode.NextSkipChilds;
    case CNode.Desc of
    ccnRoot, ccnExtern:
      NextCNode:=CNode.Next;
      
    ccnTypedef:
      ConvertTypedef(CNode,ParentNode);

    ccnVariable:
      ConvertVariable(CNode,ParentNode);

    ccnFunction:
      ConvertFunction(CNode,ParentNode);

    ccnFuncParamList:
      NextCNode:=CNode.FirstChild;
      
    ccnFuncParameter:
      ConvertFuncParameter(CNode,ParentNode);

    ccnEnumBlock:
      ConvertEnumBlock(CNode,ParentNode);

    ccnStruct:
      ConvertStruct(CNode,ParentNode);

    ccnName: ;

    ccnDirective:
      ConvertDirective(CNode,ParentNode);
    else
      DebugLn(['TH2PasTool.BuildH2PTree SKIPPING ',CCNodeDescAsString(CNode.Desc),' at ',CTool.CleanPosToStr(CNode.StartPos)]);
    end;
    // next C node
    if (ParentNode<>nil) and (not ParentNode.CNode.HasAsChild(NextCNode)) then
      NextCNode:=nil;
    CNode:=NextCNode;
  end;
end;

function TH2PasTool.FindEnclosingIFNDEF(CCode: TCodeBuffer): TCodeTreeNode;
begin
  if CTool=nil then
    CTool:=TCCodeParserTool.Create;
  // parse C header file
  CTool.Parse(CCode);
  Result:=CTool.FindEnclosingIFNDEF;
end;

procedure TH2PasTool.UndefineEnclosingIFNDEF(CCode: TCodeBuffer);
var
  Node: TCodeTreeNode;
  MacroName: String;
begin
  Node:=FindEnclosingIFNDEF(CCode);
  if Node=nil then exit;
  MacroName:=CTool.ExtractDirectiveFirstAtom(Node);
  if MacroName='' then exit;
  //DebugLn(['TH2PasTool.UndefineEnclosingIFNDEF UNDEFINE ',MacroName]);
  Undefines.Add(MacroName,'');
end;

procedure TH2PasTool.SimplifyDirectives;
(*  Check and improve the following cases
  1.a  {$DEFINE Name} and Name is never used afterwards -> disable

  1.b  {$DEFINE Name}
       ... Name is not used here ...
       {$DEFINE Name}
       -> disable first

  2.  {$IFDEF Name}... only comments and spaces ...{$ENDIF}
      -> disable the whole block

  3.  {$IFNDEF Name}
        ... only comments and spaces ...
        {$DEFINE Name}
        ... only comments and spaces ...
      {$ENDIF}
      -> disable the IFNDEF and the ENDIF and keep the DEFINE
*)
var
  Node: TH2PDirectiveNode;
  NextNode: TH2PDirectiveNode;
  Changed: Boolean;
  H2PNode: TH2PNode;
begin
  // Undefines.WriteDebugReport;
  repeat
    Changed:=false;
    InitMacros;
    Node:=TH2PDirectiveNode(DirectivesTree.Root);
    while Node<>nil do begin
      NextNode:=TH2PDirectiveNode(Node.Next);
      // mark all read macros between this node and NextNode
      H2PNode:=Node.H2PNode;
      if (H2PNode<>nil)
      and (NextNode<>nil) and (NextNode.H2PNode<>nil) then begin
        while H2PNode<>NextNode.H2PNode do begin
          if H2PNode.Directive=nil then
            MarkMacrosAsRead(H2PNode,H2PNode.PascalCode);
          H2PNode:=TH2PNode(H2PNode.Next);
        end;
      end;
      // simplify directive
      case Node.Desc of
      h2pdnUndefine:
        SimplifyUndefineDirective(Node,NextNode,Changed);
      h2pdnDefine:
        SimplifyDefineDirective(Node,NextNode,Changed);
      h2pdnIfDef:
        SimplifyIfDirective(Node,'defined('+Node.MacroName+')',NextNode,Changed);
      h2pdnIfNDef:
        SimplifyIfDirective(Node,'not defined('+Node.MacroName+')',NextNode,Changed);
      h2pdnIf:
        SimplifyIfDirective(Node,Node.Expression,NextNode,Changed);
      end;
      
      Node:=NextNode;
    end;
    SimplifyUnusedDefines(Changed);
  until not Changed;
end;

procedure TH2PasTool.WritePascal(PascalCode: TCodeBuffer);
var
  ms: TMemoryStream;
  NewSrc: string;
begin
  ms:=TMemoryStream.Create;
  try
    WritePascalToStream(ms);
  
    SetLength(NewSrc,ms.Size);
    if NewSrc<>'' then begin
      ms.Position:=0;
      ms.Read(NewSrc[1],length(NewSrc));
    end;
    PascalCode.Source:=NewSrc;
  finally
    ms.Free;
  end;
end;

procedure TH2PasTool.WritePascalToStream(s: TStream);
var
  H2PNode: TH2PNode;
  UsesClause: String;
begin
  FCurIndentStr:='';
  FCurPasSection:=ctnNone;
  FCurPasStream:=s;

  // write header
  if SourceName<>'' then begin
    W('unit '+SourceName+';');
    W('');
    W('{$mode objfpc}{$H+}');
    W('');
    W('interface');
    W('');
  end;
  
  // write uses
  UsesClause:='ctypes';
  if UsesClause<>'' then begin
    W('uses');
    IncIndent;
    W(UsesClause+';');
    DecIndent;
    W('');
  end;

  // write interface nodes
  H2PNode:=TH2PNode(Tree.Root);
  while H2PNode<>nil do begin
    case H2PNode.PascalDesc of
    
    ctnVarDefinition:
      WriteGlobalVarNode(H2PNode);

    ctnTypeDefinition:
      WriteGlobalTypeNode(H2PNode);

    ctnConstDefinition:
      WriteGlobalConstNode(H2PNode);
      
    ctnProcedure, ctnProcedureType:
      WriteGlobalProcedureNode(H2PNode);

    ctnEnumerationType:
      WriteGlobalEnumerationTypeNode(H2PNode);

    ctnRecordType:
      WriteGlobalRecordTypeNode(H2PNode);

    ctnNone:
      if H2PNode.Directive<>nil then begin
        WriteDirectiveNode(H2PNode.Directive);
      end else
        DebugLn(['TH2PasTool.WritePascalToStream SKIPPING ',H2PNode.DescAsString(CTool)]);

    else
      DebugLn(['TH2PasTool.WritePascalToStream SKIPPING ',H2PNode.DescAsString(CTool)]);
    end;
    H2PNode:=TH2PNode(H2PNode.NextBrother);
  end;
  
  // write implementation
  SetPasSection(ctnNone);
  W('implementation');
  W('');

  // write end.
  W('end.');
  
  FCurPasStream:=nil;
  FCurIndentStr:='';
end;

function TH2PasTool.GetSimplePascalTypeOfCVar(CVarNode: TCodeTreeNode): string;
begin
  Result:=CTool.ExtractVariableType(CVarNode);
  if Result='' then exit;
  Result:=ConvertSimpleCTypeToPascalType(Result,true);
end;

function TH2PasTool.GetSimplePascalTypeOfCParameter(CParamNode: TCodeTreeNode
  ): string;
begin
  Result:=CTool.ExtractParameterType(CParamNode);
  if Result='' then exit;
  if (Result='...') then
    Result:='array of const'
  else
    Result:=ConvertSimpleCTypeToPascalType(Result,true);
end;

function TH2PasTool.GetSimplePascalResultTypeOfCFunction(
  CFuncNode: TCodeTreeNode): string;
begin
  Result:=CTool.ExtractFunctionResultType(CFuncNode,false,false);
  if Result='' then exit;
  Result:=ConvertSimpleCTypeToPascalType(Result,true);
end;

function TH2PasTool.ConvertSimpleCTypeToPascalType(CType: string;
  UseSingleIdentifierAsDefault: boolean): string;
// the type must be normalized. That means no directives,
// no unneeded spaces, no tabs, no comments, no newlines.
var
  p: Integer;
  CurAtomStart: integer;
  
  function TestIsAtomAndRemove(const s: shortstring): boolean;
  begin
    if (p-CurAtomStart<>length(s))
    or (not CompareMem(@s[1],@CType[CurAtomStart],length(s))) then
      exit(false);
    Result:=true;
    // remove token
    if IsIdentStartChar[s[1]] then begin
      // token is a word => remove one space too
      if (CurAtomStart>1) and (CType[CurAtomStart-1]=' ') then
        dec(CurAtomStart)
      else if (p<=length(CType)) and (CType[p]=' ') then
        inc(p);
    end;
    // remove token
    CType:=copy(CType,1,CurAtomStart-1)+copy(CType,p,length(CType));
    p:=CurAtomStart;
    //DebugLn(['TH2PasTool.ConvertSimpleCTypeToPascalType CType="',CType,'"']);
  end;
  
begin
  // remove 'const' and 'struct'
  p:=1;
  repeat
    ReadRawNextCAtom(CType,p,CurAtomStart);
    if CurAtomStart>length(CType) then break;
    //DebugLn(['TH2PasTool.ConvertSimpleCTypeToPascalType Atom=',copy(CType,CurAtomStart,p-CurAtomStart)]);
    if (not TestIsAtomAndRemove('const'))
    and (not TestIsAtomAndRemove('struct')) then ;
  until false;
  // seach in predefined ctypes
  Result:=PredefinedCTypes[CType];
  
  if (Result='') and (UseSingleIdentifierAsDefault) and IsValidIdent(CType) then
    Result:=CType;
end;

function TH2PasTool.CreateH2PNode(var PascalName: string; const CName: string;
  CNode: TCodeTreeNode; PascalDesc: TCodeTreeNodeDesc;
  const PascalCode: string;
  ParentNode: TH2PNode; IsGlobal: boolean; InsertAsPreLast: boolean): TH2PNode;
begin
  if (PascalName<>'') and (PascalDesc<>ctnNone) and IsValidIdent(PascalName)
  then begin
    if WordIsKeyWord.DoItCaseInsensitive(PChar(PascalName)) then begin
      // C name is keyword => auto rename
      PascalName:=PascalName+'_';
    end;
    if IsGlobal then
      PascalName:=CreateUniquePascalName(PascalName);
  end;
    
  Result:=TH2PNode.Create;
  Result.PascalName:=PascalName;
  Result.CName:=CName;
  Result.CNode:=CNode;
  Result.PascalDesc:=PascalDesc;
  Result.PascalCode:=PascalCode;
  if InsertAsPreLast then
    Tree.AddNodeAsPreLastChild(ParentNode,Result)
  else
    Tree.AddNodeAsLastChild(ParentNode,Result);
  if IsGlobal then begin
    if PascalName<>'' then
      FPascalNames.Add(Result);
    if CName<>'' then
      FCNames.Add(Result);
  end;
end;

function TH2PasTool.CreateAutoGeneratedH2PNode(var PascalName: string;
  CNode: TCodeTreeNode; PascalDesc: TCodeTreeNodeDesc;
  const PascalCode: string;
  ParentNode: TH2PNode;
  IsGlobal: boolean; InsertAsPreLast: boolean): TH2PNode;
  
  function Check(TestName: string; out Node: TH2PNode): boolean;
  begin
    Node:=FindH2PNodeWithPascalName(TestName);
    if (Node=nil) then begin
      Node:=CreateH2PNode(TestName,'',CNode,PascalDesc,PascalCode,nil,
                          true,InsertAsPreLast);
      Result:=true;
    end else if ((Node.CNode=CNode) and (Node.PascalDesc=PascalDesc)
      and (Node.PascalCode=PascalCode)
      and (Node.Parent=ParentNode))
    then begin
      Result:=true;
    end else begin
      Result:=false;
      Node:=nil;
    end;
  end;
  
var
  i: Integer;
begin
  Result:=nil;
  if Check(PascalName,Result) then exit;
  i:=1;
  while not Check(PascalName+'_'+IntToStr(i),Result) do
    inc(i);
end;

function TH2PasTool.GetH2PNodeForComplexType(CNode: TCodeTreeNode;
  CreateIfNotExists: boolean; InsertAsPreLast: boolean): TH2PNode;
var
  CCode: String;
  PascalName: String;
  AtomStart: integer;
  p: Integer;
  CurAtom: String;
  BaseCType: String;
  BasePascalType: String;
  NewBasePascalType: String;
  SubH2PNode: TH2PNode;
  PascalCode: String;
  ConstantStartPos: LongInt;
  ConstantEndPos: LongInt;
  ConstantCode: String;
  ConstantNumber: int64;
  BracketOpenPos: LongInt;
  NeedH2PNode: Boolean;
begin
  Result:=nil;
  if (CNode.Desc=ccnVariable)
  and (CNode.FirstChild<>nil)
  and (CNode.FirstChild.Desc=ccnUnion) then begin
    // ToDo: union
  end else begin
    SubH2PNode:=nil;
    if CNode.Desc=ccnVariable then
      CCode:=CTool.ExtractVariableType(CNode)
    else if CNode.Desc=ccnFunction then
      CCode:=CTool.ExtractFunctionResultType(CNode)
    else if CNode.Desc=ccnFuncParameter then
      CCode:=CTool.ExtractParameterType(CNode)
    else
      exit;
      
    DebugLn(['TH2PasTool.GetH2PNodeForComplexType CCode="',CCode,'"']);
    { int[][3]  -> array of array[0..2] of cint
      char**    -> PPchar
      int *[15] -> array[0..14] of pcint
      
    }
    // read identifiers
    p:=1;
    BaseCType:='';
    repeat
      ReadRawNextCAtom(CCode,p,AtomStart);
      if AtomStart>length(CCode) then break;
      if IsIdentStartChar[CCode[AtomStart]] then begin
        CurAtom:=copy(CCode,AtomStart,p-AtomStart);
        if BaseCType<>'' then
          BaseCType:=BaseCType+' ';
        BaseCType:=BaseCType+CurAtom;
      end else
        break;
    until false;
    if BaseCType='' then begin
      DebugLn(['TH2PasTool.GetH2PNodeForComplexType no base type in c declaration: CCode="',dbgstr(CCode),'"']);
      exit;
    end;
    BasePascalType:=ConvertSimpleCTypeToPascalType(BaseCType,true);
    if (BasePascalType='') then begin
      DebugLn(['TH2PasTool.GetH2PNodeForComplexType unknown c type: "',BaseCType,'"']);
      exit;
    end;
    DebugLn(['TH2PasTool.GetH2PNodeForComplexType BasePascalType="',BasePascalType,'" BaseCType="',BaseCType,'"']);
    
    // read pointer(s)
    while (AtomStart<=length(CCode)) do begin
      CurAtom:=copy(CCode,AtomStart,p-AtomStart);
      if (CurAtom='*') then begin
        BaseCType:=BaseCType+'*';
        NewBasePascalType:=ConvertSimpleCTypeToPascalType(BaseCType,true);
        if NewBasePascalType<>'' then begin
          // for this pointer type exists already a predefined simple type
        end else begin
          // a new pointer type is needed
          NewBasePascalType:='P'+BasePascalType;
          SubH2PNode:=CreateAutoGeneratedH2PNode(NewBasePascalType,nil,
                                    ctnTypeDefinition,'^'+BasePascalType,
                                    nil,true,InsertAsPreLast);
          DebugLn(['TH2PasTool.GetH2PNodeForComplexType added new pointer type: ',SubH2PNode.DescAsString(CTool)]);
          NewBasePascalType:=SubH2PNode.PascalName;
        end;
        BasePascalType:=NewBasePascalType;
        DebugLn(['TH2PasTool.GetH2PNodeForComplexType using pointer type: BasePascalType="',BasePascalType,'" BaseCType="',BaseCType,'"']);
      end else if (CurAtom='const') then begin
        // skip 'const'
      end else begin
        break;
      end;
      ReadRawNextCAtom(CCode,p,AtomStart);
    end;
    
    PascalName:=BasePascalType;
    PascalCode:=PascalName;
    
    // read arrays
    NeedH2PNode:=false;
    while (AtomStart<=length(CCode)) do begin
      CurAtom:=copy(CCode,AtomStart,p-AtomStart);
      if CurAtom='[' then begin
        NeedH2PNode:=true;
        BracketOpenPos:=AtomStart;
        ReadRawNextCAtom(CCode,p,AtomStart);
        if AtomStart>length(CCode) then begin
          DebugLn(['TH2PasTool.GetH2PNodeForComplexType untranslatable (missing ]): CCode="',dbgstr(CCode),'"']);
          exit;
        end;
        CurAtom:=copy(CCode,AtomStart,p-AtomStart);
        if CurAtom=']' then begin
          // [] -> open array
          PascalCode:='array of '+PascalCode;
          PascalName:='ArrayOf'+PascalName;
          //DebugLn(['TH2PasTool.GetTypeForVarType open array: ',PascalCode]);
        end else begin
          // [constant] -> array[0..constant-1]
          ConstantStartPos:=AtomStart;
          p:=BracketOpenPos;
          ReadTilCBracketClose(CCode,p);
          ConstantEndPos:=p-1;
          ConstantCode:=copy(CCode,ConstantStartPos,ConstantEndPos-ConstantStartPos);
          //DebugLn(['TH2PasTool.GetTypeForVarType ConstantCode="',ConstantCode,'"']);
          if CConstantToInt64(ConstantCode,ConstantNumber) then begin
            if ConstantNumber>0 then
              dec(ConstantNumber)
            else
              ConstantNumber:=0;
            ConstantCode:=IntToStr(ConstantNumber);
          end else begin
            ConstantCode:=ConstantCode+'-1';
          end;
          PascalCode:='array[0..'+ConstantCode+'] of '+PascalCode;
          PascalName:='Array0to'+CreatePascalNameFromCCode(ConstantCode)+'Of'+PascalName;
          //DebugLn(['TH2PasTool.GetTypeForVarType fixed array: ',PascalCode]);
        end;
      end else
        break;
      ReadRawNextCAtom(CCode,p,AtomStart);
    end;
    if NeedH2PNode then begin
      PascalName:='T'+PascalName;
      PascalName:=copy(PascalName,1,DefaultMaxPascalIdentLen);
      SubH2PNode:=CreateAutoGeneratedH2PNode(PascalName,nil,ctnTypeDefinition,
        PascalCode,nil,true,InsertAsPreLast);
    end;
    
    // check if the whole declaration was translated
    if AtomStart<=length(CCode) then begin
      // unknown C type
      DebugLn(['TH2PasTool.GetTypeForVarType untranslatable: CCode="',dbgstr(CCode),'"']);
      exit;
    end;
    
    DebugLn(['TH2PasTool.GetTypeForVarType CCode="',dbgstr(CCode),'" PascalName="',PascalName,'"']);
    Result:=SubH2PNode;
  end;
end;

function TH2PasTool.CreatePascalNameFromCCode(const CCode: string;
  StartPos: integer; EndPos: integer): string;

  function Add(var PascalName: string; const Addition: string): boolean;
  begin
    if Addition='' then exit(true);
    if length(PascalName)+length(Addition)>DefaultMaxPascalIdentLen then
      exit(false);
    PascalName:=PascalName+Addition;
  end;
  
var
  p: Integer;
  AtomStart: integer;
  i: LongInt;
  c: Char;
  CurAtom: String;
begin
  Result:='';
  if EndPos<1 then
    EndPos:=length(CCode)+1;
  p:=StartPos;
  if EndPos>length(CCode) then
    EndPos:=length(CCode);
  repeat
    ReadRawNextCAtom(CCode,p,AtomStart);
    if AtomStart>EndPos then exit;
    
    if IsIdentStartChar[CCode[AtomStart]] then begin
      CurAtom:=copy(CCode,AtomStart,p-AtomStart);
      if (CurAtom<>'const')
      and (CurAtom<>'struct')
      and not Add(Result,CurAtom) then
        exit;
    end else begin
      if CCode[AtomStart] in ['0'..'9'] then begin
        CurAtom:=copy(CCode,AtomStart,p-AtomStart);
        for i:=AtomStart to p-1 do begin
          c:=CCode[i];
          if not IsIdentChar[c] then
            c:='_';
          if not Add(Result,c) then exit;
        end;
      end;
    end;
  until false;
end;

function TH2PasTool.CreateUniquePascalName(const CName: string): string;
var
  i: Integer;
begin
  Result:=CName;
  if FindH2PNodeWithPascalName(Result)=nil then exit;
  i:=1;
  repeat
    Result:=CName+'_'+IntToStr(i);
    if FindH2PNodeWithPascalName(Result)=nil then exit;
    inc(i);
  until false;
end;

function TH2PasTool.FindH2PNodeWithPascalName(const PascalName: string
  ): TH2PNode;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FPascalNames.FindKey(Pointer(PascalName),
                                @CompareStringWithH2PNodePascalName);
  if AVLNode<>nil then
    Result:=TH2PNode(AVLNode.Data)
  else
    Result:=nil;
end;

function TH2PasTool.FindH2PNodeWithCName(const CName: string): TH2PNode;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FCNames.FindKey(Pointer(CName),
                           @CompareStringWithH2PNodeCName);
  if AVLNode<>nil then
    Result:=TH2PNode(AVLNode.Data)
  else
    Result:=nil;
end;

function TH2PasTool.CreateH2PDirectiveNode(H2PNode: TH2PNode;
  Desc: TCodeTreeNodeDesc): TH2PDirectiveNode;
begin
  Result:=TH2PDirectiveNode.Create;
  Result.Desc:=Desc;
  H2PNode.Directive:=Result;
  Result.H2PNode:=H2PNode;
  DirectivesTree.AddNodeAsLastChild(FCurDirectiveNode,Result);
  //DebugLn(['TH2PasTool.CreateH2PDirectiveNode Added ',Result.DescAsString,' ',FCurDirectiveNode.FirstChild<>nil]);
end;

procedure TH2PasTool.WriteDebugReport;
begin
  DebugLn(['TH2PasTool.WriteDebugReport ']);
  if CTool<>nil then
    CTool.WriteDebugReport;
  WriteH2PNodeReport;
  WriteH2PDirectivesNodeReport;
end;

procedure TH2PasTool.WriteH2PNodeReport;
var
  Node: TH2PBaseNode;
begin
  if (Tree=nil) then begin
    DebugLn(['TH2PasTool.WriteH2PNodeReport Tree=nil']);
  end else if (Tree.Root=nil) then begin
    DebugLn(['TH2PasTool.WriteH2PNodeReport Tree.Root=nil']);
  end else begin
    Node:=Tree.Root;
    while Node<>nil do begin
      DebugLn([GetIndentStr(Node.GetLevel*2),Node.DescAsString(CTool)]);
      Node:=Node.Next;
    end;
  end;
end;

procedure TH2PasTool.WriteH2PDirectivesNodeReport;
var
  Node: TH2PBaseNode;
begin
  if (DirectivesTree=nil) then begin
    DebugLn(['TH2PasTool.WriteH2PDirectivesNodeReport Tree=nil']);
  end else if (DirectivesTree.Root=nil) then begin
    DebugLn(['TH2PasTool.WriteH2PDirectivesNodeReport Tree.Root=nil']);
  end else begin
    Node:=DirectivesTree.Root;
    while Node<>nil do begin
      DebugLn([GetIndentStr(Node.GetLevel*2),Node.DescAsString(CTool)]);
      Node:=Node.Next;
    end;
  end;
end;

constructor TH2PasTool.Create;
begin
  FPredefinedCTypes:=DefaultPredefinedCTypes;
  Tree:=TH2PTree.Create;
  DirectivesTree:=TH2PTree.Create;
  FPascalNames:=TAVLTree.Create(@CompareH2PNodePascalNames);
  FCNames:=TAVLTree.Create(@CompareH2PNodeCNames);
  FIgnoreCParts:=[icspInclude];
  FDefines:=TStringToStringTree.Create(true);
  FUndefines:=TStringToStringTree.Create(true);
  AddCommonCDefines;
end;

destructor TH2PasTool.Destroy;
begin
  FPredefinedCTypes:=nil;
  Clear;
  FreeAndNil(DirectivesTree);
  FreeAndNil(Tree);
  FreeAndNil(FPascalNames);
  FreeAndNil(FCNames);
  FreeAndNil(CTool);
  FreeAndNil(FDefines);
  FreeAndNil(FUndefines);
  inherited Destroy;
end;

procedure TH2PasTool.Clear;
begin
  FPascalNames.Clear;
  FCNames.Clear;
  Tree.Clear;
  DirectivesTree.Clear;
  ClearMacros;
  FDefines.Clear;
  FUndefines.Clear;
  AddCommonCDefines;
end;

procedure TH2PasTool.AddCommonCDefines;
begin
  Undefines['__cplusplus']:='1';// avoid C++ and use the easier c part
  Defines['__GNUC__']:='1';// assume the GNUC compiler
end;

procedure TH2PasTool.ResetMacros;
begin
  if Macros<>nil then
    Macros.FreeAndClear
  else
    Macros:=TAVLTree.Create(@CompareH2PMacroStats);
end;

procedure TH2PasTool.ClearMacros;
begin
  if Macros<>nil then begin
    Macros.FreeAndClear;
    FreeAndNil(Macros);
  end;
end;

procedure TH2PasTool.InitMacros;
var
  List: TStringList;
  i: Integer;
  CurName: string;
  CurValue: string;
begin
  ResetMacros;
  if FDefines<>nil then begin
    List:=TStringList.Create;
    FDefines.GetNames(List);
    for i:=0 to List.Count-1 do begin
      CurName:=List[i];
      CurValue:=FDefines[CurName];
      DefineMacro(CurName,CurValue,nil);
    end;
    List.Free;
  end;
  if FUndefines<>nil then begin
    List:=TStringList.Create;
    FUndefines.GetNames(List);
    for i:=0 to List.Count-1 do begin
      CurName:=List[i];
      UndefineMacro(CurName,nil);
    end;
    List.Free;
  end;
end;

function TH2PasTool.FindMacro(const MacroName: string;
  CreateIfNotExists: boolean): TH2PMacroStats;
var
  AVLNode: TAVLTreeNode;
begin
  Result:=nil;
  if Macros=nil then begin
    if not CreateIfNotExists then
      exit;
    Macros:=TAVLTree.Create(@CompareH2PMacroStats);
  end;
  AVLNode:=Macros.FindKey(Pointer(MacroName),
                          @ComparePCharWithH2PMacroStats);
  if AVLNode<>nil then
    Result:=TH2PMacroStats(AVLNode.Data)
  else if CreateIfNotExists then begin
    Result:=TH2PMacroStats.Create;
    Result.Name:=MacroName;
    Result.Status:=hmsUnknown;
    Macros.Add(Result);
  end;
end;

function TH2PasTool.DefineMacro(const MacroName, AValue: string;
  DefineNode: TH2PNode): TH2PMacroStats;
begin
  Result:=FindMacro(MacroName,true);
  Result.Value:=AValue;
  Result.Status:=hmsDefined;
  Result.LastDefineNode:=DefineNode;
  Result.LastReadNode:=nil;
end;

function TH2PasTool.UndefineMacro(const MacroName: string;
  UndefineNode: TH2PNode): TH2PMacroStats;
begin
  Result:=FindMacro(MacroName,true);
  Result.Value:='';
  Result.Status:=hmsUndefined;
  Result.LastDefineNode:=UndefineNode;
  Result.LastReadNode:=nil;
end;

procedure TH2PasTool.MarkMacrosAsRead(Node: TH2PNode; const Src: string;
  StartPos: integer; EndPos: integer);
var
  AtomStart: integer;
begin
  if EndPos<1 then EndPos:=length(Src)+1;
  if EndPos>length(Src) then EndPos:=length(Src)+1;
  repeat
    ReadRawNextCAtom(Src,StartPos,AtomStart);
    if AtomStart>=EndPos then break;
    if IsIdentStartChar[Src[AtomStart]] then begin
      MarkMacroAsRead(GetIdentifier(@Src[AtomStart]),Node);
    end;
  until false;
end;

function TH2PasTool.MarkMacroAsRead(const MacroName: string; Node: TH2PNode
  ): TH2PMacroStats;
begin
  Result:=FindMacro(MacroName,false);
  if Result<>nil then
    Result.LastReadNode:=Node;
end;

{ TH2PNode }

function TH2PNode.DescAsString(CTool: TCCodeParserTool): string;
begin
  if Self=nil then begin
    Result:='nil';
    exit;
  end;
  Result:='{PascalName="'+PascalName+'"';
  if PascalName<>CName then
    Result:=Result+',CName="'+CName+'"';
  Result:=Result+',PascalDesc="'+NodeDescriptionAsString(PascalDesc)+'"';
  if CNode<>nil then begin
    Result:=Result+',CNode='+CCNodeDescAsString(CNode.Desc);
    if CTool<>nil then
      Result:=Result+'('+CTool.CleanPosToStr(CNode.StartPos)+')';
  end else begin
    Result:=Result+', CNode=nil';
  end;
  if PascalCode<>'' then
    Result:=Result+',PascalCode="'+dbgstr(PascalCode)+'"';
  Result:=Result+'}';
end;

{ TH2PTree }

procedure TH2PTree.Unbind(Node: TH2PBaseNode);
begin
  if Node=Root then Root:=Root.NextBrother;
  if Node=LastRoot then LastRoot:=LastRoot.PriorBrother;
  with Node do begin
    if (Parent<>nil) then begin
      if (Parent.FirstChild=Node) then
        Parent.FirstChild:=NextBrother;
      if (Parent.LastChild=Node) then
        Parent.LastChild:=PriorBrother;
      Parent:=nil;
    end;
    if NextBrother<>nil then NextBrother.PriorBrother:=PriorBrother;
    if PriorBrother<>nil then PriorBrother.NextBrother:=NextBrother;
    NextBrother:=nil;
    PriorBrother:=nil;
  end;
  dec(FNodeCount);
end;

constructor TH2PTree.Create;
begin
  Root:=nil;
  FNodeCount:=0;
end;

destructor TH2PTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TH2PTree.Clear;
var ANode: TH2PBaseNode;
begin
  while Root<>nil do begin
    ANode:=Root;
    Root:=ANode.NextBrother;
    DeleteNode(ANode);
  end;
end;

procedure TH2PTree.DeleteNode(ANode: TH2PBaseNode);
begin
  if ANode=nil then exit;
  while (ANode.FirstChild<>nil) do DeleteNode(ANode.FirstChild);
  Unbind(ANode);
  ANode.Free;
end;

procedure TH2PTree.AddNodeAsLastChild(ParentNode, ANode: TH2PBaseNode);
begin
  if ParentNode=ANode then RaiseCatchableException('');
  ANode.Parent:=ParentNode;
  if Root=nil then begin
    // set as root
    Root:=ANode;
    while Root.Parent<>nil do Root:=Root.Parent;
    LastRoot:=Root;
    while LastRoot.NextBrother<>nil do
      LastRoot:=LastRoot.NextBrother;
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
    while (LastRoot.NextBrother<>nil) do LastRoot:=LastRoot.NextBrother;
    ANode.PriorBrother:=LastRoot;
    ANode.PriorBrother.NextBrother:=ANode;
    LastRoot:=ANode;
    while (LastRoot.NextBrother<>nil) do LastRoot:=LastRoot.NextBrother;
  end;
  inc(FNodeCount);
end;

procedure TH2PTree.AddNodeAsPreLastChild(ParentNode, ANode: TH2PBaseNode);
begin
  if (ParentNode=nil) and (LastRoot<>nil) then
    AddNodeInFrontOf(LastRoot,ANode)
  else if (ParentNode<>nil) and (ParentNode.FirstChild<>nil) then
    AddNodeInFrontOf(ParentNode.LastChild,ANode)
  else
    AddNodeAsLastChild(ParentNode,ANode);
end;

procedure TH2PTree.AddNodeInFrontOf(NextBrotherNode, ANode: TH2PBaseNode);
begin
  ANode.Parent:=NextBrotherNode.Parent;
  ANode.NextBrother:=NextBrotherNode;
  ANode.PriorBrother:=NextBrotherNode.PriorBrother;
  NextBrotherNode.PriorBrother:=ANode;
  if ANode.PriorBrother<>nil then
    ANode.PriorBrother.NextBrother:=ANode;
  if Root=NextBrotherNode then
    Root:=ANode;
  inc(FNodeCount);
end;

procedure TH2PTree.MoveChildsInFront(ANode: TH2PBaseNode);
var
  ChildNode: TH2PBaseNode;
begin
  if ANode.FirstChild=nil then exit;
  ANode.LastChild.NextBrother:=ANode;
  if ANode.PriorBrother<>nil then begin
    ANode.FirstChild.PriorBrother:=ANode.PriorBrother;
    ANode.PriorBrother.NextBrother:=ANode.FirstChild;
  end;
  ANode.PriorBrother:=ANode.LastChild;
  if Root=ANode then Root:=ANode.FirstChild;
  ChildNode:=ANode.FirstChild;
  while ChildNode<>nil do begin
    ChildNode.Parent:=ANode.Parent;
    ChildNode:=ChildNode.NextBrother;
  end;
  ANode.FirstChild:=nil;
  ANode.LastChild:=nil;
end;

function TH2PTree.ContainsNode(ANode: TH2PBaseNode): boolean;
begin
  if ANode=nil then exit(false);
  while ANode.Parent<>nil do ANode:=ANode.Parent;
  while ANode.PriorBrother<>nil do ANode:=ANode.PriorBrother;
  Result:=ANode=Root;
end;

procedure TH2PTree.ConsistencyCheck;
// 0 = ok
var RealNodeCount: integer;

  procedure CountNodes(ANode: TH2PBaseNode);
  begin
    if ANode=nil then exit;
    inc(RealNodeCount);
    CountNodes(ANode.FirstChild);
    CountNodes(ANode.NextBrother);
  end;

begin
  if Root<>nil then begin
    Root.ConsistencyCheck;
    if Root.Parent<>nil then
      raise Exception.Create('Root.Parent<>nil');
  end;
  RealNodeCount:=0;
  CountNodes(Root);
  if RealNodeCount<>FNodeCount then
    raise Exception.Create('RealNodeCount<>FNodeCount');
end;

procedure TH2PTree.WriteDebugReport(WithChilds: boolean);
begin
  DebugLn('[TH2PTree.WriteDebugReport] Root=',dbgs(Root<>nil));
  if Root<>nil then
    Root.WriteDebugReport(' ',true);
  ConsistencyCheck;
end;

{ TH2PDirectiveNode }

function TH2PDirectiveNode.DescAsString(CTool: TCCodeParserTool): string;
begin
  if Self=nil then begin
    Result:='nil';
    exit;
  end;
  Result:='{'+H2PDirectiveNodeDescriptionAsString(Desc);
  if (H2PNode<>nil) and (H2PNode.CNode<>nil) and (CTool<>nil) then begin
    Result:=Result+'('+CTool.CleanPosToStr(H2PNode.CNode.StartPos)+')';
  end;
  case Desc of
  h2pdnDefine,h2pdnUndefine,h2pdnIfDef,h2pdnIfNDef:
    Result:=Result+',MacroName="'+dbgstr(MacroName)+'"';
  end;
  case Desc of
  h2pdnDefine,h2pdnIf,h2pdnElseIf:
    Result:=Result+',Expression="'+dbgstr(Expression)+'"';
  end;
  Result:=Result+'}';
end;

{ TH2PBaseNode }

function TH2PBaseNode.Next: TH2PBaseNode;
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

function TH2PBaseNode.NextSkipChilds: TH2PBaseNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.NextBrother=nil) do
    Result:=Result.Parent;
  if Result<>nil then Result:=Result.NextBrother;
end;

function TH2PBaseNode.Prior: TH2PBaseNode;
begin
  if PriorBrother<>nil then begin
    Result:=PriorBrother;
    while Result.LastChild<>nil do
      Result:=Result.LastChild;
  end else
    Result:=Parent;
end;

function TH2PBaseNode.HasAsParent(Node: TH2PBaseNode): boolean;
var
  CurNode: TH2PBaseNode;
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

function TH2PBaseNode.HasAsChild(Node: TH2PBaseNode): boolean;
begin
  Result:=false;
  if Node=nil then exit;
  Result:=Node.HasAsParent(Self);
end;

function TH2PBaseNode.GetLevel: integer;
var
  ANode: TH2PBaseNode;
begin
  Result:=0;
  ANode:=Parent;
  while ANode<>nil do begin
    inc(Result);
    ANode:=ANode.Parent;
  end;
end;

procedure TH2PBaseNode.ConsistencyCheck;
begin
  if (Parent<>nil) then begin
    if (PriorBrother=nil) and (Parent.FirstChild<>Self) then
      raise Exception.Create('');
    if (NextBrother=nil) and (Parent.LastChild<>Self) then
      raise Exception.Create('');
  end;
  if (NextBrother<>nil) and (NextBrother.PriorBrother<>Self) then
    raise Exception.Create('');
  if (PriorBrother<>nil) and (PriorBrother.NextBrother<>Self) then
    raise Exception.Create('');
  if (FirstChild<>nil) then
    FirstChild.ConsistencyCheck;
  if NextBrother<>nil then
    NextBrother.ConsistencyCheck;
end;

procedure TH2PBaseNode.WriteDebugReport(const Prefix: string;
  WithChilds: boolean; CTool: TCCodeParserTool);
var
  Node: TH2PBaseNode;
begin
  DebugLn([Prefix,DescAsString(CTool)]);
  if WithChilds then begin
    Node:=FirstChild;
    while Node<>nil do begin
      Node.WriteDebugReport(Prefix+'  ',true,CTool);
      Node:=Node.NextBrother;
    end;
  end;
end;

finalization
  FreeAndNil(InternalPredefinedCTypes);

end.

