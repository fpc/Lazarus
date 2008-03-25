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
    
    enum     ->  enum
    int i;   ->  var i: integer;
    const char a; -> const a: char;
    struct   ->  var plus record
    union    ->  var plus record case
    typedef  ->  type
    void func()  -> procedure
    int func()   -> function
    #define name value  ->  alias  (const, var, type, proc)
}
unit H2PasTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, AVL_Tree,
  FileProcs, BasicCodeTools, CCodeParserTool, NonPascalCodeTools,
  KeywordFuncLists, CodeCache, CodeTree, CodeAtom;
  
const
  DefaultMaxPascalIdentLen = 70;

type

  { TH2PNode }

  TH2PNode = class
  public
    PascalName: string;
    CName: string;
    CNode: TCodeTreeNode;
    PascalDesc: TCodeTreeNodeDesc;
    PascalCode: string;
    NormalizedPascalCode: string;
    Parent, FirstChild, LastChild, NextBrother, PriorBrother: TH2PNode;
    function Next: TH2PNode;
    function NextSkipChilds: TH2PNode;
    function Prior: TH2PNode;
    function HasAsParent(Node: TH2PNode): boolean;
    function HasAsChild(Node: TH2PNode): boolean;
    function GetLevel: integer;
    function DescAsString(CTool: TCCodeParserTool = nil): string;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(const Prefix: string; WithChilds: boolean;
                               CTool: TCCodeParserTool = nil);
  end;
  
  { TH2PTree }

  TH2PTree = class
  private
    FNodeCount: integer;
  public
    Root: TH2PNode;
    LastRoot: TH2PNode;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property NodeCount: integer read FNodeCount;
    procedure DeleteNode(ANode: TH2PNode);
    procedure AddNodeAsLastChild(ParentNode, ANode: TH2PNode);
    procedure AddNodeInFrontOf(NextBrotherNode, ANode: TH2PNode);
    function ContainsNode(ANode: TH2PNode): boolean;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(WithChilds: boolean);
  end;
  
  TIgnoreCSourcePart = (
    icspInclude
    );
  TIgnoreCSourceParts = set of TIgnoreCSourcePart;
  
  { TH2PasTool }

  TH2PasTool = class
  private
    FIgnoreCParts: TIgnoreCSourceParts;
    FPredefinedCTypes: TFPStringHashTable;
    FPascalNames: TAVLTree;// tree of TH2PNode sorted for PascalName
    FCNames: TAVLTree;// tree of TH2PNode sorted for CName
    FSourceName: string;
    procedure ConvertStruct(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertVariable(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertEnumBlock(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertFunction(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertFuncParameter(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertTypedef(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure ConvertDirective(CNode: TCodeTreeNode; ParentNode: TH2PNode);
    procedure SetIgnoreCParts(const AValue: TIgnoreCSourceParts);
    function ConvertCToPascalDirectiveExpression(const CCode: string;
           StartPos, EndPos: integer; out PasExpr: string;
           out ErrorPos: integer; out ErrorMsg: string): boolean;

    procedure WriteStr(const Line: string; s: TStream);
    procedure WriteLnStr(const Line: string; s: TStream);
  public
    Tree: TH2PTree;
    CTool: TCCodeParserTool;
    function Convert(CCode, PascalCode: TCodeBuffer): boolean;
    procedure BuildH2PTree(ParentNode: TH2PNode = nil; StartNode: TCodeTreeNode = nil);
    procedure WritePascal(PascalCode: TCodeBuffer);
    procedure WritePascalToStream(s: TStream);
    
    function GetSimplePascalTypeOfCVar(CVarNode: TCodeTreeNode): string;
    function GetSimplePascalTypeOfCParameter(CParamNode: TCodeTreeNode): string;
    function GetSimplePascalResultTypeOfCFunction(CFuncNode: TCodeTreeNode): string;
    function ConvertSimpleCTypeToPascalType(CType: string;
                  UseSingleIdentifierAsDefault: boolean): string;
    
    function CreateH2PNode(const PascalName, CName: string; CNode: TCodeTreeNode;
       PascalDesc: TCodeTreeNodeDesc; const PascalCode: string;
       ParentNode: TH2PNode = nil; IsGlobal: boolean = true): TH2PNode;
    function CreateAutoGeneratedH2PNode(var PascalName: string; CNode: TCodeTreeNode;
       PascalDesc: TCodeTreeNodeDesc; const PascalCode: string;
       ParentNode: TH2PNode = nil; IsGlobal: boolean = true): TH2PNode;
    function GetH2PNodeForComplexType(CNode: TCodeTreeNode;
                               CreateIfNotExists: boolean = true): TH2PNode;
    function CreatePascalNameFromCCode(const CCode: string;
                                       StartPos: integer = 1;
                                       EndPos: integer = -1): string;
    function FindH2PNodeWithPascalName(const PascalName: string): TH2PNode;

    procedure WriteDebugReport;
    procedure WriteH2PNodeReport;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property PredefinedCTypes: TFPStringHashTable read FPredefinedCTypes;
    property IgnoreCParts: TIgnoreCSourceParts read FIgnoreCParts write SetIgnoreCParts;
    property SourceName: string read FSourceName write FSourceName;
  end;
  
  
function DefaultPredefinedCTypes: TFPStringHashTable;// types in unit ctypes

function CompareH2PNodePascalNames(Data1, Data2: Pointer): integer;
function CompareStringWithH2PNodePascalName(AString, ANode: Pointer): integer;
function CompareH2PNodeCNames(Data1, Data2: Pointer): integer;

implementation

var
  InternalPredefinedCTypes: TFPStringHashTable = nil;// types in unit ctypes

function DefaultPredefinedCTypes: TFPStringHashTable;
begin
  if InternalPredefinedCTypes=nil then begin
    InternalPredefinedCTypes:=TFPStringHashTable.Create;
    with InternalPredefinedCTypes do begin
      // int
      Add('int','cint');
      Add('int*','pcint');
      Add('signed int','csint');
      Add('signed int*','pcsint');
      Add('unsigned int','cuint');
      Add('unsigned int*','pcuint');
      // short
      Add('short','cshort');
      Add('short*','pcshort');
      Add('signed short','csshort');
      Add('signed short*','pcsshort');
      Add('unsigned short','csshort');
      Add('unsigned short*','pcsshort');
      Add('short int','cshort');
      Add('short int*','pcshort');
      Add('signed short int','csshort');
      Add('signed short int*','pcsshort');
      Add('short signed int','csshort');
      Add('short signed int*','pcsshort');
      Add('short unsigned int','csshort');
      Add('short unsigned int*','pcsshort');
      // int8
      Add('int8','cint8');
      Add('int8*','pcint8');
      Add('unsigned int8','cuint8');
      Add('unsigned int8*','pcuint8');
      // int16
      Add('int16','cint16');
      Add('int16*','pcint16');
      Add('unsigned int16','cuint16');
      Add('unsigned int16*','pcuint16');
      // int32
      Add('int32','cint32');
      Add('int32*','pcint32');
      Add('unsigned int32','cuint32');
      Add('unsigned int32*','pcuint32');
      // int64
      Add('int64','cint64');
      Add('int64*','pcint64');
      Add('unsigned int64','cuint64');
      Add('unsigned int64*','pcuint64');
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

{ TH2PasTool }

procedure TH2PasTool.ConvertStruct(CNode: TCodeTreeNode; ParentNode: TH2PNode);
var
  CurName: String;
  TypeH2PNode: TH2PNode;
begin
  CurName:=CTool.ExtractStructName(CNode);
  if CurName='' then begin
    // this is an anonymous struct -> ignore
    DebugLn(['TH2PasTool.ConvertStruct SKIPPING anonymous struct at ',CTool.CleanPosToStr(CNode.StartPos)]);
  end else begin
    // this struct has a name
    // create a type
    TypeH2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnRecordType,'',
                               nil,ParentNode=nil);
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
begin
  if (CNode.FirstChild<>nil) and (CNode.FirstChild.Desc=ccnUnion)
  then begin
    CurName:=CTool.ExtractVariableName(CNode);
    if (ParentNode<>nil) and (ParentNode.PascalDesc=ctnRecordType)
    then begin
      // create a pascal 'record case'
      TypeH2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnRecordCase,'',
                                 ParentNode,false);
      DebugLn(['TH2PasTool.BuildH2PTree added record case for nested union']);
      // build recursively the record cases
      if CNode.FirstChild.FirstChild<>nil then
        BuildH2PTree(TypeH2PNode,CNode.FirstChild.FirstChild);
    end else if (CurName<>'') and (ParentNode=nil) then begin
      // this union has a name
      // create a record type
      TypeH2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnRecordCase,'',
                                 nil,true);
      DebugLn(['TH2PasTool.BuildH2PTree added record type for union: ',TypeH2PNode.DescAsString(CTool)]);
      // build recursively
      if CNode.FirstChild.FirstChild<>nil then
        BuildH2PTree(TypeH2PNode,CNode.FirstChild.FirstChild);
      // create variable
      CurName:=CTool.ExtractUnionName(CNode);
      H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnVarDefinition,
                             TypeH2PNode.PascalName,
                             nil,ParentNode=nil);
      DebugLn(['TH2PasTool.BuildH2PTree added variable for union: ',H2PNode.DescAsString(CTool)]);
    end else begin
      DebugLn(['TH2PasTool.BuildH2PTree SKIPPING union variable at ',CTool.CleanPosToStr(CNode.StartPos)]);
    end;
  end else begin
    CurName:=CTool.ExtractVariableName(CNode);
    CurType:=CTool.ExtractVariableType(CNode);
    SimpleType:=GetSimplePascalTypeOfCVar(CNode);
    if SimpleType='' then begin
      // this variable has a complex type
      TypeH2PNode:=GetH2PNodeForComplexType(CNode);
      if TypeH2PNode<>nil then
        SimpleType:=TypeH2PNode.PascalName;
    end;
    if SimpleType<>'' then begin
      H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnVarDefinition,SimpleType,
                             ParentNode,ParentNode=nil);
      DebugLn(['TH2PasTool.BuildH2PTree added: ',H2PNode.DescAsString(CTool)]);
    end else begin
      DebugLn(['TH2PasTool.BuildH2PTree SKIPPING Variable Name="',CurName,'" Type="',CurType,'"']);
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
begin
  CurName:=CTool.ExtractEnumBlockName(CNode);
  if CurName='' then begin
    // this is an anonymous enum block => auto generate a name
    CurName:=CreatePascalNameFromCCode(CTool.Src,CNode.StartPos,CNode.EndPos);
    TypeH2PNode:=CreateAutoGeneratedH2PNode(CurName,CNode,ctnEnumerationType,'',
                                            nil,ParentNode=nil);
  end else begin
    // this enum block has a name
    TypeH2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnEnumerationType,'',
                               nil,ParentNode=nil);
  end;
  DebugLn(['TH2PasTool.BuildH2PTree added: ',TypeH2PNode.DescAsString(CTool)]);

  CNode:=CNode.FirstChild;
  while CNode<>nil do begin
    if CNode.Desc=ccnEnumID then begin
      CurName:=CTool.ExtractEnumIDName(CNode);
      CurValue:=CTool.ExtractEnumIDValue(CNode);
      H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnEnumIdentifier,CurValue,
                             TypeH2PNode,ParentNode=nil);
      DebugLn(['TH2PasTool.BuildH2PTree added: ',H2PNode.DescAsString(CTool)]);
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
begin
  CurName:=CTool.ExtractFunctionName(CNode);
  CurType:=CTool.ExtractFunctionResultType(CNode);
  SimpleType:=GetSimplePascalResultTypeOfCFunction(CNode);
  IsPointerToFunction:=CTool.IsPointerToFunction(CNode);
  StatementNode:=nil;
  Ok:=true;
  if (CNode.LastChild<>nil) and (CNode.LastChild.Desc=ccnStatementBlock) then
    StatementNode:=CNode.LastChild;
  DebugLn(['TH2PasTool.BuildH2PTree Function Name="',CurName,'" ResultType="',CurType,'" SimpleType=',SimpleType,' HasStatements=',StatementNode<>nil,' IsPointer=',IsPointerToFunction]);
  if StatementNode<>nil then begin
    // this function has a body
    Ok:=false;
  end;
  if Ok and (SimpleType='') then begin
    // this function has a complex result type
    TypeH2PNode:=GetH2PNodeForComplexType(CNode);
    if TypeH2PNode<>nil then begin
      SimpleType:=TypeH2PNode.PascalName;
    end else
      Ok:=false;
  end;

  if Ok then begin
    H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnProcedure,SimpleType,
                           nil,false);
    DebugLn(['TH2PasTool.BuildH2PTree function added: ',H2PNode.DescAsString(CTool)]);
    // build recursively
    BuildH2PTree(H2PNode);
  end else begin
    DebugLn(['TH2PasTool.BuildH2PTree SKIPPING Function Name="',CurName,'" Type="',CurType,'" at ',CTool.CleanPosToStr(CNode.StartPos)]);
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
begin
  CurName:=CTool.ExtractParameterName(CNode);
  CurType:=CTool.ExtractParameterType(CNode);
  SimpleType:=GetSimplePascalTypeOfCParameter(CNode);
  DebugLn(['TH2PasTool.BuildH2PTree Parameter: Name="',CurName,'" Type="',CurType,'" SimpleType="',SimpleType,'"']);
  if SimpleType='' then begin
    // this variable has a complex type
    TypeH2PNode:=GetH2PNodeForComplexType(CNode);
    if TypeH2PNode<>nil then
      SimpleType:=TypeH2PNode.PascalName;
  end;
  if SimpleType<>'' then begin
    H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnVarDefinition,SimpleType,
                           ParentNode,false);
    DebugLn(['TH2PasTool.BuildH2PTree added: ',H2PNode.DescAsString(CTool)]);
  end else begin
    DebugLn(['TH2PasTool.BuildH2PTree SKIPPING parameter Name="',CurName,'" Type="',CurType,'" at ',CTool.CleanPosToStr(CNode.StartPos)]);
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
begin
  if CNode.FirstChild=nil then begin
    exit;
  end;
  CurName:=CTool.ExtractTypedefName(CNode);
  DebugLn(['TH2PasTool.BuildH2PTree Typedef name="',CurName,'"']);
  ChildNode:=CNode.FirstChild;
  case ChildNode.Desc of

  ccnStruct: // typedef struct
    begin
      ChildNode:=CNode.FirstChild.FirstChild;
      if (ChildNode<>nil)
      and (ChildNode.Desc=ccnStructAlias) then begin
        // this is a struct alias
        CurType:=GetIdentifier(@CTool.Src[ChildNode.StartPos]);
        TypeH2PNode:=CreateH2PNode(CurName,CurName,CNode,
                                   ctnTypeDefinition,CurType);
      end else begin
        // this is a new struct
        TypeH2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnRecordType,'');
        DebugLn(['TH2PasTool.BuildH2PTree added record: ',TypeH2PNode.DescAsString(CTool)]);
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
        TypeH2PNode:=GetH2PNodeForComplexType(ChildNode);
        if TypeH2PNode<>nil then
          SimpleType:=TypeH2PNode.PascalName;
      end;
      if IsPointerToFunction and (SimpleType<>'') then begin
        H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnProcedureType,SimpleType,
                               nil,true);
        DebugLn(['TH2PasTool.BuildH2PTree function type added: ',H2PNode.DescAsString(CTool)]);
        // build the param list
        if ChildNode.FirstChild<>nil then
          BuildH2PTree(H2PNode,ChildNode.FirstChild);
      end else begin
        DebugLn(['TH2PasTool.BuildH2PTree typdef function CurName=',CurName,' CurType=',CTool.ExtractFunctionResultType(ChildNode),' SimpleType=',SimpleType]);
        DebugLn(['TH2PasTool.BuildH2PTree SKIPPING typedef ',CCNodeDescAsString(ChildNode.Desc),' at ',CTool.CleanPosToStr(CNode.StartPos)]);
      end;
    end;

  else // typedef
    DebugLn(['TH2PasTool.BuildH2PTree SKIPPING typedef ',CCNodeDescAsString(CNode.FirstChild.Desc),' at ',CTool.CleanPosToStr(CNode.StartPos)]);
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
      H2PNode:=CreateH2PNode('$'+Directive,'#'+Directive,CNode,ctnNone,
                             MacroName,ParentNode,false);
      DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
      exit;
    end;
  end else if (Directive='undef') or (Directive='ifdef')
  or (Directive='ifndef') then begin
    // #undef NAME
    // #ifdef NAME
    // #ifndef NAME
    CurName:=CTool.ExtractDirectiveFirstAtom(CNode);
    H2PNode:=CreateH2PNode('$'+Directive,'#'+Directive,CNode,ctnNone,
                           CurName,ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
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
      if Directive='if' then
        CurName:='if'
      else
        CurName:='elseif';
      H2PNode:=CreateH2PNode(CurName,'#'+Directive,CNode,ctnNone,
                             PascalCode,ParentNode,false);
      DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
      exit;
    end;
  end else if (Directive='else') or (Directive='endif') then begin
    // #else
    // #endif
    H2PNode:=CreateH2PNode('$'+Directive,'#'+Directive,CNode,ctnNone,
                           '',ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added: ',H2PNode.DescAsString(CTool)]);
    exit;
  end else if Directive='line' then begin
    // #line: set the current line number -> ignore
    exit;
  end else if Directive='error' then begin
    // #error
    PascalCode:=CTool.ExtractCode(CNode.StartPos+length('#error'),
                                  CNode.EndPos);
    H2PNode:=CreateH2PNode('$'+Directive,'#'+Directive,CNode,ctnNone,
                           PascalCode,ParentNode,false);
    DebugLn(['TH2PasTool.ConvertDirective added $error: ',H2PNode.DescAsString(CTool)]);
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

procedure TH2PasTool.SetIgnoreCParts(const AValue: TIgnoreCSourceParts);
begin
  if FIgnoreCParts=AValue then exit;
  FIgnoreCParts:=AValue;
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
      Add(ttValue);
      if AtomIs('defined') then begin
        // read   defined(name)
        ReadRawNextCAtom(CCode,p,AtomStart);
        if not AtomIs('(') then begin
          ErrorExpectedButFound('(');
          exit;
        end;
        Add(ttBracketOpen);
        ReadRawNextCAtom(CCode,p,AtomStart);
        if (AtomStart>=EndPos) or (not IsIdentStartChar[CCode[AtomStart]])
        then begin
          ErrorExpectedButFound('identifier');
          exit;
        end;
        Add(ttValue);
        ReadRawNextCAtom(CCode,p,AtomStart);
        if not AtomIs(')') then begin
          ErrorExpectedButFound(')');
          exit;
        end;
        Add(ttBracketClose);
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
end;

procedure TH2PasTool.WriteStr(const Line: string; s: TStream);
begin
  if Line='' then exit;
  s.Write(Line[1],length(Line));
end;

procedure TH2PasTool.WriteLnStr(const Line: string; s: TStream);
begin
  WriteStr(Line+LineEnding,s);
end;

function TH2PasTool.Convert(CCode, PascalCode: TCodeBuffer): boolean;
begin
  Result:=false;

  if CTool=nil then
    CTool:=TCCodeParserTool.Create;
  // pare C header file
  CTool.Parse(CCode);
  //CTool.WriteDebugReport;

  BuildH2PTree;
  
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
  IndentStr: string;
  CurSection: TCodeTreeNodeDesc;

  procedure W(const aStr: string);
  begin
    WriteLnStr(IndentStr+aStr,s);
  end;
  
  procedure IncIndent;
  begin
    IndentStr:=IndentStr+'  ';
  end;

  procedure DecIndent;
  begin
    IndentStr:=copy(IndentStr,1,length(IndentStr)-2);
  end;
  
  procedure SetSection(NewSection: TCodeTreeNodeDesc);
  begin
    if NewSection=CurSection then exit;
    // close old section
    case CurSection of
    ctnVarSection,ctnTypeSection,ctnConstSection:
      begin
        DecIndent;
      end;
    end;
    CurSection:=NewSection;
    // start new section
    W('');
    case CurSection of
    ctnVarSection,ctnTypeSection,ctnConstSection:
      begin
        case CurSection of
        ctnVarSection: W('var');
        ctnTypeSection: W('type');
        ctnConstSection: W('const');
        end;
        IncIndent;
      end;
    end;
  end;

var
  H2PNode: TH2PNode;
  UsesClause: String;
  PascalCode: String;
  ChildNode: TH2PNode;
  CurName: String;
  NoNameCount: Integer;
begin
  IndentStr:='';
  
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
  CurSection:=ctnNone;
  H2PNode:=Tree.Root;
  while H2PNode<>nil do begin
    case H2PNode.PascalDesc of
    
    ctnVarDefinition:
      begin
        // global variable
        SetSection(ctnVarSection);
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
      
    ctnTypeDefinition:
      begin
        // global variable
        SetSection(ctnTypeSection);
        if H2PNode.FirstChild=nil then begin
          PascalCode:=H2PNode.PascalCode+';';
          W(H2PNode.PascalName+' = '+PascalCode);
        end else
          DebugLn(['TH2PasTool.WritePascalToStream SKIPPING ',H2PNode.DescAsString(CTool)]);
      end;
      
    ctnProcedure, ctnProcedureType:
      begin
        // global procedure or procedure type
        if H2PNode.PascalDesc=ctnProcedure then
          SetSection(ctnNone)
        else
          SetSection(ctnTypeSection);
        // create param list
        PascalCode:='';
        ChildNode:=H2PNode.FirstChild;
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
            DebugLn(['TH2PasTool.WritePascalToStream SKIPPING ',ChildNode.DescAsString(CTool)]);
          end;
          ChildNode:=ChildNode.NextBrother;
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
      
    ctnEnumerationType:
      begin
        { for example:
            e2 = (
              a = 3,
              b = 9
            );
        }
        SetSection(ctnTypeSection);
        // write start
        PascalCode:=H2PNode.PascalName+' = (';
        W(PascalCode);
        // write enums
        IncIndent;
        ChildNode:=H2PNode.FirstChild;
        while ChildNode<>nil do begin
          PascalCode:=ChildNode.PascalName;
          if ChildNode.PascalCode<>'' then
            PascalCode:=PascalCode+' = '+ChildNode.PascalCode;
          if ChildNode.NextBrother<>nil then
            PascalCode:=PascalCode+',';
          W(PascalCode);
          ChildNode:=ChildNode.NextBrother;
        end;
        DecIndent;
        // write end
        W(');');
      end;
      
    ctnRecordType:
      begin
        { examples:
           TRecord = record
           end;
        }
        SetSection(ctnTypeSection);
        // write header
        PascalCode:=H2PNode.PascalName+' = record';
        W(PascalCode);
        // write sub variables
        IncIndent;
        ChildNode:=H2PNode.FirstChild;
        while ChildNode<>nil do begin
          if ChildNode.PascalDesc=ctnVarDefinition then
            PascalCode:=ChildNode.PascalName+': '+ChildNode.PascalCode+';'
          else
            DebugLn(['TH2PasTool.WritePascalToStream SKIPPING ',ChildNode.DescAsString(CTool)]);
          W(PascalCode);
          ChildNode:=ChildNode.NextBrother;
        end;
        DecIndent;
        // write end
        W('end;');
      end;
      
    else
      DebugLn(['TH2PasTool.WritePascalToStream SKIPPING ',H2PNode.DescAsString(CTool)]);
    end;
    H2PNode:=H2PNode.NextBrother;
  end;
  
  // write implementation
  SetSection(ctnNone);
  W('implementation');
  W('');

  // write end.
  W('end.');
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

function TH2PasTool.CreateH2PNode(const PascalName, CName: string;
  CNode: TCodeTreeNode; PascalDesc: TCodeTreeNodeDesc;
  const PascalCode: string;
  ParentNode: TH2PNode; IsGlobal: boolean): TH2PNode;
begin
  Result:=TH2PNode.Create;
  Result.PascalName:=PascalName;
  Result.CName:=CName;
  Result.CNode:=CNode;
  Result.PascalDesc:=PascalDesc;
  Result.PascalCode:=PascalCode;
  Tree.AddNodeAsLastChild(ParentNode,Result);
  if IsGlobal then begin
    FPascalNames.Add(Result);
    FCNames.Add(Result);
  end;
end;

function TH2PasTool.CreateAutoGeneratedH2PNode(var PascalName: string;
  CNode: TCodeTreeNode; PascalDesc: TCodeTreeNodeDesc;
  const PascalCode: string;
  ParentNode: TH2PNode;
  IsGlobal: boolean): TH2PNode;
  
  function Check(const TestName: string; out Node: TH2PNode): boolean;
  begin
    Node:=FindH2PNodeWithPascalName(TestName);
    if (Node=nil) then begin
      Node:=CreateH2PNode(TestName,'',CNode,PascalDesc,PascalCode);
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
  CreateIfNotExists: boolean): TH2PNode;
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
                                    ctnTypeDefinition,'^'+BasePascalType);
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
      SubH2PNode:=CreateAutoGeneratedH2PNode(PascalName,nil,ctnTypeDefinition,PascalCode);
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

procedure TH2PasTool.WriteDebugReport;
begin
  DebugLn(['TH2PasTool.WriteDebugReport ']);
  if CTool<>nil then
    CTool.WriteDebugReport;
  WriteH2PNodeReport;
end;

procedure TH2PasTool.WriteH2PNodeReport;
var
  Node: TH2PNode;
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

constructor TH2PasTool.Create;
begin
  FPredefinedCTypes:=DefaultPredefinedCTypes;
  Tree:=TH2PTree.Create;
  FPascalNames:=TAVLTree.Create(@CompareH2PNodePascalNames);
  FCNames:=TAVLTree.Create(@CompareH2PNodeCNames);
  FIgnoreCParts:=[icspInclude];
end;

destructor TH2PasTool.Destroy;
begin
  FPredefinedCTypes:=nil;
  Clear;
  FreeAndNil(Tree);
  FreeAndNil(FPascalNames);
  FreeAndNil(FCNames);
  FreeAndNil(CTool);
  inherited Destroy;
end;

procedure TH2PasTool.Clear;
begin
  FPascalNames.Clear;
  FCNames.Clear;
  Tree.Clear;
end;

{ TH2PNode }

function TH2PNode.Next: TH2PNode;
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

function TH2PNode.NextSkipChilds: TH2PNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.NextBrother=nil) do
    Result:=Result.Parent;
  if Result<>nil then Result:=Result.NextBrother;
end;

function TH2PNode.Prior: TH2PNode;
begin
  if PriorBrother<>nil then begin
    Result:=PriorBrother;
    while Result.LastChild<>nil do
      Result:=Result.LastChild;
  end else
    Result:=Parent;
end;

function TH2PNode.HasAsParent(Node: TH2PNode): boolean;
var CurNode: TH2PNode;
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

function TH2PNode.HasAsChild(Node: TH2PNode): boolean;
begin
  Result:=false;
  if Node=nil then exit;
  Result:=Node.HasAsParent(Self);
end;

function TH2PNode.GetLevel: integer;
var ANode: TH2PNode;
begin
  Result:=0;
  ANode:=Parent;
  while ANode<>nil do begin
    inc(Result);
    ANode:=ANode.Parent;
  end;
end;

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
  Result:=Result+',PascalCode="'+dbgstr(PascalCode)+'"';
  Result:=Result+'}';
end;

procedure TH2PNode.ConsistencyCheck;
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

procedure TH2PNode.WriteDebugReport(const Prefix: string; WithChilds: boolean;
  CTool: TCCodeParserTool);
var
  Node: TH2PNode;
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

{ TH2PTree }

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
var ANode: TH2PNode;
begin
  while Root<>nil do begin
    ANode:=Root;
    Root:=ANode.NextBrother;
    DeleteNode(ANode);
  end;
end;

procedure TH2PTree.DeleteNode(ANode: TH2PNode);
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
  ANode.Free;
end;

procedure TH2PTree.AddNodeAsLastChild(ParentNode, ANode: TH2PNode);
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

procedure TH2PTree.AddNodeInFrontOf(NextBrotherNode, ANode: TH2PNode);
begin
  ANode.Parent:=NextBrotherNode.Parent;
  ANode.NextBrother:=NextBrotherNode;
  ANode.PriorBrother:=NextBrotherNode.PriorBrother;
  NextBrotherNode.PriorBrother:=ANode;
  if ANode.PriorBrother<>nil then
    ANode.PriorBrother.NextBrother:=ANode;
end;

function TH2PTree.ContainsNode(ANode: TH2PNode): boolean;
begin
  if ANode=nil then exit(false);
  while ANode.Parent<>nil do ANode:=ANode.Parent;
  while ANode.PriorBrother<>nil do ANode:=ANode.PriorBrother;
  Result:=ANode=Root;
end;

procedure TH2PTree.ConsistencyCheck;
// 0 = ok
var RealNodeCount: integer;

  procedure CountNodes(ANode: TH2PNode);
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

finalization
  FreeAndNil(InternalPredefinedCTypes);

end.

