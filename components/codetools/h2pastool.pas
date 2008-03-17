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
    function DescAsString: string;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(const Prefix: string; WithChilds: boolean);
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
  
  { TH2PasTool }

  TH2PasTool = class
  private
    FPredefinedCTypes: TFPStringHashTable;
    FPascalNames: TAVLTree;// tree of TH2PNode sorted for PascalName
    FCNames: TAVLTree;// tree of TH2PNode sorted for CName
  public
    Tree: TH2PTree;
    CTool: TCCodeParserTool;
    function Convert(CCode, PascalCode: TCodeBuffer): boolean;
    procedure BuildH2PTree;
    
    function GetSimplePascalTypeOfCVar(CVarNode: TCodeTreeNode): string;
    function GetSimplePascalResultTypeOfCFunction(CFuncNode: TCodeTreeNode): string;
    function ConvertSimpleCTypeToPascalType(CType: string;
                  UseSingleIdentifierAsDefault: boolean): string;
    
    function CreateH2PNode(const PascalName, CName: string; CNode: TCodeTreeNode;
       PascalDesc: TCodeTreeNodeDesc; ParentNode: TH2PNode = nil;
       IsGlobal: boolean = true): TH2PNode;
    function GetTypeForVarType(CVarNode: TCodeTreeNode;
                               CreateIfNotExists: boolean = true): TH2PNode;
    function CreatePascalNameFromCCode(const CCode: string;
                                       StartPos: integer = 1;
                                       EndPos: integer = -1): string;

    procedure WriteDebugReport;
    procedure WriteH2PNodeReport;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property PredefinedCTypes: TFPStringHashTable read FPredefinedCTypes;
  end;
  
  
function DefaultPredefinedCTypes: TFPStringHashTable;// types in unit ctypes

function CompareH2PNodePascalNames(Data1, Data2: Pointer): integer;
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

function CompareH2PNodeCNames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifiersCaseSensitive(PChar(Pointer(TH2PNode(Data1).CName)),
                                          PChar(Pointer(TH2PNode(Data2).CName)));
end;

{ TH2PasTool }

function TH2PasTool.Convert(CCode, PascalCode: TCodeBuffer): boolean;
begin
  Result:=false;

  if CTool=nil then
    CTool:=TCCodeParserTool.Create;
  // pare C header file
  CTool.Parse(CCode);
  //CTool.WriteDebugReport;

  BuildH2PTree;

  Result:=true;
end;

procedure TH2PasTool.BuildH2PTree;
var
  CNode: TCodeTreeNode;
  CurName: String;
  CurType: String;
  SimpleType: String;
  H2PNode: TH2PNode;
  NextCNode: TCodeTreeNode;
  TypeH2PNode: TH2PNode;
begin
  Tree.Clear;
  CNode:=CTool.Tree.Root;
  while CNode<>nil do begin
    NextCNode:=CNode.Next;
    case CNode.Desc of
    ccnVariable:
      begin
        CurName:=CTool.ExtractVariableName(CNode);
        CurType:=CTool.ExtractVariableType(CNode);
        SimpleType:=GetSimplePascalTypeOfCVar(CNode);
        DebugLn(['TH2PasTool.BuildH2PTree Variable Name="',CurName,'" Type="',CurType,'" SimpleType=',SimpleType]);
        if SimpleType='' then begin
          // this variable has a complex type
          TypeH2PNode:=GetTypeForVarType(CNode);
          if TypeH2PNode<>nil then
            SimpleType:=TypeH2PNode.PascalName;
        end;
        if SimpleType<>'' then begin
          H2PNode:=CreateH2PNode(CurName,CurName,CNode,ctnVarDefinition);
          H2PNode.PascalCode:=SimpleType;
          //DebugLn(['TH2PasTool.BuildH2PTree CNode.Desc=',CCNodeDescAsString(CNode.Desc),' ',H2PNode.DescAsString]);
        end;
        NextCNode:=CNode.NextSkipChilds;
      end;
    ccnEnumBlock:
      begin
        CurName:=CTool.ExtractEnumBlockName(CNode);
        DebugLn(['TH2PasTool.BuildH2PTree EnumBlock name="',CurName,'"']);
      end;
    ccnEnumID:
      begin
        CurName:=CTool.ExtractEnumIDName(CNode);
        DebugLn(['TH2PasTool.BuildH2PTree EnumID name="',CurName,'"']);
      end;
    ccnFunction:
      begin
        CurName:=CTool.ExtractFunctionName(CNode);
        CurType:=CTool.ExtractFunctionResultType(CNode);
        SimpleType:=GetSimplePascalResultTypeOfCFunction(CNode);
        DebugLn(['TH2PasTool.BuildH2PTree Function Name="',CurName,'" ResultType="',CurType,'" SimpleType=',SimpleType]);
        if SimpleType='' then begin
          // this variable has a complex type

        end;
      end;
    end;
    CNode:=NextCNode;
  end;
end;

function TH2PasTool.GetSimplePascalTypeOfCVar(CVarNode: TCodeTreeNode): string;
begin
  Result:=CTool.ExtractVariableType(CVarNode);
  if Result='' then exit;
  Result:=ConvertSimpleCTypeToPascalType(Result,true);
end;

function TH2PasTool.GetSimplePascalResultTypeOfCFunction(
  CFuncNode: TCodeTreeNode): string;
begin
  Result:=CTool.ExtractFunctionResultType(CFuncNode);
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
  CNode: TCodeTreeNode; PascalDesc: TCodeTreeNodeDesc; ParentNode: TH2PNode;
  IsGlobal: boolean): TH2PNode;
begin
  Result:=TH2PNode.Create;
  Result.PascalName:=PascalName;
  Result.CName:=CName;
  Result.CNode:=CNode;
  Result.PascalDesc:=PascalDesc;
  Tree.AddNodeAsLastChild(ParentNode,Result);
  if IsGlobal then begin
    FPascalNames.Add(Result);
    FCNames.Add(Result);
  end;
end;

function TH2PasTool.GetTypeForVarType(CVarNode: TCodeTreeNode;
  CreateIfNotExists: boolean): TH2PNode;
var
  CCode: String;
  PascalName: String;
  AtomStart: integer;
  p: Integer;
  CurAtom: String;
  BaseCType: String;
  BasePascaltype: String;
begin
  Result:=nil;
  if (CVarNode.FirstChild<>nil)
  and (CVarNode.FirstChild.Desc=ccnUnion) then begin
    // ToDo: union
  end else begin
    CCode:=CTool.ExtractVariableType(CVarNode);
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
      end;
    until false;
    if BaseCType='' then begin
      DebugLn(['TH2PasTool.GetTypeForVarType no base type in c declaration: CCode="',dbgstr(CCode),'"']);
      exit;
    end;
    BasePascaltype:=ConvertSimpleCTypeToPascalType(BaseCType,true);
    if (BasePascaltype='') then begin
      DebugLn(['TH2PasTool.GetTypeForVarType unknown c type: "',BaseCType,'"']);
      exit;
    end;
    
    // read pointer
    {while (AtomStart<=length(CCode)) do begin
      CurAtom:=copy(CCode,AtomStart,p-AtomStart);
      if (CurAtom='*') then begin
        BaseCType:=BaseCType+'*';
        NewBasePascaltype:=ConvertSimpleCTypeToPascalType(BaseCType,true);

      end else if (CurAtom='const') then begin
        // skip 'const'
      end else begin
        break;
      end;
      ReadRawNextCAtom(CCode,p,AtomStart);
    end;}
    
    PascalName:=CreatePascalNameFromCCode(CCode);
    DebugLn(['TH2PasTool.GetTypeForVarType CCode="',dbgstr(CCode),'" PascalName="',PascalName,'"']);
  end;
end;

function TH2PasTool.CreatePascalNameFromCCode(const CCode: string;
  StartPos: integer; EndPos: integer): string;
const
  MaxIdentLen = 70;

  function Add(var PascalName: string; const Addition: string): boolean;
  begin
    if Addition='' then exit(true);
    if length(PascalName)+length(Addition)>MaxIdentLen then
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
      DebugLn([GetIndentStr(Node.GetLevel*2),Node.DescAsString]);
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

function TH2PNode.DescAsString: string;
begin
  Result:='PascalName="'+PascalName+'"';
  if PascalName<>CName then
    Result:=Result+' CName="'+CName+'"';
  Result:=Result+' PascalDesc='+NodeDescriptionAsString(PascalDesc);
  if CNode<>nil then begin
    Result:=Result+' CNode='+CCNodeDescAsString(CNode.Desc);
  end else begin
    Result:=Result+' CNode=nil';
  end;
  Result:=Result+' PascalCode="'+dbgstr(PascalCode)+'"';
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

procedure TH2PNode.WriteDebugReport(const Prefix: string; WithChilds: boolean);
var
  Node: TH2PNode;
begin
  DebugLn([Prefix,DescAsString]);
  if WithChilds then begin
    Node:=FirstChild;
    while Node<>nil do begin
      Node.WriteDebugReport(Prefix+'  ',true);
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

