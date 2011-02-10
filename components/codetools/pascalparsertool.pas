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
    TPascalParserTool enhances TMultiKeyWordListCodeTool.
    This tool parses the pascal code, makes simple syntax checks and provides
    a lot of useful parsing functions. It can either parse complete sources
    or parts of it.
    
}
unit PascalParserTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE ShowIgnoreErrorAfter}
{ $DEFINE VerboseUpdateNeeded}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStrConsts, CodeTree, CodeAtom,
  CustomCodeTool, MultiKeyWordListTool, KeywordFuncLists, BasicCodeTools,
  CodeToolsStructs, LinkScanner, CodeCache, AVL_Tree;

type
  TProcHeadAttribute = (
    // extract attributes:
    phpWithStart,          // proc keyword e.g. 'function', 'class procedure'
    phpWithoutClassKeyword,// without 'class' proc keyword
    phpAddClassName,       // extract/add 'ClassName.'
    phpWithoutClassName,   // skip classname
    phpWithoutName,        // skip function name
    phpWithoutParamList,   // skip param list
    phpWithVarModifiers,   // extract 'var', 'out', 'const'
    phpWithParameterNames, // extract parameter names
    phpWithoutParamTypes,  // skip colon, param types and default values
    phpWithHasDefaultValues,// extract the equal sign of default values
    phpWithDefaultValues,  // extract default values
    phpWithResultType,     // extract colon + result type
    phpWithOfObject,       // extract 'of object'
    phpWithCallingSpecs,   // extract cdecl; extdecl; popstack;
    phpWithProcModifiers,  // extract forward; alias; external; ...
    phpWithComments,       // extract comments and spaces
    phpInUpperCase,        // turn to uppercase
    phpCommentsToSpace,    // replace comments with a single space
                           //  (default is to skip unnecessary space,
                           //    e.g 'Do   ;' normally becomes 'Do;'
                           //    with this option you get 'Do ;')
    phpWithoutBrackets,    // skip start- and end-bracket of parameter list
    phpWithoutSemicolon,   // skip semicolon at end
    phpDoNotAddSemicolon,  // do not add missing semicolon at end
    // search attributes:
    phpIgnoreForwards,     // skip forward procs
    phpIgnoreProcsWithBody,// skip procs with begin..end
    phpIgnoreMethods,      // skip method bodies and definitions
    phpOnlyWithClassname,  // skip procs without the right classname
    phpFindCleanPosition,  // read til ExtractSearchPos
    // parse attributes:
    phpCreateNodes         // create nodes during reading
    );
  TProcHeadAttributes = set of TProcHeadAttribute;
  
  TParseProcHeadAttribute = (pphIsMethod, pphIsFunction, pphIsType,
     pphIsOperator, pphCreateNodes);
  TParseProcHeadAttributes =  set of TParseProcHeadAttribute;
  
  TProcHeadExtractPos = (phepNone, phepStart, phepName, phepParamList,
    phepResultType, phepSpecifiers);

  TSkipBracketCheck = (
    sbcStopOnRecord,
    sbcStopOnSemicolon
    );
  TSkipBracketChecks = set of TSkipBracketCheck;
    
  TTreeRange = (trInterface, trAll, trTillCursor, trTillCursorSection);
  
  TBuildTreeFlag = (
    btSetIgnoreErrorPos,
    btKeepIgnoreErrorPos,
    btLoadDirtySource,
    btCursorPosOutAllowed
    );
  TBuildTreeFlags = set of TBuildTreeFlag;

  { TPascalParserTool }
  
  TPascalParserTool = class(TMultiKeyWordListCodeTool)
  private
  protected
    ExtractMemStream: TMemoryStream;
    ExtractSearchPos: integer;
    ExtractFoundPos: integer;
    ExtractProcHeadPos: TProcHeadExtractPos;
    procedure RaiseCharExpectedButAtomFound(c: char);
    procedure RaiseStringExpectedButAtomFound(const s: string);
    procedure RaiseUnexpectedKeyWord;
    procedure RaiseIllegalQualifier;
    procedure RaiseEndOfSourceExpected;
  protected
    // code extraction
    procedure InitExtraction;
    function GetExtraction(InUpperCase: boolean): string;
    function ExtractStreamEndIsIdentChar: boolean;
    procedure ExtractNextAtom(AddAtom: boolean; Attr: TProcHeadAttributes);
    // sections
    function KeyWordFuncSection: boolean;
    function KeyWordFuncEndPoint: boolean;
    // type/var/const/resourcestring
    function KeyWordFuncType: boolean;
    function KeyWordFuncVar: boolean;
    function KeyWordFuncConst: boolean;
    function KeyWordFuncResourceString: boolean;
    function KeyWordFuncExports: boolean;
    function KeyWordFuncLabel: boolean;
    function KeyWordFuncProperty: boolean;
    procedure ReadConst;
    // types
    procedure ReadEqualsType;
    function KeyWordFuncClass: boolean;
    function KeyWordFuncClassInterface: boolean;
    function KeyWordFuncTypePacked: boolean;
    function KeyWordFuncTypeBitPacked: boolean;
    function KeyWordFuncSpecialize: boolean;
    function KeyWordFuncTypeArray: boolean;
    function KeyWordFuncTypeProc: boolean;
    function KeyWordFuncTypeSet: boolean;
    function KeyWordFuncTypeLabel: boolean;
    function KeyWordFuncTypeType: boolean;
    function KeyWordFuncTypeFile: boolean;
    function KeyWordFuncTypePointer: boolean;
    function KeyWordFuncTypeRecord: boolean;
    function KeyWordFuncTypeRecordCase: boolean;
    function KeyWordFuncTypeDefault: boolean;
    // procedures/functions/methods
    function KeyWordFuncProc: boolean;
    function KeyWordFuncBeginEnd: boolean;
    // class/object elements
    function KeyWordFuncClassSection: boolean;
    function KeyWordFuncClassConstSection: boolean;
    function KeyWordFuncClassTypeSection: boolean;
    function KeyWordFuncClassVarSection: boolean;
    function KeyWordFuncClassClass: boolean;
    function KeyWordFuncClassMethod: boolean;
    function KeyWordFuncClassProperty: boolean;
    function KeyWordFuncClassIdentifier: boolean;
    function KeyWordFuncClassVarTypeClass: boolean;
    function KeyWordFuncClassVarTypePacked: boolean;
    function KeyWordFuncClassVarTypeBitPacked: boolean;
    function KeyWordFuncClassVarTypeRecord: boolean;
    function KeyWordFuncClassVarTypeArray: boolean;
    function KeyWordFuncClassVarTypeSet: boolean;
    function KeyWordFuncClassVarTypeProc: boolean;
    function KeyWordFuncClassVarTypeIdent: boolean;
    // keyword lists
    procedure BuildDefaultKeyWordFunctions; override;
    function ParseType(StartPos, WordLen: integer): boolean;
    function ParseInnerClass(StartPos, WordLen: integer): boolean;
    function ParseClassVarType(StartPos, WordLen: integer): boolean;
    function SkipInnerClassInterface(StartPos, WordLen: integer): boolean;
    function UnexpectedKeyWord: boolean;
    function EndOfSourceExpected: boolean;
    // read functions
    function ReadTilProcedureHeadEnd(ParseAttr: TParseProcHeadAttributes;
        var HasForwardModifier: boolean): boolean;
    function ReadConstant(ExceptionOnError, Extract: boolean;
        const Attr: TProcHeadAttributes): boolean;
    function ReadParamType(ExceptionOnError, Extract: boolean;
        const Attr: TProcHeadAttributes): boolean;
    function ReadParamList(ExceptionOnError, Extract: boolean;
        const Attr: TProcHeadAttributes): boolean;
    function ReadUsesSection(ExceptionOnError: boolean): boolean;
    function ReadRequiresSection(ExceptionOnError: boolean): boolean;
    function ReadContainsSection(ExceptionOnError: boolean): boolean;
    function ReadSubRange(ExceptionOnError: boolean): boolean;
    function ReadTilBracketCloseOrUnexpected(ExceptionOnNotFound: boolean;
      Flags: TSkipBracketChecks): boolean;
    function ReadTilBlockEnd(StopOnBlockMiddlePart,
        CreateNodes: boolean): boolean;
    function ReadTilBlockStatementEnd(ExceptionOnNotFound: boolean): boolean;
    function ReadBackTilBlockEnd(StopOnBlockMiddlePart: boolean): boolean;
    function ReadTilVariableEnd(ExceptionOnError, WithAsOperator: boolean): boolean;
    function ReadTilStatementEnd(ExceptionOnError,
        CreateNodes: boolean): boolean;
    function ReadWithStatement(ExceptionOnError, CreateNodes: boolean): boolean;
    function ReadOnStatement(ExceptionOnError, CreateNodes: boolean): boolean;
    procedure ReadVariableType;
    function ReadTilTypeOfProperty(PropertyNode: TCodeTreeNode): boolean;
    procedure ReadGUID;
    procedure ReadClassInheritance(CreateChildNodes: boolean);
    procedure ReadSpecialize(CreateChildNodes: boolean);
    function WordIsPropertyEnd: boolean;
  public
    CurSection: TCodeTreeNodeDesc;

    InterfaceSectionFound: boolean;
    ImplementationSectionFound: boolean;
    EndOfSourceFound: boolean;

    procedure ValidateToolDependencies; virtual;
    procedure BuildTree(OnlyInterfaceNeeded: boolean);
    procedure BuildTreeAndGetCleanPos(TreeRange: TTreeRange;
        const CursorPos: TCodeXYPosition; out CleanCursorPos: integer;
        BuildTreeFlags: TBuildTreeFlags);
    procedure BuildSubTreeForClass(ClassNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForBeginBlock(BeginNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForProcHead(ProcNode: TCodeTreeNode); virtual;
    procedure BuildSubTreeForProcHead(ProcNode: TCodeTreeNode;
        out FunctionResult: TCodeTreeNode);
    procedure BuildSubTree(CleanCursorPos: integer); virtual;
    procedure BuildSubTree(ANode: TCodeTreeNode); virtual;
    function NodeNeedsBuildSubTree(ANode: TCodeTreeNode): boolean; virtual;
    function BuildSubTreeAndFindDeepestNodeAtPos(
      P: integer; ExceptionOnNotFound: boolean): TCodeTreeNode;
    function BuildSubTreeAndFindDeepestNodeAtPos(StartNode: TCodeTreeNode;
      P: integer; ExceptionOnNotFound: boolean): TCodeTreeNode;

    function DoAtom: boolean; override;

    function FindFirstNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindNextNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;
    function FindPrevNodeOnSameLvl(StartNode: TCodeTreeNode): TCodeTreeNode;

    // sections
    function FindRootNode(Desc: TCodeTreeNodeDesc): TCodeTreeNode;
    function FindInterfaceNode: TCodeTreeNode;
    function FindImplementationNode: TCodeTreeNode;
    function FindInitializationNode: TCodeTreeNode;
    function FindFinalizationNode: TCodeTreeNode;
    function FindMainBeginEndNode: TCodeTreeNode;
    function FindFirstSectionChild: TCodeTreeNode;

    function NodeHasParentOfType(ANode: TCodeTreeNode;
        NodeDesc: TCodeTreeNodeDesc): boolean;

    constructor Create;
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;
  
const
  ProcHeadAttributeNames: array[TProcHeadAttribute] of string = (
      // extract attributes:
      'phpWithStart',
      'phpWithoutClassKeyword',
      'phpAddClassName',
      'phpWithoutClassName',
      'phpWithoutName',
      'phpWithoutParamList',
      'phpWithVarModifiers',
      'phpWithParameterNames',
      'phpWithoutParamTypes',
      'phpWithHasDefaultValues',
      'phpWithDefaultValues',
      'phpWithResultType',
      'phpWithOfObject',
      'phpWithCallingSpecs',
      'phpWithProcModifiers',
      'phpWithComments',
      'phpInUpperCase',
      'phpCommentsToSpace',
      'phpWithoutBrackets',
      'phpWithoutSemicolon',
      'phpDoNotAddSemicolon',
      // search attributes:
      'phpIgnoreForwards',
      'phpIgnoreProcsWithBody',
      'phpIgnoreMethods',
      'phpOnlyWithClassname',
      'phpFindCleanPosition',
      // parse attributes:
      'phpCreateNodes'
    );

function ProcHeadAttributesToStr(Attr: TProcHeadAttributes): string;


implementation


type
  TEndBlockType = (ebtBegin, ebtAsm, ebtTry, ebtCase, ebtRepeat, ebtRecord,
                   ebtClass, ebtObject);
  TTryType = (ttNone, ttFinally, ttExcept);

function ProcHeadAttributesToStr(Attr: TProcHeadAttributes): string;
var a: TProcHeadAttribute;
begin
  Result:='';
  for a:=Low(TProcHeadAttribute) to High(TProcHeadAttribute) do begin
    if a in Attr then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+ProcHeadAttributeNames[a];
    end;
  end;
end;

{ TPascalParserTool }

constructor TPascalParserTool.Create;
begin
  inherited Create;
end;

destructor TPascalParserTool.Destroy;
begin
  if ExtractMemStream<>nil then
    ExtractMemStream.Free;
  inherited Destroy;
end;

procedure TPascalParserTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
  if ExtractMemStream<>nil then
    Stats.Add('TPascalParserTool.ExtractMemStream',
      ExtractMemStream.InstanceSize+ExtractMemStream.Size);
end;

procedure TPascalParserTool.BuildDefaultKeyWordFunctions;
begin
  inherited BuildDefaultKeyWordFunctions;
  with KeyWordFuncList do begin
    Add('PROGRAM',@KeyWordFuncSection);
    Add('LIBRARY',@KeyWordFuncSection);
    Add('PACKAGE',@KeyWordFuncSection);
    Add('UNIT',@KeyWordFuncSection);
    Add('INTERFACE',@KeyWordFuncSection);
    Add('IMPLEMENTATION',@KeyWordFuncSection);
    Add('INITIALIZATION',@KeyWordFuncSection);
    Add('FINALIZATION',@KeyWordFuncSection);

    Add('END',@KeyWordFuncEndPoint);
    Add('.',@KeyWordFuncEndPoint);

    Add('TYPE',@KeyWordFuncType);
    Add('VAR',@KeyWordFuncVar);
    Add('THREADVAR',@KeyWordFuncVar);
    Add('CONST',@KeyWordFuncConst);
    Add('RESOURCESTRING',@KeyWordFuncResourceString);
    Add('EXPORTS',@KeyWordFuncExports);
    Add('LABEL',@KeyWordFuncLabel);
    Add('PROPERTY',@KeyWordFuncProperty);

    Add('PROCEDURE',@KeyWordFuncProc);
    Add('FUNCTION',@KeyWordFuncProc);
    Add('CONSTRUCTOR',@KeyWordFuncProc);
    Add('DESTRUCTOR',@KeyWordFuncProc);
    Add('OPERATOR',@KeyWordFuncProc);
    Add('CLASS',@KeyWordFuncProc);

    Add('BEGIN',@KeyWordFuncBeginEnd);
    Add('ASM',@KeyWordFuncBeginEnd);
    
    DefaultKeyWordFunction:=@EndOfSourceExpected;
  end;
end;

function TPascalParserTool.ParseType(StartPos, WordLen: integer): boolean;
// KeyWordFunctions for parsing types
var
  p: PChar;
begin
  if StartPos>SrcLen then exit(false);
  p:=@Src[StartPos];
  case UpChars[p^] of
  'A':
    if CompareSrcIdentifiers('ARRAY',p) then exit(KeyWordFuncTypeArray);
  'B':
    if CompareSrcIdentifiers('BITPACKED',p) then exit(KeyWordFuncTypeBitPacked);
  'C':
    case UpChars[p[1]] of
    'L': if CompareSrcIdentifiers('CLASS',p) then exit(KeyWordFuncClass);
    'P': if CompareSrcIdentifiers('CPPCLASS',p) then exit(KeyWordFuncClass);
    end;
  'D':
    if CompareSrcIdentifiers('DISPINTERFACE',p) then exit(KeyWordFuncClassInterface);
  'F':
    case UpChars[p[1]] of
    'I': if CompareSrcIdentifiers('FILE',p) then exit(KeyWordFuncTypeFile);
    'U': if CompareSrcIdentifiers('FUNCTION',p) then exit(KeyWordFuncTypeProc);
    end;
  'I':
    if CompareSrcIdentifiers('INTERFACE',p) then exit(KeyWordFuncClassInterface);
  'L':
    if CompareSrcIdentifiers('LABEL',p) then exit(KeyWordFuncTypeLabel);
  'O':
    if CompareSrcIdentifiers('OBJECT',p)
    or CompareSrcIdentifiers('OBJCCLASS',p)
    or CompareSrcIdentifiers('OBJCCATEGORY',p) then
      exit(KeyWordFuncClass)
    else if CompareSrcIdentifiers('OBJCPROTOCOL',p) then
      exit(KeyWordFuncClassInterface);
  'P':
    case UpChars[p[1]] of
    'A': if CompareSrcIdentifiers('PACKED',p) then exit(KeyWordFuncTypePacked);
    'R': if CompareSrcIdentifiers('PROCEDURE',p) then exit(KeyWordFuncTypeProc);
    end;
  'R':
    if CompareSrcIdentifiers('RECORD',p) then exit(KeyWordFuncTypeRecord);
  'S':
    case UpChars[p[1]] of
    'E': if CompareSrcIdentifiers('SET',p) then exit(KeyWordFuncTypeSet);
    'P': if CompareSrcIdentifiers('SPECIALIZE',p) then exit(KeyWordFuncSpecialize);
    end;
  'T':
    if CompareSrcIdentifiers('TYPE',p) then exit(KeyWordFuncTypeType);
  '^': if WordLen=1 then exit(KeyWordFuncTypePointer);
  end;
  Result:=KeyWordFuncTypeDefault;
end;

function TPascalParserTool.ParseInnerClass(StartPos, WordLen: integer
  ): boolean;
// KeyWordFunctions for parsing in a class/object
var
  p: PChar;
begin
  if StartPos>SrcLen then exit(false);
  p:=@Src[StartPos];
  case UpChars[p^] of
  'C':
    case UpChars[p[1]] of
    'L': if CompareSrcIdentifiers(p,'CLASS') then exit(KeyWordFuncClassClass);
    'O': if CompareSrcIdentifiers(p,'CONSTRUCTOR') then exit(KeyWordFuncClassMethod)
         else if CompareSrcIdentifiers(p,'CONST') then exit(KeyWordFuncClassConstSection);
    end;
  'D':
    if CompareSrcIdentifiers(p,'DESTRUCTOR') then exit(KeyWordFuncClassMethod);
  'E':
    if CompareSrcIdentifiers(p,'END') then exit(false);
  'F':
    if CompareSrcIdentifiers(p,'FUNCTION') then exit(KeyWordFuncClassMethod);
  'P':
    case UpChars[p[1]] of
    'R':
      case UpChars[p[2]] of
      'I': if CompareSrcIdentifiers(p,'PRIVATE') then exit(KeyWordFuncClassSection);
      'O':
        case UpChars[p[3]] of
        'C': if CompareSrcIdentifiers(p,'PROCEDURE') then exit(KeyWordFuncClassMethod);
        'P': if CompareSrcIdentifiers(p,'PROPERTY') then exit(KeyWordFuncClassProperty);
        'T': if CompareSrcIdentifiers(p,'PROTECTED') then exit(KeyWordFuncClassSection);
        end;
      end;
    'U':
      if (UpChars[p[2]]='B') and (UpChars[p[3]]='L') and (UpChars[p[4]]='I') then
        case UpChars[p[5]] of
        'C': if CompareSrcIdentifiers(p,'PUBLIC') then exit(KeyWordFuncClassSection);
        'S': if CompareSrcIdentifiers(p,'PUBLISHED') then exit(KeyWordFuncClassSection);
        end;
    end;
  'S':
    if CompareSrcIdentifiers(p,'STATIC') then exit(KeyWordFuncClassMethod)
    else if CompareSrcIdentifiers(p,'STRICT') then exit(KeyWordFuncClassSection);
  'T':
    if CompareSrcIdentifiers(p,'TYPE') then exit(KeyWordFuncClassTypeSection);
  'V':
    if CompareSrcIdentifiers(p,'VAR') then exit(KeyWordFuncClassVarSection);
  '(','[':
    begin
      ReadTilBracketClose(true);
      exit(true);
    end;
  ';': exit(true);
  end;
  Result:=KeyWordFuncClassIdentifier;
end;

function TPascalParserTool.ParseClassVarType(StartPos, WordLen: integer
  ): boolean;
// KeywordFunctions for parsing the type of a variable in a class/object
var
  p: PChar;
begin
  if StartPos>SrcLen then exit(false);
  p:=@Src[StartPos];
  case UpChars[p^] of
  'A':
    if CompareSrcIdentifiers('ARRAY',p) then exit(KeyWordFuncClassVarTypeArray);
  'B':
    if CompareSrcIdentifiers('BITPACKED',p) then exit(KeyWordFuncClassVarTypeBitPacked);
  'C':
    if CompareSrcIdentifiers('CLASS',p) then exit(KeyWordFuncClassVarTypeClass);
  'F':
    if CompareSrcIdentifiers('FUNCTION',p) then exit(KeyWordFuncClassVarTypeProc);
  'O':
    if CompareSrcIdentifiers('OBJECT',p) then exit(KeyWordFuncClassVarTypeClass);
  'P':
    case UpChars[p[1]] of
    'A': if CompareSrcIdentifiers('PACKED',p) then exit(KeyWordFuncClassVarTypePacked);
    'R': if CompareSrcIdentifiers('PROCEDURE',p) then exit(KeyWordFuncClassVarTypeProc);
    end;
  'R':
    if CompareSrcIdentifiers('RECORD',p) then exit(KeyWordFuncClassVarTypeRecord);
  'S':
    if CompareSrcIdentifiers('SET',p) then exit(KeyWordFuncClassVarTypeSet);
  end;
  Result:=KeyWordFuncClassVarTypeIdent;
end;

function TPascalParserTool.SkipInnerClassInterface(StartPos, WordLen: integer
  ): boolean;
// KeyWordFunctions for skipping in a class interface, dispinterface
var
  p: PChar;
begin
  if StartPos>SrcLen then exit(false);
  p:=@Src[StartPos];
  case UpChars[p^] of
  'E': if CompareSrcIdentifiers(p,'END') then exit(false);
  'F': if CompareSrcIdentifiers(p,'FUNCTION') then exit(KeyWordFuncClassMethod);
  'P':
    if (UpChars[p[1]]='R') and (UpChars[p[2]]='O') then
      case UpChars[p[3]] of
      'C': if CompareSrcIdentifiers(p,'PROCEDURE') then exit(KeyWordFuncClassMethod);
      'P': if CompareSrcIdentifiers(p,'PROPERTY') then exit(KeyWordFuncClassProperty);
      end;
  '(','[':
    begin
      ReadTilBracketClose(true);
      exit(true);
    end;
  ';': exit(true);
  end;
  Result:=false;
end;

function TPascalParserTool.UnexpectedKeyWord: boolean;
begin
  Result:=false;
  SaveRaiseExceptionFmt(ctsUnexpectedKeyword,[GetAtom],true);
end;

function TPascalParserTool.EndOfSourceExpected: boolean;
begin
  Result:=false;
  RaiseEndOfSourceExpected;
end;

procedure TPascalParserTool.BuildTree(OnlyInterfaceNeeded: boolean);
var
  SourceType: TCodeTreeNodeDesc;
  Node: TCodeTreeNode;
begin
  {$IFDEF MEM_CHECK}CheckHeap('TBasicCodeTool.BuildTree A '+IntToStr(MemCheck_GetMem_Cnt));{$ENDIF}
  {$IFDEF CTDEBUG}
  DebugLn('TPascalParserTool.BuildTree A ',MainFilename);
  {$ENDIF}
  ValidateToolDependencies;
  if not UpdateNeeded(OnlyInterfaceNeeded) then begin
    // input is the same as last time -> output is the same
    // => if there was an error, raise it again
    //debugln(['TPascalParserTool.BuildTree ',ord(LastErrorPhase),' ',IgnoreErrorAfterValid]);
    if (LastErrorPhase in [CodeToolPhaseScan,CodeToolPhaseParse]) then begin
      // last time a parsing error occurred
      if IgnoreErrorAfterValid
      and IgnoreErrorAfterPositionIsInFrontOfLastErrMessage
      then begin
        // last error is behind needed code
        // => ignore
        exit;
      end;
      //debugln(['TPascalParserTool.BuildTree ',MainFilename,' OnlyInterfaceNeeded=',OnlyInterfaceNeeded,' ImplementationSectionFound=',ImplementationSectionFound]);
      if OnlyInterfaceNeeded and ImplementationSectionFound then begin
        Node:=FindImplementationNode;
        if (Node<>nil) and not LastErrorIsInFrontOfCleanedPos(Node.StartPos)
        then begin
          // last error was after interface section and only interface is needed
          // => ignore
          exit;
        end;
      end;
      RaiseLastError;
    end;
    exit;
  end;
  ClearLastError;
  //DebugLn('TPascalParserTool.BuildTree B OnlyIntf=',dbgs(OnlyInterfaceNeeded),'  ',TCodeBuffer(Scanner.MainCode).Filename);
  //CheckHeap('TBasicCodeTool.BuildTree B '+IntToStr(MemCheck_GetMem_Cnt));
  
  // scan code
  BeginParsing(true,OnlyInterfaceNeeded);
  {$IFDEF VerboseUpdateNeeded}
  if FForceUpdateNeeded=true then
    DebugLn(['TCustomCodeTool.BuildTree FForceUpdateNeeded:=false ',MainFilename]);
  {$ENDIF}
  FForceUpdateNeeded:=false;
  
  // parse code and build codetree
  CurrentPhase:=CodeToolPhaseParse;
  if Scanner.CompilerMode=cmDELPHI then
    WordIsKeyWordFuncList:=WordIsDelphiKeyWord
  else
    WordIsKeyWordFuncList:=WordIsKeyWord;
  
  InterfaceSectionFound:=false;
  ImplementationSectionFound:=false;
  EndOfSourceFound:=false;
  
  try
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
      SaveRaiseExceptionFmt(ctsNoPascalCodeFound,[GetAtom],true);
    SourceType:=CurSection;
    CreateChildNode;
    CurNode.Desc:=CurSection;
    ReadNextAtom; // read source name
    AtomIsIdentifier(true);
    ReadNextAtom; // read ';' (or 'platform;' or 'unimplemented;')
    if UpAtomIs('PLATFORM') then
      ReadNextAtom;
    if UpAtomIs('UNIMPLEMENTED') then
      ReadNextAtom;
    if UpAtomIs('LIBRARY') then
      ReadNextAtom;
    if UpAtomIs('EXPERIMENTAL') then
      ReadNextAtom;
    if UpAtomIs('DEPRECATED') then
      ReadNextAtom;
    if (CurPos.Flag<>cafSemicolon) then
      RaiseCharExpectedButAtomFound(';');
    if CurSection=ctnUnit then begin
      ReadNextAtom;
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode;
      //DebugLn(['TPascalParserTool.BuildTree ',MainFilename,' ',Scanner.NestedComments]);
      if not UpAtomIs('INTERFACE') then
        RaiseStringExpectedButAtomFound('"interface"');
      CreateChildNode;
      CurSection:=ctnInterface;
      CurNode.Desc:=CurSection;
    end;
    InterfaceSectionFound:=true;
    ReadNextAtom;
    if UpAtomIs('USES') then
      ReadUsesSection(true);
    if (SourceType=ctnPackage) then begin
      if UpAtomIs('REQUIRES') then
        ReadRequiresSection(true);
      if UpAtomIs('CONTAINS') then
        ReadContainsSection(true);
    end;
    repeat
      //DebugLn('[TPascalParserTool.BuildTree] ALL ',GetAtom);
      if not DoAtom then break;
      if CurSection=ctnNone then begin
        EndOfSourceFound:=true;
        break;
      end;
      ReadNextAtom;
    until (CurPos.StartPos>SrcLen);
    FForceUpdateNeeded:=false;
  except
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildTree ',MainFilename,' ERROR: ',LastErrorMessage);
    {$ENDIF}
    if (not IgnoreErrorAfterValid)
    or (not IgnoreErrorAfterPositionIsInFrontOfLastErrMessage) then
      raise;
    FForceUpdateNeeded:=false;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildTree ',MainFilename,' IGNORING ERROR: ',LastErrorMessage);
    {$ENDIF}
  end;
  {$IFDEF CTDEBUG}
  DebugLn('[TPascalParserTool.BuildTree] END');
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap('TBasicCodeTool.BuildTree END '+IntToStr(MemCheck_GetMem_Cnt));
  {$ENDIF}
  CurrentPhase:=CodeToolPhaseTool;
end;

procedure TPascalParserTool.BuildSubTreeForClass(ClassNode: TCodeTreeNode);
// reparse a quick parsed class and build the child nodes

  procedure RaiseClassDescInvalid;
  begin
    MoveCursorToNodeStart(ClassNode);
    RaiseException('[TPascalParserTool.BuildSubTreeForClass] ClassNode.Desc='
                   +ClassNode.DescAsString,true);
  end;

  procedure RaiseClassKeyWordExpected;
  begin
    RaiseException(
        'TPascalParserTool.BuildSubTreeForClass:'
       +' class/object keyword expected, but '+GetAtom+' found',true);
  end;
  
var OldPhase: integer;
begin
  if not (ClassNode.Desc in AllClasses) then
    RaiseClassDescInvalid;
  if (ClassNode.SubDesc and ctnsNeedJITParsing)=0 then begin
    // class already parsed
    if (ctnsHasParseError and ClassNode.SubDesc)>0 then
      RaiseNodeParserError(ClassNode);
    exit;
  end;
  // avoid endless loop
  OldPhase:=CurrentPhase;
  CurrentPhase:=CodeToolPhaseParse;
  try
    ClassNode.SubDesc:=ClassNode.SubDesc and (not ctnsNeedJITParsing);
    // set CursorPos after class head
    MoveCursorToNodeStart(ClassNode);
    // parse
    //   - sealed, abstract
    //   - inheritage
    //   - class sections (GUID, type, var, public, published, private, protected)
    //   - methods (procedures, functions, constructors, destructors)

    // read the "class"/"object" keyword
    ReadNextAtom;
    if UpAtomIs('PACKED') or (UpAtomIs('BITPACKED')) then ReadNextAtom;
    if not (UpAtomIs('CLASS') or UpAtomIs('OBJECT') or UpAtomIs('OBJCCLASS')
           or UpAtomIs('OBJCCATEGORY') or UpAtomIs('CPPCLASS')
           or UpAtomIs('INTERFACE') or UpAtomIs('OBJCPROTOCOL'))
    then
      RaiseClassKeyWordExpected;
    ReadNextAtom;
    // parse modifiers
    if CurPos.Flag=cafWord then begin
      if UpAtomIs('SEALED') then begin
        while UpAtomIs('SEALED') do begin
          CreateChildNode;
          CurNode.Desc:=ctnClassSealed;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          ReadNextAtom;
        end;
      end else if UpAtomIs('ABSTRACT') then begin
        while UpAtomIs('ABSTRACT') do begin
          CreateChildNode;
          CurNode.Desc:=ctnClassAbstract;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          ReadNextAtom;
        end;
      end;
    end;
    // parse the inheritage
    if CurPos.Flag=cafRoundBracketOpen then
      ReadClassInheritance(true)
    else
      UndoReadNextAtom;
    // clear the last atoms
    LastAtoms.Clear;
    // start the first class section (always published)
    CreateChildNode;
    CurNode.Desc:=ctnClassPublished;
    CurNode.StartPos:=CurPos.EndPos; // behind 'class' including the space
    ReadNextAtom;
    if CurPos.Flag=cafEdgedBracketOpen then
      ReadGUID;
    // parse till "end" of class/object
    repeat
      //DebugLn(['TPascalParserTool.BuildSubTreeForClass Atom=',GetAtom,' ',CurPos.StartPos>=ClassNode.EndPos]);
      if CurPos.StartPos>=ClassNode.EndPos then break;
      if not ParseInnerClass(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
        break;
      ReadNextAtom;
    until false;
    // end last class section (public, private, ...)
    CurNode.EndPos:=CurPos.StartPos;
    EndChildNode;
    CurrentPhase:=OldPhase;
  except
    CurrentPhase:=OldPhase;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildSubTreeForClass ',MainFilename,' ERROR: ',LastErrorMessage);
    {$ENDIF}
    if (not IgnoreErrorAfterValid)
    or (not IgnoreErrorAfterPositionIsInFrontOfLastErrMessage) then
      raise;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildSubTreeForClass',MainFilename,' IGNORING ERROR: ',LastErrorMessage);
    {$ENDIF}
  end;
end;

procedure TPascalParserTool.BuildSubTreeForBeginBlock(BeginNode: TCodeTreeNode);
// reparse a quick parsed begin..end block and build the child nodes
//   create nodes for 'with' and 'case' statements

  procedure RaiseBeginExpected;
  begin
    SaveRaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: begin expected, but '
       +GetAtom+' found',true);
  end;

var MaxPos, OldPhase: integer;
begin
  if BeginNode=nil then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: BeginNode=nil');
  if BeginNode.Desc<>ctnBeginBlock then
    RaiseException(
       'TPascalParserTool.BuildSubTreeForBeginBlock: BeginNode.Desc='
       +BeginNode.DescAsString);
  if (BeginNode.SubDesc and ctnsNeedJITParsing)=0 then begin
    // block already parsed
    if (ctnsHasParseError and BeginNode.SubDesc)>0 then
      RaiseNodeParserError(BeginNode);
    exit;
  end;

  OldPhase:=CurrentPhase;
  CurrentPhase:=CodeToolPhaseParse;
  try
    BeginNode.SubDesc:=BeginNode.SubDesc and (not ctnsNeedJITParsing);
    // set CursorPos on 'begin'
    MoveCursorToNodeStart(BeginNode);
    ReadNextAtom;
    if not UpAtomIs('BEGIN') then
      RaiseBeginExpected;
    if BeginNode.EndPos<SrcLen then
      Maxpos:=BeginNode.EndPos
    else
      MaxPos:=SrcLen;
    repeat
      ReadNextAtom;
      if BlockStatementStartKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos) then
      begin
        if not ReadTilBlockEnd(false,true) then RaiseEndOfSourceExpected;
      end else if UpAtomIs('WITH') then
        ReadWithStatement(true,true);
    until (CurPos.StartPos>=MaxPos);
    CurrentPhase:=OldPhase;
  except
    CurrentPhase:=OldPhase;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildSubTreeForBeginBlock ',MainFilename,' ERROR: ',LastErrorMessage);
    {$ENDIF}
    if (not IgnoreErrorAfterValid)
    or (not IgnoreErrorAfterPositionIsInFrontOfLastErrMessage) then begin
      raise;
    end;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildSubTreeForBeginBlock ',MainFilename,' IGNORING ERROR: ',LastErrorMessage);
    {$ENDIF}
  end;
end;

function TPascalParserTool.KeyWordFuncClassIdentifier: boolean;
{ parse class variable or type or const

  examples for variables:
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
  examples for type:
    TCompareFunc = function(const Item1, Item2: T): Integer;
}
begin
  if CurNode.Desc = ctnClassType then begin
    // create type definition node
    CreateChildNode;
    CurNode.Desc:=ctnTypeDefinition;
    ReadEqualsType;
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end else 
  if CurNode.Desc = ctnClassConst then begin
    // create const definition node
    CreateChildNode;
    CurNode.Desc:=ctnConstDefinition;
    ReadConst;
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end else begin
    // create variable definition node
    CreateChildNode;
    CurNode.Desc:=ctnVarDefinition;
    ReadNextAtom;
    while CurPos.Flag=cafComma do begin
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
    if CurPos.Flag<>cafColon then
      RaiseCharExpectedButAtomFound(':');
    // read type
    ReadVariableType;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeClass: boolean;
// class and object as type are not allowed, because they would have no name
begin
  SaveRaiseExceptionFmt(ctsAnonymDefinitionsAreNotAllowed,[GetAtom]);
  Result:=false;
end;

function TPascalParserTool.KeyWordFuncClassVarTypePacked: boolean;
// 'packed' record
begin
  ReadNextAtom;
  if UpAtomIs('RECORD') then
    Result:=KeyWordFuncClassVarTypeRecord
  else begin
    RaiseStringExpectedButAtomFound('"record"');
    Result:=true;
  end;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeBitPacked: boolean;
// 'bitpacked' array
begin
  ReadNextAtom;
  if UpAtomIs('ARRAY') then
    Result:=KeyWordFuncClassVarTypeArray
  else begin
    RaiseStringExpectedButAtomFound('"array"');
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
    else if (CurPos.Flag=cafEND) then dec(Level);
  end;
  if CurPos.StartPos>SrcLen then
    SaveRaiseException(ctsEndForRecordNotFound);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeArray: boolean;
{ read variable type 'array'

  examples:
    array of array[EnumType] of array [Range] of TypeName;
}
begin
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    // array[Range]
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseCharExpectedButAtomFound('[');
  ReadNextAtom;
  Result:=ParseClassVarType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncClassVarTypeSet: boolean;
{ read variable type 'set of'

  examples:
    set of Name
    set of (MyEnummy4 := 4 , MyEnummy5);
}
begin
  CreateChildNode;
  CurNode.Desc:=ctnSetType;
  ReadNextAtom;
  if not UpAtomIs('OF') then
    RaiseStringExpectedButAtomFound('"of"');
  ReadNextAtom;
  if CurPos.StartPos>SrcLen then
    SaveRaiseException(ctsMissingEnumList);
  if IsIdentStartChar[Src[CurPos.StartPos]] then
    // set of identifier
  else if CurPos.Flag=cafRoundBracketOpen then
    // set of ()
    ReadTilBracketClose(true);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
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
  ParseAttr: TParseProcHeadAttributes;
begin
//DebugLn('[TPascalParserTool.KeyWordFuncClassVarTypeProc]');
  IsFunction:=UpAtomIs('FUNCTION');
  ReadNextAtom;
  HasForwardModifier:=false;
  ParseAttr:=[pphIsMethod,pphIsType];
  if IsFunction then Include(ParseAttr,pphIsFunction);
  ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarTypeIdent: boolean;
// read variable type <identifier>
begin
  if CurPos.StartPos>SrcLen then
    SaveRaiseException(ctsMissingTypeIdentifier);
  if IsIdentStartChar[Src[CurPos.StartPos]] then
    // identifier
  else
    SaveRaiseException(ctsMissingTypeIdentifier);
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
  if UpAtomIs('STRICT') then ReadNextAtom;
  if UpAtomIs('PUBLIC') then
    CurNode.Desc:=ctnClassPublic
  else if UpAtomIs('PRIVATE') then
    CurNode.Desc:=ctnClassPrivate
  else if UpAtomIs('PROTECTED') then
    CurNode.Desc:=ctnClassProtected
  else if UpAtomIs('PUBLISHED') then
    CurNode.Desc:=ctnClassPublished
  else
    RaiseStringExpectedButAtomFound('public');
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassConstSection: boolean;
begin
  // end last section
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  // start new section
  CreateChildNode;
  CurNode.Desc:=ctnClassConst;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassTypeSection: boolean;
begin
  // end last section
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  // start new section
  CreateChildNode;
  CurNode.Desc:=ctnClassType;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassVarSection: boolean;
{
  var
  class var
}
begin
  // end last section
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
  // start new section
  CreateChildNode;
  if UpAtomIs('CLASS') then 
  begin
    CurNode.Desc:=ctnClassClassVar;
    ReadNextAtom;
  end
  else
    CurNode.Desc:=ctnClassVar; 
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassClass: boolean;
{ parse
    class procedure
    class property
    class constructor
    class destructor
    class var
}
begin
  ReadNextAtom;
  if UpAtomIs('PROCEDURE') or UpAtomIs('FUNCTION') or UpAtomIs('CONSTRUCTOR') or UpAtomIs('DESTRUCTOR') then begin
    UndoReadNextAtom;
    Result:=KeyWordFuncClassMethod;
  end else if UpAtomIs('PROPERTY') then begin
    UndoReadNextAtom;
    Result:=KeyWordFuncClassProperty;
  end else if UpAtomIs('VAR') then begin
    UndoReadNextAtom;
    Result:=KeyWordFuncClassVarSection;
  end else
    RaiseStringExpectedButAtomFound('procedure');
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
   function Intf.Method = ImplementingMethodName;

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline,
   compilerproc

 proc specifiers with parameters:
   message <id or number>
   dispid <id>
   enumerator <id>
   }
var IsFunction, HasForwardModifier: boolean;
  ParseAttr: TParseProcHeadAttributes;
begin
  if not (CurNode.Desc in (AllClassSections+AllClassInterfaces)) then
    RaiseIdentExpectedButAtomFound;

  HasForwardModifier:=false;
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProcedure;
  // read method keyword
  if UpAtomIs('CLASS') or (UpAtomIs('STATIC')) then begin
    ReadNextAtom;
    if (not UpAtomIs('PROCEDURE')) and (not UpAtomIs('FUNCTION')) and (not UpAtomIs('CONSTRUCTOR')) and (not UpAtomIs('DESTRUCTOR')) then begin
      RaiseStringExpectedButAtomFound(ctsProcedureOrFunctionOrConstructorOrDestructor);
    end;
  end;
  IsFunction:=UpAtomIs('FUNCTION');
  // read procedure head
  // read name
  ReadNextAtom;
  AtomIsIdentifier(true);
  // create node for procedure head
  CreateChildNode;
  CurNode.Desc:=ctnProcedureHead;
  ReadNextAtom;
  if (CurPos.Flag<>cafPoint) then begin
    // read rest
    CurNode.SubDesc:=ctnsNeedJITParsing;
    ParseAttr:=[pphIsMethod];
    if IsFunction then Include(ParseAttr,pphIsFunction);
    ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
  end else begin
    // Method resolution clause (e.g. function Intf.Method = Method_Name)
    CurNode.Parent.Desc:=ctnMethodMap;
    // read Method name of interface
    ReadNextAtom;
    AtomIsIdentifier(true);
    //DebugLn(['TPascalParserTool.KeyWordFuncClassMethod ',GetAtom,' at ',CleanPosToStr(CurPos.StartPos,true)]);
    // read '='
    ReadNextAtomIsChar('=');
    // read implementing method name
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      UndoReadNextAtom;
  end;
  // close procedure header
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  // close procedure / method map
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.ReadParamList(ExceptionOnError, Extract: boolean;
  const Attr: TProcHeadAttributes): boolean;
{ parse parameter list

 examples:
   procedure ProcName;  virtual; abstract;
   function FuncName(Parameter1: Type1; Parameter2: Type2): ResultType;
   constructor Create;
   destructor Destroy;  override;
   class function X: integer;
   function QWidget_mouseGrabber(): QWidgetH; cdecl;
   procedure Intf.Method = ImplementingMethodName;
   function CommitUrlCacheEntry; // only Delphi
   procedure MacProcName(c: char; ...); external;

 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>;
   external;
   external <id>;
   external name <id> delayed;
   external <id or number> name <id>;
   external <id or number> index <id>;
   [alias: <string constant>]
   [external name <string constant>]
   [internconst:in_const_round, external name 'FPC_ROUND'];
   dispid <id>;
}
var CloseBracket: char;
  Desc: TCodeTreeNodeDesc;
  Node: TCodeTreeNode;

  procedure ReadPrefixModifier;
  begin
    // read parameter prefix modifier
    if UpAtomIs('VAR') or UpAtomIs('CONST') or UpAtomIs('CONSTREF')
    or (UpAtomIs('OUT') and (Scanner.CompilerMode in [cmOBJFPC,cmDELPHI,cmFPC]))
    then begin
      Desc:=ctnVarDefinition;
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(phpWithVarModifiers in Attr,Attr);
    end else
      Desc:=ctnVarDefinition;
  end;
  
  procedure ReadDefaultValue;
  begin
    // read =
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom([phpWithDefaultValues,phpWithHasDefaultValues]*Attr<>[],Attr);
    ReadConstant(ExceptionOnError,
      Extract and (phpWithDefaultValues in Attr),Attr);
    if (phpCreateNodes in Attr) then begin
      Node:=CurNode;
      Node.SubDesc:=Node.SubDesc+ctnsHasDefaultValue;
      Node:=Node.PriorBrother;
      while (Node<>nil) and (Node.FirstChild=nil) do begin
        Node.SubDesc:=Node.SubDesc+ctnsHasDefaultValue;
        Node:=Node.PriorBrother;
      end;
    end;
  end;

begin
  Result:=false;
  if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
    if CurPos.Flag=cafRoundBracketOpen then
      CloseBracket:=')'
    else
      CloseBracket:=']';
    if (phpCreateNodes in Attr) then begin
      CreateChildNode;
      CurNode.Desc:=ctnParameterList;
    end;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutBrackets in Attr),Attr);
  end else
    CloseBracket:=#0;
  if not (CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose]) then begin
    repeat
      if AtomIs('...') then begin
        // MacPas '...' VarArgs parameter
        if (Scanner.CompilerMode<>cmMacPas) then begin
          if ExceptionOnError then
            RaiseIdentExpectedButAtomFound
          else
            exit;
        end;
        ReadNextAtom;
        // parse end of parameter list
        if (CurPos.StartPos>SrcLen)
        or (Src[CurPos.StartPos]<>CloseBracket) then
          if ExceptionOnError then
            RaiseCharExpectedButAtomFound(CloseBracket)
          else exit;
        break;
      end else begin
        ReadPrefixModifier;
        // read parameter name(s)
        repeat
          if not AtomIsIdentifier(ExceptionOnError) then exit;
          if (phpCreateNodes in Attr) then begin
            CreateChildNode;
            CurNode.Desc:=Desc;
          end;
          if not Extract then
            ReadNextAtom
          else
            ExtractNextAtom(phpWithParameterNames in Attr,Attr);
          if CurPos.Flag<>cafComma then
            break
          else begin
            if (phpCreateNodes in Attr) then begin
              CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
              EndChildNode;
            end;
            if not Extract then
              ReadNextAtom
            else
              ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
          end;
        until false;
        // read parameter type
        if CurPos.Flag=cafColon then begin
          if not Extract then
            ReadNextAtom
          else
            // extract the colon if parameter names and types are requested
            ExtractNextAtom(
              [phpWithoutParamList,phpWithoutParamTypes,phpWithParameterNames]*Attr
              =[phpWithParameterNames],
              Attr);
          if not ReadParamType(ExceptionOnError,Extract,Attr) then exit;
          if CurPos.Flag=cafEqual then begin
            // read default value
            ReadDefaultValue;
          end;
        end else if (CurPos.Flag in [cafSemicolon,cafRoundBracketClose,
          cafEdgedBracketClose])
        then begin
          // no type -> variant
          if (phpCreateNodes in Attr) then begin
            CreateChildNode;
            CurNode.Desc:=ctnVariantType;
            CurNode.EndPos:=CurNode.StartPos;
            EndChildNode;
          end;
        end else
          break;
        if (phpCreateNodes in Attr) then begin
          CurNode.EndPos:=CurPos.StartPos;
          EndChildNode;
        end;
      end;
      // read next parameter
      if (CurPos.StartPos>SrcLen) then
        if ExceptionOnError then
          RaiseCharExpectedButAtomFound(CloseBracket)
        else exit;
      if (CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose]) then
        break;
      if (CurPos.Flag<>cafSemicolon) then
        if ExceptionOnError then
          RaiseCharExpectedButAtomFound(';')
        else exit;
      if not Extract then
        ReadNextAtom
      else
        ExtractNextAtom(not (phpWithoutParamList in Attr),Attr);
    until false;
  end;
  if (CloseBracket<>#0) then begin
    if Src[CurPos.StartPos]<>CloseBracket then begin
      if ExceptionOnError then
        RaiseCharExpectedButAtomFound(CloseBracket)
      else
        exit;
    end;
    if (phpCreateNodes in Attr) then begin
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    if not Extract then
      ReadNextAtom
    else
      ExtractNextAtom(not (phpWithoutBrackets in Attr),Attr);
  end;
  Result:=true;
end;

function TPascalParserTool.ReadParamType(ExceptionOnError, Extract: boolean;
  const Attr: TProcHeadAttributes): boolean;
// after reading, CurPos is the atom after the type
var
  copying: boolean;
  IsArrayType: Boolean;
  IsFileType: Boolean;
  NeedIdentifier: boolean;
begin
  copying:=[phpWithoutParamList,phpWithoutParamTypes]*Attr=[];
  Result:=false;
  if CurPos.Flag in AllCommonAtomWords then begin
    NeedIdentifier:=true;
    IsArrayType:=UpAtomIs('ARRAY');
    if IsArrayType then begin
      //DebugLn(['TPascalParserTool.ReadParamType is array ',MainFilename,' ',CleanPosToStr(curPos.StartPos)]);
      if (phpCreateNodes in Attr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnOpenArrayType;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      if not UpAtomIs('OF') then
        if ExceptionOnError then
          RaiseStringExpectedButAtomFound('"of"')
        else
          exit;
      if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      if UpAtomIs('CONST') then begin
        if (phpCreateNodes in Attr) then begin
          CreateChildNode;
          CurNode.Desc:=ctnOfConstType;
        end;
        if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
        if (phpCreateNodes in Attr) then begin
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          // close ctnOpenArrayType
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
        end;
        Result:=true;
        exit;
      end;
    end;
    IsFileType:=UpAtomIs('FILE');
    if IsFileType then begin
      if (phpCreateNodes in Attr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnFileType;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      if UpAtomIs('OF') then begin
        if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      end else begin
        NeedIdentifier:=false;
      end;
    end;
    if NeedIdentifier then begin
      if not AtomIsIdentifier(ExceptionOnError) then exit;
      if (phpCreateNodes in Attr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnIdentifier;
        CurNode.EndPos:=CurPos.EndPos;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      if CurPos.Flag=cafPoint then begin
        //  first identifier was unitname -> read '.' + identifier
        if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
        if not AtomIsIdentifier(ExceptionOnError) then exit;
        if not Extract then ReadNextAtom else ExtractNextAtom(copying,Attr);
      end;
      if (phpCreateNodes in Attr) then begin
        CurNode.EndPos:=CurPos.StartPos;
        EndChildNode;
      end;
    end;
    if (phpCreateNodes in Attr) then begin
      if IsFileType then begin
        CurNode.EndPos:=CurPos.StartPos;
        EndChildNode;
      end;
      if IsArrayType then begin
        CurNode.EndPos:=CurPos.StartPos;
        EndChildNode;
      end;
    end;
  end else begin
    if ExceptionOnError then
      RaiseStringExpectedButAtomFound(ctsIdentifier)
    else exit;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadTilProcedureHeadEnd(
  ParseAttr: TParseProcHeadAttributes;
  var HasForwardModifier: boolean): boolean;
{ parse parameter list, result type, of object, method specifiers


 examples:
   procedure ProcName;  virtual; abstract;
   function FuncName(Parameter1: Type1; Parameter2: Type2): ResultType;
   constructor Create;
   destructor Destroy;  override;
   class function X: integer;
   function QWidget_mouseGrabber(): QWidgetH; cdecl;
   procedure Intf.Method = ImplementingMethodName;
   function CommitUrlCacheEntry; // only Delphi
   procedure MacProcName(c: char; ...); external;
   operator + (dp1: TPoint; dp2: TPoint) dps: TPoint;
   
   Delphi mode:
   Function TPOSControler.Logout; // missing function type


 proc specifiers without parameters:
   stdcall, virtual, abstract, dynamic, overload, override, cdecl, inline

 proc specifiers with parameters:
   message <id or number>;
   external;
   external <id>;
   external name <id> delayed;
   external <id or number> name <id>;
   external <id or number> index <id>;
   [alias: <string constant>]
   [external name <string constant>]
   [internconst:in_const_round, external name 'FPC_ROUND'];
   dispid <id>;
   enumerator <id>
}

  procedure RaiseKeyWordExampleExpected;
  begin
    SaveRaiseExceptionFmt(
      ctsKeywordExampleExpectedButAtomFound,['alias',GetAtom]);
  end;

var IsSpecifier: boolean;
  Attr: TProcHeadAttributes;
begin
  //DebugLn('[TPascalParserTool.ReadTilProcedureHeadEnd] ',
  //'Method=',IsMethod,', Function=',IsFunction,', Type=',IsType);
  Result:=true;
  HasForwardModifier:=false;
  if CurPos.Flag=cafRoundBracketOpen then begin
    Attr:=[];
    if pphCreateNodes in ParseAttr then
      Include(Attr,phpCreateNodes);
    ReadParamList(true,false,Attr);
  end;
  if (pphIsOperator in ParseAttr) and (CurPos.Flag<>cafColon) then begin
    // read operator result identifier
    AtomIsIdentifier(true);
    if (pphCreateNodes in ParseAttr) then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    ReadNextAtom;
  end;
  if ([pphIsFunction,pphIsOperator]*ParseAttr<>[]) then begin
    // read function result type
    if CurPos.Flag=cafColon then begin
      ReadNextAtom;
      AtomIsIdentifier(true);
      if (pphCreateNodes in ParseAttr) then begin
        CreateChildNode;
        CurNode.Desc:=ctnIdentifier;
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode;
      end;
      ReadNextAtom;
      if CurPos.Flag=cafPoint then begin
        //  unitname.identifier -> read identifier
        ReadNextAtom;
        AtomIsIdentifier(true);
        if (pphCreateNodes in ParseAttr) then begin
          CreateChildNode;
          CurNode.Desc:=ctnIdentifier;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
        end;
        ReadNextAtom;
      end;
    end else begin
      if (Scanner.CompilerMode<>cmDelphi) then
        RaiseCharExpectedButAtomFound(':')
      else begin
        // Delphi Mode
        if CurPos.Flag=cafEqual then begin
          // read interface alias
          ReadNextAtom;
          AtomIsIdentifier(true);
          ReadNextAtom;
        end;
      end;
    end;
  end;
  if UpAtomIs('OF') then begin
    // read 'of object'
    if not (pphIsType in ParseAttr) then
      RaiseCharExpectedButAtomFound(';');
    ReadNextAtom;
    if not UpAtomIs('OBJECT') then
      RaiseStringExpectedButAtomFound('"object"');
    ReadNextAtom;
  end;
  // read procedures/method specifiers
  if CurPos.Flag=cafEND then begin
    UndoReadNextAtom;
    exit;
  end;
  if CurPos.Flag=cafSemicolon then
    ReadNextAtom;
  if (CurPos.StartPos>SrcLen) then
    SaveRaiseException(ctsSemicolonNotFound);
  repeat
    if (pphIsMethod in ParseAttr) then
      IsSpecifier:=IsKeyWordMethodSpecifier.DoItCaseInsensitive(Src,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    else if pphIsType in ParseAttr then
      IsSpecifier:=IsKeyWordProcedureTypeSpecifier.DoItCaseInsensitive(Src,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    else
      IsSpecifier:=IsKeyWordProcedureSpecifier.DoItCaseInsensitive(Src,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
    if IsSpecifier then begin
      // read specifier
      if UpAtomIs('MESSAGE') or UpAtomIs('DISPID') or UpAtomIs('ENUMERATOR')
      or UpAtomIs('DEPRECATED')
      then begin
        ReadNextAtom;
        if not (CurPos.Flag in [cafSemicolon,cafEND]) then
          ReadConstant(true,false,[]);
      end else if UpAtomIs('IS') then begin
        ReadNextAtom;
        if not UpAtomIs('NESTED') then
          RaiseStringExpectedButAtomFound('nested');
        ReadNextAtom;
      end else if UpAtomIs('EXTERNAL') or UpAtomIs('WEAKEXTERNAL') or UpAtomIs('PUBLIC') then begin
        HasForwardModifier:=UpAtomIs('EXTERNAL') or UpAtomIs('WEAKEXTERNAL');
        ReadNextAtom;
        if CurPos.Flag<>cafSemicolon then begin
          if not UpAtomIs('NAME') then
            ReadConstant(true,false,[]);
          if UpAtomIs('NAME') or UpAtomIs('INDEX') then begin
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
          if UpAtomIs('DELAYED') then
            ReadNextAtom;
        end;
      end else if UpAtomIs('ALIAS') then begin
        if not ReadNextAtomIsChar(':') then
          RaiseCharExpectedButAtomFound(':');
        ReadNextAtom;
        ReadConstant(true,false,[]);
      end else if CurPos.Flag=cafEdgedBracketOpen then begin
        // read assembler alias   [public,alias: 'alternative name'],
        // internproc, internconst, external
        repeat
          ReadNextAtom;
          if not (CurPos.Flag in AllCommonAtomWords) then
            RaiseStringExpectedButAtomFound(ctsKeyword);
          if not IsKeyWordProcedureBracketSpecifier.DoItCaseInsensitive(Src,
            CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
          then
            RaiseKeyWordExampleExpected;
          if UpAtomIs('INTERNPROC') then
            HasForwardModifier:=true;
            
          if UpAtomIs('INTERNCONST') then begin
            ReadNextAtom;
            if AtomIsChar(':') then begin
              ReadNextAtom;
              AtomIsIdentifier(true);
              ReadNextAtom;
            end;
          end else if UpAtomIs('EXTERNAL') then begin
            HasForwardModifier:=true;
            ReadNextAtom;
            if not (CurPos.Flag in [cafComma,cafEdgedBracketClose]) then begin
              if not UpAtomIs('NAME') then
                ReadConstant(true,false,[]);
              if UpAtomIs('NAME') or UpAtomIs('INDEX') then begin
                ReadNextAtom;
                ReadConstant(true,false,[]);
              end;
            end;
          end else
            ReadNextAtom;
          if CurPos.Flag in [cafColon,cafEdgedBracketClose] then
            break;
          if CurPos.Flag<>cafComma then
            RaiseCharExpectedButAtomFound(']');
        until false;
        if CurPos.Flag=cafColon then begin
          ReadNextAtom;
          if (not AtomIsStringConstant) and (not AtomIsIdentifier(false)) then
            RaiseStringExpectedButAtomFound(ctsStringConstant);
          ReadConstant(true,false,[]);
        end;
        if CurPos.Flag<>cafEdgedBracketClose then
          RaiseCharExpectedButAtomFound(']');
        ReadNextAtom;
        if CurPos.Flag=cafEND then begin
          UndoReadNextAtom;
          exit;
        end;
      end else begin
        // read specifier without parameters
        if UpAtomIs('FORWARD') then HasForwardModifier:=true;
        ReadNextAtom;
        if CurPos.Flag=cafEND then begin
          UndoReadNextAtom;
          exit;
        end;
      end;
      // check semicolon
      if CurPos.Flag=cafSemicolon then begin
        ReadNextAtom;
      end else begin
        // Delphi/FPC allow procs without ending semicolon
      end;
    end else begin
      // current atom does not belong to procedure/method declaration
      UndoReadNextAtom; // unread unknown atom
      break;
    end;
  until false;
end;

function TPascalParserTool.ReadConstant(ExceptionOnError, Extract: boolean;
  const Attr: TProcHeadAttributes): boolean;
// after reading, the CurPos will be on the atom after the constant

 procedure RaiseConstantExpected;
 begin
   if ExceptionOnError then
     RaiseStringExpectedButAtomFound(ctsConstant);
 end;

var
  BracketType: TCommonAtomFlag;
  p: PChar;
begin
  Result:=false;
  repeat
    // read unary operators
    repeat
      if (CurPos.StartPos>SrcLen) then begin
        RaiseConstantExpected;
        exit;
      end;
      p:=@Src[CurPos.StartPos];
      case p^ of
        '-','+','@':
          if CurPos.EndPos-CurPos.StartPos<>1 then break;
        'n','N':
          if not UpAtomIs('NOT') then break;
        'i','I':
          if not UpAtomIs('INHERITED') then break;
      else
        break;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    until false;
    // read operand
    if CurPos.Flag in AllCommonAtomWords then begin
      // word (identifier or keyword)
      if AtomIsKeyWord and (not IsKeyWordInConstAllowed.DoItCaseInsensitive(Src,
              CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)) then begin
        if ExceptionOnError then
          RaiseUnexpectedKeyWord
        else exit;
      end;
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      while CurPos.Flag=cafPoint do begin
        // Unitname.Constant
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
        if AtomIsKeyWord and (not IsKeyWordInConstAllowed.DoItCaseInsensitive(Src,
                CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)) then begin
          if ExceptionOnError then
            RaiseUnexpectedKeyWord
          else exit;
        end;
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      end;
      if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
      begin
        // type cast or constant array
        BracketType:=CurPos.Flag;
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
        if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
        if (BracketType=cafRoundBracketOpen)
        and (CurPos.Flag<>cafRoundBracketClose) then
          if ExceptionOnError then
            RaiseCharExpectedButAtomFound(')')
          else exit;
        if (BracketType=cafEdgedBracketOpen)
        and (CurPos.Flag<>cafEdgedBracketClose) then
          if ExceptionOnError then
            RaiseCharExpectedButAtomFound(']')
          else exit;
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      end;
    end else if AtomIsNumber or AtomIsStringConstant then begin
      // number or '...' or #...
      if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
    end else begin
      if CurPos.Flag=cafRoundBracketOpen then begin
        // open bracket + ? + close bracket
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
        if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
        if (CurPos.Flag<>cafRoundBracketClose) then
          if ExceptionOnError then
            RaiseCharExpectedButAtomFound(')')
          else exit;
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      end else if CurPos.Flag=cafEdgedBracketOpen then begin
        // open bracket + ? + close bracket
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
        repeat
          if (CurPos.Flag=cafEdgedBracketClose) then break;
          // read
          if not ReadConstant(ExceptionOnError,Extract,Attr) then exit;
          if (CurPos.Flag=cafComma) or AtomIs('..') then begin
            // continue
            if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
          end else if (CurPos.Flag<>cafEdgedBracketClose) then begin
            if ExceptionOnError then
              RaiseCharExpectedButAtomFound(']')
            else exit;
          end;
        until false;
        if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
      end else begin
        // syntax error
        RaiseConstantExpected;
        exit;
      end;
    end;
    if CurPos.StartPos>SrcLen then break;
    if not WordIsTermOperator.DoItCaseInsensitive(Src,
         CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // not a operator
      break;
    end;
    // operator => read further
    if not Extract then ReadNextAtom else ExtractNextAtom(true,Attr);
  until false;
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
    if CurPos.Flag=cafSemicolon then break;
    AtomIsIdentifier(true);
    CreateChildNode;
    CurNode.Desc:=ctnUseUnit;
    CurNode.EndPos:=CurPos.EndPos;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        if ExceptionOnError then
          RaiseStringExpectedButAtomFound(ctsStringConstant)
        else exit;
      CurNode.EndPos:=CurPos.EndPos;
      ReadNextAtom;
    end;
    EndChildNode;
    if CurPos.Flag=cafSemicolon then break;
    if CurPos.Flag<>cafComma then
      if ExceptionOnError then
        RaiseCharExpectedButAtomFound(';')
      else exit;
  until (CurPos.StartPos>SrcLen);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.ReadRequiresSection(ExceptionOnError: boolean
  ): boolean;
{ parse requires section

  examples:
    requires name1, name2, name3;

}
begin
  CreateChildNode;
  CurNode.Desc:=ctnRequiresSection;
  repeat
    ReadNextAtom;  // read name
    if CurPos.Flag=cafSemicolon then break;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if CurPos.Flag=cafSemicolon then break;
    if CurPos.Flag<>cafComma then
      if ExceptionOnError then
        RaiseCharExpectedButAtomFound(';')
      else exit;
  until (CurPos.StartPos>SrcLen);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.ReadContainsSection(ExceptionOnError: boolean
  ): boolean;
{ parse contains section

  examples:
    contains name1, name2 in '', name3;

}
begin
  CreateChildNode;
  CurNode.Desc:=ctnContainsSection;
  repeat
    ReadNextAtom;  // read name
    if CurPos.Flag=cafSemicolon then break;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        if ExceptionOnError then
          RaiseStringExpectedButAtomFound(ctsStringConstant)
        else exit;
      ReadNextAtom;
    end;
    if CurPos.Flag=cafSemicolon then break;
    if CurPos.Flag<>cafComma then
      if ExceptionOnError then
        RaiseCharExpectedButAtomFound(';')
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
    Pred(identifier)..Succ(identifier)
}
var RangeOpFound: boolean;
begin
  RangeOpFound:=false;
  repeat
    if CurPos.Flag in [cafSemicolon,cafColon,cafComma,cafRoundBracketClose,
                       cafEdgedBracketClose]
    then
      break;
    if CurPos.StartPos>SrcLen then
      RaiseCharExpectedButAtomFound(';');
    if AtomIs('..') then begin
      if RangeOpFound then
        RaiseCharExpectedButAtomFound(';');
      RangeOpFound:=true;
    end else if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
      ReadTilBracketClose(ExceptionOnError);
    ReadNextAtom;
  until false;
  Result:=true;
end;

function TPascalParserTool.ReadTilBracketCloseOrUnexpected(
  ExceptionOnNotFound: boolean; Flags: TSkipBracketChecks): boolean;
{ Cursor must be on round/edged bracket open
  After parsing cursor will be on closing bracket or on the unexpected atom
}
type
  TStackItemType = (
    siNone,
    siRoundBracketOpen,
    siEdgedBracketOpen,
    siRecord
    );
  TStackItem = record
    Typ: TStackItemType;
    StartPos: integer;
  end;
  PStackItem = ^TStackItem;
var
  Stack: array[0..16] of TStackItem;
  ExtStack: PStackItem;
  ExtStackCapacity: integer;
  Ptr: integer;
  Top: TStackItemType;
  p: PChar;

  procedure Push(Item: TStackItemType);
  var
    p: Integer;
  begin
    inc(Ptr);
    if Ptr<=High(Stack) then begin
      Stack[Ptr].Typ:=Item;
      Stack[Ptr].StartPos:=CurPos.StartPos;
    end else begin
      // need ExStack
      if (ExtStack=nil) then begin
        ExtStackCapacity:=10;
        GetMem(ExtStack,SizeOf(TStackItem)*ExtStackCapacity);
      end else begin
        ExtStackCapacity:=ExtStackCapacity*2;
        ReAllocMem(ExtStack,SizeOf(TStackItem)*ExtStackCapacity);
      end;
      p:=Ptr-High(Stack)-1;
      ExtStack[p].Typ:=Item;
      ExtStack[p].StartPos:=CurPos.StartPos;
    end;
    Top:=Item;
  end;

  procedure Pop;
  begin
    dec(Ptr);
    if Ptr<0 then
      Top:=siNone
    else if Ptr<=High(Stack) then
      Top:=Stack[Ptr].Typ
    else
      Top:=ExtStack[Ptr-High(Stack)-1].Typ;
  end;

  function GetTopPos: integer;
  begin
    if Ptr<0 then
      Result:=0
    else if Ptr<=High(Stack) then
      Result:=Stack[Ptr].StartPos
    else
      Result:=ExtStack[Ptr-High(Stack)-1].StartPos;
  end;

  procedure Unexpected;
  var
    p: LongInt;
    Msg: String;
  begin
    ReadTilBracketCloseOrUnexpected:=false;
    if not ExceptionOnNotFound then exit;
    // the unexpected keyword is wrong, but probably the closing bracket is
    // missing and the method has read too far
    p:=GetTopPos;
    CleanPosToCaret(p,ErrorNicePosition);
    case Top of
    siNone: Msg:='closing bracket not found';
    siRoundBracketOpen: Msg:='bracket ) not found';
    siEdgedBracketOpen: Msg:='bracket ] not found';
    siRecord: Msg:='record end not found';
    end;
    if CurPos.StartPos<=SrcLen then
      Msg:=Msg+', found unexpected '+GetAtom
        +' at '+CleanPosToRelativeStr(CurPos.StartPos,ErrorNicePosition);
    SaveRaiseException(Msg,not CleanPosToCaret(p,ErrorNicePosition));
  end;

begin
  Result:=true;
  Ptr:=-1;
  ExtStack:=nil;
  if CurPos.Flag=cafRoundBracketOpen then
    Push(siRoundBracketOpen)
  else if CurPos.Flag=cafEdgedBracketOpen then
    Push(siEdgedBracketOpen)
  else
    RaiseBracketOpenExpectedButAtomFound;
  try
    repeat
      ReadNextAtom;
      //debugln(['TPascalParserTool.ReadTilBracketCloseOrUnexpected ',GetAtom]);
      case CurPos.Flag of

      cafNone:
        if CurPos.StartPos>SrcLen then Unexpected;

      cafSemicolon:
        if sbcStopOnSemicolon in Flags then Unexpected;

      cafRoundBracketOpen:
        Push(siRoundBracketOpen);

      cafRoundBracketClose:
        if Top=siRoundBracketOpen then begin
          if Ptr=0 then exit(true);
          Pop;
        end else
          Unexpected;

      cafEdgedBracketOpen:
        Push(siEdgedBracketOpen);

      cafEdgedBracketClose:
        if Top=siEdgedBracketOpen then begin
          if Ptr=0 then exit(true);
          Pop;
        end else
          Unexpected;

      cafWord:
        begin
          p:=@Src[CurPos.StartPos];
          case UpChars[p^] of
          'A':
            case UpChars[p[1]] of
            'S': if UpAtomIs('ASM') then Unexpected;
            end;
          'B':
            case UpChars[p[1]] of
            'E': if UpAtomIs('BEGIN') then Unexpected;
            end;
          'C':
            case UpChars[p[1]] of
            'O': if UpAtomIs('CONST') then Unexpected;
            end;
          'D':
            case UpChars[p[1]] of
            'O': if UpAtomIs('DO') then Unexpected;
            end;
          'E':
            if UpAtomIs('END') then begin
              if Top=siRecord then
                Pop
              else
                Unexpected;
            end;
          'I':
            case UpChars[p[1]] of
            'N':
              case UpChars[p[2]] of
              'I': if UpAtomIs('INITIALIZATION') then Unexpected;
              'T': if UpAtomIs('INTERFACE') then Unexpected;
              end;
            'M': if UpAtomIs('IMPLEMENTATION') then Unexpected;
            end;
          'F':
            case UpChars[p[1]] of
            'I':
              if UpAtomIs('FINALIZATION')
              or UpAtomIs('FINALLY')
              then Unexpected;
            'O': if UpAtomIs('FOR') then Unexpected;
            end;
          'L':
            case UpChars[p[1]] of
            'A': if UpAtomIs('LABEL') then Unexpected;
            end;
          'P':
            case UpChars[p[1]] of
            'U':
              case UpChars[p[2]] of
              'B':
                if UpAtomIs('PUBLIC')
                or UpAtomIs('PUBLISHED') then Unexpected;
              end;
            'R':
              case UpChars[p[2]] of
              'I': if UpAtomIs('PRIVATE') then Unexpected;
              'O': if UpAtomIs('PROTECTED') then Unexpected;
              end;
            end;
          'R':
            case UpChars[p[1]] of
            'E':
              case UpChars[p[2]] of
              'C':
                if UpAtomIs('RECORD') then begin
                  if sbcStopOnRecord in Flags then
                    Unexpected
                  else
                    Push(siRecord);
                end;
              'P': if UpAtomIs('REPEAT') then Unexpected;
              'S': if UpAtomIs('RESOURCESTRING') then Unexpected;
              end;
            end;
          'T':
            case UpChars[p[1]] of
            'R': if UpAtomIs('TRY') then Unexpected;
            end;
          'V':
            case UpChars[p[1]] of
            'A': if UpAtomIs('VAR') then Unexpected;
            end;
          'W':
            case UpChars[p[1]] of
            'H': if UpAtomIs('WHILE') then Unexpected;
            end;
          end;
        end;
      end;
    until false;
  finally
    if ExtStack<>nil then FreeMem(ExtStack);
  end;
end;

function TPascalParserTool.KeyWordFuncClassProperty: boolean;
{ parse class/object property

 examples:
   property Visible;
   property Count: integer;
   property Color: TColor read FColor write SetColor;
   property Items[Index1, Index2: integer]: integer read GetItems; default;
   property X: integer index 1 read GetCoords write SetCoords stored IsStored; deprecated;
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8, IColor;
   property Value: Integer read FCurrent; enumerator Current;
   property Visible: WordBool readonly dispid 401;

   property specifiers before semicolon:
     index <id or number>, read <id>, write <id>, stored <id>, default <constant>,
     implements <id>[,<id>...], nodefault
   for dispinterfaces:
     dispid <number>, readonly, writeonly
   property modifiers after semicolon:
     default, deprecated, enumerator <id>
}

  procedure RaiseSemicolonAfterPropSpecMissing(const s: string);
  begin
    SaveRaiseExceptionFmt(ctsSemicolonAfterPropSpecMissing,[s,GetAtom]);
  end;

begin
  if not (CurNode.Desc in (AllClassBaseSections+AllClassInterfaces)) then
    RaiseIdentExpectedButAtomFound;
  // create class method node
  CreateChildNode;
  CurNode.Desc:=ctnProperty;
  // read property Name
  if UpAtomIs('CLASS') then begin
    ReadNextAtom;
    if not UpAtomIs('PROPERTY') then
      RaiseStringExpectedButAtomFound('property');
  end;
  ReadNextAtom;
  AtomIsIdentifier(true);
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  while (CurPos.StartPos<=SrcLen) do begin
    case CurPos.Flag of
    cafSemicolon: break;
    cafEnd:       break;
    cafWord:  if WordIsPropertyEnd then break;
    end;
    ReadNextAtom;
  end;
  if CurPos.Flag=cafSemicolon then begin
    // read modifiers
    ReadNextAtom;
    if UpAtomIs('DEFAULT') then begin
      ReadNextAtom;
      if CurPos.Flag<>cafSemicolon then
        RaiseSemicolonAfterPropSpecMissing('default');
    end else if UpAtomIs('NODEFAULT') then begin
      ReadNextAtom;
      if CurPos.Flag<>cafSemicolon then
        RaiseSemicolonAfterPropSpecMissing('nodefault');
    end else if UpAtomIs('ENUMERATOR') then begin
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
      if CurPos.Flag<>cafSemicolon then
        RaiseSemicolonAfterPropSpecMissing('enumerator');
    end else
      UndoReadNextAtom;

    if CurPos.Flag=cafSemicolon then begin
      // read hint directives
      ReadNextAtom;
      if UpAtomIs('DEPRECATED') then begin
        ReadNextAtom;
        if AtomIsStringConstant then
          ReadConstant(true,false,[]);
        if CurPos.Flag<>cafSemicolon then
          RaiseSemicolonAfterPropSpecMissing('deprecated');
      end else if UpAtomIs('PLATFORM') or UpAtomIs('UNIMPLEMENTED')
        or UpAtomIs('EXPERIMENTAL')
      then begin
        ReadNextAtom;
        if CurPos.Flag<>cafSemicolon then
          RaiseSemicolonAfterPropSpecMissing('hint directive');
      end else
        UndoReadNextAtom;
    end;

  end else
    UndoReadNextAtom;
  // close property
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.DoAtom: boolean;
begin
  //DebugLn('[TPascalParserTool.DoAtom] A ',DbgS(CurKeyWordFuncList));
  if (CurPos.StartPos<=SrcLen) and (CurPos.EndPos>CurPos.StartPos) then begin
    if IsIdentStartChar[Src[CurPos.StartPos]] then
      Result:=KeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
                                      CurPos.EndPos-CurPos.StartPos)
    else begin
      if Src[CurPos.StartPos] in ['(','['] then
        ReadTilBracketClose(true);
      Result:=true;
    end;
  end else
    Result:=false;
end;

function TPascalParserTool.KeyWordFuncSection: boolean;
// parse section keywords (program, unit, interface, implementation, ...)

  procedure RaiseUnexpectedSectionKeyWord;
  begin
    SaveRaiseExceptionFmt(ctsUnknownSectionKeyword,[GetAtom]);
  end;

begin
  if UpAtomIs('IMPLEMENTATION') then begin
    if not (CurSection in [ctnInterface,ctnUnit,ctnLibrary,ctnPackage]) then
      RaiseUnexpectedSectionKeyWord;
    // close section node
    CurNode.EndPos:=CurPos.StartPos;
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
  end else if (UpAtomIs('INITIALIZATION') or UpAtomIs('FINALIZATION')) then
  begin
    if UpAtomIs('INITIALIZATION')
    and (not CurSection in [ctnInterface,ctnImplementation,
                            ctnUnit,ctnLibrary,ctnPackage])
    then
      RaiseUnexpectedSectionKeyWord;
    if UpAtomIs('FINALIZATION')
    and (not CurSection in [ctnInterface,ctnImplementation,ctnInitialization,
                            ctnUnit,ctnLibrary,ctnPackage])
    then
      RaiseUnexpectedSectionKeyWord;
    // close section node
    CurNode.EndPos:=CurPos.StartPos;
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
      end else if EndKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then
      begin
        ReadTilBlockEnd(false,false);
      end else if CurPos.Flag=cafEND then begin
        Result:=KeyWordFuncEndPoint;
        break;
      end;
    until (CurPos.StartPos>SrcLen);
    Result:=true;
  end else begin
    RaiseUnexpectedSectionKeyWord;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncEndPoint: boolean;
// keyword 'end' or '.'  (source end.)
var
  LastNodeEnd: LongInt;
begin
  if CurPos.Flag=cafPoint then begin
    if not LastUpAtomIs(0,'END') then
      RaiseIllegalQualifier;
    UndoReadNextAtom;
    if CurNode.Desc in [ctnInterface] then
      RaiseStringExpectedButAtomFound('"implementation"');
    if not (CurNode.Desc in [ctnImplementation,ctnInitialization,
      ctnFinalization,ctnProgram,ctnLibrary])
    then begin
      ReadNextAtom;
      SaveRaiseException(ctsUnexpectedEndOfSource+' 1');
    end;
  end else if CurPos.Flag=cafEND then begin
    if LastAtomIs(0,'@') then
      RaiseStringExpectedButAtomFound(ctsIdentifier);
    if LastAtomIs(0,'@@') then begin
      // for Delphi compatibility @@end is allowed
      Result:=true;
      exit;
    end;
  end else
    SaveRaiseException('[TPascalParserTool.KeyWordFuncEndPoint] internal error');
  if CurNode.Desc in [ctnBeginBlock] then
    CurNode.EndPos:=CurPos.EndPos
  else
    CurNode.EndPos:=CurPos.StartPos;
  LastNodeEnd:=CurNode.EndPos;
  EndChildNode;
  CreateChildNode;
  CurNode.Desc:=ctnEndPoint;
  CurNode.StartPos:=LastNodeEnd;
  ReadNextAtom;
  if CurPos.Flag<>cafPoint then
    RaiseCharExpectedButAtomFound('.');
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  CurSection:=ctnNone;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncProc: boolean;
// procedure, function, constructor, destructor, operator
var ChildCreated: boolean;
  IsFunction, HasForwardModifier, IsClassProc, IsOperator: boolean;
  ProcNode: TCodeTreeNode;
  ParseAttr: TParseProcHeadAttributes;
begin
  if UpAtomIs('CLASS') then begin
    if not (CurSection in [ctnImplementation]+AllSourceTypes) then
      RaiseStringExpectedButAtomFound(ctsIdentifier);
    ReadNextAtom;
    if UpAtomIs('PROCEDURE') or UpAtomIs('FUNCTION') or UpAtomIs('CONSTRUCTOR') or UpAtomIs('DESTRUCTOR') then
      IsClassProc:=true
    else
      RaiseStringExpectedButAtomFound(ctsProcedureOrFunctionOrConstructorOrDestructor);
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
  IsOperator:=UpAtomIs('OPERATOR');
  ReadNextAtom;// read first atom of head (= name/operator + parameterlist + resulttype;)
  if not IsOperator then AtomIsIdentifier(true);
  if ChildCreated then begin
    // create node for procedure head
    CreateChildNode;
    CurNode.Desc:=ctnProcedureHead;
    CurNode.SubDesc:=ctnsNeedJITParsing;
  end;
  ReadNextAtom;
  if (CurSection<>ctnInterface) and (CurPos.Flag=cafPoint) and (not IsOperator)
  then begin
    // read procedure name of a class method (the name after the . )
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  // read rest of procedure head
  HasForwardModifier:=false;
  ParseAttr:=[];
  if IsFunction then Include(ParseAttr,pphIsFunction);
  if IsOperator then Include(ParseAttr,pphIsOperator);
  ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
  if ChildCreated then begin
    if HasForwardModifier then
      ProcNode.SubDesc:=ctnsForwardDeclaration;
    // close head
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if ChildCreated and ((ProcNode.SubDesc and ctnsForwardDeclaration)>0) then
  begin
    // close method
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  Result:=true;
end;

function TPascalParserTool.ReadTilBlockEnd(
  StopOnBlockMiddlePart, CreateNodes: boolean): boolean;
// after reading cursor will be on the keyword ending the block (e.g. 'end')
var BlockType: TEndBlockType;
  TryType: TTryType;
  BlockStartPos: integer;
  Desc: TCodeTreeNodeDesc;

  procedure SaveRaiseExceptionWithBlockStartHint(const AMessage: string);
  var CaretXY: TCodeXYPosition;
  begin
    if (CleanPosToCaret(BlockStartPos,CaretXY))
    and (CaretXY.Code<>nil) then begin
      if CaretXY.Code=TCodeBuffer(Scanner.MainCode) then
        SaveRaiseException(AMessage+ctsPointStartAt
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')')
      else
        SaveRaiseException(AMessage+ctsPointStartAt
          +TCodeBuffer(CaretXY.Code).Filename
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')');
    end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
      SaveRaiseException(AMessage);
    end;
  end;
  
  procedure RaiseUnknownBlockType;
  begin
    SaveRaiseException('internal codetool error in '
      +'TPascalParserTool.ReadTilBlockEnd: unknown block type: '+GetAtom);
  end;
  
  procedure RaiseStrExpectedWithBlockStartHint(const Msg: string);
  begin
    SaveRaiseExceptionWithBlockStartHint(
      Format(ctsStrExpectedButAtomFound,[Msg,GetAtom]));
  end;
  
  procedure RaiseUnexpectedKeywordInAsmBlock;
  begin
    SaveRaiseExceptionFmt(ctsUnexpectedKeywordInAsmBlock,[GetAtom]);
  end;
  
  procedure RaiseUnexpectedKeyWordInBeginEndBlock;
  begin
    SaveRaiseExceptionWithBlockStartHint(
      Format(ctsUnexpectedKeywordInBeginEndBlock,[GetAtom]));
  end;
  
begin
  Result:=true;
  TryType:=ttNone;
  Desc:=ctnNone;
  if UpAtomIs('BEGIN') then begin
    BlockType:=ebtBegin;
    Desc:=ctnBeginBlock;
  end else if UpAtomIs('REPEAT') then
    BlockType:=ebtRepeat
  else if UpAtomIs('TRY') then
    BlockType:=ebtTry
  else if UpAtomIs('CASE') then
    BlockType:=ebtCase
  else if UpAtomIs('ASM') then
    BlockType:=ebtAsm
  else if UpAtomIs('RECORD') then
    BlockType:=ebtRecord
  else
    RaiseUnknownBlockType;
  if (Desc<>ctnNone) then begin
    if CreateNodes then begin
      CreateChildNode;
      CurNode.Desc:=Desc;
    end else
      Desc:=ctnNone;
  end;
  BlockStartPos:=CurPos.StartPos;
  repeat
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) then
      SaveRaiseExceptionWithBlockStartHint(ctsUnexpectedEndOfSource);
      
    if not (CurPos.Flag in AllCommonAtomWords) then continue;
    
    if (CurPos.Flag=cafEND) then begin
      if (BlockType<>ebtAsm) or (CurPos.StartPos=1) or (Src[CurPos.StartPos-1]<>'@')
      then begin
        if BlockType=ebtRepeat then
          RaiseStrExpectedWithBlockStartHint('"until"');
        if (BlockType=ebtTry) and (TryType=ttNone) then
          RaiseStrExpectedWithBlockStartHint('"finally"');
        if Desc<>ctnNone then begin
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
        end;
        ReadNextAtom;
        if (CurPos.Flag=cafPoint) and (BlockType<>ebtBegin) then begin
          RaiseCharExpectedButAtomFound(';');
        end;
        UndoReadNextAtom;
        break;
      end;
    end else if EndKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos)
      or UpAtomIs('REPEAT') then
    begin
      if BlockType=ebtAsm then
        RaiseUnexpectedKeywordInAsmBlock;
      if (BlockType<>ebtRecord) or (not UpAtomIs('CASE')) then
        ReadTilBlockEnd(false,CreateNodes);
    end else if UpAtomIs('UNTIL') then begin
      if BlockType<>ebtRepeat then
        RaiseStrExpectedWithBlockStartHint('"end"');
      if Desc<>ctnNone then begin
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode;
      end;
      break;
    end else if UpAtomIs('FINALLY') then begin
      if (BlockType=ebtTry) and (TryType=ttNone) then begin
        if StopOnBlockMiddlePart then break;
        TryType:=ttFinally;
      end else
        RaiseStrExpectedWithBlockStartHint('"end"');
    end else if UpAtomIs('EXCEPT') then begin
      if (BlockType=ebtTry) and (TryType=ttNone) then begin
        if StopOnBlockMiddlePart then break;
        TryType:=ttExcept;
      end else
        RaiseStrExpectedWithBlockStartHint('"end"');
    end else if CreateNodes and UpAtomIs('WITH') then begin
      ReadWithStatement(true,CreateNodes);
    end else if CreateNodes and UpAtomIs('ON') and (BlockType=ebtTry)
    and (TryType=ttExcept) then begin
      ReadOnStatement(true,CreateNodes);
    end else begin
      // check for unexpected keywords
      case BlockType of
      
      ebtBegin,ebtTry,ebtCase,ebtRepeat:
        if UnexpectedKeyWordInBeginBlock.DoItCaseInsensitive(Src,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
        then
          RaiseUnexpectedKeyWordInBeginEndBlock;
          
      ebtAsm:
        if UnexpectedKeyWordInAsmBlock.DoItCaseInsensitive(Src,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
        then
          RaiseUnexpectedKeyWordInBeginEndBlock;

      end;
    end;
  until false;
end;

function TPascalParserTool.ReadTilBlockStatementEnd(
  ExceptionOnNotFound: boolean): boolean;
begin
  if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
    Result:=ReadTilBracketClose(ExceptionOnNotFound)
  else if WordIsBlockStatementStart.DoItCaseInsensitive(Src,
    CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
  then
    Result:=ReadTilBlockEnd(false,false)
  else
    Result:=false;
end;

function TPascalParserTool.ReadBackTilBlockEnd(
  StopOnBlockMiddlePart: boolean): boolean;
// read begin..end, try..finally, case..end, repeat..until, asm..end blocks
// backwards
var BlockType: TEndBlockType;

  procedure RaiseBlockError;
  begin
    case BlockType of
      ebtBegin:
        SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,['"begin"',GetAtom]);
      ebtTry:
        SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,['"try"',GetAtom]);
      ebtRepeat:
        SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,['"repeat"',GetAtom]);
    else
      SaveRaiseExceptionFmt(ctsUnexpectedKeywordWhileReadingBackwards,[GetAtom]);
    end;
  end;
  
  procedure RaiseUnknownBlockType;
  begin
    SaveRaiseException('internal codetool error in '
      +'TPascalParserTool.ReadBackTilBlockEnd: unknown block type: '+GetAtom);
  end;

var OldAtom: TAtomPosition;
begin
  Result:=true;
  if CurPos.Flag=cafEND then
    BlockType:=ebtBegin
  else if UpAtomIs('UNTIL') then
    BlockType:=ebtRepeat
  else if UpAtomIs('FINALLY') or UpAtomIs('EXCEPT') then
    BlockType:=ebtTry
  else
    RaiseUnknownBlockType;
  repeat
    ReadPriorAtom;
    if (CurPos.StartPos<1) then begin
      SaveRaiseExceptionFmt(ctsWordNotFound,['begin']);
    end else if WordIsBlockKeyWord.DoItCaseInsensitive(Src,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then
    begin
      if (CurPos.Flag=cafEND) or (UpAtomIs('UNTIL')) then begin
        ReadBackTilBlockEnd(false);
      end else if UpAtomIs('BEGIN') or UpAtomIs('RECORD') or UpAtomIs('ASM')
      then begin
        if BlockType=ebtBegin then
          break
        else
          RaiseBlockError;
      end else if UpAtomIs('OBJECT') then begin
        if BlockType=ebtBegin then begin
          // could also be 'of object'
          ReadPriorAtom;
          if not UpAtomIs('OF') then begin
            CurPos:=NextPos;
            NextPos.StartPos:=-1;
            break;
          end;
        end else
          RaiseBlockError;
      end else if UpAtomIs('CLASS') then begin
        ReadNextAtom;
        if UpAtomIs('FUNCTION') or UpAtomIs('PROCEDURE')
        or (CurPos.Flag=cafSemicolon) or UpAtomIs('OF') then
          UndoReadNextAtom
        else begin
          UndoReadNextAtom;
          break;
        end;
      end else if UpAtomIs('CASE') then begin
        // case could also be in a record, then it should not close the block
        if BlockType=ebtBegin then begin
          // check if case in a record
          OldAtom:=CurPos;
          repeat
            ReadPriorAtom;
            if WordIsBlockKeyWord.DoItCaseInsensitive(Src,CurPos.StartPos,
              CurPos.EndPos-CurPos.StartPos) then
            begin
              if UpAtomIs('CASE') then begin
                // could be another variant record, -> read further ...
              end else if UpAtomIs('RECORD') then begin
                // record start found -> the case is a variant record
                // block start found
                break;
              end else begin
                // this is not a variant record
                MoveCursorToCleanPos(OldAtom.StartPos);
                ReadNextAtom;
                break;
              end;
            end;
          until (CurPos.StartPos<1);
          break;
        end else
          RaiseBlockError;
      end else if UpAtomIs('REPEAT') then begin
        if BlockType=ebtRepeat then
          break
        else
          RaiseBlockError;
      end else if UpAtomIs('FINALLY') or UpAtomIs('EXCEPT') then begin
        if BlockType=ebtBegin then begin
          if StopOnBlockMiddlePart then break;
          BlockType:=ebtTry;
        end else
          RaiseBlockError;
      end else if UpAtomIs('TRY') then begin
        if BlockType=ebtTry then
          break
        else
          RaiseBlockError;
      end;
    end;
  until false;
end;

function TPascalParserTool.ReadTilVariableEnd(
  ExceptionOnError, WithAsOperator: boolean): boolean;
{ Examples:
    A
    A.B^.C[...].D(...).E
    (...).A
    @B
    inherited A
    A as B
}
begin
  while AtomIsChar('@') do
    ReadNextAtom;
  while UpAtomIs('INHERITED') do
    ReadNextAtom;
  Result:=AtomIsIdentifier(false)
          or (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]);
  if not Result then exit;
  repeat
    if AtomIsIdentifier(false) then
      ReadNextAtom;
    repeat
      if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then begin
        Result:=ReadTilBracketClose(ExceptionOnError);
        if not Result then exit;
        ReadNextAtom;
      end else if AtomIsChar('^') then begin
        ReadNextAtom;
      end else
        break;
    until false;
    if (CurPos.Flag=cafPoint)
    or (WithAsOperator and UpAtomIs('AS')) then
      ReadNextAtom
    else
      break;
  until false;
end;

function TPascalParserTool.ReadTilStatementEnd(ExceptionOnError,
  CreateNodes: boolean): boolean;
// after reading the current atom will be on the last atom of the statement
begin
  Result:=true;
  repeat
    if BlockStatementStartKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
          CurPos.EndPos-CurPos.StartPos) then
    begin
      if not ReadTilBlockEnd(false,CreateNodes) then exit(false);
      ReadNextAtom;
      if CurPos.Flag<>cafSemicolon then UndoReadNextAtom;
      exit;
    end else if UpAtomIs('WITH') then begin
      Result:=ReadWithStatement(ExceptionOnError,CreateNodes);
      exit;
    end else begin
      case CurPos.Flag of
      cafEND:
        begin
          UndoReadNextAtom;
          exit;
        end;
      cafSemicolon: exit;
      else
        if CurPos.StartPos>SrcLen then exit;
        ReadNextAtom;
      end;
    end;
  until false;
end;

function TPascalParserTool.ReadWithStatement(ExceptionOnError,
  CreateNodes: boolean): boolean;
  
  procedure CloseNodes;
  var WithVarNode: TCodeTreeNode;
    EndPos: LongInt;
  begin
    if CreateNodes then begin
      EndPos:=CurPos.EndPos;
      if CurNode.Desc=ctnWithStatement then begin
        if not (CurPos.Flag in [cafSemicolon,cafEnd]) then begin
          // the with statement is valid until the next atom
          // this is important for context when cursor is behind last atom of the
          // with statement, but in front of the next atom
          ReadNextAtom;
          EndPos:=CurPos.StartPos;
          UndoReadNextAtom;
        end;
        CurNode.EndPos:=EndPos;
        //DebugLn(['CloseNodes "',copy(Src,CurNode.StartPos,CurNode.EndPos-CurNode.STartPos),'"']);
        EndChildNode; // ctnWithStatement
      end;
      WithVarNode:=CurNode;
      CurNode.EndPos:=EndPos;
      EndChildNode; // ctnWithVariable
      // set all with variable ends
      repeat
        WithVarNode:=WithVarNode.PriorBrother;
        if (WithVarNode=nil) or (WithVarNode.Desc<>ctnWithVariable)
        or (WithVarNode.EndPos>0) then break;
        WithVarNode.EndPos:=EndPos;
      until false;
    end;
  end;
  
begin
  ReadNextAtom; // read start of variable
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnWithVariable;
  end;
  // read til the end of the variable
  if not ReadTilVariableEnd(ExceptionOnError,true) then begin
    CloseNodes;
    Result:=false;
    exit;
  end;
  // read all other variables
  while CurPos.Flag=cafComma do begin
    if CreateNodes then
      EndChildNode;
    ReadNextAtom;
    if CreateNodes then begin
      CreateChildNode;
      CurNode.Desc:=ctnWithVariable
    end;
    if not ReadTilVariableEnd(ExceptionOnError,true) then begin
      CloseNodes;
      Result:=false;
      exit;
    end;
  end;
  // read DO
  if not UpAtomIs('DO') then begin
    if ExceptionOnError then
      RaiseStringExpectedButAtomFound('"do"')
    else begin
      CloseNodes;
      Result:=false;
      exit;
    end;
  end;
  // read statement
  ReadNextAtom;
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnWithStatement;
  end;
  Result:=ReadTilStatementEnd(ExceptionOnError,CreateNodes);
  CloseNodes;
end;

function TPascalParserTool.ReadOnStatement(ExceptionOnError,
  CreateNodes: boolean): boolean;
// for example:
// on E: Exception do ;
// on Exception do ;
// on Unit.Exception do ;
begin
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnOnBlock;
  end;
  // read variable name
  ReadNextAtom;
  AtomIsIdentifier(true);
  if CreateNodes then begin
    // ctnOnIdentifier for the variable or the type
    CreateChildNode;
    CurNode.Desc:=ctnOnIdentifier;
    CurNode.EndPos:=CurPos.EndPos;
  end;
  ReadNextAtom;
  if CurPos.Flag=cafColon then begin
    // this is for example: on E: Exception do ;
    if CreateNodes then begin
      // close the variable
      EndChildNode;
    end;
    ReadNextAtom;
    AtomIsIdentifier(true);
    if CreateNodes then begin
      // ctnOnIdentifier for the type
      CreateChildNode;
      CurNode.Desc:=ctnOnIdentifier;
    end;
    ReadNextAtom;
  end;
  if CurPos.Flag=cafPoint then begin
    // this is for example: on Unit.Exception do ;
    ReadNextAtom;
    AtomIsIdentifier(true);
    if CreateNodes then begin
      CurNode.EndPos:=CurPos.EndPos;
    end;
    ReadNextAtom;
  end;
  if CreateNodes then begin
    // close the type
    EndChildNode;
  end;
  // read 'do'
  if not UpAtomIs('DO') then
    RaiseStringExpectedButAtomFound('DO');
  // ctnOnStatement
  if CreateNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnOnStatement;
  end;
  ReadTilStatementEnd(true,CreateNodes);
  if CreateNodes then begin
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode; // ctnOnStatement
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode; // ctnOnVariable
  end;
  Result:=true;
end;

procedure TPascalParserTool.ReadVariableType;
{ creates nodes for variable type

  examples:

    interface
      var a:b;
        a:b; cvar;
        a:b; public name 'string constant';
        a:b; public name <id>;
        a:b; external name 'string constant';
        a:b; cvar; external;
        a:b; external 'library' name 'avar';

    implementation

    procedure c;
    var d:e;
      f:g=h;
}
begin
  ReadNextAtom;
  ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
  if UpAtomIs('ABSOLUTE') then begin
    ReadNextAtom;
    ReadConstant(true,false,[]);
  end;
  if CurPos.Flag=cafEqual then begin
    // read constant
    repeat
      ReadNextAtom;
      if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
        ReadTilBracketClose(true);
      if (CurPos.Flag in AllCommonAtomWords)
      and (not IsKeyWordInConstAllowed.DoItCaseInsensitive(Src,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
      and AtomIsKeyWord
      then
        RaiseCharExpectedButAtomFound(';');
    until (CurPos.Flag=cafSemicolon) or (CurPos.StartPos>SrcLen);
  end;
  if UpAtomIs('DEPRECATED') then
    ReadNextAtom;
  // read ;
  if CurPos.Flag<>cafSemicolon then
    RaiseCharExpectedButAtomFound(';');
  ReadNextAtom;
  if UpAtomIs('CVAR') then begin
    // for example: 'var a: char; cvar;'
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
    ReadNextAtom;
  end;
  if UpAtomIs('STATIC') and (CurNode.Parent<>nil)
  and (CurNode.Parent.Desc in AllClassSections) then begin
    // 'static' is allowed for class variables
    // for example: 'a: char; static;'
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
    ReadNextAtom;
  end;
  if (CurNode.Parent.Desc=ctnVarSection)
  and (UpAtomIs('PUBLIC') or UpAtomIs('EXPORT') or UpAtomIs('EXTERNAL')) then
  begin
    // examples:
    //   a: b; public;
    //   a: b; external;
    //   a: b; external c;
    //   a: b; external name 'c';
    //   a: b; external 'library' name 'c';
    if UpAtomIs('EXTERNAL') then begin
      // read external identifier
      ReadNextAtom;
      if (CurPos.Flag<>cafSemicolon) and (not UpAtomIs('NAME')) then
        ReadConstant(true,false,[]); // library name
    end else
      ReadNextAtom;
    if UpAtomIs('NAME') then begin
      // for example 'var a: char; public name 'b' ;'
      // for example 'var a: char; public name test;'
      ReadNextAtom;
      if (not AtomIsStringConstant)
      and (not AtomIsIdentifier(false)) then
        RaiseStringExpectedButAtomFound(ctsStringConstant);
      ReadConstant(true,false,[]);
    end;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
  end else
    UndoReadNextAtom;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncBeginEnd: boolean;
// Keyword: begin, asm

  procedure SaveRaiseExceptionWithHint;
  var CaretXY: TCodeXYPosition;
    AMessage: string;
  begin
    AMessage:=Format(ctsStrExpectedButAtomFound,[';','.']);
    if (CleanPosToCaret(CurNode.StartPos,CaretXY))
    and (CaretXY.Code<>nil) then begin
      if CaretXY.Code=TCodeBuffer(Scanner.MainCode) then
        SaveRaiseException(AMessage+ctsPointHintProcStartAt
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')')
      else
        SaveRaiseException(AMessage+ctsPointHintProcStartAt
          +TCodeBuffer(CaretXY.Code).Filename
          +'('+IntToStr(CaretXY.Y)+','+IntToStr(CaretXY.X)+')');
    end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
      SaveRaiseException(AMessage);
    end;
  end;

var
  ChildNodeCreated: boolean;
begin
  //DebugLn('TPascalParserTool.KeyWordFuncBeginEnd CurNode=',CurNode.DescAsString);
  if (CurNode<>nil)
  and (not (CurNode.Desc in
    [ctnProcedure,ctnProgram,ctnLibrary,ctnImplementation]))
  then
    RaiseStringExpectedButAtomFound('end');
  ChildNodeCreated:=UpAtomIs('BEGIN') or UpAtomIs('ASM');
  if ChildNodeCreated then begin
    CreateChildNode;
    if UpAtomIs('BEGIN') then
      CurNode.Desc:=ctnBeginBlock
    else
      CurNode.Desc:=ctnAsmBlock;
    CurNode.SubDesc:=ctnsNeedJITParsing;
  end;
  // search "end"
  ReadTilBlockEnd(false,false);
  // close node
  if ChildNodeCreated then begin
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if (CurSection<>ctnInterface)
  and (CurNode<>nil) and (CurNode.Desc=ctnProcedure) then begin
    // close procedure
    CurNode.EndPos:=CurPos.EndPos;
    ReadNextAtom;
    if (CurPos.Flag=cafPoint) then
      SaveRaiseExceptionWithHint;
    UndoReadNextAtom;
    EndChildNode;
  end else if (CurNode.Desc in [ctnProgram,ctnLibrary,ctnImplementation]) then
  begin
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then
      SaveRaiseException(ctsMissingPointAfterEnd);
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
      type
        a=b;
        generic c<> = d;
      
    implementation
    
    procedure c;
    type d=e;
}

  function IsTypeName: boolean;
  begin
    Result:=false;
    if not AtomIsIdentifier(false) then exit;
    if (Scanner.CompilerMode in [cmOBJFPC,cmFPC]) then begin
      if UpAtomIs('PROPERTY') or UpAtomIs('OPERATOR') then
        exit;
    end;
    //debugln(['IsTypeName ',GetAtom,' ',CompilerModeNames[Scanner.CompilerMode]]);
    Result:=true;
  end;

begin
  if not (CurSection in [ctnProgram,ctnLibrary,ctnInterface,ctnImplementation])
  then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnTypeSection;
  // read all type definitions  Name = Type; or generic Name<List> = Type;
  repeat
    ReadNextAtom;  // name
    if UpAtomIs('GENERIC') then begin
      CreateChildNode;
      CurNode.Desc:=ctnGenericType;
      // read name
      ReadNextAtom;
      AtomIsIdentifier(true);
      CreateChildNode;
      CurNode.Desc:=ctnGenericName;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
      // read <
      ReadNextAtom;
      if not AtomIsChar('<') then
        RaiseCharExpectedButAtomFound('<');
      CreateChildNode;
      CurNode.Desc:=ctnGenericParams;
      // read parameter list
      ReadNextAtom;
      if AtomIsIdentifier(false) then begin
        repeat
          CreateChildNode;
          CurNode.Desc:=ctnGenericParameter;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
          // read name
          ReadNextAtom;
          if CurPos.Flag=cafComma then begin
            ReadNextAtom;
            AtomIsIdentifier(true);
          end else if AtomIsChar('>') then begin
            break;
          end else if AtomIs('>=') then begin
            // this is the rare case where >= are two separate atoms
            dec(CurPos.EndPos);
            break;
          end else
            RaiseCharExpectedButAtomFound('>');
        until false;
      end else begin
        if AtomIs('>=') then
          // this is the rare case where >= are two separate atoms
          dec(CurPos.EndPos);
        if not AtomIsChar('>') then
          RaiseCharExpectedButAtomFound('>');
      end;
      // close ctnGenericParams
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
      ReadEqualsType;
      // close ctnGenericType
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else if IsTypeName then begin
      CreateChildNode;
      CurNode.Desc:=ctnTypeDefinition;
      ReadEqualsType;
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
        a:b; public name 'string constant';
        a:b; public name <id>;
        a:b; external name 'string constant';
        a:b; cvar; external;
        a:b; external 'library' name 'avar';

    implementation

    procedure c;
    var d:e;
      f:g=h;
}
var
  LastIdentifierEnd: LongInt;
begin
  if not (CurSection in [ctnProgram,ctnLibrary,ctnInterface,ctnImplementation])
  then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnVarSection;
  // read all variable definitions  Name : Type; [cvar;] [public [name '']]
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false)
    and ((not (Scanner.CompilerMode in [cmOBJFPC,cmFPC]))
         or (not UpAtomIs('PROPERTY')))
    then begin
      CreateChildNode;
      CurNode.Desc:=ctnVarDefinition;
      LastIdentifierEnd:=CurPos.EndPos;
      ReadNextAtom;
      while (CurPos.Flag=cafComma) do begin
        CurNode.EndPos:=LastIdentifierEnd;
        EndChildNode; // close variable definition
        ReadNextAtom;
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        LastIdentifierEnd:=CurPos.EndPos;
        ReadNextAtom;
      end;
      if (CurPos.Flag<>cafColon) then begin
        RaiseCharExpectedButAtomFound(':');
      end;
      // read type
      ReadVariableType;
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
      ;
      c =4;

    implementation

    procedure c;
    const d=2;
}
begin
  if not (CurSection in [ctnProgram,ctnLibrary,ctnInterface,ctnImplementation])
  then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnConstSection;
  // read all constants  Name = <Const>; or Name : type = <Const>;
  repeat
    ReadNextAtom;  // name
    if CurPos.Flag=cafSemicolon then begin
      // ignore empty semicolons
    end else if AtomIsIdentifier(false)
    and ((not (Scanner.CompilerMode in [cmOBJFPC,cmFPC]))
         or (not UpAtomIs('PROPERTY')))
    then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      ReadConst;
      // close ctnConstDefinition node
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
  if not (CurSection in [ctnProgram,ctnLibrary,ctnInterface,ctnImplementation])
  then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnResStrSection;
  // read all string constants Name = 'abc';
  repeat
    ReadNextAtom;  // name
    if AtomIsIdentifier(false)
    and ((not (Scanner.CompilerMode in [cmOBJFPC,cmFPC]))
         or (not UpAtomIs('PROPERTY')))
    then begin
      CreateChildNode;
      CurNode.Desc:=ctnConstDefinition;
      ReadNextAtom;
      if (CurPos.Flag<>cafEqual) then
        RaiseCharExpectedButAtomFound('=');
      // read string constant
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseStringExpectedButAtomFound(ctsStringConstant);
      ReadConstant(true,false,[]);
      if UpAtomIs('DEPRECATED') then begin
        ReadNextAtom;
        if AtomIsStringConstant then ReadConstant(true,false,[]);
      end;
      // read ;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(';');
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

function TPascalParserTool.KeyWordFuncExports: boolean;
{ exports keyword  - only allowed in library

  examples:
  
  exports i, j index 3+4, k name 'StrConst', l index 0 name 's';
}

  procedure RaiseExportsOnlyAllowedInLibraries;
  begin
    SaveRaiseException(ctsExportsClauseOnlyAllowedInLibraries);
  end;

begin
  if not (CurSection in [ctnLibrary,ctnProgram]) then
    RaiseExportsOnlyAllowedInLibraries;
  CreateChildNode;
  CurNode.Desc:=ctnExportsSection;
  repeat
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if UpAtomIs('INDEX') then begin
      ReadNextAtom;
      ReadConstant(true,false,[]);
    end;
    if UpAtomIs('NAME') then begin
      ReadNextAtom;
      ReadConstant(true,false,[]);
    end;
    if (CurPos.Flag=cafSemicolon) then break;
    if (CurPos.Flag<>cafComma) then
      RaiseCharExpectedButAtomFound(';');
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncLabel: boolean;
{
  examples:
    label a, 23, b;
}
begin
  if not (CurSection in [ctnProgram,ctnLibrary,ctnInterface,ctnImplementation])
  then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnLabelSection;
  // read all constants
  repeat
    ReadNextAtom;  // identifier or number
    if (not AtomIsIdentifier(false)) and (not AtomIsNumber) then begin
      RaiseStringExpectedButAtomFound(ctsIdentifier);
    end;
    CreateChildNode;
    CurNode.Desc:=ctnLabelType;
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
    ReadNextAtom;
    if CurPos.Flag=cafSemicolon then begin
      break;
    end else if (CurPos.Flag<>cafComma) then begin
      RaiseCharExpectedButAtomFound(';');
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncProperty: boolean;
{
  examples:
   property
     errno : cint read fpgeterrno write fpseterrno;
     A2 : Integer Read GetA2 Write SetA2;
}
begin
  if not (CurSection in [ctnProgram,ctnLibrary,ctnInterface,ctnImplementation])
  then
    RaiseUnexpectedKeyWord;
  CreateChildNode;
  CurNode.Desc:=ctnPropertySection;
  // read all global properties
  repeat
    // read property Name
    ReadNextAtom;
    if AtomIsIdentifier(false) then begin
      CreateChildNode;
      CurNode.Desc:=ctnGlobalProperty;
      ReadNextAtom;
      if CurPos.Flag=cafEdgedBracketOpen then begin
        // read parameter list
        ReadTilBracketClose(true);
        ReadNextAtom;
      end;
      while (CurPos.StartPos<=SrcLen) and (CurPos.Flag<>cafSemicolon) do
        ReadNextAtom;
      // close global property
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end else begin
      UndoReadNextAtom;
      break;
    end;
  until CurPos.StartPos>SrcLen;
  // close property section
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

procedure TPascalParserTool.ReadConst;
begin
  ReadNextAtom;
  if (CurPos.Flag=cafColon) then begin
    // read type
    ReadNextAtom;
    ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
  end;
  if (CurPos.Flag<>cafEqual) then
    RaiseCharExpectedButAtomFound('=');
  // read constant
  ReadNextAtom;
  CreateChildNode;
  CurNode.Desc:=ctnConstant;
  repeat
    if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
      ReadTilBracketClose(true);
    if (CurPos.Flag in AllCommonAtomWords)
    and (not IsKeyWordInConstAllowed.DoItCaseInsensitive(Src,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos))
    and AtomIsKeyWord then
      RaiseStringExpectedButAtomFound('constant');
    if (CurPos.Flag=cafSemicolon) then break;
    CurNode.EndPos:=CurPos.EndPos;
    ReadNextAtom;
  until (CurPos.StartPos>SrcLen);
  // close ctnConstant node
  EndChildNode;
end;

procedure TPascalParserTool.ReadEqualsType;
// read   = type;
begin
  // read =
  ReadNextAtom;
  if (CurPos.Flag<>cafEqual) then
    RaiseCharExpectedButAtomFound('=');
  // read type
  ReadNextAtom;
  ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
  // read ;
  if CurPos.Flag<>cafSemicolon then
    RaiseCharExpectedButAtomFound(';');
end;

function TPascalParserTool.KeyWordFuncTypePacked: boolean;
begin
  ReadNextAtom;
  if not PackedTypesKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos)
  then
    RaiseStringExpectedButAtomFound('"record"');
  Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncTypeBitPacked: boolean;
begin
  ReadNextAtom;
  if not BitPackedTypesKeyWordFuncList.DoItCaseInsensitive(Src,CurPos.StartPos,
    CurPos.EndPos-CurPos.StartPos)
  then
    RaiseStringExpectedButAtomFound('"array"');
  Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TPascalParserTool.KeyWordFuncSpecialize: boolean;
begin
  ReadSpecialize(true);
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClass: boolean;
// class, object
//   this is a quick parser, which will only create one node for each class
//   the nodes for the methods and properties are created in a second
//   parsing phase (in KeyWordFuncClassMethod)
var
  BracketLvl: Integer;
  ChildCreated: boolean;
  ClassAtomPos: TAtomPosition;
  Level: integer;
  ContextDesc: Word;
  IsForward: Boolean;
  p: PChar;
  ClassDesc: TCodeTreeNodeDesc;
begin
  ContextDesc:=CurNode.Desc;
  if not (ContextDesc in [ctnTypeDefinition,ctnGenericType,
    ctnVarDefinition,ctnConstDefinition])
  then
    SaveRaiseExceptionFmt(ctsAnonymDefinitionsAreNotAllowed,['class']);
  if CurNode.Parent.Desc<>ctnTypeSection then
    SaveRaiseExceptionFmt(ctsNestedDefinitionsAreNotAllowed,['class']);
  if LastUpAtomIs(0,'PACKED') or LastUpAtomIs(0,'BITPACKED') then begin
    ClassAtomPos:=LastAtoms.GetValueAt(0);
  end else begin
    ClassAtomPos:=CurPos;
  end;
  // class or 'class of' start found
  if UpAtomIs('CLASS') then
    ClassDesc:=ctnClass
  else if UpAtomIs('OBJECT') then
    ClassDesc:=ctnObject
  else if UpAtomIs('OBJCCLASS') then
    ClassDesc:=ctnObjCClass
  else if UpAtomIs('OBJCCATEGORY') then
    ClassDesc:=ctnObjCCategory
  else if UpAtomIs('CPPCLASS') then
    ClassDesc:=ctnCPPClass
  else
    ClassDesc:=ctnNone;
  ChildCreated:=ClassDesc<>ctnNone;
  if ChildCreated then begin
    CreateChildNode;
    CurNode.Desc:=ClassDesc;
    CurNode.StartPos:=ClassAtomPos.StartPos;
    CurNode.SubDesc:=CurNode.SubDesc+ctnsNeedJITParsing; // will not create sub nodes now
  end;
  // find end of class
  IsForward:=true;
  ReadNextAtom;
  if UpAtomIs('OF') then begin
    IsForward:=false;
    if ChildCreated then begin
      CurNode.Desc:=ctnClassOfType;
      CurNode.SubDesc:=CurNode.SubDesc-ctnsNeedJITParsing;
    end;
    ReadNextAtom;
    AtomIsIdentifier(true);
    if ChildCreated then begin
      CreateChildNode;
      CurNode.Desc:=ctnIdentifier;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode;
    end;
    ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
  end else if not (ContextDesc in [ctnTypeDefinition,ctnGenericType]) then begin
    MoveCursorToNodeStart(CurNode);
    SaveRaiseExceptionFmt(ctsAnonymDefinitionsAreNotAllowed,['class']);
  end else begin
    if UpAtomIs('SEALED') then begin
      while UpAtomIs('SEALED') do
        ReadNextAtom;
    end else if UpAtomIs('ABSTRACT') then begin
      IsForward:=false;
      while UpAtomIs('ABSTRACT') do
        ReadNextAtom;
    end;
    if (CurPos.Flag=cafRoundBracketOpen) then begin
      // read inheritage brackets
      IsForward:=false;
      ReadTilBracketCloseOrUnexpected(true,[sbcStopOnSemicolon,sbcStopOnRecord]);
      ReadNextAtom;
    end;
  end;
  if CurPos.Flag=cafSemicolon then begin
    if ChildCreated and (ClassDesc in AllClassObjects) then
    begin
      if IsForward then begin
        // forward class definition found
        CurNode.SubDesc:=CurNode.SubDesc+ctnsForwardDeclaration-ctnsNeedJITParsing;
      end else begin
        // very short class found e.g. = class(TAncestor);
      end;
    end;
  end else begin
    // read til end or any suspicious keyword
    Level:=1;
    BracketLvl:=0;
    while (CurPos.StartPos<=SrcLen) do begin
      case CurPos.Flag of
      cafEND:
        begin
          dec(Level);
          if Level=0 then break;
        end;
      cafRoundBracketOpen,cafEdgedBracketOpen:
        inc(BracketLvl);
      cafRoundBracketClose,cafEdgedBracketClose:
        dec(BracketLvl);
      cafEqual:
        ; // Note: this is allowed: function a=b;
      cafWord:
        begin
          p:=@Src[CurPos.StartPos];
          case UpChars[p^] of
          'B':
            if CompareSrcIdentifiers(p,'BEGIN') then
              SaveRaiseException(ctsEndForClassNotFound);
          'C':
            if CompareSrcIdentifiers(p,'CONST') then begin
              if BracketLvl>0 then begin
                if (not (LastAtomIs(0,';') or LastAtomIs(0,'(')
                or LastAtomIs(0,'[')  or LastUpAtomIs(0,'OF'))) then
                  SaveRaiseException(ctsEndForClassNotFound);
              end;
            end;
          'I':
            if CompareSrcIdentifiers(p,'INTERFACE')
            or CompareSrcIdentifiers(p,'IMPLEMENTATION') then
              SaveRaiseException(ctsEndForClassNotFound);
          'O':
            if CompareSrcIdentifiers(p,'OBJECT') then
              inc(Level);
          'R':
            if CompareSrcIdentifiers(p,'RECORD') then
              inc(Level)
            else if CompareSrcIdentifiers(p,'RESOURCESTRING') then
              SaveRaiseException(ctsEndForClassNotFound);
          'T':
            if CompareSrcIdentifiers(p,'THREADVAR') then
              SaveRaiseException(ctsEndForClassNotFound)
            else if CompareSrcIdentifiers(p,'TYPE')
            and (BracketLvl>0) then
              SaveRaiseException(ctsEndForClassNotFound);
          'V':
            if CompareSrcIdentifiers(p,'VAR') then begin
              if BracketLvl>0 then begin
                if (not (LastAtomIs(0,';') or LastAtomIs(0,'(')
                or LastAtomIs(0,'[')  or LastUpAtomIs(0,'OF'))) then
                  SaveRaiseException(ctsEndForClassNotFound);
              end;
            end;
          end;
        end;
      end;
      ReadNextAtom;
    end;
    if (CurPos.StartPos>SrcLen) then
      SaveRaiseException(ctsEndForClassNotFound);
  end;
  if CurPos.Flag=cafEND then begin
    // read extra flags
    ReadNextAtom;
    if CurPos.Flag=cafSemicolon then
      ReadNextAtom;
    if UpAtomIs('DEPRECATED') then begin
      ReadNextAtom;
      if AtomIsStringConstant then
        ReadConstant(true,false,[]);
    end else if UpAtomIs('PLATFORM')
      or UpAtomIs('UNIMPLEMENTED') or UpAtomIs('EXPERIMENTAL')
      or UpAtomIs('LIBRARY')
    then
      ReadNextAtom;
    if CurPos.Flag=cafSemicolon then
      ReadNextAtom;
    if UpAtomIs('EXTERNAL') then begin
      ReadNextAtom;
      if UpAtomIs('NAME') then begin
        ReadNextAtom;
        ReadConstant(true,false,[]);
      end;
    end;
    if CurPos.Flag<>cafSemicolon then
      UndoReadNextAtom;
  end;
  if ChildCreated then begin
    // close class
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncClassInterface: boolean;
// class interface, dispinterface
var
  ChildCreated: boolean;
  IntfAtomPos: TAtomPosition;
  IntfDesc: TCodeTreeNodeDesc;
begin
  if not (CurNode.Desc in [ctnTypeDefinition,ctnGenericType]) then
    SaveRaiseExceptionFmt(ctsAnonymDefinitionsAreNotAllowed,['interface']);
  if CurNode.Parent.Desc<>ctnTypeSection then
    SaveRaiseExceptionFmt(ctsNestedDefinitionsAreNotAllowed,['interface']);
  IntfAtomPos:=CurPos;
  // class interface start found
  ChildCreated:=true; // maybe change this in future to jit parsing
  if UpAtomIs('INTERFACE') then
    IntfDesc:=ctnClassInterface
  else
  if UpAtomIs('DISPINTERFACE') then
    IntfDesc:=ctnDispinterface
  else
    IntfDesc:=ctnObjCProtocol;
  if ChildCreated then begin
    CreateChildNode;
    CurNode.Desc:=IntfDesc;
    CurNode.StartPos:=IntfAtomPos.StartPos;
  end;
  // find end of interface
  ReadNextAtom;
  if (CurPos.Flag<>cafSemicolon) then begin
    if (CurPos.Flag=cafRoundBracketOpen) then begin
      // read inheritage brackets
      ReadClassInheritance(ChildCreated);
      ReadNextAtom;
    end;
    if CurPos.Flag=cafEdgedBracketOpen then
      ReadGUID;
    // parse till "end" of interface
    repeat
      if (CurPos.Flag=cafEnd) or (CurPos.StartPos>SrcLen) then break;
      if not SkipInnerClassInterface(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
      then
        break;
      ReadNextAtom;
    until false;
  end else begin
    // forward definition
    CurNode.SubDesc:=CurNode.SubDesc+ctnsForwardDeclaration;
  end;
  if ChildCreated then begin
    // close class interface
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
  if CurPos.Flag=cafEND then begin
    ReadNextAtom;
    if CurPos.Flag=cafSemicolon then
      ReadNextAtom;
    if UpAtomIs('DEPRECATED') then begin
      ReadNextAtom;
      if AtomIsStringConstant then
        ReadConstant(true,false,[]);
    end else if UpAtomIs('EXTERNAL') then begin
      ReadNextAtom;
      if UpAtomIs('NAME') then begin
        ReadNextAtom;
        ReadConstant(true,false,[]);
      end;
    end else if UpAtomIs('PLATFORM') or UpAtomIs('UNIMPLEMENTED') or
       UpAtomIs('EXPERIMENTAL') or UpAtomIs('LIBRARY')
    then
      ReadNextAtom;
    if CurPos.Flag<>cafSemicolon then
      UndoReadNextAtom;
  end;
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
  // first set the type to open array (an array type without brackets)
  CurNode.Desc:=ctnOpenArrayType;
  ReadNextAtom;
  if (CurPos.Flag=cafEdgedBracketOpen) then begin
    repeat
      ReadNextAtom;
      // this is a ranged array -> change type
      CurNode.Desc:=ctnRangedArrayType;
      CreateChildNode;
      CurNode.Desc:=ctnRangeType;
      ReadSubRange(true);
      CurNode.EndPos:=LastAtoms.GetValueAt(0).EndPos;
      EndChildNode;
      if (CurPos.Flag=cafEdgedBracketClose) then break;
      if (CurPos.Flag<>cafComma) then
        RaiseCharExpectedButAtomFound(']');
    until false;
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then
    RaiseStringExpectedButAtomFound('"of"');
  ReadNextAtom;
  Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
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
var IsFunction, EqualFound: boolean;
begin
  IsFunction:=UpAtomIs('FUNCTION');
  CreateChildNode;
  CurNode.Desc:=ctnProcedureType;
  ReadNextAtom;
  CreateChildNode;
  CurNode.Desc:=ctnProcedureHead;
  CurNode.SubDesc:=ctnsNeedJITParsing;
  if (CurPos.Flag=cafRoundBracketOpen) then begin
    // read parameter list
    ReadParamList(true,false,[]);
  end;
  if IsFunction then begin
    if (CurPos.Flag=cafColon) then begin
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
      if CurPos.Flag=cafPoint then begin
        ReadNextAtom;
        AtomIsIdentifier(true);
        ReadNextAtom;
      end;
    end else begin
      RaiseCharExpectedButAtomFound(':');
    end;
  end;
  if UpAtomIs('OF') then begin
    if not ReadNextUpAtomIs('OBJECT') then
      RaiseStringExpectedButAtomFound('"object"');
    ReadNextAtom;
  end;
  if (CurPos.Flag=cafEqual)
  and (CurNode.Parent.Desc in [ctnConstDefinition,ctnVarDefinition]) then begin
    // for example  'const f: procedure = nil;'
  end else begin
    if CurPos.Flag=cafSemicolon then begin
      ReadNextAtom;
      EqualFound:=false;
    end else if (CurPos.Flag=cafEqual) then begin
      EqualFound:=true;
    end else
      EqualFound:=false;
    if not EqualFound then begin
      // read modifiers
      repeat
        if (not IsKeyWordProcedureTypeSpecifier.DoItCaseInsensitive(Src,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)) then
        begin
          UndoReadNextAtom;
          break;
        end;
        if UpAtomIs('IS') then begin
          ReadNextAtom;
          if not UpAtomIs('NESTED') then
            RaiseStringExpectedButAtomFound('nested');
        end else if UpAtomIs('OF') then begin
          ReadNextAtom;
          if not UpAtomIs('OBJECT') then
            RaiseStringExpectedButAtomFound('object');
        end;
        ReadNextAtom;
        if CurPos.Flag<>cafSemicolon then begin
          if (CurPos.Flag=cafEqual) then begin
            break;
          end;
          // delphi/fpc allow proc modifiers without semicolons
          if not IsKeyWordProcedureTypeSpecifier.DoItCaseInsensitive(Src,
            CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
          begin
            RaiseCharExpectedButAtomFound(';');
          end;
          UndoReadNextAtom;
        end;
        ReadNextAtom;
      until false;
    end;
  end;
  CurNode.EndPos:=CurPos.StartPos;
  EndChildNode;
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
    RaiseStringExpectedButAtomFound('"of"');
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
    RaiseStringExpectedButAtomFound(ctsIdentifier);
  CreateChildNode;
  CurNode.Desc:=ctnTypeType;
  ReadNextAtom;
  Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
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
    Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
    if not Result then exit;
  end;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypePointer: boolean;
// '^Identifier'
begin
  CreateChildNode;
  CurNode.Desc:=ctnPointerType;
  ReadNextAtom;
  Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
end;

function TPascalParserTool.KeyWordFuncTypeDefault: boolean;
{ check for enumeration, subrange and identifier types

  examples:
    integer
    1..3
    (a,b:=3,c=4)
    (a)..4
    Low(integer)..High(integer)
    'a'..'z'
}
var SubRangeOperatorFound: boolean;

  procedure ReadTillTypeEnd;
  begin
    // read till ';', ':', ')', '=', 'end'
    while (CurPos.StartPos<=SrcLen) do begin
      if (CurPos.Flag in [cafSemicolon,cafColon,cafRoundBracketClose,
        cafEqual,cafEdgedBracketClose])
      or (AtomIsKeyWord
          and (not IsKeyWordInConstAllowed.DoItCaseInsensitive(Src,
                                CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)))
      then
        break;
      if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then
        ReadTilBracketClose(true)
      else if AtomIs('..') then begin
        if SubRangeOperatorFound then
          SaveRaiseException(ctsUnexpectedSubRangeOperatorFound);
        SubRangeOperatorFound:=true;
      end;
      ReadNextAtom;
    end;
  end;

// TPascalParserTool.KeyWordFuncTypeDefault: boolean
begin
  CreateChildNode;
  SubRangeOperatorFound:=false;
  if CurPos.Flag in AllCommonAtomWords then begin
    AtomIsIdentifier(true);
    ReadNextAtom;
    if (CurPos.Flag=cafPoint) then begin
      // first word was unit name
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
    end;
    while (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) do begin
      ReadTilBracketClose(true);
      ReadNextAtom;
    end;
    if AtomIs('..') then begin
      // a subrange
      CurNode.Desc:=ctnRangeType;
      ReadTillTypeEnd;
      if not SubRangeOperatorFound then
        SaveRaiseException(ctsInvalidSubrange);
      CurNode.EndPos:=CurPos.StartPos;
    end else if AtomIsChar('<') and (Scanner.CompilerMode in [cmOBJFPC,cmFPC])
    and (LastUpAtomIs(0,'STRING')) then begin
      // string<   a string with an encoding
      CurNode.Desc:=ctnIdentifier;
      repeat
        ReadNextAtom;
        if AtomIsChar('>') then break;
        case CurPos.Flag of
        cafRoundBracketOpen,cafEdgedBracketOpen: ReadTilBracketClose(true);
        cafNone:
          if (CurPos.StartPos>SrcLen) then
            RaiseCharExpectedButAtomFound('>')
          else if (((CurPos.EndPos-CurPos.StartPos=1)
                and (Src[CurPos.StartPos] in ['+','-','*','&','$'])))
              or AtomIsNumber
          then begin
          end else begin
            RaiseCharExpectedButAtomFound('>')
          end;
        else
          RaiseCharExpectedButAtomFound('>');
        end;
      until false;
      CurNode.EndPos:=CurPos.EndPos;
      ReadNextAtom;
    end else begin
      // an identifier
      CurNode.Desc:=ctnIdentifier;
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
      // an enum or syntax error
      MoveCursorToNodeStart(CurNode);
      ReadNextAtom;
      if (CurPos.Flag=cafRoundBracketOpen) then begin
        // an enumeration -> read all enums
        CurNode.Desc:=ctnEnumerationType;
        repeat
          ReadNextAtom; // read enum name
          if (CurPos.Flag=cafRoundBracketClose) then break;
          AtomIsIdentifier(true);
          CreateChildNode;
          CurNode.Desc:=ctnEnumIdentifier;
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode; // close enum node
          ReadNextAtom;
          if AtomIs(':=') or (CurPos.Flag=cafEqual) then begin
            // read ordinal value
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
          if (CurPos.Flag=cafRoundBracketClose) then break;
          if (CurPos.Flag<>cafComma) then
            RaiseCharExpectedButAtomFound(')');
        until false;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
      end else
        SaveRaiseException(ctsInvalidType);
    end;
  end;
  if UpAtomIs('PLATFORM') or UpAtomIs('UNIMPLEMENTED') or
     UpAtomIs('EXPERIMENTAL') or UpAtomIs('LIBRARY')
  then
    ReadNextAtom;
  if UpAtomIs('DEPRECATED') then begin
    ReadNextAtom;
    if AtomIsStringConstant then
      ReadConstant(true,false,[]);
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
           case y: integer of
             0: (a: integer);
             1,2,3: (b: array[char] of char; c: char);
             3: ( d: record
                       case byte of
                         10: (i: integer; );
                         11: (y: byte);
                     end; );
             4: (e: integer;
                   case z of
                   8: (f: integer)
                 );
         end;
    end;
}
// function TPascalParserTool.KeyWordFuncTypeRecord: boolean;
begin
  CreateChildNode;
  CurNode.Desc:=ctnRecordType;
  if LastUpAtomIs(0,'PACKED') or LastUpAtomIs(0,'BITPACKED') then
    CurNode.StartPos:=LastAtoms.GetValueAt(0).StartPos;
  // read all variables
  repeat
    ReadNextAtom;
    if CurPos.Flag=cafEND then break;
    if UpAtomIs('CASE') then begin
      KeyWordFuncTypeRecordCase;
      break;
    end else begin
      // read variable names
      repeat
        AtomIsIdentifier(true);
        CreateChildNode;
        CurNode.Desc:=ctnVarDefinition;
        CurNode.EndPos:=CurPos.EndPos;
        ReadNextAtom;
        if (CurPos.Flag=cafColon) then break;
        if (CurPos.Flag<>cafComma) then
          RaiseCharExpectedButAtomFound(':');
        EndChildNode; // close variable
        ReadNextAtom; // read next variable name
      until false;
      ReadNextAtom;
      Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
      if not Result then exit;
      CurNode.EndPos:=CurPos.EndPos;
      EndChildNode; // close variable
      if CurPos.Flag=cafEND then break;
    end;
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode; // close record
  ReadNextAtom;
  if UpAtomIs('PLATFORM') or UpAtomIs('DEPRECATED') or UpAtomIs('UNIMPLEMENTED') or
     UpAtomIs('EXPERIMENTAL') or UpAtomIs('LIBRARY')
  then
    ReadNextAtom;
  Result:=true;
end;

function TPascalParserTool.KeyWordFuncTypeRecordCase: boolean;
begin
  if not UpAtomIs('CASE') then
    SaveRaiseException('[TPascalParserTool.KeyWordFuncTypeRecordCase] '
      +'internal error');
  CreateChildNode;
  CurNode.Desc:=ctnRecordCase;
  ReadNextAtom; // read ordinal type
  { case a of
    case a:b of
    case a:b.c of
  }
  AtomIsIdentifier(true);
  ReadNextAtom;
  if (CurPos.Flag=cafColon) then begin
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  if CurPos.Flag=cafPoint then begin
    ReadNextAtom; // unit.type
    AtomIsIdentifier(true);
    ReadNextAtom;
  end;
  if not UpAtomIs('OF') then // read 'of'
    RaiseStringExpectedButAtomFound('"of"');
  // read all variants
  repeat
    ReadNextAtom;  // read constant (variant identifier)
    if (CurPos.Flag in [cafRoundBracketClose,cafEnd]) then break;
    CreateChildNode;
    CurNode.Desc:=ctnRecordVariant;
    repeat
      ReadConstant(true,false,[]);
      if (CurPos.Flag=cafColon) then break;
      if (CurPos.Flag<>cafComma) then
        RaiseCharExpectedButAtomFound(':');
      ReadNextAtom;
    until false;
    ReadNextAtom;  // read '('
    if (CurPos.Flag<>cafRoundBracketOpen) then
      RaiseCharExpectedButAtomFound('(');
    // read all variables
    ReadNextAtom; // read first variable name
    repeat
      if (CurPos.Flag=cafRoundBracketClose) then begin
        // end of variant record
        break;
      end else if UpAtomIs('CASE') then begin
        // sub record variant
        KeyWordFuncTypeRecordCase();
        if (CurPos.Flag<>cafRoundBracketClose) then
          RaiseCharExpectedButAtomFound(')');
        break;
      end else begin
        // sub identifier
        repeat
          AtomIsIdentifier(true);
          CreateChildNode;
          CurNode.Desc:=ctnVarDefinition;
          CurNode.EndPos:=CurPos.EndPos;
          ReadNextAtom;
          if (CurPos.Flag=cafColon) then break;
          if (CurPos.Flag<>cafComma) then
            RaiseCharExpectedButAtomFound(',');
          EndChildNode;
          ReadNextAtom; // read next variable name
        until false;
        ReadNextAtom; // read type
        Result:=ParseType(CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
        if not Result then exit;
        CurNode.EndPos:=CurPos.EndPos;
        EndChildNode; // close variable definition
      end;
      if (CurPos.Flag=cafRoundBracketClose) then break;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(';');
      ReadNextAtom;
    until false;
    ReadNextAtom;
    if (CurPos.Flag in [cafEnd,cafRoundBracketClose]) then begin
      CurNode.EndPos:=CurPos.StartPos;
      EndChildNode; // close variant
      break;
    end;
    if CurPos.Flag<>cafSemicolon then
      RaiseCharExpectedButAtomFound(';');
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode; // close variant
    // read next variant
  until false;
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode; // close case
  Result:=true;
end;

procedure TPascalParserTool.RaiseCharExpectedButAtomFound(c: char);
begin
  SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,[c,GetAtom]);
end;

procedure TPascalParserTool.RaiseStringExpectedButAtomFound(const s: string);
begin
  SaveRaiseExceptionFmt(ctsStrExpectedButAtomFound,[s,GetAtom]);
end;

procedure TPascalParserTool.RaiseUnexpectedKeyWord;
begin
  SaveRaiseExceptionFmt(ctsUnexpectedKeyword,[GetAtom]);
end;

procedure TPascalParserTool.RaiseIllegalQualifier;
begin
  SaveRaiseExceptionFmt(ctsIllegalQualifier,[GetAtom]);
end;

procedure TPascalParserTool.RaiseEndOfSourceExpected;
begin
  SaveRaiseExceptionFmt(ctsEndofSourceExpectedButAtomFound,[GetAtom]);
end;

procedure TPascalParserTool.InitExtraction;
begin
  if ExtractMemStream=nil then
    ExtractMemStream:=TMemoryStream.Create;
  ExtractMemStream.Position:=0;
end;

function TPascalParserTool.GetExtraction(InUpperCase: boolean): string;
begin
  SetLength(Result,ExtractMemStream.Position);
  ExtractMemStream.Position:=0;
  if Result<>'' then
    ExtractMemStream.Read(Result[1],length(Result));
  if InUpperCase then
    Result:=UpperCaseStr(Result);
end;

function TPascalParserTool.ExtractStreamEndIsIdentChar: boolean;
var c: char;
begin
  if ExtractMemStream.Position=0 then begin
    Result:=false;
    exit;
  end;
  ExtractMemStream.Position:=ExtractMemStream.Position-1;
  ExtractMemStream.Read(c,1);
  Result:=IsIdentChar[c];
end;

procedure TPascalParserTool.ExtractNextAtom(AddAtom: boolean;
  Attr: TProcHeadAttributes);
// add current atom and text before, then read next atom
// if not phpWithComments in Attr then the text before will be shortened
var
  LastAtomEndPos: integer;
  LastStreamPos: TFPCStreamSeekType;
const
  space: char = ' ';
begin
  LastStreamPos:=ExtractMemStream.Position;
  if LastAtoms.Count>0 then begin
    LastAtomEndPos:=LastAtoms.GetValueAt(0).EndPos;
    if phpWithComments in Attr then begin
      // add space/comment between pascal atoms
      ExtractMemStream.Write(Src[LastAtomEndPos],CurPos.StartPos-LastAtomEndPos);
    end else if (ExtractMemStream.Position>0) then
    begin
      // some space/comments were skipped
      // -> check if a space must be inserted
      if AddAtom
      and ( ((phpCommentsToSpace in Attr) and (CurPos.StartPos>LastAtomEndPos))
         or ((CurPos.StartPos<=SrcLen) and (IsIdentStartChar[Src[CurPos.StartPos]])
             and ExtractStreamEndIsIdentChar)
         )
      then begin
        ExtractMemStream.Write(space,1);
        LastStreamPos:=ExtractMemStream.Position;
      end;
    end;
  end;
  if AddAtom then begin
    ExtractMemStream.Write(Src[CurPos.StartPos],CurPos.EndPos-CurPos.StartPos);
  end;
  if (ExtractSearchPos>0)
  and (ExtractSearchPos<=ExtractMemStream.Position)
  then begin
    ExtractFoundPos:=ExtractSearchPos-1-LastStreamPos+CurPos.StartPos;
    ExtractSearchPos:=-1;
  end;
  ReadNextAtom;
end;

function TPascalParserTool.FindFirstNodeOnSameLvl(
  StartNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=StartNode;
  if Result=nil then exit;
  if Result.Parent=nil then begin
    while Result.PriorBrother<>nil do
      Result:=Result.PriorBrother;
  end else begin
    Result:=Result.Parent;
    while (Result.Desc in AllCodeSections) and (Result.PriorBrother<>nil) do
      Result:=Result.PriorBrother;
    while (Result<>nil) and (Result.FirstChild=nil) do
      Result:=Result.NextBrother;
    Result:=Result.FirstChild;
  end;
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

function TPascalParserTool.FindPrevNodeOnSameLvl(StartNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=StartNode;
  if Result=nil then exit;
  if Result.PriorBrother<>nil then
    Result:=Result.PriorBrother
  else begin
    Result:=Result.Parent;
    if Result=nil then exit;
    Result:=Result.PriorBrother;
    while (Result<>nil) and (Result.LastChild=nil) do
      Result:=Result.PriorBrother;
    if Result=nil then exit;
    Result:=Result.LastChild;
  end;
end;

function TPascalParserTool.FindRootNode(Desc: TCodeTreeNodeDesc
  ): TCodeTreeNode;
begin
  Result:=Tree.FindRootNode(Desc);
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

procedure TPascalParserTool.BuildTreeAndGetCleanPos(
  TreeRange: TTreeRange; const CursorPos: TCodeXYPosition;
  out CleanCursorPos: integer; BuildTreeFlags: TBuildTreeFlags);
var
  CaretType: integer;
  IgnorePos: TCodePosition;
  RealTreeRange: TTreeRange;
  Node: TCodeTreeNode;
begin
  RealTreeRange:=TreeRange;

  //DebugLn(['TPascalParserTool.BuildTreeAndGetCleanPos ',MainFilename,' btSetIgnoreErrorPos=',btSetIgnoreErrorPos in BuildTreeFlags,' btKeepIgnoreErrorPos=',btKeepIgnoreErrorPos in BuildTreeFlags,' CursorPos=x=',CursorPos.X,',y=',CursorPos.Y]);
  if (btSetIgnoreErrorPos in BuildTreeFlags) then begin
    // ignore errors after cursor position
    if (CursorPos.Code<>nil) then begin
      IgnorePos.Code:=CursorPos.Code;
      IgnorePos.Code.LineColToPosition(CursorPos.Y,CursorPos.X,IgnorePos.P);
      if IgnorePos.P<1 then IgnorePos.Code:=nil;
      //debugln(['TPascalParserTool.BuildTreeAndGetCleanPos IgnorePos=',dbgsCP(IgnorePos),' After=',IgnorePos.P,'=',copy(CursorPos.Code.Source,IgnorePos.P,10)]);
      IgnoreErrorAfter:=IgnorePos;
    end else
      ClearIgnoreErrorAfter;
  end
  else if not (btKeepIgnoreErrorPos in BuildTreeFlags) then
    ClearIgnoreErrorAfter;

  if (RealTreeRange in [trTillCursor,trTillCursorSection]) then begin
    // find out, if interface is enough
    if (Tree<>nil) and (Tree.Root<>nil) then begin
      Node:=Tree.Root;
      while (Node<>nil) and (Node.Desc<>ctnImplementation) do
        Node:=Node.NextBrother;
      if Node<>nil then begin
        // start of implementation section found
        // => whole interface was read
        CaretType:=CaretToCleanPos(CursorPos, CleanCursorPos);
        if (CaretType=0) or (CaretType=-1) then begin
          if (CleanCursorPos<=Node.StartPos)
          and (not UpdateNeeded(true)) then begin
            // interface section is already parsed, is still valid and
            // cursor is in this section
            ValidateToolDependencies;
            exit;
          end;
        end;
      end;
    end;
    if RealTreeRange=trTillCursorSection then begin
      // interface is no enough => parse whole unit
      RealTreeRange:=trAll;
    end;
  end;

  if (RealTreeRange=trTillCursor) and (not UpdateNeeded(false)) then begin
    // tree is valid
    // -> if there was an error, raise it again
    if (LastErrorPhase in [CodeToolPhaseScan,CodeToolPhaseParse])
    and ((not IgnoreErrorAfterValid)
      or (not IgnoreErrorAfterPositionIsInFrontOfLastErrMessage))
    then begin
      DebugLn('TPascalParserTool.BuildTreeAndGetCleanPos RaiseLastError ',MainFilename);
      RaiseLastError;
    end;
    // check if cursor is in interface
    CaretType:=CaretToCleanPos(CursorPos, CleanCursorPos);
    if (CaretType=0) or (CaretType=-1) then begin
      BuildSubTree(CleanCursorPos);
      if (CaretType=-1) and (btLoadDirtySource in BuildTreeFlags) then begin
        // cursor position is in dead code (skipped code between IFDEF/ENDIF)
        LoadDirtySource(CursorPos);
      end;
      exit;
    end;
    // cursor is not in partially parsed code -> parse complete code
  end;

  // parse code
  BuildTree(RealTreeRange=trInterface);
  if (not IgnoreErrorAfterValid) and (not EndOfSourceFound) then
    SaveRaiseException(ctsEndOfSourceNotFound);
  // find the CursorPos in cleaned source
  CaretType:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (CaretType=0) or (CaretType=-1) then begin
    BuildSubTree(CleanCursorPos);
    if (CaretType=-1) and (btLoadDirtySource in BuildTreeFlags) then begin
      // cursor position lies in dead code (skipped code between IFDEF/ENDIF)
      LoadDirtySource(CursorPos);
    end;
    exit;
  end;
  if (CaretType=-2) or (not (btCursorPosOutAllowed in BuildTreeFlags)) then
    RaiseException(ctsCursorPosOutsideOfCode);
  // cursor outside of clean code
  CleanCursorPos:=-1;
end;

function TPascalParserTool.ReadTilTypeOfProperty(
  PropertyNode: TCodeTreeNode): boolean;
begin
  MoveCursorToNodeStart(PropertyNode);
  ReadNextAtom; // read keyword 'property'
  if UpAtomIs('CLASS') then ReadNextAtom;
  ReadNextAtom; // read property name
  AtomIsIdentifier(true);
  ReadNextAtom;
  if (CurPos.Flag=cafEdgedBracketOpen) then begin
    // read parameter list
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if (CurPos.Flag<>cafColon) then begin
    Result:=false;
    exit;
  end;
  ReadNextAtom; // read type
  AtomIsIdentifier(true);
  Result:=true;
end;

procedure TPascalParserTool.ReadGUID;

  procedure RaiseStringConstantExpected;
  begin
    RaiseStringExpectedButAtomFound(ctsStringConstant);
  end;

begin
  CreateChildNode;
  CurNode.Desc:=ctnClassGUID;
  // read GUID
  ReadNextAtom;
  if (not AtomIsStringConstant) and (not AtomIsIdentifier(false)) then
    RaiseStringConstantExpected;
  ReadNextAtom;
  if CurPos.Flag<>cafEdgedBracketClose then
    RaiseCharExpectedButAtomFound(']');
  CurNode.EndPos:=CurPos.EndPos;
  EndChildNode;
  ReadNextAtom;
end;

procedure TPascalParserTool.ReadClassInheritance(CreateChildNodes: boolean);
// cursor must be the round bracket open
// at the end cursor will be on round bracket close
begin
  // read inheritage
  if CreateChildNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnClassInheritance;
  end;
  // read list of ancestors, interfaces
  ReadNextAtom;
  if CurPos.Flag<>cafRoundBracketClose then begin
    repeat
      if UpAtomIs('SPECIALIZE') then begin
        // specialize Identifier<Identifier>
        ReadSpecialize(CreateChildNodes);
      end else begin
        // read Identifier or Unit.Identifier
        AtomIsIdentifier(true);
        if CreateChildNodes then begin
          CreateChildNode;
          CurNode.Desc:=ctnIdentifier;
        end;
        ReadNextAtom;
        if CurPos.Flag=cafPoint then begin
          ReadNextAtom;
          AtomIsIdentifier(true);
          ReadNextAtom;
        end;
        if CreateChildNodes then begin
          CurNode.EndPos:=CurPos.EndPos;
          EndChildNode;
        end;
      end;
      // read comma or )
      if CurPos.Flag=cafRoundBracketClose then break;
      if CurPos.Flag<>cafComma then
        RaiseCharExpectedButAtomFound(')');
      ReadNextAtom;
    until false;
  end;
  // close ctnClassInheritance
  if CreateChildNodes then begin
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode;
  end;
end;

procedure TPascalParserTool.ReadSpecialize(CreateChildNodes: boolean);
// specialize template
// after parsing the cursor is on the atom behind the >
// examples:
//   type TListOfInteger = specialize TGenericList<integer,string>;
//   type TListOfChar = specialize Classes.TGenericList<integer,objpas.integer>;
//   type l = class(specialize TFPGObjectList<TControl>)
begin
  if CreateChildNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnSpecialize;
  end;
  // read identifier (the name of the generic)
  ReadNextAtom;
  AtomIsIdentifier(true);
  if CreateChildNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnSpecializeType;
    CurNode.EndPos:=CurPos.EndPos;
  end;
  ReadNextAtom;
  if Curpos.Flag=cafPoint then begin
    // first identifier was unitname, now read the type
    ReadNextAtom;
    AtomIsIdentifier(true);
    if CreateChildNodes then
      CurNode.EndPos:=CurPos.EndPos;
    ReadNextAtom;
  end;
  if CreateChildNodes then begin
    EndChildNode; // end ctnSpecializeType
  end;
  // read type list
  if not AtomIsChar('<') then
    RaiseCharExpectedButAtomFound('<');
  if CreateChildNodes then begin
    CreateChildNode;
    CurNode.Desc:=ctnSpecializeParams;
  end;
  // read list of types
  repeat
    // read identifier (a parameter of the generic type)
    ReadNextAtom;
    AtomIsIdentifier(true);
    ReadNextAtom;
    if Curpos.Flag=cafPoint then begin
      // first identifier was unitname, now read the type
      ReadNextAtom;
      AtomIsIdentifier(true);
      ReadNextAtom;
    end;
    if AtomIsChar('>') then
      break
    else if CurPos.Flag=cafComma then begin
      // read next parameter
    end else
      RaiseCharExpectedButAtomFound('>');
  until false;
  if CreateChildNodes then begin
    // close list
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode; // end ctnSpecializeParams
    // close specialize
    CurNode.EndPos:=CurPos.EndPos;
    EndChildNode; // end ctnSpecialize
  end;
  ReadNextAtom;
end;

function TPascalParserTool.WordIsPropertyEnd: boolean;
var
  p: PChar;
begin
  p:=@Src[CurPos.StartPos];
  case UpChars[p^] of
  'C': if UpAtomIs('CLASS') then exit(true);
  'F': if UpAtomIs('FUNCTION') then exit(true);
  'S': if UpAtomIs('STRICT') then exit(true);
  'P':
    case UpChars[p[1]] of
    'R':
      case UpChars[p[2]] of
      'I': if UpAtomIs('PRIVATE') then exit(true);
      'O': if UpAtomIs('PROTECTED') or UpAtomIs('PROCEDURE') then exit(true);
      end;
    'U': if UpAtomIs('PUBLIC') or UpAtomIs('PUBLISHED') then exit(true);
    end;
  'T': if UpAtomIs('TYPE') then exit(true);
  'V': if UpAtomIs('VAR') then exit(true);
  end;
  Result:=false;
end;

procedure TPascalParserTool.ValidateToolDependencies;
begin

end;

procedure TPascalParserTool.BuildSubTreeForProcHead(ProcNode: TCodeTreeNode);
var HasForwardModifier, IsFunction, IsOperator, IsMethod: boolean;
  ParseAttr: TParseProcHeadAttributes;
  OldPhase: integer;
  IsProcType: Boolean;
  ProcHeadNode: TCodeTreeNode;
begin
  if ProcNode.Desc=ctnProcedureHead then ProcNode:=ProcNode.Parent;
  if ProcNode.Desc=ctnMethodMap then begin
    exit;
  end;
  if (not (ProcNode.Desc in [ctnProcedure,ctnProcedureType])) then begin
    {$IFDEF CheckNodeTool}
    CTDumpStack;
    {$ENDIF}
    if ProcNode<>nil then begin
      DebugLn(['TPascalParserTool.BuildSubTreeForProcHead Desc=',ProcNode.DescAsString]);
      if ProcNode.FirstChild<>nil then
        DebugLn(['TPascalParserTool.BuildSubTreeForProcHead FirstChild=',ProcNode.FirstChild.DescAsString]);
    end;
    RaiseException('[TPascalParserTool.BuildSubTreeForProcHead] '
      +'internal error: invalid ProcNode');
  end;
  ProcHeadNode:=ProcNode.FirstChild;
  if (ProcHeadNode<>nil)
  and ((ProcHeadNode.SubDesc and ctnsNeedJITParsing)=0) then begin
    // proc head already parsed
    if (ProcHeadNode<>nil) and ((ctnsHasParseError and ProcHeadNode.SubDesc)>0)
    then
      RaiseNodeParserError(ProcHeadNode);
    exit;
  end;
  OldPhase:=CurrentPhase;
  CurrentPhase:=CodeToolPhaseParse;
  try
    IsMethod:=ProcNode.Parent.Desc in (AllClasses+AllClassSections);
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    if UpAtomIs('CLASS') then
      ReadNextAtom;
    IsFunction:=UpAtomIs('FUNCTION');
    IsOperator:=UpAtomIs('OPERATOR');
    IsProcType:=ProcNode.Desc=ctnProcedureType;
    // read procedure head (= [name] + parameterlist + resulttype;)
    ReadNextAtom;// read first atom of head
    CurNode:=ProcHeadNode;
    if CurNode=nil then
      if ProcNode.Desc=ctnProcedureType then
        RaiseCharExpectedButAtomFound(';')
      else
        RaiseStringExpectedButAtomFound('identifier');
    ProcHeadNode.SubDesc:=ProcHeadNode.SubDesc and (not ctnsNeedJITParsing);

    if not IsProcType then begin
      if not IsOperator then AtomIsIdentifier(true);
      ReadNextAtom;
      if (CurPos.Flag=cafPoint) then begin
        // read procedure name of a class method (the name after the . )
        ReadNextAtom;
        AtomIsIdentifier(true);
        ReadNextAtom;
      end;
    end;
    // read rest of procedure head and build nodes
    HasForwardModifier:=false;
    ParseAttr:=[pphCreateNodes];
    if IsMethod then Include(ParseAttr,pphIsMethod);
    if IsFunction then Include(ParseAttr,pphIsFunction);
    if IsOperator then Include(ParseAttr,pphIsOperator);
    if IsProcType then Include(ParseAttr,pphIsType);
    ReadTilProcedureHeadEnd(ParseAttr,HasForwardModifier);
    CurrentPhase:=OldPhase;
  except
    CurrentPhase:=OldPhase;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildSubTreeForProcHead ',MainFilename,' ERROR: ',LastErrorMessage);
    {$ENDIF}
    if (not IgnoreErrorAfterValid)
    or (not IgnoreErrorAfterPositionIsInFrontOfLastErrMessage) then
      raise;
    {$IFDEF ShowIgnoreErrorAfter}
    DebugLn('TPascalParserTool.BuildSubTreeForProcHead ',MainFilename,' IGNORING ERROR: ',LastErrorMessage);
    {$ENDIF}
  end;
end;

procedure TPascalParserTool.BuildSubTreeForProcHead(ProcNode: TCodeTreeNode;
  out FunctionResult: TCodeTreeNode);
begin
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then
    RaiseException('INTERNAL ERROR: TPascalParserTool.BuildSubTreeForProcHead with FunctionResult');
  BuildSubTreeForProcHead(ProcNode);
  FunctionResult:=ProcNode.FirstChild.FirstChild;
  if (FunctionResult<>nil) and (FunctionResult.Desc=ctnParameterList) then
    FunctionResult:=FunctionResult.NextBrother;
end;

procedure TPascalParserTool.BuildSubTree(CleanCursorPos: integer);
begin
  BuildSubTree(FindDeepestNodeAtPos(CleanCursorPos,false));
end;

procedure TPascalParserTool.BuildSubTree(ANode: TCodeTreeNode);
begin
  if ANode=nil then exit;
  case ANode.Desc of
  ctnClass,ctnClassInterface,ctnDispinterface,ctnObject,
  ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,ctnCPPClass:
    BuildSubTreeForClass(ANode);
  ctnProcedure,ctnProcedureHead:
    BuildSubTreeForProcHead(ANode);
  ctnBeginBlock:
    BuildSubTreeForBeginBlock(ANode);
  end;
end;

function TPascalParserTool.NodeNeedsBuildSubTree(ANode: TCodeTreeNode
  ): boolean;
begin
  Result:=false;
  if ANode=nil then exit;
  if ANode.Desc in (AllClasses+[ctnProcedureHead,ctnBeginBlock]) then begin
    Result:=(ANode.SubDesc and ctnsNeedJITParsing)>0;
  end;
end;

function TPascalParserTool.BuildSubTreeAndFindDeepestNodeAtPos(P: integer;
  ExceptionOnNotFound: boolean): TCodeTreeNode;
begin
  Result:=BuildSubTreeAndFindDeepestNodeAtPos(Tree.Root,P,ExceptionOnNotFound);
end;

function TPascalParserTool.BuildSubTreeAndFindDeepestNodeAtPos(
  StartNode: TCodeTreeNode; P: integer; ExceptionOnNotFound: boolean
  ): TCodeTreeNode;
var
  Node: TCodeTreeNode;
begin
  Result:=FindDeepestNodeAtPos(StartNode,P,ExceptionOnNotFound);
  //debugln('TPascalParserTool.BuildSubTreeAndFindDeepestNodeAtPos A ',Result.DescAsString,' ',dbgs(NodeNeedsBuildSubTree(Result)));
  while NodeNeedsBuildSubTree(Result) do begin
    BuildSubTree(Result);
    Node:=FindDeepestNodeAtPos(Result,P,ExceptionOnNotFound);
    if Node=Result then break;
    Result:=Node;
    //debugln('TPascalParserTool.BuildSubTreeAndFindDeepestNodeAtPos B ',Result.DescAsString,' ',dbgs(NodeNeedsBuildSubTree(Result)));
  end;
  // re-raise parse errors
  if (Result<>nil) and ((ctnsHasParseError and Result.SubDesc)>0) then
    RaiseNodeParserError(Result);
end;

function TPascalParserTool.FindInterfaceNode: TCodeTreeNode;
begin
  Result:=FindRootNode(ctnInterface);
end;

function TPascalParserTool.FindImplementationNode: TCodeTreeNode;
begin
  Result:=FindRootNode(ctnImplementation);
end;

function TPascalParserTool.FindInitializationNode: TCodeTreeNode;
begin
  Result:=FindRootNode(ctnInitialization);
end;

function TPascalParserTool.FindFinalizationNode: TCodeTreeNode;
begin
  Result:=FindRootNode(ctnFinalization);
end;

function TPascalParserTool.FindMainBeginEndNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if (Result=nil) then exit;
  if (Result.Desc in [ctnProgram,ctnLibrary]) then
    Result:=Result.LastChild
  else begin
    Result:=FindImplementationNode;
    if Result<>nil then
      Result:=Result.LastChild;
  end;
  if Result=nil then exit;
  if Result.Desc<>ctnBeginBlock then Result:=nil;
end;

function TPascalParserTool.FindFirstSectionChild: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.FirstChild=nil) do
    Result:=Result.NextBrother;
  if (Result=nil) then exit;
  Result:=Result.FirstChild;
end;


end.


