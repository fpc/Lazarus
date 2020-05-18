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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TPascalReaderTool enhances TPascalParserTool.
    This tool provides a lot of useful functions to read the output of the
    TPascalParserTool.
}
unit PascalReaderTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazDbgLog,
  // Codetools
  FileProcs, CodeToolsStrConsts, CodeTree, CodeCache, CodeAtom,
  PascalParserTool, KeywordFuncLists, BasicCodeTools, LinkScanner;

type
  TPascalHintModifier = (
    phmDeprecated,
    phmPlatform,
    phmLibrary,
    phmUnimplemented,
    phmExperimental
    );
  TPascalHintModifiers = set of TPascalHintModifier;

  TEPRIRange = (
    epriInCode,
    epriInComment,
    epriInDirective
    );

  //the scope groups of pascal methods.
  //please note that Destructor is principally a method and thus is not listed here -> you cannot define "procedure Destroy;" and "destructor Destroy" in one class
  TPascalMethodGroup = (
    mgMethod,
    mgConstructor,
    mgClassConstructor,
    mgClassDestructor,
    mgClassOperator);

  TPascalMethodHeader = record
    Name, ResultType: string;
    Group: TPascalMethodGroup;
  end;

  TClassSectionVisibility = (
    csvEverything,//same class same unit
    csvPrivateAndHigher,//same unit different class
    csvProtectedAndHigher,//ancestor class different unit
    csvPublicAndHigher);//other class other unit

  TOnEachPRIdentifier = procedure(Sender: TPascalParserTool;
    IdentifierCleanPos: integer; Range: TEPRIRange;
    Node: TCodeTreeNode; Data: Pointer; var Abort: boolean) of object;

  { TPascalReaderTool }

  TPascalReaderTool = class(TPascalParserTool)
  protected
    CachedSourceName: string;
    procedure RaiseStrConstExpected(id: int64);
  public
    // comments
    function CleanPosIsInComment(CleanPos, CleanCodePosInFront: integer;
        out CommentStart, CommentEnd: integer;
        OuterCommentBounds: boolean = true): boolean;

    // general extraction
    function ExtractNode(ANode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractCode(StartPos, EndPos: integer;
        Attr: TProcHeadAttributes): string;
    function ExtractBrackets(BracketStartPos: integer;
        Attr: TProcHeadAttributes): string;
    function ExtractIdentifierWithPoints(StartPos: integer;
        ExceptionOnError: boolean): string;
    function ExtractNextTypeRef(Add: boolean; const Attr: TProcHeadAttributes): boolean;
    function ExtractNextSpecializeParams(Add: boolean;
        const Attr: TProcHeadAttributes): boolean;
    function ExtractIdentCharsFromStringConstant(
        StartPos, MinPos, MaxPos, MaxLen: integer): string;
    function ReadStringConstantValue(StartPos: integer): string;
    function GetNodeIdentifier(Node: TCodeTreeNode): PChar;
    function GetHintModifiers(Node: TCodeTreeNode): TPascalHintModifiers;
    procedure ForEachIdentifierInCleanSrc(StartPos, EndPos: integer;
        SkipComments: boolean; Node: TCodeTreeNode;
        const OnIdentifier: TOnEachPRIdentifier; Data: pointer;
        var Abort: boolean); // range in clean source
    procedure ForEachIdentifierInNode(Node: TCodeTreeNode; SkipComments: boolean;
        const OnIdentifier: TOnEachPRIdentifier; Data: Pointer; var Abort: boolean); // node and child nodes
    procedure ForEachIdentifier(SkipComments: boolean;
        const OnIdentifier: TOnEachPRIdentifier; Data: Pointer); // whole unit/program

    // properties
    function ExtractPropType(PropNode: TCodeTreeNode;
                             InUpperCase, EmptyIfIndexed: boolean): string;
    function MoveCursorToPropType(PropNode: TCodeTreeNode): boolean;
    function MoveCursorToPropName(PropNode: TCodeTreeNode): boolean;
    procedure MoveCursorBehindPropName(PropNode: TCodeTreeNode);
    function ExtractPropName(PropNode: TCodeTreeNode;
                             InUpperCase: boolean): string;
    function ExtractProperty(PropNode: TCodeTreeNode;
                             Attr: TProcHeadAttributes): string;
    function GetPropertyNameIdentifier(PropNode: TCodeTreeNode): PChar;
    function GetPropertyTypeIdentifier(PropNode: TCodeTreeNode): PChar;
    function PositionInPropertyName(PropNode: TCodeTreeNode;
                                    CleanPos: integer): boolean;
    function PropertyIsDefault(PropertyNode: TCodeTreeNode): boolean;
    function PropertyNodeHasParamList(PropNode: TCodeTreeNode): boolean;
    function PropNodeIsTypeLess(PropNode: TCodeTreeNode): boolean;
    function PropertyHasSpecifier(PropNode: TCodeTreeNode;
                 UpperKeyword: string; ExceptionOnNotFound: boolean = true): boolean;

    // procs
    function ExtractProcName(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractProcHead(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractProcHeadWithGroup(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): TPascalMethodHeader;
    function ExtractProcedureHeader(CursorPos: TCodeXYPosition;
      Attributes: TProcHeadAttributes; out ProcHead: string): boolean;
    function ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode;
        AddParentClasses: boolean = true; KeepGeneric: boolean = false): string;
    function GetProcNameIdentifier(ProcNode: TCodeTreeNode): PChar;
    function FindProcNode(StartNode: TCodeTreeNode; const AProcHead: string;
        AProcSpecType: TPascalMethodGroup;
        Attr: TProcHeadAttributes; Visibility: TClassSectionVisibility = csvEverything): TCodeTreeNode; overload;
    function FindProcNode(StartNode: TCodeTreeNode; const AProcHead: TPascalMethodHeader;
        Attr: TProcHeadAttributes; Visibility: TClassSectionVisibility = csvEverything): TCodeTreeNode; overload;
    function FindCorrespondingProcNode(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes = [phpWithoutClassKeyword,phpWithoutClassName]
        ): TCodeTreeNode;
    function FindCorrespondingProcParamNode(ProcParamNode: TCodeTreeNode;
        Attr: TProcHeadAttributes = [phpInUpperCase,phpWithoutClassName,phpWithVarModifiers]
        ): TCodeTreeNode;
    function FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
    function ProcBodyIsEmpty(ProcNode: TCodeTreeNode): boolean;
    function ExtractProcedureGroup(ProcNode: TCodeTreeNode): TPascalMethodGroup;
    function ExtractFuncResultType(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    procedure MoveCursorToFirstProcSpecifier(ProcNode: TCodeTreeNode);
    function MoveCursorToProcSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    procedure MoveCursorToProcName(ProcNode: TCodeTreeNode;
        SkipClassName: boolean);
    procedure MoveCursorBehindProcName(ProcNode: TCodeTreeNode);
    function PositionInProcName(ProcNode: TCodeTreeNode;
                                SkipClassName: boolean; CleanPos: integer): boolean;
    function PositionInFuncResultName(ProcNode: TCodeTreeNode;
                                      CleanPos: integer): boolean;
    function ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    function ProcNodeHasParamList(ProcNode: TCodeTreeNode): boolean;
    function ProcNodeHasOfObject(ProcNode: TCodeTreeNode): boolean;
    function GetProcParamList(ProcNode: TCodeTreeNode;
                              Parse: boolean = true): TCodeTreeNode;
    function GetProcResultNode(ProcNode: TCodeTreeNode): TCodeTreeNode;
    function NodeIsInAMethod(Node: TCodeTreeNode): boolean;
    function NodeIsMethodBody(ProcNode: TCodeTreeNode): boolean;
    function GetMethodOfBody(Node: TCodeTreeNode): TCodeTreeNode;
    function NodeIsFunction(ProcNode: TCodeTreeNode): boolean;
    function NodeIsClassConstructorOrDestructor(ProcNode: TCodeTreeNode): boolean;
    function NodeIsConstructor(ProcNode: TCodeTreeNode): boolean;
    function NodeIsDestructor(ProcNode: TCodeTreeNode): boolean;
    function NodeIsForwardProc(ProcNode: TCodeTreeNode): boolean;
    function NodeIsOperator(ProcNode: TCodeTreeNode): boolean;
    function NodeIsResultIdentifier(Node: TCodeTreeNode): boolean;
    function NodeIsResultType(Node: TCodeTreeNode): boolean;

    // classes
    function ExtractClassName(Node: TCodeTreeNode;
        InUpperCase: boolean; WithParents: boolean = true;
        WithGenericParams: boolean = false): string;
    function ExtractClassPath(Node: TCodeTreeNode): string;
    function ExtractClassInheritance(ClassNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function FindClassNode(StartNode: TCodeTreeNode;
        const AClassName: string; // nested: A.B
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindClassNodeBackwards(StartNode: TCodeTreeNode;
        const AClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindNestedClass(RootClassNode: TCodeTreeNode;
         AClassName: PChar; SkipFirst: boolean): TCodeTreeNode;
    function FindClassNode(CursorNode: TCodeTreeNode): TCodeTreeNode;
    function FindClassNodeForMethodBody(ProcNode: TCodeTreeNode;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindClassOrInterfaceNode(CursorNode: TCodeTreeNode;
        FindClassOfMethod: boolean = false): TCodeTreeNode;
    function FindClassSection(ClassNode: TCodeTreeNode;
        NodeDesc: TCodeTreeNodeDesc): TCodeTreeNode;
    function FindLastClassSection(ClassNode: TCodeTreeNode;
        NodeDesc: TCodeTreeNodeDesc): TCodeTreeNode;
    function GetClassVisibility(Node: TCodeTreeNode): TCodeTreeNodeDesc;
    function FindClassNodeInInterface(const AClassName: string;
        IgnoreForwards, IgnoreNonForwards, ErrorOnNotFound: boolean): TCodeTreeNode;
    function FindClassNodeInUnit(const AClassName: string;
        IgnoreForwards, IgnoreNonForwards, IgnoreImplementation,
        ErrorOnNotFound: boolean): TCodeTreeNode;
    function FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function FindLastIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function FindNextIdentNodeInClass(Node: TCodeTreeNode): TCodeTreeNode;
    function FindPriorIdentNodeInClass(Node: TCodeTreeNode): TCodeTreeNode;
    function ClassSectionNodeStartsWithWord(ANode: TCodeTreeNode): boolean;
    function IsClassNode(Node: TCodeTreeNode): boolean; // class, not object
    function FindInheritanceNode(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function FindHelperForNode(HelperNode: TCodeTreeNode): TCodeTreeNode;
    function FindClassExternalNode(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function IdentNodeIsInVisibleClassSection(Node: TCodeTreeNode; Visibility: TClassSectionVisibility): Boolean;

    // records
    function ExtractRecordCaseType(RecordCaseNode: TCodeTreeNode): string;

    // variables, types
    function FindVarNode(StartNode: TCodeTreeNode;
        const UpperVarName: string;
        Visibility: TClassSectionVisibility = csvEverything): TCodeTreeNode;
    function FindTypeNodeOfDefinition(
        DefinitionNode: TCodeTreeNode): TCodeTreeNode;
    function NodeIsPartOfTypeDefinition(ANode: TCodeTreeNode): boolean;
    function ExtractDefinitionNodeType(DefinitionNode: TCodeTreeNode): string;
    function ExtractDefinitionName(DefinitionNode: TCodeTreeNode): string;
    function FindDefinitionNameNode(DefinitionNode: TCodeTreeNode): TCodeTreeNode;
    function PositionInDefinitionName(DefinitionNode: TCodeTreeNode;
                                      CleanPos: integer): boolean;
    function MoveCursorToParameterSpecifier(DefinitionNode: TCodeTreeNode
                                            ): boolean;
    function GetFirstGroupVarNode(VarNode: TCodeTreeNode): TCodeTreeNode;
    function NodeIsIdentifierInInterface(Node: TCodeTreeNode): boolean;
    function NodeCanHaveForwardType(TypeNode: TCodeTreeNode): boolean;
    function NodeIsForwardType(TypeNode: TCodeTreeNode): boolean;
    function FindForwardTypeNode(TypeNode: TCodeTreeNode;
                                 SearchFirst: boolean): TCodeTreeNode;
    function FindTypeOfForwardNode(TypeNode: TCodeTreeNode): TCodeTreeNode;
    function FindEndOfWithExpr(WithVarNode: TCodeTreeNode): integer;
    function ExtractWithBlockExpression(WithVarNode: TCodeTreeNode; Attr: TProcHeadAttributes = []): string;
    function FindWithBlockStatement(WithVarNode: TCodeTreeNode): TCodeTreeNode;

    // arrays
    function ExtractArrayRange(ArrayNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractArrayRanges(ArrayNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;

    // module sections
    function ExtractSourceName: string;
    function GetSourceNamePos(out NamePos: TAtomPosition): boolean;
    function GetSourceName(DoBuildTree: boolean = true): string;
    function GetSourceType: TCodeTreeNodeDesc;
    function PositionInSourceName(CleanPos: integer): boolean;

    // uses sections
    procedure MoveCursorToUsesStart(UsesNode: TCodeTreeNode);
    procedure MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
    function ReadNextUsedUnit(out UnitNameRange, InAtom: TAtomPosition;
          SyntaxExceptions: boolean = true): boolean;
    procedure ReadPriorUsedUnit(out UnitNameRange, InAtom: TAtomPosition);
    function ExtractUsedUnitNameAtCursor(InFilename: PAnsiString = nil): string;
    function ExtractUsedUnitName(UseUnitNode: TCodeTreeNode;
          InFilename: PAnsiString = nil): string;
    function ReadAndCompareUsedUnit(const AnUnitName: string): boolean;

    // comments
    function FindCommentInFront(const StartPos: TCodeXYPosition;
          const CommentText: string; InvokeBuildTree, SearchInParentNode,
          WithCommentBounds, CaseSensitive, IgnoreSpaces,
          CompareOnlyStart: boolean;
          out CommentStart, CommentEnd: TCodeXYPosition): boolean;
    function FindCommentInFront(StartPos: integer;
          const CommentText: string; SearchInParentNode,
          WithCommentBounds, CaseSensitive, IgnoreSpaces,
          CompareOnlyStart: boolean;
          out CommentStart, CommentEnd: integer): boolean;
    function GetPasDocComments(const StartPos: TCodeXYPosition;
                               InvokeBuildTree: boolean;
                               out ListOfPCodeXYPosition: TFPList): boolean;
    function GetPasDocComments(Node: TCodeTreeNode;
                               out ListOfPCodeXYPosition: TFPList): boolean;

    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;

function CompareMethodHeaders(
  const Method1Name: string; Method1Group: TPascalMethodGroup; const Method1ResultType: string;
  const Method2Name: string; Method2Group: TPascalMethodGroup; const Method2ResultType: string): Integer; overload;
function CompareMethodHeaders(const Method1Head: TPascalMethodHeader; const Method2Head: TPascalMethodHeader): Integer; overload;
function SameMethodHeaders(
  const Method1Name: string; Method1Group: TPascalMethodGroup; const Method1ResultType: string;
  const Method2Name: string; Method2Group: TPascalMethodGroup; const Method2ResultType: string): Boolean; overload;
function SameMethodHeaders(const Method1Head: TPascalMethodHeader; const Method2Head: TPascalMethodHeader): Boolean; overload;
function CompareCodeTreeNodeExtMethodHeaders(NodeData1, NodeData2: pointer): integer;

implementation

function CompareMethodHeaders(const Method1Name: string;
  Method1Group: TPascalMethodGroup; const Method1ResultType: string;
  const Method2Name: string; Method2Group: TPascalMethodGroup;
  const Method2ResultType: string): Integer;
begin
  Result := (Ord(Method1Group) - Ord(Method2Group));
  if Result <> 0 then exit;
  Result := CompareTextIgnoringSpace(Method1Name,Method2Name,false);
  if Result <> 0 then exit;
  if Method1Group=mgClassOperator then
    Result := CompareTextIgnoringSpace(Method1ResultType,Method2ResultType,false);
end;

function CompareMethodHeaders(const Method1Head: TPascalMethodHeader;
  const Method2Head: TPascalMethodHeader): Integer;
begin
  Result := CompareMethodHeaders(
    Method1Head.Name, Method1Head.Group, Method1Head.ResultType,
    Method2Head.Name, Method2Head.Group, Method2Head.ResultType);
end;

function SameMethodHeaders(const Method1Name: string;
  Method1Group: TPascalMethodGroup; const Method1ResultType: string;
  const Method2Name: string; Method2Group: TPascalMethodGroup;
  const Method2ResultType: string): Boolean;
begin
  Result := CompareMethodHeaders(
    Method1Name, Method1Group, Method1ResultType,
    Method2Name, Method2Group, Method2ResultType) = 0;
end;

function SameMethodHeaders(const Method1Head: TPascalMethodHeader;
  const Method2Head: TPascalMethodHeader): Boolean;
begin
  Result := CompareMethodHeaders(Method1Head, Method2Head) = 0;
end;

function CompareCodeTreeNodeExtMethodHeaders(NodeData1, NodeData2: pointer): integer;
var
  NodeExt1: TCodeTreeNodeExtension absolute NodeData1;
  NodeExt2: TCodeTreeNodeExtension absolute NodeData2;
begin
  Result := CompareMethodHeaders(
    NodeExt1.Txt,TPascalMethodGroup(NodeExt1.Flags),NodeExt1.ExtTxt4,
    NodeExt2.Txt,TPascalMethodGroup(NodeExt2.Flags),NodeExt2.ExtTxt4);
end;


{ TPascalReaderTool }

procedure TPascalReaderTool.RaiseStrConstExpected(id: int64);
begin
  RaiseExceptionFmt(id,ctsStrExpectedButAtomFound,[ctsStringConstant,GetAtom]);
end;

function TPascalReaderTool.CleanPosIsInComment(CleanPos,
  CleanCodePosInFront: integer; out CommentStart, CommentEnd: integer;
  OuterCommentBounds: boolean): boolean;
var CommentLvl, CurCommentPos: integer;
  CurEnd: Integer;
  CurCommentInnerEnd: Integer;
begin
  Result:=false;
  CommentStart:=0;
  CommentEnd:=0;
  if CleanPos>SrcLen then exit;
  if CleanCodePosInFront>CleanPos then
    RaiseException(20170421195949,
      'TPascalReaderTool.CleanPosIsInComment CleanCodePosInFront>CleanPos');
  MoveCursorToCleanPos(CleanCodePosInFront);
  repeat
    ReadNextAtom;
    if CurPos.StartPos>CleanPos then begin
      //DebugLn(['TPascalReaderTool.CleanPosIsInComment ',GetAtom,' StartPos=',CurPos.StartPos,' CleanPos=',CleanPos]);
      // CleanPos between two atoms -> parse space between for comments
      if LastAtoms.HasPrior then
        CommentStart:=LastAtoms.GetPriorAtom.EndPos
      else
        CommentStart:=CleanCodePosInFront;
      CurEnd:=CurPos.StartPos;
      if CurEnd>SrcLen then CurEnd:=SrcLen+1;
      while CommentStart<CurEnd do begin
        if IsCommentStartChar[Src[CommentStart]] then begin
          CurCommentPos:=CommentStart;
          CurCommentInnerEnd:=CurEnd;
          case Src[CommentStart] of
          '{':
            begin
              inc(CurCommentPos);
              if (CurCommentPos<=SrcLen) and (Src[CurCommentPos]=#3) then begin
                // codetools-skip-comment
                inc(CurCommentPos);
                if not OuterCommentBounds then CommentStart:=CurCommentPos;
                while (CurCommentPos<CurEnd) do begin
                  if (Src[CurCommentPos]=#3)
                  and (CurCommentPos+1<CurEnd) and (Src[CurCommentPos+1]='}')
                  then begin
                    CurCommentInnerEnd:=CurCommentPos;
                    inc(CurCommentPos,2);
                    break;
                  end;
                  inc(CurCommentPos);
                end;
              end else begin
                // pascal comment
                if not OuterCommentBounds then CommentStart:=CurCommentPos;
                CommentLvl:=1;
                while (CurCommentPos<CurEnd) do begin
                  case Src[CurCommentPos] of
                  '{': if Scanner.NestedComments then inc(CommentLvl);
                  '}':
                    begin
                      dec(CommentLvl);
                      if (CommentLvl=0) then begin
                        CurCommentInnerEnd:=CurCommentPos;
                        inc(CurCommentPos);
                        break;
                      end;
                    end;
                  end;
                  inc(CurCommentPos);
                end;
              end;
            end;
          '/':  // Delphi comment
            if (CurCommentPos<SrcLen) and (Src[CurCommentPos+1]='/') then
            begin
              inc(CurCommentPos,2);
              if not OuterCommentBounds then CommentStart:=CurCommentPos;
              while (CurCommentPos<CurEnd)
              and (not (Src[CurCommentPos] in [#10,#13])) do
                inc(CurCommentPos);
              CurCommentInnerEnd:=CurCommentPos;
              inc(CurCommentPos);
              if (CurCommentPos<CurEnd)
              and (Src[CurCommentPos] in [#10,#13])
              and (Src[CurCommentPos-1]<>Src[CurCommentPos]) then
                inc(CurCommentPos);
            end else
              break;
          '(': // Turbo pascal comment
            if (CurCommentPos<SrcLen) and (Src[CurCommentPos+1]='*') then
            begin
              inc(CurCommentPos,2);
              if not OuterCommentBounds then CommentStart:=CurCommentPos;
              while (CurCommentPos<CurEnd) do begin
                if (Src[CurCommentPos]='*') and (CurCommentPos+1<CurEnd)
                and (Src[CurCommentPos+1]=')') then
                begin
                  CurCommentInnerEnd:=CurCommentPos;
                  inc(CurCommentPos,2);
                  break;
                end;
                inc(CurCommentPos);
              end;
            end else
              break;
          end;
          if (CurCommentPos>CommentStart) and (CleanPos<CurCommentPos) then
          begin
            // CleanPos in front of comment-end
            if OuterCommentBounds then
              CommentEnd:=CurCommentPos
            else
              CommentEnd:=CurCommentInnerEnd;
            exit(CleanPos>=CommentStart);
          end;
          // next
          CommentStart:=CurCommentPos;
        end else if IsSpaceChar[Src[CommentStart]] then begin
          repeat
            inc(CommentStart);
          until (CommentStart>=CommentEnd)
          or (not (IsSpaceChar[Src[CommentStart]]));
        end else begin
          break;
        end;
      end;
      // CleanPos not in a comment
      exit;
    end else if CurPos.EndPos>CleanPos then begin
      // CleanPos not in a comment
      exit;
    end;
    CleanCodePosInFront:=CurPos.EndPos;
  until CurPos.StartPos>=SrcLen;
end;

function TPascalReaderTool.ExtractPropType(PropNode: TCodeTreeNode;
  InUpperCase, EmptyIfIndexed: boolean): string;
begin
  Result:='';
  if (PropNode=nil)
  or ((PropNode.Desc<>ctnProperty) and (PropNode.Desc<>ctnGlobalProperty)) then
    exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if (PropNode.Desc=ctnProperty) then begin
    if UpAtomIs('CLASS') then ReadNextAtom;
    if (not UpAtomIs('PROPERTY')) then exit;
    ReadNextAtom;
  end;
  AtomIsIdentifierE;
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    if EmptyIfIndexed then exit;
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if CurPos.Flag in [cafSemicolon,cafEND] then exit;
  if not (CurPos.Flag=cafColon) then
    RaiseExceptionFmt(20170421195952,ctsStrExpectedButAtomFound,[':',GetAtom]);
  ReadNextAtom;
  AtomIsIdentifierE;
  if InUpperCase then
    Result:=GetUpAtom
  else
    Result:=GetAtom;
end;

function TPascalReaderTool.ExtractProcName(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  ProcHeadNode: TCodeTreeNode;
  Part: String;
  HasClassName: Boolean;
begin
  Result:='';
  if [phpWithoutClassName,phpWithoutName]*Attr=
     [phpWithoutClassName,phpWithoutName]
  then
    exit;
  while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then exit;
  ProcHeadNode:=ProcNode.FirstChild;
  if (ProcHeadNode=nil) or (ProcHeadNode.StartPos<1) then exit;
  MoveCursorToNodeStart(ProcHeadNode);
  HasClassName:=false;
  repeat
    ReadNextAtom;
    if not AtomIsIdentifier then break;
    if phpInUpperCase in Attr then
      Part:=GetUpAtom
    else
      Part:=GetAtom;
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then begin
      // end of method identifier is the proc name
      if phpWithoutName in Attr then break;
      if Result<>'' then Result:=Result+'.';
      Result:=Result+Part;
      break;
    end;
    if not (phpWithoutClassName in Attr) then begin
      // in front of . is class name
      if Result<>'' then Result:=Result+'.';
      Result:=Result+Part;
      HasClassName:=true;
    end;
  until false;
  if (not HasClassName)
  and ([phpWithoutClassName,phpAddClassName]*Attr=[phpAddClassName]) then begin
    Part:=ExtractClassName(ProcNode,false,true);
    if Part<>'' then
      Result:=Part+'.'+Result;
  end;
end;

function TPascalReaderTool.ExtractProcHead(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  TheClassName, s: string;
  IsClassName, IsProcType, IsProcedure, IsFunction, IsOperator: Boolean;
  EndPos: Integer;
  ParentNode: TCodeTreeNode;
  OldPos: TAtomPosition;
const
  SemiColon : char = ';';

  procedure PrependName(const Prepend: string; var aPath: string);
  begin
    if Prepend='' then exit;
    if aPath<>'' then
      aPath:=Prepend+'.'+aPath
    else
      aPath:=Prepend;
  end;

begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ProcNode=nil) or (ProcNode.StartPos<1) then exit;
  if ProcNode.Desc=ctnProcedureHead then begin
    ProcNode:=ProcNode.Parent;
    if ProcNode=nil then exit;
  end;
  if ProcNode.Desc=ctnReferenceTo then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  if ProcNode.Desc=ctnProcedure then
    IsProcType:=false
  else if ProcNode.Desc=ctnProcedureType then
    IsProcType:=true
  else
    exit;

  TheClassName:='';

  if (phpAddParentProcs in Attr) and (ProcNode.Parent.Desc=ctnProcedure) then begin
    // local proc
    ParentNode:=ProcNode.Parent;
    while ParentNode.Desc=ctnProcedure do begin
      PrependName(ExtractProcName(ParentNode,Attr*[phpInUpperCase]),TheClassName);
      ParentNode:=ParentNode.Parent;
    end;
  end;

  // build full class name
  if ([phpAddClassname,phpWithoutClassName]*Attr=[phpAddClassName]) then
    PrependName(ExtractClassName(ProcNode,phpInUpperCase in Attr,true,Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE]),TheClassName);

  // reparse the clean source
  InitExtraction;
  MoveCursorToNodeStart(ProcNode);
  // parse procedure head = start + name + parameterlist + result type ;
  ExtractNextAtom(false,Attr);
  // read procedure start keyword
  if UpAtomIs('GENERIC') then
    ExtractNextAtom((phpWithStart in Attr)
                    and not (phpWithoutClassKeyword in Attr),Attr);
  if (UpAtomIs('CLASS') or UpAtomIs('STATIC')) then
    ExtractNextAtom((phpWithStart in Attr)
                    and not (phpWithoutClassKeyword in Attr),Attr);
  IsProcedure:=UpAtomIs('PROCEDURE');
  IsFunction:=(not IsProcedure) and UpAtomIs('FUNCTION');
  IsOperator:=(not IsProcedure) and (not IsFunction) and UpAtomIs('OPERATOR');
  if IsProcedure or IsFunction or IsOperator
  or (UpAtomIs('CONSTRUCTOR')) or (UpAtomIs('DESTRUCTOR'))
  then
    ExtractNextAtom(phpWithStart in Attr,Attr)
  else
    exit;
  ExtractProcHeadPos:=phepStart;
  if not IsProcType then begin
    // read name
    if ((not IsOperator)
    or (not WordIsCustomOperator.DoItCaseInsensitive(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)))
    and (not AtomIsIdentifier) then exit;

    if TheClassName<>'' then begin
      s:=TheClassName+'.';
      if phpInUpperCase in Attr then s:=UpperCaseStr(s);
      if ExtractStreamEndIsIdentChar then
        s:=' '+s;
      ExtractMemStream.Write(s[1],length(s));
    end;

    if [phpWithoutClassName,phpWithoutName]*Attr=[] then begin
      // read classname and name
      repeat
        ExtractNextAtom(true,Attr);
        if Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE] then
        begin
          // delphi generics
          if AtomIsChar('<') then
          begin
            //writeln('TPascalReaderTool.ExtractProcHead B ',GetAtom);
            if not ExtractNextSpecializeParams(not (phpWithoutGenericParams in Attr),Attr) then
              exit;
            //writeln('TPascalReaderTool.ExtractProcHead C ',GetAtom);
            ExtractNextAtom(not (phpWithoutGenericParams in Attr),Attr);
          end;
        end;
        if CurPos.Flag<>cafPoint then break;
        ExtractNextAtom(true,Attr);
        if ((not IsOperator)
        or (not WordIsCustomOperator.DoItCaseInsensitive(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)))
        and (not AtomIsIdentifier) then exit;
      until false;
    end else begin
      // read only part of name
      repeat
        OldPos:=CurPos;
        ReadNextAtom;
        if (Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE]) and AtomIsChar('<') then
        begin
          repeat
            ReadNextAtom;
          until AtomIsChar('>') or (CurPos.EndPos > SrcLen);
          ReadNextAtom;
        end;
        IsClassName:=(CurPos.Flag=cafPoint);
        MoveCursorToAtomPos(OldPos);
        if IsClassName then begin
          // read class name
          ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
          if (Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE]) and AtomIsChar('<') then
          begin
            if not ExtractNextSpecializeParams(false,Attr) then
              exit;
            ExtractNextAtom(false,Attr);
          end;
          // read '.'
          ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
          if ((not IsOperator)
          or (not WordIsCustomOperator.DoItCaseInsensitive(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)))
          and (not AtomIsIdentifier) then exit;
        end else begin
          // read name
          ExtractNextAtom(not (phpWithoutName in Attr),Attr);
          if (Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE]) and AtomIsChar('<') then
          begin
            if not ExtractNextSpecializeParams(false,Attr) then
              exit;
            ExtractNextAtom(false,Attr);
          end;
          break;
        end;
      until false;
    end;
    ExtractProcHeadPos:=phepName;
  end;
  // read parameter list
  if (CurPos.Flag=cafRoundBracketOpen) then
    ReadParamList(false,true,Attr);
  ExtractProcHeadPos:=phepParamList;
  if IsOperator and (CurPos.Flag=cafWord) then begin
    // read operator result name
    ExtractNextAtom([phpWithParameterNames,phpWithResultType]*Attr
                   =[phpWithParameterNames,phpWithResultType],Attr);
  end;
  // read result type
  if (CurPos.Flag=cafColon) then begin
    ExtractNextTypeRef(phpWithResultType in Attr,Attr);
    ExtractProcHeadPos:=phepResultType;
  end;
  // read 'of object'
  if UpAtomIs('OF') then begin
    if IsProcType then begin
      ExtractNextAtom(phpWithOfObject in Attr,Attr);
      if not UpAtomIs('OBJECT') then exit;
      ExtractNextAtom(phpWithOfObject in Attr,Attr);
    end;
  end;
  // read semicolon
  if CurPos.Flag=cafSemicolon then
    ExtractNextAtom(not (phpWithoutSemicolon in Attr),Attr);
  // read specifiers
  if [phpWithCallingSpecs,phpWithProcModifiers,phpWithAssembler]*Attr<>[] then begin
    if ProcNode.FirstChild<>nil then
      EndPos:=ProcNode.FirstChild.EndPos
    else
      EndPos:=SrcLen+1;
    while (CurPos.StartPos<EndPos) do begin
      if CurPos.Flag=cafWord then begin
        if IsKeyWordCallingConvention.DoIdentifier(@Src[CurPos.StartPos])
        or ((phpWithAssembler in Attr) and UpAtomIs('ASSEMBLER'))
        then begin
          ExtractNextAtom(true,Attr);
          if not (phpWithProcModifiers in Attr) then
            ExtractMemStream.Write(SemiColon,1);
          continue;
        end;
      end else if (CurPos.Flag=cafEdgedBracketOpen) then begin
        ReadTilBracketClose(false);
      end;
      ExtractNextAtom(phpWithProcModifiers in Attr,Attr);
    end;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
  
  // add semicolon
  if ([phpWithoutSemicolon,phpDoNotAddSemicolon]*Attr=[])
  and (Result<>'') and (Result[length(Result)]<>';') then
    Result:=Result+';';
end;

function TPascalReaderTool.ExtractProcHeadWithGroup(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): TPascalMethodHeader;
begin
  Result.Name := ExtractProcHead(ProcNode, Attr);
  Result.Group := ExtractProcedureGroup(ProcNode);
  if Result.Group=mgClassOperator then
    Result.ResultType := ExtractFuncResultType(ProcNode, Attr);
end;

function TPascalReaderTool.ExtractProcedureHeader(CursorPos: TCodeXYPosition;
  Attributes: TProcHeadAttributes; out ProcHead: string): boolean;
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  ProcHead:='';
  BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,
    [btSetIgnoreErrorPos,btCursorPosOutAllowed]);
  ANode:=FindDeepestNodeAtPos(CleanCursorPos,True);
  while (ANode<>nil) and (ANode.Desc<>ctnProcedure) do
    ANode:=ANode.Parent;
  if ANode=nil then exit;
  ProcHead:=ExtractProcHead(ANode,Attributes);
  Result:=true;
end;

function TPascalReaderTool.ExtractClassName(Node: TCodeTreeNode;
  InUpperCase: boolean; WithParents: boolean; WithGenericParams: boolean
  ): string;
var
  ParamsNode: TCodeTreeNode;
  ParamNode: TCodeTreeNode;
  Params: String;
begin
  Result:='';
  while Node<>nil do begin
    case Node.Desc of
    ctnTypeDefinition:
      begin
        if Result<>'' then Result:='.'+Result;
        Result:=GetIdentifier(@Src[Node.StartPos])+Result;
        if not WithParents then break;
      end;
    ctnGenericType:
      begin
        if Result<>'' then Result:='.'+Result;
        if (Node.Desc = ctnGenericType) then begin
          // extract generic type param names
          if WithGenericParams then begin
            ParamsNode:=Node.FirstChild.NextBrother;
            Params:='';
            while ParamsNode<>nil do begin
              if ParamsNode.Desc=ctnGenericParams then begin
                ParamNode:=ParamsNode.FirstChild;
                while ParamNode<>nil do begin
                  if ParamNode.Desc=ctnGenericParameter then begin
                    if Params<>'' then
                      Params:=Params+',';
                    Params:=Params+GetIdentifier(@Src[ParamNode.StartPos]);
                  end;
                  ParamNode:=ParamNode.NextBrother;
                end;
                Result:='<'+Params+'>'+Result;
              end;
              ParamsNode:=ParamsNode.NextBrother;
            end;
          end;
          Result:=GetIdentifier(@Src[Node.FirstChild.StartPos])+Result;
        end;
        if not WithParents then break;
      end;
    ctnParameterList:
      break;
    end;
    Node:=Node.Parent;
  end;

  if InUpperCase then
    Result:=UpperCaseStr(Result);
end;

function TPascalReaderTool.ExtractClassPath(Node: TCodeTreeNode): string;
var
  InArray: Boolean;
begin
  Result:='';
  InArray:=false;
  while Node<>nil do begin
    case Node.Desc of
    ctnTypeDefinition,ctnGenericType:
      begin
        if Result<>'' then Result:='.'+Result;
        if Node.Desc=ctnTypeDefinition then
          Result:=GetIdentifier(@Src[Node.StartPos])+Result
        else if Node.FirstChild<>nil then
        begin
          if (Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE]) and (Node.Desc = ctnGenericType) then
            Result := Result + ExtractNode(Node.FirstChild.NextBrother, []);
          Result:=GetIdentifier(@Src[Node.FirstChild.StartPos])+Result;
        end;
      end;
    ctnParameterList:
      break;
    ctnRangedArrayType, ctnOpenArrayType:
       begin
         InArray := True;
         Result := '[]' + Result;
       end;
    ctnVarDefinition:
       if InArray then begin
         Result := GetIdentifier(@Src[Node.StartPos]) + Result;
         InArray := False;
       end;
    end;
    Node:=Node.Parent;
  end;
end;

function TPascalReaderTool.ExtractClassInheritance(
  ClassNode: TCodeTreeNode; Attr: TProcHeadAttributes): string;
begin
  Result:='';
  if (ClassNode=nil) or (not (ClassNode.Desc in AllClasses)) then exit;
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // class
  if UpAtomIs('PACKED') then ReadNextAtom;
  if not (UpAtomIs('CLASS') or UpAtomIs('OBJECT') or UpAtomIs('OBJCLASS')
       or (UpAtomIs('INTERFACE')))
  then
    exit;
  ReadNextAtom; // '('
  if CurPos.Flag<>cafRoundBracketOpen then exit;
  ReadNextAtom;
  if not AtomIsIdentifier then exit;
  MoveCursorToCleanPos(CurPos.StartPos);
  ExtractProcHeadPos:=phepNone;
  InitExtraction;
  while (CurPos.StartPos<=SrcLen) do begin
    ExtractNextAtom(true,Attr); // read ancestor/interface
    if not AtomIsIdentifier then break;
    ExtractNextAtom(true,Attr); // read ','
    if not AtomIsChar(',') then break;
  end;
  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode;
  AddParentClasses: boolean; KeepGeneric: boolean): string;
var
  Part: String;
begin
  Result:='';
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then exit;
  MoveCursorToNodeStart(ProcNode);
  repeat
    ReadNextAtom;
    if not AtomIsIdentifier then break;
    Part:=GetAtom;
    ReadNextAtom;
    if (Scanner.CompilerMode in [cmDELPHI,cmDELPHIUNICODE]) and AtomIsChar('<') then
    begin { delphi generics }
      if KeepGeneric then
        Part := Part + GetAtom;
      repeat
        ReadNextAtom;
        if KeepGeneric then
          Part := Part + GetAtom;
      until (CurPos.StartPos > SrcLen) or AtomIsChar('>');
      ReadNextAtom;
    end;
    if (CurPos.Flag<>cafPoint) then break;
    if Result<>'' then Result:=Result+'.';
    Result:=Result+Part;
  until false;
  if not AddParentClasses then exit;
  Part:=ExtractClassName(ProcNode,false,true);
  if Part='' then exit;
  Result:=Part+'.'+Result;
end;

function TPascalReaderTool.FindProcNode(StartNode: TCodeTreeNode;
  const AProcHead: TPascalMethodHeader; Attr: TProcHeadAttributes;
  Visibility: TClassSectionVisibility): TCodeTreeNode;
// search in all next brothers for a Procedure Node with the Name ProcName
// if there are no further brothers and the parent is a section node
// ( e.g. 'interface', 'implementation', ...) or a class visibility node
// (e.g. 'public', 'private', ...) then the search will continue in the next
// section
var
  InClass: Boolean;
  CurProcHead: TPascalMethodHeader;
begin
  Result:=StartNode;
  InClass:=FindClassOrInterfaceNode(StartNode)<>nil;
  while (Result<>nil) do begin
    if Result.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
               and ((Result.SubDesc and ctnsForwardDeclaration)>0)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(Result)<>nil)))
      and (not InClass or IdentNodeIsInVisibleClassSection(Result, Visibility))
      then
      begin
        CurProcHead:=ExtractProcHeadWithGroup(Result,Attr);
        //DebugLn(['TPascalReaderTool.FindProcNode B "',CurProcHead,'" =? "',AProcHead,'" Result=',CompareTextIgnoringSpace(CurProcHead,AProcHead,false)]);
        if (CurProcHead.Name<>'') and
            SameMethodHeaders(AProcHead, CurProcHead)
        then
          exit;
      end;
    end;
    // next node
    if InClass then
      Result:=FindNextIdentNodeInClass(Result)
    else
      Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalReaderTool.FindProcNode(StartNode: TCodeTreeNode;
  const AProcHead: string; AProcSpecType: TPascalMethodGroup;
  Attr: TProcHeadAttributes; Visibility: TClassSectionVisibility): TCodeTreeNode;
var
  ProcHead: TPascalMethodHeader;
begin
  ProcHead.Name := AProcHead;
  ProcHead.Group := AProcSpecType;
  Result := FindProcNode(StartNode, ProcHead, Attr, Visibility);
end;

function TPascalReaderTool.FindCorrespondingProcNode(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): TCodeTreeNode;
var
  ClassNode: TCodeTreeNode;
  StartNode: TCodeTreeNode;
  ProcHead: TPascalMethodHeader;
begin
  Result:=nil;
  // get ctnProcedure
  //debugln('TPascalReaderTool.FindCorrespondingProcNode Start');
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedureHead then begin
    ProcNode:=ProcNode.Parent;
    if (ProcNode=nil) then exit;
  end;
  if ProcNode.Desc<>ctnProcedure then exit;
  
  // check proc kind
  //debugln('TPascalReaderTool.FindCorrespondingProcNode Check kind');
  ClassNode:=FindClassOrInterfaceNode(ProcNode);
  if ClassNode<>nil then begin
    //debugln('TPascalReaderTool.FindCorrespondingProcNode Class');
    // in a class definition -> search method body
    StartNode:=ClassNode.GetTopMostNodeOfType(ctnTypeSection);
  end else if NodeIsMethodBody(ProcNode) then begin
    //debugln('TPascalReaderTool.FindCorrespondingProcNode Method ',ExtractClassNameOfProcNode(ProcNode));
    // in a method body -> search in class
    StartNode:=FindClassNodeInUnit(ExtractClassNameOfProcNode(ProcNode,true),
             true,false,false,true);
    if StartNode=nil then exit;
    if (StartNode<>nil) and (StartNode.Desc in AllClasses)
    then begin
      StartNode:=StartNode.FirstChild;
      while (StartNode<>nil) do begin
        if (StartNode.Desc in AllClassBaseSections)
        and (StartNode.FirstChild<>nil) then begin
          StartNode:=StartNode.FirstChild;
          break;
        end;
        StartNode:=StartNode.NextBrother;
      end;
    end;
  end else begin
    //DebugLn('TPascalReaderTool.FindCorrespondingProcNode Normal');
    // else: search on same lvl
    StartNode:=FindFirstNodeOnSameLvl(ProcNode);
  end;
  if StartNode=nil then exit;

  ProcHead:=ExtractProcHeadWithGroup(ProcNode,Attr);
  //debugln('TPascalReaderTool.FindCorrespondingProcNode StartNode=',StartNode.DescAsString,' ProcHead=',dbgstr(ProcHead),' ',dbgs(Attr),' ',StartNode.DescAsString);
  Result:=FindProcNode(StartNode,ProcHead,Attr);
  if Result=ProcNode then begin
    // found itself -> search further
    StartNode:=FindNextNodeOnSameLvl(Result);
    Result:=FindProcNode(StartNode,ProcHead,Attr);
  end;
  //if Result<>nil then debugln(['TPascalReaderTool.FindCorrespondingProcNode Result=',CleanPosToStr(Result.StartPos),' ',dbgstr(copy(Src,Result.StartPos,50))]);
end;

function TPascalReaderTool.FindCorrespondingProcParamNode(
  ProcParamNode: TCodeTreeNode; Attr: TProcHeadAttributes): TCodeTreeNode;
var
  ProcNode: TCodeTreeNode;
begin
  Result:=nil;
  if ProcParamNode=nil then exit;
  if (ProcParamNode.Desc=ctnVarDefinition)
  and (ProcParamNode.Parent.Desc=ctnParameterList)
  and (ProcParamNode.Parent.Parent.Desc=ctnProcedureHead) then begin
    // this is a parameter name
    ProcNode:=ProcParamNode.GetNodeOfType(ctnProcedure);
    if ProcNode=nil then exit;
    // search alias for parameter
    ProcNode:=FindCorrespondingProcNode(ProcNode,Attr);
    if ProcNode=nil then exit;
    BuildSubTreeForProcHead(ProcNode);
    Result:=ProcNode;
    while (Result<>nil) do begin
      //debugln(['TPascalReaderTool.FindCorrespondingProcParamNode ',dbgstr(copy(Src,Result.StartPos,20))]);
      if Result.Desc
        in [ctnProcedure,ctnProcedureHead,ctnParameterList]
      then
        Result:=Result.FirstChild
      else begin
        if Result.StartPos<1 then break;
        if CompareIdentifiers(@Src[ProcParamNode.StartPos],@Src[Result.StartPos])=0
        then exit;
        Result:=Result.NextBrother;
      end;
    end;
    Result:=nil;
  end;
end;

function TPascalReaderTool.FindDefinitionNameNode(DefinitionNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  if DefinitionNode.Desc=ctnGenericType then
  begin
    if DefinitionNode.FirstChild<>nil then
      Result:=DefinitionNode.FirstChild
    else
      Result:=nil;
  end else
    Result:=DefinitionNode;
end;

function TPascalReaderTool.FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=ProcNode;
  if Result=nil then exit;
  if Result.Desc<>ctnProcedure then exit;
  Result:=Result.LastChild;
  while Result<>nil do begin
    if Result.Desc in [ctnBeginBlock,ctnAsmBlock] then
      exit;
    Result:=Result.PriorBrother;
  end;
end;

function TPascalReaderTool.ProcBodyIsEmpty(ProcNode: TCodeTreeNode): boolean;
var
  BodyNode: TCodeTreeNode;
  LastPos: LongInt;
begin
  Result:=false;
  BodyNode:=FindProcBody(ProcNode);
  if (BodyNode=nil) then exit;
  // check if there are nodes in front (e.g. local variables)
  if (BodyNode.PriorBrother<>nil)
  and (BodyNode.PriorBrother.Desc<>ctnProcedureHead) then
    exit;
  // check if there are child nodes
  if BodyNode.FirstChild<>nil then exit;
  // check if bodynode is only 'asm end' or 'begin end'
  // not even a comment should be there, only spaces are allowed
  if ProcNode.FirstChild.Desc<>ctnProcedureHead then exit;
  MoveCursorToCleanPos(ProcNode.FirstChild.EndPos);
  LastPos:=CurPos.EndPos;
  ReadNextAtom;
  if FindNextNonSpace(Src,LastPos)<>CurPos.StartPos then exit;
  if CurPos.Flag=cafSemicolon then begin
    // semicolon is allowed
    LastPos:=CurPos.EndPos;
    ReadNextAtom;
    if FindNextNonSpace(Src,LastPos)<>CurPos.StartPos then exit;
  end;
  if not (UpAtomIs('ASM') or UpAtomIs('BEGIN')) then exit;
  LastPos:=CurPos.EndPos;
  ReadNextAtom;
  if FindNextNonSpace(Src,LastPos)<>CurPos.StartPos then exit;
  // inherited is allowed
  if UpAtomIs('INHERITED') then begin
    ReadNextAtom;
    if CurPos.Flag=cafSemicolon then begin
      // semicolon is allowed
      LastPos:=CurPos.EndPos;
      ReadNextAtom;
      if FindNextNonSpace(Src,LastPos)<>CurPos.StartPos then exit;
    end;
  end;
  if not UpAtomIs('END') then exit;
  Result:=true;
end;

procedure TPascalReaderTool.MoveCursorToFirstProcSpecifier(ProcNode: TCodeTreeNode);
// After the call,
// CurPos will stand on the first proc specifier or on a semicolon
// this can be 'of object'
begin
  //DebugLn(['TPascalReaderTool.MoveCursorToFirstProcSpecifier ',ProcNode.DescAsString,' ',ProcNode.StartPos]);
  if (ProcNode<>nil) and (ProcNode.Desc in [ctnProcedureType,ctnProcedure]) then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then begin
    RaiseException(20170421195956,'Internal Error in'
      +' TPascalParserTool.MoveCursorFirstProcSpecifier: '
      +' (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)');
  end;
  //DebugLn(['TPascalReaderTool.MoveCursorToFirstProcSpecifier ',ProcNode.DescAsString,' StartPos=',CleanPosToStr(ProcNode.StartPos)]);
  if (ProcNode.LastChild<>nil)
  and (
    (ProcNode.LastChild.Desc=ctnIdentifier)
    or (ProcNode.LastChild.Desc=ctnSpecialize)
  ) then
  begin
    // jump behind function result type
    MoveCursorToCleanPos(ProcNode.LastChild.EndPos);
    ReadNextAtom;
  end else if GetProcParamList(ProcNode)<>nil then begin
    // jump behind parameter list
    MoveCursorToCleanPos(GetProcParamList(ProcNode).EndPos);
    ReadNextAtom;
  end else begin
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    if AtomIsCustomOperator(true,false,false) then begin
      // read name
      ReadNextAtom;
      while (CurPos.Flag=cafPoint) do begin
        ReadNextAtom;
        if CurPos.Flag in [cafPoint,cafRoundBracketOpen,cafEdgedBracketOpen,cafColon,cafEnd,cafSemicolon]
        then break;
        ReadNextAtom;
      end;
    end;
    if (CurPos.Flag=cafRoundBracketOpen) then begin
      // read paramlist
      ReadTilBracketClose(false);
      ReadNextAtom;
    end;
  end;
  if (CurPos.Flag=cafColon) then begin
    // read function result type
    ReadNextAtom;
    SkipTypeReference(false);
  end;
  // CurPos now stands on the first proc specifier or on a semicolon or on the syntax error
end;

function TPascalReaderTool.MoveCursorToProcSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin
  if ProcNode.FirstChild=nil then begin
    exit(false);
  end;
  MoveCursorToFirstProcSpecifier(ProcNode);
  while (CurPos.StartPos<=ProcNode.FirstChild.EndPos) do begin
    if CurPos.Flag=cafSemicolon then begin
      ReadNextAtom;
    end else begin
      if UpAtomIs(ProcedureSpecifierNames[ProcSpec]) then begin
        Result:=true;
        exit;
      end;
      if (CurPos.Flag=cafEdgedBracketOpen) then begin
        ReadTilBracketClose(false);
        ReadNextAtom;
      end else if UpAtomIs('MESSAGE') then begin
        ReadNextAtom;
        ReadConstant(true,false,[]);
      end else if UpAtomIs('EXTERNAL') then begin
        ReadNextAtom;
        if CurPos.Flag<>cafSemicolon then begin
          if not UpAtomIs('NAME') then
            ReadConstant(true,false,[]);
          if UpAtomIs('NAME') or UpAtomIs('INDEX') then begin
            ReadNextAtom;
            ReadConstant(true,false,[]);
          end;
        end;
      end else begin
        ReadNextAtom;
      end;
    end;
  end;
  Result:=false;
end;

procedure TPascalReaderTool.MoveCursorToProcName(ProcNode: TCodeTreeNode;
  SkipClassName: boolean);
begin
  if (ProcNode.Desc=ctnProcedure) and (ProcNode.FirstChild<>nil)
  and (ProcNode.FirstChild.Desc=ctnProcedureHead) then
    ProcNode:=ProcNode.FirstChild;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if (ProcNode.Desc=ctnProcedure) then begin
    if UpAtomIs('CLASS') then ReadNextAtom;
    ReadNextAtom; // skip proc keyword
  end;
  if not SkipClassName then exit;
  repeat
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then begin
      UndoReadNextAtom;
      break;
    end;
    ReadNextAtom;
  until not AtomIsIdentifier;
end;

procedure TPascalReaderTool.MoveCursorBehindProcName(ProcNode: TCodeTreeNode);
begin
  if (ProcNode.FirstChild<>nil)
  and (ProcNode.FirstChild.Desc=ctnProcedureHead) then
    ProcNode:=ProcNode.FirstChild;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if AtomIsIdentifier then begin
    ReadNextAtom;
    while CurPos.Flag=cafPoint do begin
      ReadNextAtom;
      if not AtomIsIdentifier then exit;
      ReadNextAtom;
    end;
  end else if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen,cafColon]
  then begin
  end else begin
    // operator
    ReadNextAtom;
  end;
end;

function TPascalReaderTool.PositionInProcName(ProcNode: TCodeTreeNode;
  SkipClassName: boolean; CleanPos: integer): boolean;
begin
  if (ProcNode.Desc=ctnProcedure) and (ProcNode.FirstChild<>nil)
  and (ProcNode.FirstChild.Desc=ctnProcedureHead) then
    ProcNode:=ProcNode.FirstChild;
  if (CleanPos<ProcNode.StartPos) or (CleanPos>ProcNode.EndPos) then exit(false);
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if (ProcNode.Desc=ctnProcedure) then begin
    if UpAtomIs('CLASS') then ReadNextAtom;
    ReadNextAtom; // skip proc keyword
  end;
  if CleanPos<CurPos.StartPos then exit(false);
  while CurPos.Flag=cafWord do begin
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then begin
      UndoReadNextAtom;
      break;
    end;
    ReadNextAtom;
  end;
  // CurPos is now on the proc name
  if CleanPos>CurPos.EndPos then exit(false);
  if SkipClassName and (CleanPos<CurPos.StartPos) then exit(false);
  Result:=true;
end;

function TPascalReaderTool.PositionInFuncResultName(ProcNode: TCodeTreeNode;
  CleanPos: integer): boolean;
// true if position between ) and :
var
  Node: TCodeTreeNode;
begin
  Result:=false;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnProcedure then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  if (ProcNode.Desc in [ctnIdentifier,ctnVarDefinition])
  and (ProcNode.Parent<>nil)
  and (ProcNode.Parent.Desc=ctnProcedureHead)
  and (CleanPos>=ProcNode.StartPos) and (CleanPos<=ProcNode.EndPos) then begin
    exit(true);
  end;
  if ProcNode.Desc=ctnProcedureHead then begin
    Node:=ProcNode.FirstChild;
    while (Node<>nil) and (Node.Desc<>ctnIdentifier) do begin
      if (Node.Desc=ctnIdentifier)
      and (CleanPos>=Node.StartPos) and (CleanPos<=Node.EndPos) then
        exit(true);
      Node:=Node.NextBrother;
    end;
  end;
  // read behind parameter list
  if ProcNode.Desc<>ctnProcedureHead then exit;
  if (ProcNode.FirstChild<>nil) and (ProcNode.FirstChild.Desc=ctnParameterList)
  then begin
    if (CleanPos<ProcNode.FirstChild.EndPos) then
      exit;
    MoveCursorToCleanPos(ProcNode.FirstChild.EndPos);
  end else begin
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    while AtomIsIdentifier do begin
      ReadNextAtom;
      if (CurPos.Flag<>cafPoint) then break;
      ReadNextAtom;
    end;
    if CurPos.Flag=cafRoundBracketOpen then
      if not ReadTilBracketClose(false) then exit;
  end;
  if CurPos.StartPos>CleanPos then exit;
  // read optional result variable (e.g. operator can have them)
  ReadNextAtom;
  if AtomIsIdentifier then ReadNextAtom;
  if CurPos.Flag<>cafColon then exit;
  Result:=CleanPos<=CurPos.StartPos;
end;

function TPascalReaderTool.MoveCursorToPropType(PropNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (PropNode=nil)
  or ((PropNode.Desc<>ctnProperty) and (PropNode.Desc<>ctnGlobalProperty)) then
    exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if (PropNode.Desc=ctnProperty) then begin
    if UpAtomIs('CLASS') then ReadNextAtom;
    if (not UpAtomIs('PROPERTY')) then exit;
    ReadNextAtom;
  end;
  if not AtomIsIdentifier then exit;
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if CurPos.Flag in [cafSemicolon,cafEND] then exit;
  if CurPos.Flag<>cafColon then exit;
  ReadNextAtom;
  Result:=CurPos.Flag=cafWord;
end;

function TPascalReaderTool.MoveCursorToPropName(PropNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (PropNode=nil)
  or ((PropNode.Desc<>ctnProperty) and (PropNode.Desc<>ctnGlobalProperty)) then
    exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if (PropNode.Desc=ctnProperty) then begin
    if UpAtomIs('CLASS') then ReadNextAtom;
    if (not UpAtomIs('PROPERTY')) then exit;
    ReadNextAtom;
  end;
  Result:=CurPos.Flag=cafWord;
end;

procedure TPascalReaderTool.MoveCursorBehindPropName(PropNode: TCodeTreeNode);
begin
  if (PropNode=nil)
  or ((PropNode.Desc<>ctnProperty) and (PropNode.Desc<>ctnGlobalProperty)) then
    exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if (PropNode.Desc=ctnProperty) then begin
    if UpAtomIs('CLASS') then ReadNextAtom;
    if (not UpAtomIs('PROPERTY')) then exit;
    ReadNextAtom;
  end;
  if not AtomIsIdentifier then exit;
  ReadNextAtom;
end;

function TPascalReaderTool.GetProcNameIdentifier(ProcNode: TCodeTreeNode): PChar;
begin

  // ToDo: ppu, dcu

  Result:=nil;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnProcedure then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  MoveCursorToNodeStart(ProcNode);
  repeat
    ReadNextAtom;
    if not AtomIsIdentifier then exit(nil);
    Result:=@Src[CurPos.StartPos];
    ReadNextAtom;
  until CurPos.Flag<>cafPoint;
end;

function TPascalReaderTool.ExtractNode(ANode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ANode=nil) or (ANode.StartPos<1) then exit;
  InitExtraction;
  // reparse the clean source
  MoveCursorToNodeStart(ANode);
  while (ANode.EndPos>CurPos.StartPos)
  and (CurPos.StartPos<=SrcLen) do
    ExtractNextAtom(true,Attr);
  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.ExtractCode(StartPos, EndPos: integer;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (StartPos<1) or (StartPos>=EndPos) or (StartPos>SrcLen) then exit;
  InitExtraction;
  // reparse the clean source
  MoveCursorToCleanPos(StartPos);
  while (EndPos>CurPos.StartPos)
  and (CurPos.StartPos<=SrcLen) do
    ExtractNextAtom(true,Attr);
  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.ExtractBrackets(BracketStartPos: integer;
  Attr: TProcHeadAttributes): string;

  function ExtractTilBracketClose(ExtractBrackets: boolean): boolean;
  var
    CloseBracket: TCommonAtomFlag;
    First: Boolean;
  begin
    Result:=true;
    case CurPos.Flag of
    cafRoundBracketOpen: CloseBracket:=cafRoundBracketClose;
    cafEdgedBracketOpen: CloseBracket:=cafEdgedBracketClose;
    else exit;
    end;
    First:=true;
    repeat
      if First then
        ExtractNextAtom(ExtractBrackets,Attr)
      else
        ExtractNextAtom(true,Attr);
      if CurPos.StartPos>SrcLen then exit;
      if CurPos.Flag=CloseBracket then exit(true);
      if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then begin
        if not ExtractTilBracketClose(true) then exit;
      end;
    until false;
  end;

begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (BracketStartPos<1) or (BracketStartPos>SrcLen) then exit;
  InitExtraction;
  // reparse the clean source
  MoveCursorToCleanPos(BracketStartPos);
  ReadNextAtom;
  if not ExtractTilBracketClose(not (phpWithoutBrackets in Attr)) then exit;
  if not (phpWithoutBrackets in Attr) then
    ExtractNextAtom(true,Attr);
  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.ExtractIdentifierWithPoints(StartPos: integer;
  ExceptionOnError: boolean): string;
begin
  Result:='';
  MoveCursorToCleanPos(StartPos);
  ReadNextAtom;
  if not AtomIsIdentifierE(ExceptionOnError) then exit;
  Result:=GetAtom;
  repeat
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then
      exit;
    ReadNextAtom;
    if not AtomIsIdentifierE(ExceptionOnError) then exit;
    Result+='.'+GetAtom;
  until false;
end;

function TPascalReaderTool.ExtractNextTypeRef(Add: boolean;
  const Attr: TProcHeadAttributes): boolean;
begin
  Result:=false;
  ExtractNextAtom(Add,Attr);
  if (Scanner.CompilerMode in [cmOBJFPC]) and UpAtomIs('SPECIALIZE') then
    ExtractNextAtom(Add,Attr);
  if not AtomIsIdentifier then exit;
  ExtractNextAtom(Add,Attr);
  repeat
    if CurPos.Flag=cafPoint then begin
      ExtractNextAtom(Add,Attr);
      if not AtomIsIdentifier then exit;
      ExtractNextAtom(Add,Attr);
    end else if AtomIsChar('<') then begin
      if not ExtractNextSpecializeParams(Add,Attr) then
        exit;
      ExtractNextAtom(Add,Attr);
    end else
      break;
  until false;
  Result:=true;
end;

function TPascalReaderTool.ExtractNextSpecializeParams(Add: boolean;
  const Attr: TProcHeadAttributes): boolean;
// at start: CurPos is at <
// at end: CurPos is at >

  function ExtractNextTil(c: char): boolean;
  var
    CurC: Char;
  begin
    Result:=false;
    repeat
      ExtractNextAtom(Add,Attr);
      if CurPos.EndPos-CurPos.StartPos=1 then begin
        CurC:=Src[CurPos.StartPos];
        if CurC=c then
          exit(true);
        case CurC of
        '<': if not ExtractNextTil('>') then exit;
        '(': if not ExtractNextTil(')') then exit;
        '[': if not ExtractNextTil(']') then exit;
        ')',']': exit;
        end;
      end else if CurPos.StartPos>SrcLen then
        exit(false);
    until false;
  end;

begin
  Result:=ExtractNextTil('>');
end;

function TPascalReaderTool.ExtractPropName(PropNode: TCodeTreeNode;
  InUpperCase: boolean): string;
begin
  Result:='';
  if not MoveCursorToPropName(PropNode) then exit;
  if InUpperCase then
    Result:=GetUpAtom
  else
    Result:=GetAtom;
end;

function TPascalReaderTool.ExtractProperty(PropNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (PropNode=nil) or (PropNode.StartPos<1)
  or ((PropNode.Desc<>ctnProperty) and (PropNode.Desc<>ctnGlobalProperty)) then
    exit;
  // start extraction
  InitExtraction;
  MoveCursorToNodeStart(PropNode);
  ExtractNextAtom(false,Attr);
  if (PropNode.Desc=ctnProperty) then begin
    if UpAtomIs('CLASS') then
      ExtractNextAtom(phpWithStart in Attr,Attr);
    // parse 'property'
    ExtractNextAtom(phpWithStart in Attr,Attr);
  end;
  ExtractProcHeadPos:=phepStart;
  // parse name
  ExtractNextAtom(not (phpWithoutName in Attr),Attr);
  ExtractProcHeadPos:=phepName;
  // read parameter list
  if (CurPos.Flag=cafEdgedBracketOpen) then
    ReadParamList(false,true,Attr);
  ExtractProcHeadPos:=phepParamList;
  // read result type
  if (CurPos.Flag=cafColon) then begin
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if not AtomIsIdentifier then exit;
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if CurPos.Flag=cafPoint then begin
      // unit.type
      ExtractNextAtom(phpWithResultType in Attr,Attr);
      if not AtomIsIdentifier then exit;
      ExtractNextAtom(phpWithResultType in Attr,Attr);
    end;
    ExtractProcHeadPos:=phepResultType;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.GetPropertyNameIdentifier(PropNode: TCodeTreeNode): PChar;
begin
  // ToDo: ppu, dcu

  Result:=nil;
  if PropNode=nil then exit;
  if not MoveCursorToPropName(PropNode) then exit;
  Result:=@Src[CurPos.StartPos];
end;

function TPascalReaderTool.GetPropertyTypeIdentifier(PropNode: TCodeTreeNode): PChar;
begin

  // ToDo: ppu, dcu

  Result:=nil;
  if PropNode=nil then exit;
  if not MoveCursorToPropType(PropNode) then exit;
  Result:=@Src[CurPos.StartPos];
end;

function TPascalReaderTool.PositionInPropertyName(PropNode: TCodeTreeNode;
  CleanPos: integer): boolean;
begin
  if PropNode=nil then exit(false);
  MoveCursorToNodeStart(PropNode);
  if (PropNode.Desc=ctnProperty) then begin
    ReadNextAtom; // read 'property'
    if UpAtomIs('CLASS') then ReadNextAtom;
  end;
  ReadNextAtom; // read name
  Result:=(CurPos.Flag=cafWord)
          and (CleanPos>=CurPos.StartPos) and (CleanPos<=CurPos.EndPos);
end;

function TPascalReaderTool.ExtractIdentCharsFromStringConstant(StartPos,
  MinPos, MaxPos, MaxLen: integer): string;
var
  APos: Integer;
  IdentStartPos: Integer;
  IdentStr: String;
  IdentEndPos: LongInt;
begin
  Result:='';
  APos:=StartPos;
  while APos<SrcLen do begin
    if (Src[APos]='#') then begin
      // skip char constant
      inc(APos);
      if IsNumberChar[Src[APos]] then begin
        while (APos<CurPos.EndPos) and IsNumberChar[Src[APos]] do
          inc(APos)
      end else if Src[APos]='$' then begin
        while (APos<CurPos.EndPos) and IsHexNumberChar[Src[APos]] do
          inc(APos);
      end;
    end else if (Src[APos]='''') then begin
      inc(APos);
      repeat
        // read identifier chars
        IdentStartPos:=APos;
        while (APos<SrcLen) and (IsIdentChar[Src[APos]]) do
          inc(APos);
        IdentEndPos:=APos;
        if IdentStartPos<MinPos then IdentStartPos:=MinPos;
        if IdentEndPos>MaxPos then IdentEndPos:=MaxPos;
        if (IdentEndPos>IdentStartPos) then begin
          if IdentEndPos-IdentStartPos+length(Result)>MaxLen then
            IdentEndPos:=IdentStartPos+MaxLen-length(Result);
          IdentStr:=copy(Src,IdentStartPos,IdentEndPos-IdentStartPos);
          if (IdentStr<>'') then begin
            IdentStr[1]:=UpChars[IdentStr[1]];
            Result:=Result+IdentStr;
          end;
        end;
        // skip non identifier chars
        while (APos<SrcLen) and (Src[APos]<>'''')
        and (not IsIdentChar[Src[APos]])
        do
          inc(APos);
      until (APos>=SrcLen) or (Src[APos]='''') or (length(Result)>=MaxLen);
      inc(APos);
    end else
      break;
  end;
end;

function TPascalReaderTool.ReadStringConstantValue(StartPos: integer): string;
// reads a string constant and returns the resulting string
var
  Run: Integer;
  NumberStart: PChar;
  ResultLen: Integer;
  Number: Integer;
  p: PChar;
begin
  Result:='';
  if StartPos>SrcLen then exit;
  // first read and calculate the resulting length, then copy the chars
  for Run:=1 to 2 do begin
    ResultLen:=0;
    p:=@Src[StartPos];
    while true do begin
      case p^ of
      '''':
        begin
          // read string
          inc(p);
          while true do begin
            if p^='''' then begin
              if p[1]='''' then begin
                // a double ' means a single '
                inc(ResultLen);
                if Run=2 then Result[ResultLen]:='''';
                inc(p,2);
              end else begin
                // a single ' means end of string constant
                inc(p);
                break;
              end;
            end else begin
              // normal char
              inc(ResultLen);
              if Run=2 then Result[ResultLen]:=p^;
              inc(p);
            end;
          end;
        end;
      '#':
        begin
          // read char constant
          inc(p);
          NumberStart:=p;
          if IsNumberChar[p^] then begin
            // read decimal number
            while IsNumberChar[p^] do
              inc(p);
            Number:=StrToIntDef(copy(Src,NumberStart-PChar(Src)+1,p-NumberStart),-1);
          end else if p^='$' then begin
            // read hexnumber
            inc(p);
            while IsHexNumberChar[p^] do
              inc(p);
            Number:=HexStrToIntDef(NumberStart,-1);
          end else
            Number:=-1;
          // add special character
          if (Number<0) or (Number>255) then break;
          inc(ResultLen);
          if Run=2 then Result[ResultLen]:=chr(Number);
        end;
      '^':
        begin
          inc(p);
          if p^ in ['A'..'Z'] then begin
            inc(ResultLen);
            if Run=2 then Result[ResultLen]:=chr(ord(p^)-ord('A'));
          end else begin
            break;
          end;
        end;
      else
        break;
      end;
    end;
    if Run=1 then SetLength(Result,ResultLen);
  end;
end;

function TPascalReaderTool.GetNodeIdentifier(Node: TCodeTreeNode): PChar;
begin
  Result:=nil;
  if (Node=nil) or (Node.StartPos>SrcLen) then exit;
  case Node.Desc of
  ctnProcedure,ctnProcedureHead:
    Result:=GetProcNameIdentifier(Node);
  ctnProperty:
     Result:=GetPropertyNameIdentifier(Node);
  ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition,
  ctnEnumIdentifier,ctnIdentifier,ctnSrcName:
    Result:=@Src[Node.StartPos];
  end;
end;

function TPascalReaderTool.GetHintModifiers(Node: TCodeTreeNode): TPascalHintModifiers;

  function IsHintModifier: boolean;
  begin
    if CurPos.Flag<>cafWord then exit(false);
    Result:=true;
    if UpAtomIs('PLATFORM') then
      Include(GetHintModifiers,phmPlatform)
    else if UpAtomIs('UNIMPLEMENTED') then
      Include(GetHintModifiers,phmUnimplemented)
    else if UpAtomIs('LIBRARY') then
      Include(GetHintModifiers,phmLibrary)
    else if UpAtomIs('EXPERIMENTAL') then
      Include(GetHintModifiers,phmExperimental)
    else if UpAtomIs('DEPRECATED') then
      Include(GetHintModifiers,phmDeprecated)
    else
      Result:=false;
  end;

begin
  Result:=[];
  if Node=nil then exit;
  case Node.Desc of

  ctnProgram,ctnPackage,ctnLibrary,ctnUnit:
    begin
      MoveCursorToNodeStart(Node);
      ReadNextAtom;
      if not (UpAtomIs('PROGRAM') or UpAtomIs('PACKAGE') or UpAtomIs('LIBRARY')
        or UpAtomIs('UNIT')) then exit;
      ReadNextAtom;// name
      while IsHintModifier do ReadNextAtom;
    end;

  ctnProcedureHead:
    begin
      MoveCursorToFirstProcSpecifier(Node);
      // ToDo:
    end;

  ctnProcedure,ctnProcedureType:
    begin
      Node:=Node.FirstChild;
      if Node=nil then exit;
      MoveCursorToFirstProcSpecifier(Node);
      // ToDo:
    end;

  ctnReferenceTo:
    begin
      Node:=Node.FirstChild;
      if (Node=nil) or (Node.Desc<>ctnProcedureType) then exit;
      Node:=Node.FirstChild;
      if Node=nil then exit;
      MoveCursorToFirstProcSpecifier(Node);
      // ToDo:
    end;

  ctnProperty:
    begin
      Node:=Node.LastChild;
      while Node<>nil do begin
        if Node.Desc=ctnHintModifier then begin
          MoveCursorToNodeStart(Node);
          ReadNextAtom;
          IsHintModifier;
        end;
        Node:=Node.PriorBrother;
      end;
    end;

  ctnVarDefinition,ctnConstant,ctnConstDefinition,
  ctnTypeDefinition,ctnGenericType:
    begin
      Node:=FindTypeNodeOfDefinition(Node);
      if Node=nil then exit;
      while (Node<>nil) do begin
        if Node.Desc=ctnHintModifier then begin
          MoveCursorToNodeStart(Node);
          ReadNextAtom;
          IsHintModifier;
        end;
        Node:=Node.NextBrother;
      end;
    end;

  end;
end;

procedure TPascalReaderTool.ForEachIdentifierInCleanSrc(StartPos,
  EndPos: integer; SkipComments: boolean; Node: TCodeTreeNode;
  const OnIdentifier: TOnEachPRIdentifier; Data: pointer; var Abort: boolean);
var
  CommentLvl: Integer;
  InStrConst: Boolean;
  p: PChar;
  EndP: Pointer;
  Range: TEPRIRange;

  procedure SkipIdentifier; inline;
  begin
    while (p<EndP) and IsIdentChar[p^] do inc(p);
  end;

begin
  //debugln(['TPascalReaderTool.ForEachIdentifierInCleanSrc Node=',Node.DescAsString,' "',dbgstr(Src,StartPos,EndPos-StartPos),'"']);
  if (StartPos<1) then
    StartPos:=1;
  if StartPos>SrcLen then exit;
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  if StartPos>=EndPos then exit;
  Range:=epriInCode;
  p:=@Src[StartPos];
  EndP:=p+EndPos-StartPos;
  while p<EndP do begin
    case p^ of

    '{':
      begin
        inc(p);
        if p^=#3 then begin
          // codetools skip comment {#3 #3}
          inc(p);
          repeat
            if p>=EndP then exit;
            if (p^=#3) and (p[1]='}')
            then begin
              inc(p,2);
              break;
            end;
            inc(p);
          until false;
        end else begin
          // pascal comment {}
          CommentLvl:=1;
          InStrConst:=false;
          if p^='$' then
            Range:=epriInDirective
          else
            Range:=epriInComment;
          repeat
            if p>=EndP then exit;
            case p^ of
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then break;
              end;
            'a'..'z','A'..'Z','_':
              if not InStrConst then begin
                if not SkipComments then begin
                  OnIdentifier(Self,p-PChar(Src)+1,Range,Node,Data,Abort);
                  SkipIdentifier;
                  if Abort then exit;
                end;
                while (p<EndP) and IsIdentChar[p^] do inc(p);
              end;
            '''':
              InStrConst:=not InStrConst;
            #10,#13:
              InStrConst:=false;
            end;
            inc(p);
          until false;
          inc(p);
          //debugln(StartPos,' ',copy(Src,CommentStart,StartPos-CommentStart));
        end;
      end;

    '/':  // Delphi comment
      if p[1]<>'/' then begin
        inc(p);
      end else begin
        inc(p,2);
        InStrConst:=false;
        repeat
          if p>=EndP then exit;
          case p^ of
          #10,#13:
            break;
          'a'..'z','A'..'Z','_':
            if not InStrConst then begin
              if not SkipComments then begin
                OnIdentifier(Self,p-PChar(Src)+1,Range,Node,Data,Abort);
                SkipIdentifier;
                if Abort then exit;
              end;
              while (p<EndP) and IsIdentChar[p^] do inc(p);
            end;
          '''':
            InStrConst:=not InStrConst;
          end;
          inc(p);
        until false;
        inc(p);
        if (p<EndP) and (p^ in [#10,#13])
        and (p[-1]<>p^) then
          inc(p);
      end;

    '(': // turbo pascal comment
      if (p[1]<>'*') then begin
        inc(p);
      end else begin
        inc(p,3);
        InStrConst:=false;
        repeat
          if p>=EndP then exit;
          case p^ of
          ')':
            if p[-1]='*' then break;
          'a'..'z','A'..'Z','_':
            if not InStrConst then begin
              if not SkipComments then begin
                OnIdentifier(Self,p-PChar(Src)+1,Range,Node,Data,Abort);
                SkipIdentifier;
                if Abort then exit;
              end;
              SkipIdentifier;
            end;
          '''':
            InStrConst:=not InStrConst;
          #10,#13:
            InStrConst:=false;
          end;
          inc(p);
        until false;
        inc(p);
      end;

    'a'..'z','A'..'Z','_':
      begin
        OnIdentifier(Self,p-PChar(Src)+1,epriInCode,Node,Data,Abort);
        SkipIdentifier;
        if Abort then exit;
      end;

    '''':
      begin
        // skip string constant
        inc(p);
        while p<EndP do begin
          if (not (p^ in ['''',#10,#13])) then
            inc(p)
          else begin
            inc(p);
            break;
          end;
        end;
      end;

    else
      inc(p);
    end;
  end;
end;

procedure TPascalReaderTool.ForEachIdentifierInNode(Node: TCodeTreeNode;
  SkipComments: boolean; const OnIdentifier: TOnEachPRIdentifier;
  Data: Pointer; var Abort: boolean);
var
  StartPos: Integer;
  EndPos: Integer;
  Child: TCodeTreeNode;
begin
  //debugln(['TPascalReaderTool.ForEachIdentifierInNode START ',Node.DescAsString]);
  if NodeNeedsBuildSubTree(Node) then
    BuildSubTree(Node);
  if Node.FirstChild<>nil then begin
    EndPos:=Node.StartPos;
    Child:=Node.FirstChild;
    while Child<>nil do begin
      // scan in front of child
      ForEachIdentifierInCleanSrc(EndPos,Child.StartPos,SkipComments,
        Node,OnIdentifier,Data,Abort);
      if Abort then exit;
      // scan child
      ForEachIdentifierInNode(Child,SkipComments,OnIdentifier,Data,Abort);
      if Abort then exit;
      EndPos:=Child.EndPos;
      Child:=Child.NextBrother;
    end;
    // scan behind children
    ForEachIdentifierInCleanSrc(EndPos,Node.EndPos,SkipComments,
      Node,OnIdentifier,Data,Abort);
  end else begin
    // leaf node
    StartPos:=Node.StartPos;
    EndPos:=Node.EndPos;
    // nodes without children can overlap with their NextBrother
    if (Node.NextBrother<>nil)
    and (Node.NextBrother.StartPos<EndPos) then
      EndPos:=Node.NextBrother.StartPos;
    // scan node range
    ForEachIdentifierInCleanSrc(StartPos,EndPos,SkipComments,
      Node,OnIdentifier,Data,Abort);
  end;
end;

procedure TPascalReaderTool.ForEachIdentifier(SkipComments: boolean;
  const OnIdentifier: TOnEachPRIdentifier; Data: Pointer);
var
  Node: TCodeTreeNode;
  Abort: boolean;
begin
  //debugln(['TPascalReaderTool.ForEachIdentifier START']);
  Node:=Tree.Root;
  Abort:=false;
  while Node<>nil do begin
    ForEachIdentifierInNode(Node,SkipComments,OnIdentifier,Data,Abort);
    if Abort then exit;
    Node:=Node.NextBrother;
  end;
end;

function TPascalReaderTool.FindVarNode(StartNode: TCodeTreeNode;
  const UpperVarName: string; Visibility: TClassSectionVisibility
  ): TCodeTreeNode;
var
  InClass: Boolean;
begin
  Result:=StartNode;
  InClass:=FindClassOrInterfaceNode(StartNode)<>nil;
  while Result<>nil do begin
    if (Result.Desc=ctnVarDefinition)
    and (not InClass or IdentNodeIsInVisibleClassSection(Result, Visibility))
    and (CompareNodeIdentChars(Result,UpperVarName)=0) then
      exit;
    if InClass then
      Result:=FindNextIdentNodeInClass(Result)
    else
      Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalReaderTool.FindTypeNodeOfDefinition(
  DefinitionNode: TCodeTreeNode): TCodeTreeNode;
// for example: 'var a,b,c: integer;'  only c has a type child
begin
  Result:=DefinitionNode;
  while (Result<>nil)
  and (Result.Desc in AllIdentifierDefinitions) do begin
    if (Result.FirstChild<>nil) then begin
      Result:=Result.FirstChild;
      if Result.Desc=ctnGenericName then begin
        // skip generic name and params
        Result:=Result.NextBrother;
        if Result=nil then exit;
        Result:=Result.NextBrother;
        if Result=nil then exit;
      end;
      if (not (Result.Desc in AllPascalTypes)) then
        Result:=nil;
      exit;
    end;
    if Result.Desc<>ctnVarDefinition then exit(nil);
    Result:=Result.NextBrother;
  end;
end;

function TPascalReaderTool.FindClassNode(StartNode: TCodeTreeNode;
  const AClassName: string; IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
// search for class like types on same level
var
  ANode, CurClassNode: TCodeTreeNode;
  NameNode: TCodeTreeNode;
  p: PChar;
begin
  ANode:=StartNode;
  Result:=nil;
  if AClassName='' then exit;
  p:=PChar(AClassName);
  while (ANode<>nil) do begin
    if ANode.Desc in [ctnTypeDefinition,ctnGenericType] then begin
      //debugln(['TPascalReaderTool.FindClassNode ',GetIdentifier(@Src[ANode.StartPos])]);
      CurClassNode:=FindTypeNodeOfDefinition(ANode);
      if (CurClassNode<>nil)
      and (CurClassNode.Desc in AllClassObjects) then begin
        if (not (IgnoreForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)>0)))
        and (not (IgnoreNonForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)=0)))
        then begin
          NameNode:=ANode;
          if (ANode.Desc=ctnGenericType) and (ANode.FirstChild<>nil) then
            NameNode:=ANode.FirstChild;
          //debugln(['TPascalReaderTool.FindClassNode class name = "',GetIdentifier(@Src[NameNode.StartPos]),'"']);
          if NameNode.StartPos>SrcLen then exit;
          if CompareIdentifiers(p,@Src[NameNode.StartPos])=0 then begin
            Result:=FindNestedClass(CurClassNode,p,true);
            exit;
          end;
        end;
      end;
    end;
    // next node
    if (ANode.Desc in [ctnTypeSection]+AllCodeSections)
    and (ANode.FirstChild<>nil) then
      ANode:=ANode.FirstChild
    else if ANode.NextBrother<>nil then
      ANode:=ANode.NextBrother
    else begin
      // skip procs, const and var sections
      repeat
        ANode:=ANode.Parent;
        if (ANode=nil) then exit;
        if (not (ANode.Desc in [ctnTypeSection]+AllCodeSections)) then exit;
        if ANode.NextBrother<>nil then begin
          ANode:=ANode.NextBrother;
          break;
        end;
      until false;
    end;
  end;
end;

function TPascalReaderTool.FindClassNodeBackwards(StartNode: TCodeTreeNode;
  const AClassName: string; IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
var
  ANode: TCodeTreeNode;
  CurClassNode: TCodeTreeNode;
  p: PChar;
begin
  ANode:=StartNode;
  p:=PChar(AClassName);
  while ANode<>nil do begin
    if ANode.Desc=ctnTypeDefinition then begin
      CurClassNode:=ANode.FirstChild;
      if (CurClassNode<>nil)
      and (CurClassNode.Desc in AllClassObjects) then begin
        if (not (IgnoreForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)>0)))
        and (not (IgnoreNonForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)=0)))
        then begin
          if CompareIdentifiers(p,@Src[ANode.StartPos])=0 then begin
            Result:=FindNestedClass(CurClassNode,p,true);
            exit;
          end;
        end;
      end;
    end;
    if ANode.PriorBrother<>nil then begin
      ANode:=ANode.PriorBrother;
      if (ANode.FirstChild<>nil) and (ANode.Desc in AllCodeSections) then
        ANode:=ANode.LastChild;
      if (ANode.FirstChild<>nil) and (ANode.Desc in AllDefinitionSections) then
        ANode:=ANode.LastChild;
    end else begin
      ANode:=ANode.Parent;
    end;
  end;
  Result:=nil;
end;

function TPascalReaderTool.FindNestedClass(RootClassNode: TCodeTreeNode;
  AClassName: PChar; SkipFirst: boolean): TCodeTreeNode;
var
  p: PChar;
  Node: TCodeTreeNode;
  EndNode: TCodeTreeNode;
begin
  Result:=nil;
  if RootClassNode=nil then exit;
  if AClassName=nil then exit;
  p:=AClassName;
  if SkipFirst then begin
    while IsIdentChar[p^] do inc(p);
    if p^='<' then
    begin
      while not (p^ in [#0,'>']) do Inc(p);
      if p^ = '>' then Inc(p);
    end;
    if p^=#0 then exit(RootClassNode);
    if p^<>'.' then exit;
    inc(p);
  end;
  //debugln(['TPascalReaderTool.FindNestedClass p="',p,'"']);
  if not IsIdentStartChar[p^] then exit;
  EndNode:=RootClassNode.NextSkipChilds;
  Node:=RootClassNode.Next;
  while Node<>EndNode do begin
    // debugln(['TPascalReaderTool.FindNestedClass Node=',node.DescAsString]);
    if Node.Desc in [ctnTypeDefinition,ctnGenericType] then begin
      if (Node.LastChild<>nil) and (Node.LastChild.Desc in AllClasses) then begin
        if ((Node.Desc=ctnTypeDefinition)
          and (CompareIdentifierPtrs(p,@Src[Node.StartPos])=0))
        or ((Node.FirstChild.Desc=ctnGenericName)
          and (CompareIdentifierPtrs(p,@Src[Node.FirstChild.StartPos])=0))
        then begin
          // class found
          Node:=Node.LastChild;
          while IsIdentChar[p^] do inc(p);
          if p^=#0 then exit(Node);
          if p^<>'.' then exit;
          Result:=FindNestedClass(Node,p+1,false);
          exit;
        end;
      end;
    end;
    if Node.Desc in AllClassSections then
      Node:=Node.Next
    else
      Node:=Node.NextSkipChilds;
  end;
end;

function TPascalReaderTool.FindClassNode(CursorNode: TCodeTreeNode): TCodeTreeNode;
// find class node of a node in a procedure (declaration or body)
begin
  while CursorNode<>nil do begin
    if CursorNode.Desc in AllClassObjects then begin
      Result:=CursorNode;
      exit;
    end else if NodeIsMethodBody(CursorNode) then begin
      Result:=FindClassNodeForMethodBody(CursorNode,true,false);
      exit;
    end;
    CursorNode:=CursorNode.Parent;
  end;
  Result:=nil;
end;

function TPascalReaderTool.FindClassNodeForMethodBody(ProcNode: TCodeTreeNode;
  IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
var
  ProcClassName: String;
begin
  Result:=nil;
  ProcClassName:=ExtractClassNameOfProcNode(ProcNode,true);
  if ProcClassName='' then exit;
  Result:=FindClassNodeBackwards(ProcNode,ProcClassName,IgnoreForwards,
                                 IgnoreNonForwards);
end;

function TPascalReaderTool.FindClassOrInterfaceNode(CursorNode: TCodeTreeNode;
  FindClassOfMethod: boolean): TCodeTreeNode;
begin
  while CursorNode<>nil do begin
    if CursorNode.Desc in AllClasses then begin
      Result:=CursorNode;
      exit;
    end else if FindClassOfMethod and NodeIsMethodBody(CursorNode) then begin
      Result:=FindClassNodeForMethodBody(CursorNode,true,false);
      exit;
    end;
    CursorNode:=CursorNode.Parent;
  end;
  Result:=nil;
end;

function TPascalReaderTool.FindClassSection(ClassNode: TCodeTreeNode;
  NodeDesc: TCodeTreeNodeDesc): TCodeTreeNode;
begin
  Result:=ClassNode.FirstChild;
  while (Result<>nil) and (Result.Desc<>NodeDesc) do
    Result:=Result.NextBrother;
end;

function TPascalReaderTool.FindLastClassSection(ClassNode: TCodeTreeNode;
  NodeDesc: TCodeTreeNodeDesc): TCodeTreeNode;
begin
  Result:=ClassNode.LastChild;
  while (Result<>nil) and (Result.Desc<>NodeDesc) do
    Result:=Result.PriorBrother;
end;

function TPascalReaderTool.GetClassVisibility(Node: TCodeTreeNode
  ): TCodeTreeNodeDesc;
begin
  Result:=ctnNone;
  if Node=nil then exit;
  if Node.Desc=ctnProcedureHead then
    Node:=Node.Parent;
  if not (Node.Desc in AllClassSections) then begin
    Node:=Node.Parent;
    if Node=nil then exit;
  end;
  if Node.Desc in AllClassSubSections then
    Node:=Node.Parent;
  if Node.Desc in AllClassBaseSections then
    Result:=Node.Desc;
end;

function TPascalReaderTool.FindClassNodeInInterface(
  const AClassName: string; IgnoreForwards, IgnoreNonForwards,
  ErrorOnNotFound: boolean): TCodeTreeNode;
  
  procedure RaiseClassNotFound;
  begin
    RaiseExceptionFmt(20170421200001,ctsClassSNotFound, [AClassName]);
  end;
  
begin
  Result:=Tree.Root;
  if Result<>nil then begin
    if Result.Desc=ctnUnit then
      Result:=Result.NextBrother;
    if Result<>nil then begin
      Result:=FindClassNode(Result.FirstChild,AClassName,
                            IgnoreForwards, IgnoreNonForwards);
      if (Result<>nil) and Result.HasParentOfType(ctnImplementation) then
        Result:=nil;
    end;
  end;
  if (Result=nil) and ErrorOnNotFound then
    RaiseClassNotFound;
end;

function TPascalReaderTool.FindClassNodeInUnit(const AClassName: string;
  IgnoreForwards, IgnoreNonForwards, IgnoreImplementation,
  ErrorOnNotFound: boolean): TCodeTreeNode;

  procedure RaiseClassNotFound;
  begin
    RaiseExceptionFmt(20170421200003,ctsClassSNotFound, [AClassName]);
  end;

begin
  Result:=Tree.Root;
  if Result<>nil then begin
    if Result.Desc in [ctnUnit,ctnLibrary,ctnPackage] then begin
      Result:=Result.NextBrother;
    end;
    if Result<>nil then begin
      Result:=FindClassNode(Result.FirstChild,AClassName,
                            IgnoreForwards, IgnoreNonForwards);
      if (Result<>nil) and IgnoreImplementation
      and Result.HasParentOfType(ctnImplementation) then
        Result:=nil;
    end;
  end;
  if (Result=nil) and ErrorOnNotFound then
    RaiseClassNotFound;
end;

function TPascalReaderTool.FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  if (ClassNode=nil) then exit(nil);
  Result:=FindNextIdentNodeInClass(ClassNode.FirstChild);
end;

function TPascalReaderTool.FindLastIdentNodeInClass(ClassNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  if (ClassNode=nil) then exit(nil);
  Result:=ClassNode.LastChild;
  if Result=nil then exit;
  while (Result.FirstChild<>nil) and (Result.Desc in AllClassSections) do
    Result:=Result.LastChild;
  if not (Result.Desc in AllClassSections) then
    Result:=FindPriorIdentNodeInClass(Result);
end;

function TPascalReaderTool.FindNextIdentNodeInClass(Node: TCodeTreeNode
  ): TCodeTreeNode;
// Node must be nil or a class section or an identifier node in a class
begin
  Result:=Node;
  if Result=nil then exit;
  repeat
    // descend into class sections, skip empty class sections
    if (Result.FirstChild<>nil) and (Result.Desc in AllClassSections) then
      Result:=Result.FirstChild
    else begin
      while Result.NextBrother=nil do begin
        Result:=Result.Parent;
        if (Result=nil) or (not (Result.Desc in AllClassSections)) then
          exit(nil);
      end;
      Result:=Result.NextBrother
    end;
  until not (Result.Desc in AllClassSections);
end;

function TPascalReaderTool.FindPriorIdentNodeInClass(Node: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=Node;
  if Result=nil then exit;
  repeat
    if Result.PriorBrother<>nil then begin
      Result:=Result.PriorBrother;
      while (Result.LastChild<>nil) and (Result.Desc in AllClassSections) do
        Result:=Result.LastChild;
    end else if Result.Parent.Desc in AllClassSections then
      Result:=Result.Parent
    else
      exit(nil);
  until not (Result.Desc in AllClassSections);
end;

function TPascalReaderTool.ClassSectionNodeStartsWithWord(ANode: TCodeTreeNode
  ): boolean;
begin
  Result:=(ANode<>nil) and (ANode.StartPos<ANode.EndPos)
    and (IsIdentStartChar[Src[ANode.StartPos]]);
end;

function TPascalReaderTool.IsClassNode(Node: TCodeTreeNode): boolean;
begin
  Result:=(Node<>nil) and (Node.Desc=ctnClass);
end;

function TPascalReaderTool.FindInheritanceNode(ClassNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=ClassNode.FirstChild;
  while (Result<>nil) and (Result.Desc in [ctnClassSealed,ctnClassAbstract,ctnClassExternal]) do
    Result:=Result.NextBrother;
  if (Result<>nil) and (Result.Desc<>ctnClassInheritance) then
    Result:=nil;
end;

function TPascalReaderTool.ExtractRecordCaseType(RecordCaseNode: TCodeTreeNode): string;
//  case a:b.c of
//  case a:(b,c) of
var
  VarNode: TCodeTreeNode;
begin
  Result:='';
  VarNode:=RecordCaseNode.FirstChild;
  if VarNode=nil then exit;
  if VarNode.FirstChild<>nil then
    Result:=ExtractNode(RecordCaseNode.FirstChild,[]);
end;

function TPascalReaderTool.GetSourceType: TCodeTreeNodeDesc;
begin
  if Tree.Root<>nil then
    Result:=Tree.Root.Desc
  else
    Result:=ctnNone;
end;

function TPascalReaderTool.IdentNodeIsInVisibleClassSection(
  Node: TCodeTreeNode; Visibility: TClassSectionVisibility): Boolean;
begin
  if Visibility = csvEverything then
    Result := True
  else
  if (Node.Parent<>nil) then
    case Visibility of
      //csvAbovePrivate: todo: add strict private and strict protected (should be registered as new sections)
      csvProtectedAndHigher:
        Result := not(Node.Parent.Desc = ctnClassPrivate);//todo: add strict private
      csvPublicAndHigher:
        Result := not(Node.Parent.Desc in [ctnClassPrivate, ctnClassProtected]);//todo: strict private and strict protected
    else
      Result := True
    end
  else
    Result := False;
end;

function TPascalReaderTool.ExtractProcedureGroup(ProcNode: TCodeTreeNode
  ): TPascalMethodGroup;
begin
  Result:=mgMethod;
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if UpAtomIs('CLASS') then
  begin
    ReadNextAtom;
    if UpAtomIs('CONSTRUCTOR') then
      Result := mgClassConstructor
    else if UpAtomIs('DESTRUCTOR') then
      Result := mgClassDestructor
    else if UpAtomIs('OPERATOR') then
      Result := mgClassOperator;
  end else
  if UpAtomIs('CONSTRUCTOR') then
    Result := mgConstructor
end;

function TPascalReaderTool.PositionInSourceName(CleanPos: integer): boolean;
var
  NamePos: TAtomPosition;
begin
  Result:=false;
  if not GetSourceNamePos(NamePos) then exit;
  Result:=(CleanPos>=NamePos.StartPos) and (CleanPos<NamePos.EndPos);
end;

function TPascalReaderTool.ExtractSourceName: string;
var
  NamePos: TAtomPosition;
begin
  Result:='';
  if Tree.Root=nil then exit;
  if GetSourceNamePos(NamePos) then
    Result:=copy(Src, NamePos.StartPos, NamePos.EndPos-NamePos.StartPos)
  else if Tree.Root.Desc=ctnProgram then
    // a program without the 'program' header uses the file name as name
    Result:=ExtractFileNameOnly(MainFilename)
end;

function TPascalReaderTool.GetSourceNamePos(out NamePos: TAtomPosition): boolean;
begin
  Result:=false;
  NamePos.StartPos:=-1;
  if Tree.Root=nil then exit;
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read source type 'program', 'unit' ...
  if (Tree.Root.Desc=ctnProgram) and (not UpAtomIs('PROGRAM')) then exit;
  ReadNextAtom; // read name
  if not AtomIsIdentifier then exit;
  NamePos:=CurPos;
  Result:=true;
  ReadNextAtom;
  while CurPos.Flag=cafPoint do begin
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    NamePos.EndPos:=CurPos.EndPos;
    ReadNextAtom;
  end;
end;

function TPascalReaderTool.GetSourceName(DoBuildTree: boolean): string;
begin
  Result:='';
  if DoBuildTree then
    BuildTree(lsrSourceName);
  CachedSourceName:=ExtractSourceName;
  Result:=CachedSourceName;
end;

function TPascalReaderTool.NodeIsInAMethod(Node: TCodeTreeNode): boolean;
begin
  Result:=false;
  while (Node<>nil) do begin
    if (Node.Desc=ctnProcedure) then begin
      if NodeIsMethodBody(Node) then begin
        Result:=true;
        exit;
      end;
    end;
    Node:=Node.Parent;
  end;
end;

function TPascalReaderTool.NodeIsMethodBody(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure)
  and (ProcNode.FirstChild<>nil) then begin

    // ToDo: ppu, dcu

    MoveCursorToNodeStart(ProcNode.FirstChild); // ctnProcedureHead
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then exit;
    Result:=true;
    exit;
  end;
end;

function TPascalReaderTool.GetMethodOfBody(Node: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=Node;
  while (Result<>nil) and not NodeIsMethodBody(Result) do
    Result:=Result.Parent;
end;

function TPascalReaderTool.NodeIsFunction(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if UpAtomIs('CLASS') then ReadNextAtom;
  Result:=UpAtomIs('FUNCTION');
end;

function TPascalReaderTool.NodeIsConstructor(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if UpAtomIs('CLASS') then ReadNextAtom;
  Result:=UpAtomIs('CONSTRUCTOR');
  if not Result and UpAtomIs('FUNCTION')
  and ([cmsObjectiveC1,cmsObjectiveC2]*Scanner.CompilerModeSwitches<>[]) then
  begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
    if (ProcNode.SubDesc and ctnsNeedJITParsing)>0 then
      BuildSubTreeForProcHead(ProcNode);
    ProcNode:=ProcNode.FirstChild;
    if (ProcNode=nil) then exit;
    if ProcNode.Desc=ctnParameterList then
      ProcNode:=ProcNode.NextBrother;
    if (ProcNode=nil) then exit;
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    Result:=UpAtomIs('ID');
  end;
end;

function TPascalReaderTool.NodeIsDestructor(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  Result:=UpAtomIs('DESTRUCTOR');
end;

function TPascalReaderTool.NodeIsForwardProc(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  // check if procedure
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then exit;
  // check if in interface
  if (ProcNode.Parent<>nil) and (ProcNode.Parent.Desc=ctnInterface) then
    exit(true);
  // check if has forward
  if (ctnsForwardDeclaration and ProcNode.SubDesc)>0 then exit(true);
end;

function TPascalReaderTool.NodeIsOperator(ProcNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  if ProcNode.Desc<>ctnProcedure then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if UpAtomIs('CLASS') then ReadNextAtom;
  Result:=UpAtomIs('OPERATOR');
end;

function TPascalReaderTool.NodeIsResultIdentifier(Node: TCodeTreeNode): boolean;
begin
  Result:=(Node<>nil)
    and (Node.Desc=ctnVarDefinition)
    and (Node.Parent<>nil)
    and (Node.Parent.Desc=ctnProcedureHead);
end;

function TPascalReaderTool.NodeIsResultType(Node: TCodeTreeNode): boolean;
begin
  Result:=(Node<>nil)
    and (Node.Desc=ctnIdentifier)
    and (Node.Parent<>nil)
    and (Node.Parent.Desc=ctnProcedureHead);
end;

function TPascalReaderTool.NodeIsPartOfTypeDefinition(ANode: TCodeTreeNode
  ): boolean;
begin
  ANode:=ANode.Parent;
  while ANode<>nil do begin
    if ANode.Desc in (AllIdentifierDefinitions+AllPascalTypes) then begin
      Result:=true;
      exit;
    end;
    ANode:=ANode.Parent;
  end;
  Result:=false;
end;

function TPascalReaderTool.ExtractDefinitionNodeType(
  DefinitionNode: TCodeTreeNode): string;
var
  TypeNode: TCodeTreeNode;
begin
  Result:='';
  TypeNode:=FindTypeNodeOfDefinition(DefinitionNode);
  if TypeNode=nil then exit;
  if TypeNode.Desc=ctnIdentifier then
    Result:=GetIdentifier(@Src[TypeNode.StartPos]);
end;

function TPascalReaderTool.ExtractFuncResultType(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result := '';
  if (ProcNode=nil) then exit;
  if ProcNode.Desc=ctnProcedure then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or(ProcNode.Desc<>ctnProcedureHead) then
    Exit;
  MoveCursorToCleanPos(ProcNode.EndPos);
  CurNode:=ProcNode;
  ReadPriorAtom;
  if CurPos.Flag<>cafSemicolon then
    Exit;
  ReadPriorAtom;
  if CurPos.Flag<>cafWord then
    Exit;
  if phpInUpperCase in Attr then
    Result := GetUpAtom
  else
    Result := GetAtom;
end;

function TPascalReaderTool.ExtractDefinitionName(DefinitionNode: TCodeTreeNode
  ): string;
begin
  DefinitionNode:=FindDefinitionNameNode(DefinitionNode);
  if DefinitionNode<>nil then
    Result:=GetIdentifier(@Src[DefinitionNode.StartPos])
  else
    Result:='';
end;

function TPascalReaderTool.PositionInDefinitionName(
  DefinitionNode: TCodeTreeNode; CleanPos: integer): boolean;
var
  StartPos: LongInt;
begin
  if DefinitionNode.Desc=ctnGenericType then begin
    if DefinitionNode.FirstChild<>nil then
      StartPos:=DefinitionNode.FirstChild.StartPos
    else
      StartPos:=0;
  end else begin
    StartPos:=DefinitionNode.StartPos;
  end;
  Result:=(CleanPos>=StartPos) and (CleanPos<StartPos+GetIdentLen(@Src[StartPos]));
end;

function TPascalReaderTool.MoveCursorToParameterSpecifier(
  DefinitionNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (DefinitionNode=nil) or (DefinitionNode.Desc<>ctnVarDefinition)
  or (DefinitionNode.Parent=nil)
  or (DefinitionNode.Parent.Desc<>ctnParameterList) then exit;
  // find first variable node of this type (e.g. var a,b,c,d: integer)
  DefinitionNode:=GetFirstGroupVarNode(DefinitionNode);
  if DefinitionNode.PriorBrother<>nil then
    MoveCursorToCleanPos(DefinitionNode.PriorBrother.EndPos)
  else
    MoveCursorToCleanPos(DefinitionNode.Parent.StartPos);
  ReadNextAtom;
  while (CurPos.StartPos<DefinitionNode.StartPos) do ReadNextAtom;
  UndoReadNextAtom;
  Result:=CurPos.Flag=cafWord;
end;

function TPascalReaderTool.GetFirstGroupVarNode(VarNode: TCodeTreeNode): TCodeTreeNode;
begin
  Result:=VarNode;
  if (VarNode=nil) or (VarNode.Desc<>ctnVarDefinition) then exit;
  while VarNode<>nil do begin
    VarNode:=VarNode.PriorBrother;
    if (VarNode=nil) or (VarNode.Desc<>ctnVarDefinition)
    or (VarNode.FirstChild<>nil) then exit;
    Result:=VarNode;
  end;
end;

function TPascalReaderTool.FindEndOfWithExpr(WithVarNode: TCodeTreeNode): integer;
begin
  if WithVarNode.Desc<>ctnWithVariable then exit(-1);
  MoveCursorToCleanPos(WithVarNode.StartPos);
  ReadNextAtom;
  if not ReadTilVariableEnd(true,true) then exit(-1);
  UndoReadNextAtom;
  Result:=CurPos.EndPos;
end;

function TPascalReaderTool.ExtractWithBlockExpression(
  WithVarNode: TCodeTreeNode; Attr: TProcHeadAttributes): string;
var
  EndPos: Integer;
begin
  EndPos:=FindEndOfWithExpr(WithVarNode);
  if EndPos<1 then exit('');
  Result:=ExtractCode(WithVarNode.StartPos,EndPos,Attr);
end;

function TPascalReaderTool.FindWithBlockStatement(WithVarNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=WithVarNode;
  repeat
    if Result=nil then exit;
    if Result.Desc<>ctnWithVariable then exit(nil);
    if Result.FirstChild<>nil then begin
      Result:=Result.FirstChild;
      if Result.Desc=ctnWithStatement then exit;
      exit(nil);
    end;
  until false;
end;

function TPascalReaderTool.NodeIsIdentifierInInterface(Node: TCodeTreeNode): boolean;
// true if identifier is visible from other units (without prefixing)
begin
  case Node.Desc of
  ctnEnumIdentifier:
    Result:=true;
  ctnVarDefinition:
    Result:=(Node.Parent.Desc=ctnVarSection)
            and (Node.Parent.Parent.Desc=ctnInterface);
  ctnConstDefinition:
    Result:=(Node.Parent.Desc=ctnConstSection)
            and (Node.Parent.Parent.Desc=ctnInterface);
  ctnTypeDefinition,ctnGenericType:
    Result:=(Node.Parent.Desc=ctnTypeSection)
            and (Node.Parent.Parent.Desc=ctnInterface);
  ctnProcedure,ctnProperty:
    Result:=Node.Parent.Desc=ctnInterface;
  ctnProcedureHead:
    Result:=(Node.Parent.Desc=ctnProcedure)
        and (Node.Parent.Parent.Desc=ctnInterface);
  end;
  Result:=false;
end;

function TPascalReaderTool.NodeCanHaveForwardType(TypeNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (TypeNode=nil) or (TypeNode.Desc<>ctnTypeDefinition)
  or (TypeNode.FirstChild=nil) then
    exit;
  if (TypeNode.FirstChild.Desc in AllClasses)
  and (TypeNode.FirstChild.SubDesc and ctnsForwardDeclaration=0) then
    Result:=true;
end;

function TPascalReaderTool.NodeIsClassConstructorOrDestructor(
  ProcNode: TCodeTreeNode): boolean;
begin
  Result := ExtractProcedureGroup(ProcNode) in [mgClassConstructor, mgClassDestructor];
end;

function TPascalReaderTool.NodeIsForwardType(TypeNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (TypeNode=nil) or (TypeNode.Desc<>ctnTypeDefinition)
  or (TypeNode.FirstChild=nil) then
    exit;
  if (TypeNode.FirstChild.Desc in AllClasses)
  and (TypeNode.FirstChild.SubDesc and ctnsForwardDeclaration>0) then
    Result:=true;
end;

function TPascalReaderTool.FindForwardTypeNode(TypeNode: TCodeTreeNode;
  SearchFirst: boolean): TCodeTreeNode;
{ Find the first forward type of TypeNode
}

  function Next: TCodeTreeNode;
  begin
    Result:=FindForwardTypeNode;
    if Result.PriorBrother<>nil then
      // search upwards
      Result:=Result.PriorBrother
    else if Result.Parent.Desc in AllDefinitionSections then begin
      // type section was searched
      // check for other type sections in front
      Result:=Result.Parent;
      repeat
        while (Result.PriorBrother<>nil) do begin
          Result:=Result.PriorBrother;
          if (Result.Desc in AllDefinitionSections)
          and (Result.LastChild<>nil) then begin
            Result:=Result.LastChild;
            exit;
          end;
        end;
        // check if in implementation section
        if (Result.Parent=nil) or (Result.Parent.Desc<>ctnImplementation) then
          exit(nil);
        Result:=Result.Parent;
        // check if there is an interface section
        if (Result.PriorBrother=nil) or (Result.PriorBrother.Desc<>ctnInterface)
        then
          exit(nil);
        // search in interface section
        Result:=Result.PriorBrother;
        Result:=Result.LastChild;
      until Result=nil;
    end else
      exit;
  end;

var
  Node: TCodeTreeNode;
begin
  Result:=nil;
  if not NodeCanHaveForwardType(TypeNode) then exit;
  Node:=TypeNode;
  while Node<>nil do begin
    if Node.Desc in AllIdentifierDefinitions then begin
      if CompareIdentifiers(@Src[TypeNode.StartPos],@Src[Node.StartPos])=0
      then begin
        if (Node.Desc=ctnTypeDefinition) and NodeIsForwardType(Node) then begin
          // a forward
          Result:=Node;
          if not SearchFirst then exit;
        end else begin
          // a redefinition
          exit;
        end;
      end;
    end;
    Node:=Next;
  end;
end;

function TPascalReaderTool.FindHelperForNode(HelperNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=HelperNode.FirstChild;
  while (Result<>nil) and (Result.Desc = ctnClassInheritance) do
    Result:=Result.NextBrother;
  if (Result<>nil) and (Result.Desc<>ctnHelperFor) then
    Result:=nil;
end;

function TPascalReaderTool.FindClassExternalNode(ClassNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  if ClassNode=nil then exit(nil);
  Result:=ClassNode.FirstChild;
  while (Result<>nil) do
    begin
    if Result.Desc=ctnClassExternal then exit;
    if Result.Desc in AllClassBaseSections then exit(nil);
    Result:=Result.NextBrother;
    end;
end;

function TPascalReaderTool.FindTypeOfForwardNode(TypeNode: TCodeTreeNode
  ): TCodeTreeNode;

  function Next: TCodeTreeNode;
  begin
    Result:=FindTypeOfForwardNode;
    if Result.NextBrother<>nil then
      // search forwards
      Result:=Result.NextBrother
    else if Result.Parent.Desc in AllDefinitionSections then begin
      // type section was searched
      // check for other type sections in front
      Result:=Result.Parent;
      repeat
        while (Result.NextBrother<>nil) do begin
          Result:=Result.NextBrother;
          if (Result.Desc in AllDefinitionSections)
          and (Result.FirstChild<>nil) then begin
            Result:=Result.FirstChild;
            exit;
          end;
        end;
        // check if in interface section
        if (Result.Parent=nil) or (Result.Parent.Desc<>ctnInterface) then
          exit(nil);
        Result:=Result.Parent;
        // check if there is an implementation section
        if (Result.NextBrother=nil) or (Result.NextBrother.Desc<>ctnImplementation)
        then
          exit(nil);
        // search in implementation section
        Result:=Result.NextBrother;
        Result:=Result.FirstChild;
      until Result=nil;
    end else
      exit;
  end;

var
  Node: TCodeTreeNode;
begin
  Result:=nil;
  if not NodeIsForwardType(TypeNode) then exit;
  Node:=TypeNode;
  while Node<>nil do begin
    if Node.Desc in AllIdentifierDefinitions then begin
      if CompareIdentifiers(@Src[TypeNode.StartPos],@Src[Node.StartPos])=0
      then begin
        if (Node.Desc=ctnTypeDefinition) and (not NodeIsForwardType(Node)) then
        begin
          // a type
          Result:=Node;
          exit;
        end else begin
          // a redefinition
          exit;
        end;
      end;
    end;
    Node:=Next;
  end;
end;

function TPascalReaderTool.ExtractArrayRange(ArrayNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  if (ArrayNode=nil) or (ArrayNode.Desc<>ctnRangedArrayType) then exit;
  MoveCursorToNodeStart(ArrayNode);
  if not ReadNextUpAtomIs('ARRAY') then exit;
  if not ReadNextAtomIsChar('[') then exit;
  Result:=ExtractBrackets(CurPos.StartPos,Attr);
end;

function TPascalReaderTool.ExtractArrayRanges(ArrayNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
const
  AllArrays = [ctnRangedArrayType, ctnOpenArrayType];
begin
  Result:='';
  if (ArrayNode=nil) or not (ArrayNode.Desc in AllArrays) then exit;
  while Assigned(ArrayNode.Parent) and (ArrayNode.Parent.Desc in AllArrays) do
    ArrayNode:=ArrayNode.Parent;
  MoveCursorToNodeStart(ArrayNode);
  if not ReadNextUpAtomIs('ARRAY') then exit;
  repeat
    if Result<>'' then Result:=Result+',';
    if ArrayNode.Desc=ctnRangedArrayType then begin
      MoveCursorToNodeStart(ArrayNode.FirstChild); // FirstChild=Index type
      Result:=Result+ExtractNode(ArrayNode.FirstChild,Attr);
    end else begin
      Result:=Result+'PtrUInt';
    end;
    ArrayNode:=ArrayNode.LastChild;
  until not (ArrayNode.Desc in AllArrays);
  Result:='['+Result+']';
end;

function TPascalReaderTool.PropertyIsDefault(PropertyNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if (PropertyNode=nil) or (PropertyNode.Desc<>ctnProperty) then exit;
  MoveCursorToCleanPos(PropertyNode.EndPos);
  ReadPriorAtom;
  if (CurPos.Flag<>cafSemicolon) then exit;
  ReadPriorAtom;
  Result:=UpAtomIs('DEFAULT');
end;

function TPascalReaderTool.PropertyNodeHasParamList(PropNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, dcu

  Result:=false;
  if not MoveCursorToPropName(PropNode) then exit;
  ReadNextAtom;
  Result:=(CurPos.Flag=cafEdgedBracketOpen);
end;

function TPascalReaderTool.PropNodeIsTypeLess(PropNode: TCodeTreeNode): boolean;
begin

  // ToDo: ppu, dcu

  Result:=false;
  if PropNode.Desc<>ctnProperty then exit;
  if not MoveCursorToPropName(PropNode) then exit;
  ReadNextAtom; // read colon, skip parameters
  if CurPos.Flag=cafEdgedBracketOpen then begin
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  Result:=(CurPos.Flag<>cafColon);
end;

function TPascalReaderTool.PropertyHasSpecifier(PropNode: TCodeTreeNode;
  UpperKeyword: string; ExceptionOnNotFound: boolean): boolean;
// true if cursor is on keyword
begin

  // ToDo: ppu, dcu

  Result:=false;
  if not MoveCursorToPropName(PropNode) then exit;
  if not AtomIsIdentifierE(ExceptionOnNotFound) then exit;
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
    ReadNextAtom;
  end;
  if CurPos.Flag=cafColon then begin
    // read type
    ReadNextAtom;
    if not AtomIsIdentifierE(ExceptionOnNotFound) then exit;
    ReadNextAtom;
    if CurPos.Flag=cafPoint then begin
      ReadNextAtom;
      if not AtomIsIdentifierE(ExceptionOnNotFound) then exit;
      ReadNextAtom;
    end;
  end;

  UpperKeyword:=UpperCaseStr(UpperKeyword);
  // read specifiers
  while not (CurPos.Flag in [cafSemicolon,cafNone]) do begin
    if WordIsPropertySpecifier.DoIdentifier(@Src[CurPos.StartPos])
    then begin
      if UpAtomIs(UpperKeyword) then exit(true);
    end else if CurPos.Flag=cafEdgedBracketOpen then begin
      if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
      ReadNextAtom;
    end;
    ReadNextAtom;
  end;
  // read modifiers
  while CurPos.Flag=cafSemicolon do begin
    ReadNextAtom;
    if UpAtomIs('DEFAULT') or UpAtomIs('NODEFAULT') or UpAtomIs('DEPRECATED')
    then begin
      if CompareIdentifierPtrs(@Src[CurPos.StartPos],Pointer(UpperKeyword))=0 then exit(true);
    end else if UpAtomIs('ENUMERATOR') then begin
      if CompareIdentifierPtrs(@Src[CurPos.StartPos],Pointer(UpperKeyword))=0 then exit(true);
      ReadNextAtom;
      if not AtomIsIdentifier then exit;
    end else
      exit;
    ReadNextAtom;
  end;
end;

function TPascalReaderTool.ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin
  Result:=false;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnProcedureHead then
    ProcNode:=ProcNode.Parent;
  {$IFDEF CheckNodeTool}
  if ProcNode.Desc<>ctnProcedure then begin
    DebugLn(['TPascalReaderTool.ProcNodeHasSpecifier Desc=',ProcNode.DescAsString]);
    CTDumpStack;
    RaiseException(20170421195959,'[TPascalReaderTool.ProcNodeHasSpecifier] '
      +'internal error: invalid ProcNode');
  end;
  {$ENDIF}
  if (ProcNode.FirstChild=nil)
  or ((ProcNode.SubDesc and ctnsNeedJITParsing)>0) then
    BuildSubTreeForProcHead(ProcNode);

  // ToDo: ppu, dcu

  Result:=MoveCursorToProcSpecifier(ProcNode,ProcSpec);
end;

function TPascalReaderTool.ProcNodeHasParamList(ProcNode: TCodeTreeNode): boolean;
begin

  // ToDo: ppu, dcu

  Result:=false;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnProcedure then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  if ProcNode.Desc<>ctnProcedureHead then exit;
  if ProcNode.FirstChild<>nil then begin
    Result:=ProcNode.FirstChild.Desc=ctnParameterList;
    exit;
  end;
  MoveCursorBehindProcName(ProcNode);
  Result:=CurPos.Flag=cafRoundBracketOpen;
end;

function TPascalReaderTool.ProcNodeHasOfObject(ProcNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, dcu

  Result:=false;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnReferenceTo then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  if ProcNode.Desc<>ctnProcedureType then exit;
  MoveCursorToFirstProcSpecifier(ProcNode);
  Result:=UpAtomIs('OF') and ReadNextUpAtomIs('OBJECT');
end;

function TPascalReaderTool.GetProcParamList(ProcNode: TCodeTreeNode;
  Parse: boolean): TCodeTreeNode;
begin
  Result:=ProcNode;
  if Result=nil then exit;
  if Result.Desc=ctnProcedure then begin
    Result:=Result.FirstChild;
    if Result=nil then exit;
  end;
  if Result.Desc<>ctnProcedureHead then exit(nil);
  if Parse then
    BuildSubTreeForProcHead(Result);
  Result:=Result.FirstChild;
  while Result<>nil do begin
    if Result.Desc=ctnParameterList then exit;
    Result:=Result.NextBrother;
  end;
end;

function TPascalReaderTool.GetProcResultNode(ProcNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=nil;
  if ProcNode=nil then exit;
  if ProcNode.Desc in [ctnProcedure,ctnProcedureType] then begin
    Result:=ProcNode.FirstChild;
    if Result=nil then exit;
  end;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then exit;
  Result:=ProcNode.FirstChild;
  while Result<>nil do begin
    if Result.Desc=ctnIdentifier then exit;
    Result:=Result.NextBrother;
  end;
end;

procedure TPascalReaderTool.MoveCursorToUsesStart(UsesNode: TCodeTreeNode);
begin
  if (UsesNode=nil)
  or ((UsesNode.Desc<>ctnUsesSection) and (UsesNode.Desc<>ctnContainsSection))
  then
    RaiseException(20170421200006,'[TPascalParserTool.MoveCursorToUsesStart] '
      +'internal error: invalid UsesNode');
  // search through the uses section
  MoveCursorToCleanPos(UsesNode.StartPos);
  ReadNextAtom;
  if (not UpAtomIs('USES')) and (not UpAtomIs('CONTAINS')) then
    RaiseExceptionFmt(20170421200009,ctsStrExpectedButAtomFound,['uses',GetAtom]);
  ReadNextAtom;
end;

procedure TPascalReaderTool.MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
begin
  if (UsesNode=nil)
  or ((UsesNode.Desc<>ctnUsesSection) and (UsesNode.Desc<>ctnContainsSection))
  then
    RaiseException(20170421200012,'[TPascalParserTool.MoveCursorToUsesEnd] '
      +'internal error: invalid UsesNode');
  // search backwards through the uses section
  MoveCursorToCleanPos(UsesNode.EndPos);
  ReadPriorAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseExceptionFmt(20170421200014,ctsStrExpectedButAtomFound,[';',GetAtom]);
end;

function TPascalReaderTool.ReadNextUsedUnit(out UnitNameRange,
  InAtom: TAtomPosition; SyntaxExceptions: boolean): boolean;
// after reading CurPos is on atom behind, i.e. comma or semicolon
begin
  Result:=false;
  if not AtomIsIdentifierE(SyntaxExceptions) then exit;
  UnitNameRange:=CurPos;
  repeat
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then break;
    ReadNextAtom;
    if not AtomIsIdentifierE(SyntaxExceptions) then exit;
    UnitNameRange.EndPos:=CurPos.EndPos;
  until false;
  if UpAtomIs('IN') then begin
    ReadNextAtom; // read filename
    if not AtomIsStringConstant then begin
      if not SyntaxExceptions then exit;
      RaiseStrConstExpected(20170421200017);
    end;
    InAtom:=CurPos;
    ReadNextAtom; // read comma or semicolon
  end else begin
    InAtom:=CleanAtomPosition;
  end;
  Result:=true;
end;

procedure TPascalReaderTool.ReadPriorUsedUnit(out UnitNameRange,InAtom: TAtomPosition);
begin
  ReadPriorAtom; // read unitname
  if AtomIsStringConstant then begin
    InAtom:=CurPos;
    ReadPriorAtom; // read 'in'
    if not UpAtomIs('IN') then
      RaiseExceptionFmt(20170421200021,ctsStrExpectedButAtomFound,[ctsKeywordIn,GetAtom]);
    ReadPriorAtom; // read unitname
  end else begin
    InAtom:=CleanAtomPosition;
  end;
  AtomIsIdentifierE;
  UnitNameRange:=CurPos;
  repeat
    ReadPriorAtom;
    if CurPos.Flag<>cafPoint then break;
    ReadPriorAtom;
    AtomIsIdentifierE;
    UnitNameRange.StartPos:=CurPos.StartPos;
  until false;
end;

function TPascalReaderTool.ExtractUsedUnitNameAtCursor(InFilename: PAnsiString): string;
begin
  Result:='';
  if InFilename<>nil then
    InFilename^:='';
  while CurPos.Flag=cafWord do begin
    if Result<>'' then
      Result:=Result+'.';
    Result:=Result+GetAtom;
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then break;
    ReadNextAtom;
  end;
  if UpAtomIs('IN') then begin
    ReadNextAtom;
    if not AtomIsStringConstant then exit;
    if InFilename<>nil then
      InFilename^:=copy(Src,CurPos.StartPos+1,CurPos.EndPos-CurPos.StartPos-2);
    ReadNextAtom;
  end;
end;

function TPascalReaderTool.ExtractUsedUnitName(UseUnitNode: TCodeTreeNode;
  InFilename: PAnsiString): string;
// after reading CurPos is on atom behind, i.e. comma or semicolon
begin
  Result:='';
  if InFilename<>nil then InFilename^:='';
  if (UseUnitNode=nil) or (UseUnitNode.Desc<>ctnUseUnit) then exit;
  MoveCursorToCleanPos(UseUnitNode.StartPos);
  ReadNextAtom;
  Result:=ExtractUsedUnitNameAtCursor(InFilename);
end;

function TPascalReaderTool.ReadAndCompareUsedUnit(const AnUnitName: string): boolean;
// after reading cursor is on atom behind unit name
var
  p: PChar;
begin
  Result:=false;
  if IsDottedIdentifier(AnUnitName) then
    p:=PChar(AnUnitName)
  else
    p:=nil;
  repeat
    if not AtomIsIdentifier then exit;
    if (p<>nil) then begin
      if CompareIdentifiers(p,@Src[CurPos.StartPos])=0 then
        inc(p,CurPos.EndPos-CurPos.StartPos)
      else
        p:=nil;
    end;
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then begin
      // end of unit name
      Result:=(p<>nil) and (p^=#0);
      exit;
    end;
    // dot
    if (p<>nil) then begin
      if p^='.' then
        inc(p)
      else
        p:=nil;
    end;
    ReadNextAtom;
  until false;
end;

function TPascalReaderTool.FindCommentInFront(const StartPos: TCodeXYPosition;
  const CommentText: string; InvokeBuildTree, SearchInParentNode,
  WithCommentBounds, CaseSensitive, IgnoreSpaces, CompareOnlyStart: boolean;
  out CommentStart, CommentEnd: TCodeXYPosition): boolean;
var
  CleanCursorPos: integer;
  CommentCleanStart: integer;
  CommentCleanEnd: integer;
begin
  Result:=false;
  if CommentText='' then exit;

  {debugln('TPascalReaderTool.FindCommentInFront A CommentText="',CommentText,'" ',
    ' StartPos=Y='+dbgs(StartPos.Y)+',X='+dbgs(StartPos.X),
    ' InvokeBuildTree='+dbgs(InvokeBuildTree),
    ' SearchInParentNode='+dbgs(SearchInParentNode),
    ' WithCommentBounds='+dbgs(WithCommentBounds),
    ' CaseSensitive='+dbgs(CaseSensitive),
    ' IgnoreSpaces='+dbgs(IgnoreSpaces),
    ' CompareOnlyStart='+dbgs(CompareOnlyStart)); }

  // parse source and find clean positions
  if InvokeBuildTree then
    BuildTreeAndGetCleanPos(StartPos,CleanCursorPos,[])
  else
    if CaretToCleanPos(StartPos,CleanCursorPos)<>0 then
      exit;
  Result:=FindCommentInFront(CleanCursorPos,CommentText,SearchInParentNode,
                  WithCommentBounds,CaseSensitive,IgnoreSpaces,CompareOnlyStart,
                  CommentCleanStart,CommentCleanEnd);
  if not Result then exit;
  Result:=(CommentCleanStart>=1)
          and CleanPosToCaret(CommentCleanStart,CommentStart)
          and CleanPosToCaret(CommentCleanEnd,CommentEnd);
end;

function TPascalReaderTool.FindCommentInFront(StartPos: integer;
  const CommentText: string;
  SearchInParentNode, WithCommentBounds, CaseSensitive,
  IgnoreSpaces, CompareOnlyStart: boolean;
  out CommentStart, CommentEnd: integer): boolean;
// searches a comment in front of StartPos starting with CommentText.
var
  FoundStartPos: integer;
  FoundEndPos: integer;

  procedure CompareComment(CStartPos, CEndPos: integer);
  var
    Found: LongInt;
    CompareStartPos: LongInt;
    CompareEndPos: LongInt;
    CompareLen: Integer;
    CompareCommentLength: Integer;
  begin
    //debugln('CompareComment "',copy(Src,CStartPos,CEndPos-CStartPos),'"');

    CompareStartPos:=CStartPos;
    CompareEndPos:=CEndPos;
    if not WithCommentBounds then begin
      // chomp comment boundaries
      case Src[CompareStartPos] of
      '/','(': inc(CompareStartPos,2);
      '{':
        if (CompareStartPos<SrcLen) and (Src[CompareStartPos+1]=#3) then
          // the codetools skip comment is no real comment
          exit
        else
          inc(CompareStartPos,1);
      end;
      case Src[CompareEndPos-1] of
      '}': dec(CompareEndPos);
      ')': dec(CompareEndPos,2);
      #10,#13:
        begin
          dec(CompareEndPos);
          if (Src[CompareEndPos-1] in [#10,#13])
          and (Src[CompareEndPos-1]<>Src[CompareEndPos]) then
            dec(CompareEndPos);
        end;
      end;
    end;
    if CompareStartPos>CompareEndPos then exit;

    if IgnoreSpaces then begin
      while (CompareStartPos<=CompareEndPos)
      and IsSpaceChar[Src[CompareStartPos]]
      do
        inc(CompareStartPos);
    end;

    CompareCommentLength:=length(CommentText);
    CompareLen:=CompareEndPos-CompareStartPos;
    if CompareOnlyStart and (CompareLen>CompareCommentLength) then
      CompareLen:=CompareCommentLength;

    //debugln('Compare: "',copy(Src,CompareStartPos,CompareEndPos-CompareStartPos),'"',
    //  ' "',CommentText,'"');
    if IgnoreSpaces then begin
      Found:=CompareTextIgnoringSpace(
                          @Src[CompareStartPos],CompareLen,
                          @CommentText[1],length(CommentText),
                          CaseSensitive);
    end else begin
      Found:=CompareText(@Src[CompareStartPos],CompareLen,
                         @CommentText[1],length(CommentText),
                         CaseSensitive);
    end;
    if Found=0 then begin
      FoundStartPos:=CStartPos;
      FoundEndPos:=CEndPos;
    end;
  end;

var
  ANode: TCodeTreeNode;
  p: LongInt;
  CommentStartPos: LongInt;
begin
  Result:=false;
  if StartPos>SrcLen then
    StartPos:=SrcLen+1;
  if CommentText='' then exit;

  {debugln('TPascalReaderTool.FindCommentInFront A CommentText="',CommentText,'" ',
    ' StartPos=Y='+dbgs(StartPos.Y)+',X='+dbgs(StartPos.X),
    ' InvokeBuildTree='+dbgs(InvokeBuildTree),
    ' SearchInParentNode='+dbgs(SearchInParentNode),
    ' WithCommentBounds='+dbgs(WithCommentBounds),
    ' CaseSensitive='+dbgs(CaseSensitive),
    ' IgnoreSpaces='+dbgs(IgnoreSpaces),
    ' CompareOnlyStart='+dbgs(CompareOnlyStart)); }

  // find node
  ANode:=FindDeepestNodeAtPos(StartPos,true);
  if (ANode=nil) then exit;

  { find end of last atom in front of node
    for example:
      uses classes;

      // Comment
      type

    If ANode is the 'type' block, the position after the semicolon is searched
  }

  if SearchInParentNode and (ANode.Parent<>nil) then begin
    // search all siblings in front
    ANode:=ANode.Parent;
    MoveCursorToCleanPos(ANode.Parent.StartPos);
  end else if ANode.PriorBrother<>nil then begin
    // search between prior sibling and this node
    //DebugLn('TPascalReaderTool.FindCommentInFront ANode.Prior=',ANode.Prior.DescAsString);
    MoveCursorToLastNodeAtom(ANode.PriorBrother);
  end else if ANode.Parent<>nil then begin
    // search from start of parent node to this node
    //DebugLn('TPascalReaderTool.FindCommentInFront ANode.Parent=',ANode.Parent.DescAsString);
    MoveCursorToCleanPos(ANode.Parent.StartPos);
  end else begin
    // search in this node
    //DebugLn('TPascalReaderTool.FindCommentInFront Aode=',ANode.DescAsString);
    MoveCursorToCleanPos(ANode.StartPos);
  end;
  p:=CurPos.EndPos;

  //debugln('TPascalReaderTool.FindCommentInFront B Area="',copy(Src,CurPos.StartPos,StartPos-CurPos.StartPos),'"');

  FoundStartPos:=-1;
  repeat
    //debugln('TPascalReaderTool.FindCommentInFront Atom=',GetAtom);
    CommentStartPos:=FindNextComment(Src,p,StartPos);
    if CommentStartPos>=StartPos then break;
    p:=FindCommentEnd(Src,CommentStartPos,Scanner.NestedComments);
    if p>StartPos then break;
    CompareComment(CommentStartPos,p);
  until false;

  Result:=(FoundStartPos>=1);
  CommentStart:=FoundStartPos;
  CommentEnd:=FoundEndPos;
end;

function TPascalReaderTool.GetPasDocComments(const StartPos: TCodeXYPosition;
  InvokeBuildTree: boolean; out ListOfPCodeXYPosition: TFPList): boolean;
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  ListOfPCodeXYPosition:=nil;
  Result:=false;

  // parse source and find clean positions
  if InvokeBuildTree then
    BuildTreeAndGetCleanPos(StartPos,CleanCursorPos)
  else
    if CaretToCleanPos(StartPos,CleanCursorPos)<>0 then
      exit;

  ANode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  Result:=GetPasDocComments(ANode,ListOfPCodeXYPosition);
end;

function TPascalReaderTool.GetPasDocComments(Node: TCodeTreeNode;
  out ListOfPCodeXYPosition: TFPList): boolean;
// Comments are normally in front.
// { Description of TMyClass. }
//  TMyClass = class
//
// Comments can be behind in the same line
// property Color; // description of Color
//
// Comments can be in the following line if started with <
//
// comment starting with $ or % are ignored

  function CommentBelongsToPrior(CommentStart: integer): boolean;
  var
    p: Integer;
  begin
    //DebugLn(['CommentBelongsToPrior Comment=',dbgstr(copy(Src,CommentStart,20))]);
    if (CommentStart<SrcLen) and (Src[CommentStart]='{')
    and (Src[CommentStart+1]='<') then
      Result:=true
    else if (CommentStart+2<=SrcLen) and (Src[CommentStart]='(')
    and (Src[CommentStart+1]='*') and (Src[CommentStart+2]='<') then
      Result:=true
    else if (CommentStart+2<=SrcLen) and (Src[CommentStart]='/')
    and (Src[CommentStart+1]='/') and (Src[CommentStart+2]='<') then
      Result:=true
    else begin
      p:=CommentStart-1;
      while (p>=1) and (Src[p] in [' ',#9]) do dec(p);
      //DebugLn(['CommentBelongsToPrior Code in front: ',dbgstr(copy(Src,p,20))]);
      if (p<1) or (Src[p] in [#10,#13]) then
        Result:=false
      else
        Result:=true; // there is code in the same line in front of the comment
    end;
  end;

  procedure Add(CleanPos: integer);
  var
    CodePos: TCodeXYPosition;
  begin
    if not CleanPosToCaret(CleanPos,CodePos) then exit;
    AddCodePosition(ListOfPCodeXYPosition,CodePos);
  end;

  function Scan(StartPos, EndPos: integer): boolean;
  var
    p: LongInt;
    pp: PChar;
  begin
    // read comments (start in front of node)
    //DebugLn(['TPascalReaderTool.GetPasDocComments Scan Src=',copy(Src,StartPos,EndPos-StartPos)]);
    if EndPos>SrcLen then EndPos:=SrcLen+1;
    p:=FindLineEndOrCodeInFrontOfPosition(StartPos,true);
    while p<EndPos do begin
      p:=FindNextComment(Src,p,EndPos);
      if (p>=EndPos) then break;
      pp:=@Src[p];
      if ((pp^='/') and (pp[1]='/') and (pp[2] in ['$','%']))
      or ((pp^='{') and (pp[1] in ['$','%']))
      or ((pp^='(') and (pp[1]='*') and (pp[2] in ['$','%']))
      then
        break;
      //debugln(['TStandardCodeTool.GetPasDocComments Comment="',copy(Src,p,FindCommentEnd(Src,p,Scanner.NestedComments)-p),'"']);
      if (p<StartPos) then begin
        // comment in front of node
        if not CommentBelongsToPrior(p) then
          Add(p);
      end else if (p<EndPos) then begin
        // comment in the middle or behind
        if CommentBelongsToPrior(p) then
          Add(p);
      end;
      p:=FindCommentEnd(Src,p,Scanner.NestedComments);
    end;
    Result:=true;
  end;

var
  NextNode: TCodeTreeNode;
  EndPos: LongInt;
  TypeNode: TCodeTreeNode;
begin
  ListOfPCodeXYPosition:=nil;
  Result:=false;
  if (Node=nil) then exit;
  if (Node.Desc=ctnProcedureHead)
  and (Node.Parent<>nil) and (Node.Parent.Desc=ctnProcedure) then
    Node:=Node.Parent;

  // add space behind node to scan range
  NextNode:=Node.Next;
  if NextNode<>nil then
    EndPos:=NextNode.StartPos
  else
    EndPos:=Node.EndPos;

  // scan range for comments
  if not Scan(Node.StartPos,EndPos) then exit;

  if Node.Desc in AllIdentifierDefinitions then begin
    // scan behind type
    // for example:   i: integer; // comment
    TypeNode:=FindTypeNodeOfDefinition(Node);
    if TypeNode<>nil then begin
      NextNode:=TypeNode.Next;
      if NextNode<>nil then
        EndPos:=NextNode.StartPos
      else
        EndPos:=Node.EndPos;
      if not Scan(TypeNode.EndPos,EndPos) then exit;
    end;
  end;
  Result:=true;
end;

procedure TPascalReaderTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
  Stats.Add('TPascalReaderTool',MemSizeString(CachedSourceName));
end;

end.

