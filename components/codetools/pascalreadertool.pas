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
  Classes, SysUtils, FileProcs, CodeToolsStrConsts, CodeTree, CodeAtom,
  CustomCodeTool, PascalParserTool, KeywordFuncLists, BasicCodeTools,
  SourceChanger, LinkScanner, AVL_Tree;

type
  TPascalHintModifier = (
    phmDeprecated,
    phmPlatform,
    phmLibrary,
    phmUnimplemented,
    phmExperimental
    );
  TPascalHintModifiers = set of TPascalHintModifier;

  { TPascalReaderTool }

  TPascalReaderTool = class(TPascalParserTool)
  protected
    CachedSourceName: string;
    procedure RaiseStrConstExpected;
  public
    // comments
    function CleanPosIsInComment(CleanPos, CleanCodePosInFront: integer;
        var CommentStart, CommentEnd: integer): boolean;

    // general extraction
    function ExtractNode(ANode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractCode(StartPos, EndPos: integer;
        Attr: TProcHeadAttributes): string;
    function ExtractBrackets(BracketStartPos: integer;
        Attr: TProcHeadAttributes): string;
    function ExtractIdentCharsFromStringConstant(
        StartPos, MinPos, MaxPos, MaxLen: integer): string;
    function ReadStringConstantValue(StartPos: integer): string;
    function GetNodeIdentifier(Node: TCodeTreeNode): PChar;
    function GetHintModifiers(Node: TCodeTreeNode): TPascalHintModifiers;

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
                 const s: string; ExceptionOnNotFound: boolean = true): boolean;

    // procs
    function ExtractProcName(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractProcHead(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode;
        AddParentClasses: boolean = true): string;
    function ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    function GetProcNameIdentifier(ProcNode: TCodeTreeNode): PChar;
    function FindProcNode(StartNode: TCodeTreeNode; const AProcHead: string;
        Attr: TProcHeadAttributes): TCodeTreeNode;
    function FindCorrespondingProcNode(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes = [phpWithoutClassKeyword,phpWithoutClassName]
        ): TCodeTreeNode;
    function FindCorrespondingProcParamNode(ProcParamNode: TCodeTreeNode;
        Attr: TProcHeadAttributes = [phpInUpperCase,phpWithoutClassName,phpWithVarModifiers]
        ): TCodeTreeNode;
    function FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
    function ProcBodyIsEmpty(ProcNode: TCodeTreeNode): boolean;
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
    function ProcNodeHasParamList(ProcNode: TCodeTreeNode): boolean;
    function GetProcParamList(ProcNode: TCodeTreeNode;
                              Parse: boolean = true): TCodeTreeNode;
    function NodeIsInAMethod(Node: TCodeTreeNode): boolean;
    function NodeIsMethodBody(ProcNode: TCodeTreeNode): boolean;
    function GetMethodOfBody(Node: TCodeTreeNode): TCodeTreeNode;
    function NodeIsFunction(ProcNode: TCodeTreeNode): boolean;
    function NodeIsConstructor(ProcNode: TCodeTreeNode): boolean;
    function NodeIsDestructor(ProcNode: TCodeTreeNode): boolean;
    function NodeIsForwardProc(ProcNode: TCodeTreeNode): boolean;
    function NodeIsOperator(ProcNode: TCodeTreeNode): boolean;
    function NodeIsResultIdentifier(Node: TCodeTreeNode): boolean;
    function NodeIsResultType(Node: TCodeTreeNode): boolean;

    // classes
    function ExtractClassName(ClassNode: TCodeTreeNode;
        InUpperCase: boolean; WithParents: boolean = true): string;
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
    function FindClassNodeInInterface(const AClassName: string;
        IgnoreForwards, IgnoreNonForwards, ErrorOnNotFound: boolean): TCodeTreeNode;
    function FindClassNodeInUnit(const AClassName: string;
        IgnoreForwards, IgnoreNonForwards, IgnoreImplementation,
        ErrorOnNotFound: boolean): TCodeTreeNode;
    function FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function ClassSectionNodeStartsWithWord(ANode: TCodeTreeNode): boolean;
    function IsClassNode(Node: TCodeTreeNode): boolean; // class, not object
    function FindInheritanceNode(ClassNode: TCodeTreeNode): TCodeTreeNode;

    // records
    function ExtractRecordCaseType(RecordCaseNode: TCodeTreeNode): string;

    // variables, types
    function FindVarNode(StartNode: TCodeTreeNode;
        const UpperVarName: string): TCodeTreeNode;
    function FindTypeNodeOfDefinition(
        DefinitionNode: TCodeTreeNode): TCodeTreeNode;
    function NodeIsPartOfTypeDefinition(ANode: TCodeTreeNode): boolean;
    function ExtractDefinitionNodeType(DefinitionNode: TCodeTreeNode): string;
    function ExtractDefinitionName(DefinitionNode: TCodeTreeNode): string;
    function PositionInDefinitionName(DefinitionNode: TCodeTreeNode;
                                      CleanPos: integer): boolean;
    function MoveCursorToParameterSpecifier(DefinitionNode: TCodeTreeNode
                                            ): boolean;
    function GetFirstGroupVarNode(VarNode: TCodeTreeNode): TCodeTreeNode;
    function FindEndOfWithVar(WithVarNode: TCodeTreeNode): integer;
    function NodeIsIdentifierInInterface(Node: TCodeTreeNode): boolean;
    function NodeCanHaveForwardType(TypeNode: TCodeTreeNode): boolean;
    function NodeIsForwardType(TypeNode: TCodeTreeNode): boolean;
    function FindForwardTypeNode(TypeNode: TCodeTreeNode;
                                 SearchFirst: boolean): TCodeTreeNode;
    function FindTypeOfForwardNode(TypeNode: TCodeTreeNode): TCodeTreeNode;

    // arrays
    function ExtractArrayRange(ArrayNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;

    // module sections
    function GetSourceName(DoBuildTree: boolean = true): string;
    function GetSourceType: TCodeTreeNodeDesc;
    function GetSourceNamePos(var NamePos: TAtomPosition): boolean;
    function PositionInSourceName(CleanPos: integer): boolean;
    function ExtractSourceName: string;

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
    function FindCommentInFront(const StartPos: integer;
          const CommentText: string; SearchInParentNode,
          WithCommentBounds, CaseSensitive, IgnoreSpaces,
          CompareOnlyStart: boolean;
          out CommentStart, CommentEnd: integer): boolean;
    function CommentCode(const StartPos, EndPos: integer;
          SourceChangeCache: TSourceChangeCache; Apply: boolean): boolean;
    function GetPasDocComments(const StartPos: TCodeXYPosition;
                               InvokeBuildTree: boolean;
                               out ListOfPCodeXYPosition: TFPList): boolean;
    function GetPasDocComments(Node: TCodeTreeNode;
                               out ListOfPCodeXYPosition: TFPList): boolean;

    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;

implementation

{ TPascalReaderTool }

procedure TPascalReaderTool.RaiseStrConstExpected;
begin
  RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsStringConstant,GetAtom]);
end;

function TPascalReaderTool.CleanPosIsInComment(CleanPos,
  CleanCodePosInFront: integer; var CommentStart, CommentEnd: integer
  ): boolean;
var CommentLvl, CurCommentPos: integer;
begin
  Result:=false;
  if CleanPos>SrcLen then exit;
  if CleanCodePosInFront>CleanPos then
    SaveRaiseException(
      'TPascalReaderTool.CleanPosIsInComment CleanCodePosInFront>CleanPos');
  MoveCursorToCleanPos(CleanCodePosInFront);
  repeat
    ReadNextAtom;
    if CurPos.StartPos>CleanPos then begin
      //DebugLn(['TPascalReaderTool.CleanPosIsInComment ',GetATom,' StartPos=',CurPos.StartPos,' CleanPos=',CleanPos]);
      // CleanPos between two atoms -> parse space between for comments
      CommentStart:=CleanCodePosInFront;
      CommentEnd:=CurPos.StartPos;
      if CommentEnd>SrcLen then CommentEnd:=SrcLen+1;
      while CommentStart<CommentEnd do begin
        if IsCommentStartChar[Src[CommentStart]] then begin
          CurCommentPos:=CommentStart;
          case Src[CurCommentPos] of
          '{': // pascal comment
            begin
              CommentLvl:=1;
              inc(CurCommentPos);
              while (CurCommentPos<CommentEnd) and (CommentLvl>0) do begin
                case Src[CurCommentPos] of
                '{': if Scanner.NestedComments then inc(CommentLvl);
                '}': dec(CommentLvl);
                end;
                inc(CurCommentPos);
              end;
            end;
          '/':  // Delphi comment
            if (CurCommentPos<CommentEnd-1) and (Src[CurCommentPos+1]='/') then
            begin
              inc(CurCommentPos,2);
              while (CurCommentPos<CommentEnd)
              and (not (Src[CurCommentPos] in [#10,#13])) do
                inc(CurCommentPos);
              inc(CurCommentPos);
              if (CurCommentPos<CommentEnd)
              and (Src[CurCommentPos] in [#10,#13])
              and (Src[CurCommentPos-1]<>Src[CurCommentPos]) then
                inc(CurCommentPos);
            end else
              break;
          '(': // old turbo pascal comment
            if (CurCommentPos<CommentEnd-1) and (Src[CurCommentPos+1]='*') then
            begin
              inc(CurCommentPos,3);
              while (CurCommentPos<CommentEnd)
              and ((Src[CurCommentPos-1]<>'*') or (Src[CurCommentPos]<>')'))
              do
                inc(CurCommentPos);
              inc(CurCommentPos);
            end else
              break;
          end;
          if (CurCommentPos>CommentStart) and (CleanPos<CurCommentPos) then
          begin
            // CleanPos in comment
            CommentEnd:=CurCommentPos;
            Result:=true;
            exit;
          end;
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
  AtomIsIdentifier(true);
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    if EmptyIfIndexed then exit;
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if CurPos.Flag in [cafSemicolon,cafEND] then exit;
  if not (CurPos.Flag=cafColon) then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[':',GetAtom]);
  ReadNextAtom;
  AtomIsIdentifier(true);
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
  repeat
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    if phpInUpperCase in Attr then
      Part:=GetUpAtom
    else
      Part:=GetAtom;
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then begin
      // end of method identifier is the proc name
      if phpWithoutName in Attr then exit;
      if Result<>'' then Result:=Result+'.';
      Result:=Result+Part;
      exit;
    end;
    if not (phpWithoutClassName in Attr) then begin
      // in front of . is class name
      if Result<>'' then Result:=Result+'.';
      Result:=Result+Part;
    end;
  until false;
end;

function TPascalReaderTool.ExtractProcHead(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  TheClassName, s: string;
  IsClassName, IsProcType: boolean;
  IsProcedure: Boolean;
  IsFunction: Boolean;
  IsOperator: Boolean;
const
  SemiColon : char = ';';
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ProcNode=nil) or (ProcNode.StartPos<1) then exit;
  if ProcNode.Desc=ctnProcedureHead then begin
    ProcNode:=ProcNode.Parent;
    if ProcNode=nil then exit;
  end;
  if (ProcNode.Desc<>ctnProcedure) and (ProcNode.Desc<>ctnProcedureType) then
    exit;
  IsProcType:=(ProcNode.Desc=ctnProcedureType);

  // build full class name
  TheClassName:='';
  if ([phpAddClassname,phpWithoutClassName]*Attr=[phpAddClassName]) then
    TheClassName:=ExtractClassName(ProcNode,phpInUpperCase in Attr,true);

  // reparse the clean source
  InitExtraction;
  MoveCursorToNodeStart(ProcNode);
  // parse procedure head = start + name + parameterlist + result type ;
  ExtractNextAtom(false,Attr);
  // read procedure start keyword
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
    if (not IsOperator) and (not AtomIsIdentifier(false)) then exit;

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
        if Scanner.CompilerMode = cmDELPHI then
        begin { delphi generics }
          if AtomIsChar('<') then
          begin
            while not AtomIsChar('>') and (CurPos.EndPos < SrcLen) do
              ExtractNextAtom(true,Attr);
            ExtractNextAtom(true,Attr);
          end;
        end;
        if CurPos.Flag<>cafPoint then break;
        ExtractNextAtom(true,Attr);
        if not AtomIsIdentifier(false) then exit;
      until false;
    end else begin
      // read only part of name
      repeat
        ReadNextAtom;
        if (Scanner.CompilerMode = cmDELPHI) and AtomIsChar('<') then
        begin
          while not AtomIsChar('>') and (CurPos.EndPos < SrcLen) do
            ReadNextAtom;
          ReadNextAtom;
        end;
        IsClassName:=(CurPos.Flag=cafPoint);
        UndoReadNextAtom;
        if IsClassName then begin
          // read class name
          ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
          // read '.'
          ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
          if not AtomIsIdentifier(false) then break;
        end else begin
          // read name
          ExtractNextAtom(not (phpWithoutName in Attr),Attr);
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
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if not AtomIsIdentifier(false) then exit;
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if CurPos.Flag=cafPoint then begin
      ExtractNextAtom(phpWithResultType in Attr,Attr);
      if not AtomIsIdentifier(false) then exit;
      ExtractNextAtom(phpWithResultType in Attr,Attr);
    end;
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
  if [phpWithCallingSpecs,phpWithProcModifiers]*Attr<>[] then begin
    while (CurPos.StartPos<ProcNode.FirstChild.EndPos) do begin
      if CurPos.Flag=cafSemicolon then begin
        ExtractNextAtom(phpWithProcModifiers in Attr,Attr);
      end else begin
        if IsKeyWordCallingConvention.DoIdentifier(@Src[CurPos.StartPos])
        then begin
          ExtractNextAtom([phpWithCallingSpecs,phpWithProcModifiers]*Attr<>[],
                          Attr);
          if not (phpWithProcModifiers in Attr) then
            ExtractMemStream.Write(SemiColon,1);
        end
        else if (CurPos.Flag=cafEdgedBracketOpen) then begin
          ReadTilBracketClose(false);
          ExtractNextAtom(phpWithProcModifiers in Attr,Attr);
        end else begin
          ExtractNextAtom(phpWithProcModifiers in Attr,Attr);
        end;
      end;
    end;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
  
  // add semicolon
  if ([phpWithoutSemicolon,phpDoNotAddSemicolon]*Attr=[])
  and (Result<>'') and (Result[length(Result)]<>';') then
    Result:=Result+';';
end;

function TPascalReaderTool.ExtractClassName(ClassNode: TCodeTreeNode;
  InUpperCase: boolean; WithParents: boolean): string;
begin
  Result:='';
  while ClassNode<>nil do begin
    if ClassNode.Desc in [ctnTypeDefinition,ctnGenericType] then begin
      if Result<>'' then Result:='.'+Result;
      if ClassNode.Desc=ctnTypeDefinition then
        Result:=GetIdentifier(@Src[ClassNode.StartPos])+Result
      else if ClassNode.FirstChild<>nil then
      begin
        if (Scanner.CompilerMode = cmDELPHI) and (ClassNode.Desc = ctnGenericType) then
          Result := Result + ExtractNode(ClassNode.FirstChild.NextBrother, []);
        Result:=GetIdentifier(@Src[ClassNode.FirstChild.StartPos])+Result;
      end;
      if not WithParents then break;
    end;
    ClassNode:=ClassNode.Parent;
  end;

  if InUpperCase then
    Result:=UpperCaseStr(Result);
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
  if not AtomIsIdentifier(false) then exit;
  MoveCursorToCleanPos(CurPos.StartPos);
  ExtractProcHeadPos:=phepNone;
  InitExtraction;
  while (CurPos.StartPos<=SrcLen) do begin
    ExtractNextAtom(true,Attr); // read ancestor/interface
    if not AtomIsIdentifier(false) then break;
    ExtractNextAtom(true,Attr); // read ','
    if not AtomIsChar(',') then break;
  end;
  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode;
  AddParentClasses: boolean): string;
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
    if not AtomIsIdentifier(false) then break;
    Part:=GetAtom;
    ReadNextAtom;
    if (Scanner.CompilerMode = cmDELPHI) and AtomIsChar('<') then
    begin { delphi generics }
      Part := Part + GetAtom;
      repeat
        ReadNextAtom;
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
  const AProcHead: string; Attr: TProcHeadAttributes): TCodeTreeNode;
// search in all next brothers for a Procedure Node with the Name ProcName
// if there are no further brothers and the parent is a section node
// ( e.g. 'interface', 'implementation', ...) or a class visibility node
// (e.g. 'public', 'private', ...) then the search will continue in the next
// section
var CurProcHead: string;
begin
  Result:=StartNode;
  while (Result<>nil) do begin
    //DebugLn('TPascalReaderTool.FindProcNode A "',NodeDescriptionAsString(Result.Desc),'"');
    if Result.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
               and ((Result.SubDesc and ctnsForwardDeclaration)>0)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(Result)<>nil))) then
      begin
        CurProcHead:=ExtractProcHead(Result,Attr);
        //DebugLn('TPascalReaderTool.FindProcNode B "',CurProcHead,'" =? "',AProcHead,'"');
        if (CurProcHead<>'')
        and (CompareTextIgnoringSpace(CurProcHead,AProcHead,false)=0) then
          exit;
      end;
    end;
    // next node
    Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalReaderTool.FindCorrespondingProcNode(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): TCodeTreeNode;
var
  ClassNode: TCodeTreeNode;
  StartNode: TCodeTreeNode;
  ProcHead: String;
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
    StartNode:=ClassNode.GetTopMostNodeOfType(ctnTypeSection)
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

  ProcHead:=ExtractProcHead(ProcNode,Attr);
  //debugln('TPascalReaderTool.FindCorrespondingProcNode StartNode=',StartNode.DescAsString,' ProcHead=',dbgstr(ProcHead));
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

function TPascalReaderTool.FindProcBody(ProcNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=ProcNode;
  if Result=nil then exit;
  if Result.Desc<>ctnProcedure then exit;
  Result:=Result.FirstChild;
  while Result<>nil do begin
    if Result.Desc in [ctnBeginBlock,ctnAsmBlock] then
      exit;
    Result:=Result.NextBrother;
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
  if not UpAtomIs('END') then exit;
  Result:=true;
end;

procedure TPascalReaderTool.MoveCursorToFirstProcSpecifier(
  ProcNode: TCodeTreeNode);
// After the call,
// CurPos will stand on the first proc specifier or on a semicolon
begin
  //DebugLn(['TPascalReaderTool.MoveCursorToFirstProcSpecifier ',ProcNode.DescAsString,' ',ProcNode.StartPos]);
  if (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedureHead) then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then begin
    SaveRaiseException('Internal Error in'
      +' TPascalParserTool.MoveCursorFirstProcSpecifier: '
      +' (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)');
  end;
  if (ProcNode.LastChild<>nil) and (ProcNode.LastChild.Desc=ctnIdentifier) then
  begin
    // jump behind function result type
    MoveCursorToCleanPos(ProcNode.LastChild.EndPos);
    ReadNextAtom;
  end else if (ProcNode.FirstChild<>nil)
    and (ProcNode.FirstChild.Desc=ctnParameterList)
  then begin
    // jump behind parameter list
    MoveCursorToCleanPos(ProcNode.FirstChild.EndPos);
    ReadNextAtom;
  end else begin
    MoveCursorToNodeStart(ProcNode);
    ReadNextAtom;
    if AtomIsIdentifier(false) then begin
      // read name
      ReadNextAtom;
      if (CurPos.Flag=cafPoint) then begin
        // read method name
        ReadNextAtom;
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
    if AtomIsIdentifier(false) then begin
      ReadNextAtom;
      if CurPos.Flag=cafPoint then begin
        ReadNextAtom;
        if AtomIsIdentifier(false) then
          ReadNextAtom;
      end;
    end;
  end;
  // CurPos now stands on the first proc specifier or on a semicolon
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
  until not AtomIsIdentifier(false);
end;

procedure TPascalReaderTool.MoveCursorBehindProcName(ProcNode: TCodeTreeNode);
begin
  if (ProcNode.FirstChild<>nil)
  and (ProcNode.FirstChild.Desc=ctnProcedureHead) then
    ProcNode:=ProcNode.FirstChild;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if AtomIsIdentifier(false) then begin
    ReadNextAtom;
    while CurPos.Flag=cafPoint do begin
      ReadNextAtom;
      if not AtomIsIdentifier(false) then exit;
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
    while AtomIsIdentifier(false) do begin
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
  if AtomIsIdentifier(false) then ReadNextAtom;
  if CurPos.Flag<>cafColon then exit;
  Result:=CleanPos<=CurPos.StartPos;
end;

function TPascalReaderTool.MoveCursorToPropType(PropNode: TCodeTreeNode
  ): boolean;
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
  if not AtomIsIdentifier(false) then exit;
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

function TPascalReaderTool.MoveCursorToPropName(PropNode: TCodeTreeNode
  ): boolean;
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
  if not AtomIsIdentifier(false) then exit;
  ReadNextAtom;
end;

function TPascalReaderTool.ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin
  Result:=false;
  if ProcNode=nil then exit;
  if (ProcNode.FirstChild=nil)
  or ((ProcNode.SubDesc and ctnsNeedJITParsing)>0) then
    BuildSubTreeForProcHead(ProcNode);

  // ToDo: ppu, dcu

  Result:=MoveCursorToProcSpecifier(ProcNode,ProcSpec);
end;

function TPascalReaderTool.GetProcNameIdentifier(ProcNode: TCodeTreeNode
  ): PChar;
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
    if not AtomIsIdentifier(false) then exit(nil);
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
    if not AtomIsIdentifier(false) then exit;
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if CurPos.Flag=cafPoint then begin
      // unit.type
      ExtractNextAtom(phpWithResultType in Attr,Attr);
      if not AtomIsIdentifier(false) then exit;
      ExtractNextAtom(phpWithResultType in Attr,Attr);
    end;
    ExtractProcHeadPos:=phepResultType;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction(phpInUpperCase in Attr);
end;

function TPascalReaderTool.GetPropertyNameIdentifier(PropNode: TCodeTreeNode
  ): PChar;
begin
  // ToDo: ppu, dcu

  Result:=nil;
  if PropNode=nil then exit;
  if not MoveCursorToPropName(PropNode) then exit;
  Result:=@Src[CurPos.StartPos];
end;

function TPascalReaderTool.GetPropertyTypeIdentifier(PropNode: TCodeTreeNode
  ): PChar;
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
  ctnEnumIdentifier,ctnIdentifier:
    Result:=@Src[Node.StartPos];
  end;
end;

function TPascalReaderTool.GetHintModifiers(Node: TCodeTreeNode
  ): TPascalHintModifiers;

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

  ctnProcedure,ctnProcedureType,ctnProcedureHead:
    begin
      if Node.Desc<>ctnProcedureHead then begin
        Node:=Node.FirstChild;
        if Node=nil then exit;
      end;
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

function TPascalReaderTool.FindVarNode(StartNode: TCodeTreeNode;
  const UpperVarName: string): TCodeTreeNode;
begin
  Result:=StartNode;
  while Result<>nil do begin
    if (Result.Desc=ctnVarDefinition)
    and (CompareNodeIdentChars(Result,UpperVarName)=0) then
      exit;
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
  const AClassName: string; IgnoreForwards, IgnoreNonForwards: boolean
  ): TCodeTreeNode;
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
  const AClassName: string; IgnoreForwards, IgnoreNonForwards: boolean
  ): TCodeTreeNode;
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

function TPascalReaderTool.FindClassNode(CursorNode: TCodeTreeNode
  ): TCodeTreeNode;
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

function TPascalReaderTool.FindClassNodeInInterface(
  const AClassName: string; IgnoreForwards, IgnoreNonForwards,
  ErrorOnNotFound: boolean): TCodeTreeNode;
  
  procedure RaiseClassNotFound;
  begin
    RaiseExceptionFmt(ctsClassSNotFound, [AClassName]);
  end;
  
begin
  Result:=Tree.Root;
  if Result<>nil then begin
    if Result.Desc=ctnUnit then begin
      Result:=Result.NextBrother;
    end;
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
    RaiseExceptionFmt(ctsClassSNotFound, [AClassName]);
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
  Result:=nil;
  if (ClassNode=nil) then exit;
  Result:=ClassNode.FirstChild;
  while (Result<>nil) and (Result.FirstChild=nil) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
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

function TPascalReaderTool.FindInheritanceNode(ClassNode: TCodeTreeNode
  ): TCodeTreeNode;
begin
  Result:=ClassNode.FirstChild;
  while (Result<>nil) and (Result.Desc in [ctnClassSealed,ctnClassAbstract]) do
    Result:=Result.NextBrother;
  if (Result<>nil) and (Result.Desc<>ctnClassInheritance) then
    Result:=nil;
end;

function TPascalReaderTool.ExtractRecordCaseType(RecordCaseNode: TCodeTreeNode
  ): string;
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

function TPascalReaderTool.GetSourceNamePos(var NamePos: TAtomPosition
  ): boolean;
begin
  Result:=false;
  NamePos.StartPos:=-1;
  if Tree.Root=nil then exit;
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read source type 'program', 'unit' ...
  if (Tree.Root.Desc=ctnProgram) and (not UpAtomIs('PROGRAM')) then exit;
  ReadNextAtom; // read name
  if not AtomIsIdentifier(false) then exit;
  NamePos:=CurPos;
  Result:=true;
  ReadNextAtom;
  while CurPos.Flag=cafPoint do begin
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    NamePos.EndPos:=CurPos.EndPos;
    ReadNextAtom;
  end;
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
begin
  Result:='';
  if Tree.Root<>nil then begin
    MoveCursorToNodeStart(Tree.Root);
    ReadNextAtom; // read source type 'program', 'unit' ...
    if (Tree.Root.Desc<>ctnProgram) or UpAtomIs('PROGRAM') then begin
      ReadNextAtom; // read name
      if AtomIsIdentifier(false) then begin
        Result:=copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
        ReadNextAtom;
        while CurPos.Flag=cafPoint do begin
          ReadNextAtom;
          if not AtomIsIdentifier(false) then exit;
          Result:=Result+'.'+copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
          ReadNextAtom;
        end;
        exit;
      end;
    end;
  end;
  if (Tree.Root<>nil) and (Tree.Root.Desc=ctnProgram) then
    // a program without the 'program' header uses the file name as name
    Result:=ExtractFileNameOnly(MainFilename)
  else
    Result:='';
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
    if not AtomIsIdentifier(false) then exit;
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
  Result:=UpAtomIs('CONSTRUCTOR');
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

function TPascalReaderTool.NodeIsResultIdentifier(Node: TCodeTreeNode
  ): boolean;
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

function TPascalReaderTool.ExtractDefinitionName(DefinitionNode: TCodeTreeNode
  ): string;
begin
  if DefinitionNode.Desc=ctnGenericType then begin
    if DefinitionNode.FirstChild<>nil then
      Result:=GetIdentifier(@Src[DefinitionNode.FirstChild.StartPos])
    else
      Result:='';
  end else begin
    Result:=GetIdentifier(@Src[DefinitionNode.StartPos]);
  end;
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

function TPascalReaderTool.GetFirstGroupVarNode(VarNode: TCodeTreeNode
  ): TCodeTreeNode;
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

function TPascalReaderTool.FindEndOfWithVar(WithVarNode: TCodeTreeNode
  ): integer;
begin
  MoveCursorToCleanPos(WithVarNode.StartPos);
  if not ReadTilVariableEnd(true,true) then exit(-1);
  UndoReadNextAtom;
  Result:=CurPos.EndPos;
end;

function TPascalReaderTool.NodeIsIdentifierInInterface(Node: TCodeTreeNode
  ): boolean;
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

function TPascalReaderTool.NodeCanHaveForwardType(TypeNode: TCodeTreeNode
  ): boolean;
begin
  Result:=false;
  if (TypeNode=nil) or (TypeNode.Desc<>ctnTypeDefinition)
  or (TypeNode.FirstChild=nil) then
    exit;
  if (TypeNode.FirstChild.Desc in AllClasses)
  and (TypeNode.FirstChild.SubDesc and ctnsForwardDeclaration=0) then
    Result:=true;
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

function TPascalReaderTool.GetSourceName(DoBuildTree: boolean): string;
begin
  Result:='';
  if DoBuildTree then
    BuildTree(lsrSourceName);
  CachedSourceName:=ExtractSourceName;
  Result:=CachedSourceName;
end;

function TPascalReaderTool.PropertyIsDefault(PropertyNode: TCodeTreeNode
  ): boolean;
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

function TPascalReaderTool.PropNodeIsTypeLess(PropNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, dcu

  Result:=false;
  if not MoveCursorToPropName(PropNode) then exit;
  ReadNextAtom; // read colon, skip parameters
  if CurPos.Flag=cafEdgedBracketOpen then begin
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  Result:=(CurPos.Flag<>cafColon);
end;

function TPascalReaderTool.PropertyHasSpecifier(PropNode: TCodeTreeNode;
  const s: string; ExceptionOnNotFound: boolean): boolean;
begin

  // ToDo: ppu, dcu

  Result:=false;
  if not MoveCursorToPropName(PropNode) then exit;
  if not AtomIsIdentifier(ExceptionOnNotFound) then exit;
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
    ReadNextAtom;
  end;
  if CurPos.Flag=cafColon then begin
    // read type
    ReadNextAtom;
    if not AtomIsIdentifier(ExceptionOnNotFound) then exit;
    ReadNextAtom;
    if CurPos.Flag=cafPoint then begin
      ReadNextAtom;
      if not AtomIsIdentifier(ExceptionOnNotFound) then exit;
      ReadNextAtom;
    end;
  end;
  // read specifiers
  while not (CurPos.Flag in [cafSemicolon,cafNone]) do begin
    if WordIsPropertySpecifier.DoIdentifier(@Src[CurPos.StartPos])
    then begin
      if AtomIs(s) then exit(true);
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
      if CompareIdentifierPtrs(@Src[CurPos.StartPos],Pointer(s))=0 then exit(true);
    end else if UpAtomIs('ENUMERATOR') then begin
      if CompareIdentifierPtrs(@Src[CurPos.StartPos],Pointer(s))=0 then exit(true);
      ReadNextAtom;
      if not AtomIsIdentifier(false) then exit;
    end else
      exit;
    ReadNextAtom;
  end;
end;

function TPascalReaderTool.ProcNodeHasParamList(ProcNode: TCodeTreeNode
  ): boolean;
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
  if Result=nil then exit;
  if Result.Desc<>ctnParameterList then exit(nil);
end;

procedure TPascalReaderTool.MoveCursorToUsesStart(UsesNode: TCodeTreeNode);
begin
  if (UsesNode=nil)
  or ((UsesNode.Desc<>ctnUsesSection) and (UsesNode.Desc<>ctnContainsSection))
  then
    RaiseException('[TPascalParserTool.MoveCursorToUsesStart] '
      +'internal error: invalid UsesNode');
  // search through the uses section
  MoveCursorToCleanPos(UsesNode.StartPos);
  ReadNextAtom;
  if (not UpAtomIs('USES')) and (not UpAtomIs('CONTAINS')) then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,['uses',GetAtom]);
  ReadNextAtom;
end;

procedure TPascalReaderTool.MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
begin
  if (UsesNode=nil)
  or ((UsesNode.Desc<>ctnUsesSection) and (UsesNode.Desc<>ctnContainsSection))
  then
    RaiseException('[TPascalParserTool.MoveCursorToUsesEnd] '
      +'internal error: invalid UsesNode');
  // search backwards through the uses section
  MoveCursorToCleanPos(UsesNode.EndPos);
  ReadPriorAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[';',GetAtom]);
end;

function TPascalReaderTool.ReadNextUsedUnit(out UnitNameRange,
  InAtom: TAtomPosition; SyntaxExceptions: boolean): boolean;
// after reading CurPos is on atom behind, i.e. comma or semicolon
begin
  Result:=false;
  if not AtomIsIdentifier(SyntaxExceptions) then exit;
  UnitNameRange:=CurPos;
  repeat
    ReadNextAtom;
    if CurPos.Flag<>cafPoint then break;
    ReadNextAtom;
    if not AtomIsIdentifier(SyntaxExceptions) then exit;
    UnitNameRange.EndPos:=CurPos.EndPos;
  until false;
  if UpAtomIs('IN') then begin
    ReadNextAtom; // read filename
    if not AtomIsStringConstant then begin
      if not SyntaxExceptions then exit;
      RaiseStrConstExpected;
    end;
    InAtom:=CurPos;
    ReadNextAtom; // read comma or semicolon
  end else begin
    InAtom:=CleanAtomPosition;
  end;
  Result:=true;
end;

procedure TPascalReaderTool.ReadPriorUsedUnit(out UnitNameRange,
  InAtom: TAtomPosition);
begin
  ReadPriorAtom; // read unitname
  if AtomIsStringConstant then begin
    InAtom:=CurPos;
    ReadPriorAtom; // read 'in'
    if not UpAtomIs('IN') then
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsKeywordIn,GetAtom]);
    ReadPriorAtom; // read unitname
  end else begin
    InAtom:=CleanAtomPosition;
  end;
  AtomIsIdentifier(true);
  UnitNameRange:=CurPos;
  repeat
    ReadPriorAtom;
    if CurPos.Flag<>cafPoint then break;
    ReadPriorAtom;
    AtomIsIdentifier(true);
    UnitNameRange.StartPos:=CurPos.StartPos;
  until false;
end;

function TPascalReaderTool.ExtractUsedUnitNameAtCursor(InFilename: PAnsiString
  ): string;
begin
  if InFilename<>nil then InFilename^:='';
  while CurPos.Flag=cafWord do begin
    if Result<>'' then Result:=Result+'.';
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

function TPascalReaderTool.ReadAndCompareUsedUnit(const AnUnitName: string
  ): boolean;
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
    if not AtomIsIdentifier(false) then exit;
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
      if p='.' then
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

function TPascalReaderTool.FindCommentInFront(const StartPos: integer;
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
      '{': inc(CompareStartPos,1);
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
  CommentLvl: Integer;
  CommentStartPos: LongInt;
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

  //debugln('TPascalReaderTool.FindCommentInFront B Area="',copy(Src,CurPos.StartPos,StartPos-CurPos.StartPos),'"');

  FoundStartPos:=-1;
  repeat
    p:=CurPos.EndPos;
    //debugln('TPascalReaderTool.FindCommentInFront Atom=',GetAtom);

    // read space and comment till next atom
    CommentLvl:=0;
    while true do begin
      case Src[p] of
      #0:
        if p>SrcLen then
          break
        else
          inc(p);
      #1..#32:
        inc(p);
      '{': // pascal comment
        begin
          CommentLvl:=1;
          CommentStartPos:=p;
          inc(p);
          while true do begin
            case Src[p] of
            #0:  if p>SrcLen then break;
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then break;
              end;
            end;
            inc(p);
          end;
          inc(p);
          CompareComment(CommentStartPos,p);
        end;
      '/':  // Delphi comment
        if (Src[p+1]<>'/') then begin
          break;
        end else begin
          CommentStartPos:=p;
          inc(p,2);
          while (not (Src[p] in [#10,#13,#0])) do
            inc(p);
          inc(p);
          if (p<=SrcLen) and (Src[p] in [#10,#13])
          and (Src[p-1]<>Src[p]) then
            inc(p);
          CompareComment(CommentStartPos,p);
        end;
      '(': // old turbo pascal comment
        if (Src[p+1]<>'*') then begin
          break;
        end else begin
          CommentStartPos:=p;
          inc(p,3);
          while (p<=SrcLen)
          and ((Src[p-1]<>'*') or (Src[p]<>')')) do
            inc(p);
          inc(p);
          CompareComment(CommentStartPos,p);
        end;
      else
        break;
      end;
    end;
    ReadNextAtom;
    //DebugLn('TPascalReaderTool.FindCommentInFront NextAtom=',GetAtom);
  until (CurPos.StartPos>=StartPos) or (CurPos.EndPos>=SrcLen);

  Result:=(FoundStartPos>=1);
  CommentStart:=FoundStartPos;
  CommentEnd:=FoundEndPos;
end;

function TPascalReaderTool.CommentCode(const StartPos, EndPos: integer;
  SourceChangeCache: TSourceChangeCache; Apply: boolean): boolean;
var
  i: LongInt;
  CurStartPos: LongInt;
  CommentNeeded: Boolean;
  CurEndPos: LongInt;
begin
  if StartPos>=EndPos then
    RaiseException('TStandardCodeTool CommentCode');

  Result:=false;
  // comment with curly brackets {}
  i:=StartPos;
  CurStartPos:=i;
  CurEndPos:=CurStartPos;
  CommentNeeded:=false;
  repeat
    //debugln(['TPascalReaderTool.CommentCode ',dbgstr(Src[i]),' Needed=',CommentNeeded,' ',dbgstr(copy(Src,CurStartPos,CurEndPos-CurStartPos))]);
    if (Src[i]='{') or (i>=EndPos) then begin
      // the area contains a comment -> comment in front
      if CommentNeeded then begin
        if not SourceChangeCache.Replace(gtNone,gtNone,
          CurStartPos,CurStartPos,'{') then exit;
        if not SourceChangeCache.Replace(gtNone,gtNone,
          CurEndPos,CurEndPos,'}') then exit;
        //DebugLn('Comment "',copy(Src,CurStartPos,i-CurStartPos),'"');
        CommentNeeded:=false;
      end;
      if i>=EndPos then break;
      // skip comment
      i:=FindCommentEnd(Src,i,Scanner.NestedComments)-1;
    end else if not IsSpaceChar[Src[i]] then begin
      if not CommentNeeded then begin
        CurStartPos:=i;
        CommentNeeded:=true;
      end;
      CurEndPos:=i+1;
    end;
    inc(i);
  until false;
  if Apply then
    Result:=SourceChangeCache.Apply
  else
    Result:=true;
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
  begin
    // read comments (start in front of node)
    //DebugLn(['TPascalReaderTool.GetPasDocComments Scan Src=',copy(Src,StartPos,EndPos-StartPos)]);
    if EndPos>SrcLen then EndPos:=SrcLen+1;
    p:=FindLineEndOrCodeInFrontOfPosition(StartPos,true);
    while p<EndPos do begin
      p:=FindNextComment(Src,p,EndPos);
      if (p>=EndPos)
      or ((Src[p]='{') and (Src[p+1]='$'))
      or ((Src[p]='(') and (Src[p+1]='*') and (Src[p+2]='$'))
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

