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
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, CustomCodeTool,
  MultiKeyWordListTool, PascalParserTool, SourceLog, KeywordFuncLists,
  BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo, SourceChanger;

type
  TPascalReaderTool = class(TPascalParserTool)
  public
    function FindDeepestExpandedNodeAtPos(CleanCursorPos: integer;
        ExceptionOnNotFound: boolean): TCodeTreeNode;

    // comments
    function CleanPosIsInComment(CleanPos, CleanCodePosInFront: integer;
        var CommentStart, CommentEnd: integer): boolean;

    // general extraction
    function ExtractNode(ANode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractCode(StartPos, EndPos: integer;
        Attr: TProcHeadAttributes): string;
    function ExtractIdentCharsFromStringConstant(
        StartPos, MaxLen: integer): string;
    function ReadStringConstantValue(StartPos: integer): string;

    // properties
    function ExtractPropType(PropNode: TCodeTreeNode;
        InUpperCase, EmptyIfIndexed: boolean): string;
    function MoveCursorToPropType(PropNode: TCodeTreeNode): boolean;
    function ExtractPropName(PropNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractProperty(PropNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function GetPropertyNameIdentifier(PropNode: TCodeTreeNode): PChar;
    function PropertyIsDefault(PropertyNode: TCodeTreeNode): boolean;
    function PropertyNodeHasParamList(PropNode: TCodeTreeNode): boolean;
    function PropNodeIsTypeLess(PropNode: TCodeTreeNode): boolean;

    // procs
    function ExtractProcName(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractProcHead(ProcNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode): string;
    function ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    function GetProcNameIdentifier(ProcNode: TCodeTreeNode): PChar;
    function FindProcNode(StartNode: TCodeTreeNode; const AProcHead: string;
        Attr: TProcHeadAttributes): TCodeTreeNode;
    function FindProcBody(ProcNode: TCodeTreeNode): TCodeTreeNode;
    procedure MoveCursorToFirstProcSpecifier(ProcNode: TCodeTreeNode);
    function MoveCursorToProcSpecifier(ProcNode: TCodeTreeNode;
        ProcSpec: TProcedureSpecifier): boolean;
    function ProcNodeHasParamList(ProcNode: TCodeTreeNode): boolean;
    function NodeIsInAMethod(Node: TCodeTreeNode): boolean;
    function NodeIsMethodBody(ProcNode: TCodeTreeNode): boolean;
    function NodeIsFunction(ProcNode: TCodeTreeNode): boolean;
    function NodeIsConstructor(ProcNode: TCodeTreeNode): boolean;

    // classes
    function ExtractClassName(ClassNode: TCodeTreeNode;
        InUpperCase: boolean): string;
    function ExtractClassInheritance(ClassNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): string;
    function FindClassNode(StartNode: TCodeTreeNode;
        const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindClassNodeInInterface(const UpperClassName: string;
        IgnoreForwards, IgnoreNonForwards: boolean): TCodeTreeNode;
    function FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode): TCodeTreeNode;
    function ClassSectionNodeStartsWithWord(ANode: TCodeTreeNode): boolean;

    // variables, types
    function FindVarNode(StartNode: TCodeTreeNode;
        const UpperVarName: string): TCodeTreeNode;
    function FindTypeNodeOfDefinition(
        DefinitionNode: TCodeTreeNode): TCodeTreeNode;
    function NodeIsPartOfTypeDefinition(ANode: TCodeTreeNode): boolean;

    // sections
    function GetSourceType: TCodeTreeNodeDesc;
    function GetSourceNamePos(var NamePos: TAtomPosition): boolean;
    function ExtractSourceName: string;
    function FindInterfaceNode: TCodeTreeNode;
    function FindImplementationNode: TCodeTreeNode;
    function FindInitializationNode: TCodeTreeNode;
    function FindMainBeginEndNode: TCodeTreeNode;

    // uses sections
    procedure MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
    procedure ReadPriorUsedUnit(var UnitNameAtom, InAtom: TAtomPosition);
  end;

implementation

{ TPascalReaderTool }

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
  if (PropNode=nil) or (PropNode.Desc<>ctnProperty) then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if not UpAtomIs('PROPERTY') then exit;
  ReadNextAtom;
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
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  if phpInUpperCase in Attr then
    Result:=GetUpAtom
  else
    Result:=GetAtom;
  ReadNextAtom;
  if (CurPos.Flag=cafPoint) then begin
    if (phpWithoutClassName in Attr) then begin
      Result:='';
    end else begin
      if not (phpWithoutName in Attr) then
        Result:=Result+'.';
    end;
    ReadNextAtom;
    if not (phpWithoutName in Attr) then begin
      if phpInUpperCase in Attr then
        Result:=Result+GetUpAtom
      else
        Result:=Result+GetAtom;
    end;
  end else begin
    if phpWithoutName in Attr then
      Result:='';
  end;
end;

function TPascalReaderTool.ExtractProcHead(ProcNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
var
  GrandPaNode: TCodeTreeNode;
  TheClassName, s: string;
  HasClassName, IsProcType: boolean;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (ProcNode=nil) or (ProcNode.StartPos<1) then exit;
  if ProcNode.Desc=ctnProcedureHead then begin
    ProcNode:=ProcNode.Parent;
    if ProcNode=nil then exit;
  end;
  if not ProcNode.Desc in [ctnProcedure,ctnProcedureType] then exit;
  IsProcType:=(ProcNode.Desc=ctnProcedureType);
  if (phpAddClassname in Attr) then begin
    GrandPaNode:=ProcNode.Parent;
    if GrandPaNode=nil then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if (GrandPaNode=nil) or (GrandPaNode.Desc<>ctnClass) then exit;
    GrandPaNode:=GrandPaNode.Parent;
    if GrandPaNode.Desc<>ctnTypeDefinition then exit;
    MoveCursorToCleanPos(GrandPaNode.StartPos);
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    TheClassName:=GetAtom;
  end;
  InitExtraction;
  // reparse the clean source
  MoveCursorToNodeStart(ProcNode);
  // parse procedure head = start + name + parameterlist + result type ;
  ExtractNextAtom(false,Attr);
  // read procedure start keyword
  if (UpAtomIs('CLASS') or UpAtomIs('STATIC')) then
    ExtractNextAtom((phpWithStart in Attr)
                    and not (phpWithoutClassKeyword in Attr),Attr);
  if (UpAtomIs('PROCEDURE')) or (UpAtomIs('FUNCTION'))
  or (UpAtomIs('CONSTRUCTOR')) or (UpAtomIs('DESTRUCTOR'))
  or (UpAtomIs('OPERATOR')) then
    ExtractNextAtom(phpWithStart in Attr,Attr)
  else
    exit;
  ExtractProcHeadPos:=phepStart;
  if not IsProcType then begin
    // read name
    if not AtomIsIdentifier(false) then exit;
    ReadNextAtom;
    HasClassName:=(CurPos.Flag=cafPoint);
    UndoReadNextAtom;
    if HasClassName then begin
      // read class name
      ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
      // read '.'
      ExtractNextAtom(not (phpWithoutClassName in Attr),Attr);
      // read name
      if not AtomIsIdentifier(false) then exit;
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
        ExtractNextAtom(false,Attr);
        if phpInUpperCase in Attr then s:=UpperCaseStr(s);
        if ExtractStreamEndIsIdentChar then
          s:=' '+s;
        ExtractMemStream.Write(s[1],length(s));
      end;
    end;
    ExtractProcHeadPos:=phepName;
  end;
  // read parameter list
  if (CurPos.Flag=cafRoundBracketOpen) then
    ReadParamList(false,true,Attr);
  ExtractProcHeadPos:=phepParamList;
  // read result type
  if (CurPos.Flag=cafColon) then begin
    ExtractNextAtom(phpWithResultType in Attr,Attr);
    if not AtomIsIdentifier(false) then exit;
    ExtractNextAtom(phpWithResultType in Attr,Attr);
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
    ExtractNextAtom(true,Attr);
  // read specifiers
  if phpWithCallingSpecs in Attr then begin
    while (CurPos.StartPos<=ProcNode.FirstChild.EndPos) do begin
      if CurPos.Flag=cafSemicolon then begin
        ExtractNextAtom(false,Attr);
      end else begin
        if (UpAtomIs('INLINE') or UpAtomIs('CDECL')) then begin
          ExtractNextAtom(phpWithCallingSpecs in Attr,Attr);
          ExtractMemStream.Write(';',1);
        end
        else if (CurPos.Flag=cafEdgedBracketOpen) then begin
          ReadTilBracketClose(false);
          ExtractNextAtom(false,Attr);
        end else begin
          ExtractNextAtom(false,Attr);
        end;
      end;
    end;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction;
end;

function TPascalReaderTool.ExtractClassName(ClassNode: TCodeTreeNode;
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

function TPascalReaderTool.ExtractClassInheritance(
  ClassNode: TCodeTreeNode; Attr: TProcHeadAttributes): string;
begin
  Result:='';
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then exit;
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // class
  if UpAtomIs('PACKED') then ReadNextAtom;
  if not UpAtomIs('CLASS') then exit;
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
  Result:=GetExtraction;
end;

function TPascalReaderTool.ExtractClassNameOfProcNode(ProcNode: TCodeTreeNode
  ): string;
var TheClassName: string;
begin
  Result:='';
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) then
    ProcNode:=ProcNode.FirstChild;
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedureHead) then exit;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  TheClassName:=GetAtom;
  ReadNextAtom;
  if (CurPos.Flag<>cafPoint) then exit;
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  Result:=TheClassName;
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
    //writeln('TPascalParserTool.FindProcNode A "',NodeDescriptionAsString(Result.Desc),'"');
    if Result.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
               and ((Result.SubDesc and ctnsForwardDeclaration)>0)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(Result)<>nil))) then
      begin
        CurProcHead:=ExtractProcHead(Result,Attr);
        //writeln('TPascalParserTool.FindProcNode B "',CurProcHead,'" =? "',AProcHead,'"');
        if (CurProcHead<>'')
        and (CompareTextIgnoringSpace(CurProcHead,AProcHead,false)=0) then
          exit;
      end;
    end;
    // next node
    Result:=FindNextNodeOnSameLvl(Result);
  end;
end;

function TPascalReaderTool.FindProcBody(ProcNode: TCodeTreeNode
  ): TCodeTreeNode;
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

procedure TPascalReaderTool.MoveCursorToFirstProcSpecifier(
  ProcNode: TCodeTreeNode);
// After the call,
// CurPos will stand on the first proc specifier or on a semicolon
begin
  if (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure) then begin
    SaveRaiseException('Internal Error in'
      +' TPascalParserTool.MoveCursorFirstProcSpecifier: '
      +' (ProcNode=nil) or (ProcNode.Desc<>ctnProcedure)');
  end;
  MoveCursorToNodeStart(ProcNode.FirstChild);
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
  if (CurPos.Flag=cafColon) then begin
    // read function result type
    ReadNextAtom;
    ReadNextAtom;
  end;
  // CurPos now stands on the first proc specifier or on a semicolon
end;

function TPascalReaderTool.MoveCursorToProcSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin
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

function TPascalReaderTool.MoveCursorToPropType(PropNode: TCodeTreeNode
  ): boolean;
begin
  Result:=false;
  if (PropNode=nil) or (PropNode.Desc<>ctnProperty) then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom;
  if not UpAtomIs('PROPERTY') then exit;
  ReadNextAtom;
  AtomIsIdentifier(true);
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  if CurPos.Flag in [cafSemicolon,cafEND] then exit;
  if not (CurPos.Flag=cafColon) then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[':',GetAtom]);
  ReadNextAtom;
  AtomIsIdentifier(true);
end;

function TPascalReaderTool.ProcNodeHasSpecifier(ProcNode: TCodeTreeNode;
  ProcSpec: TProcedureSpecifier): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=MoveCursorToProcSpecifier(ProcNode,ProcSpec);
end;

function TPascalReaderTool.GetProcNameIdentifier(ProcNode: TCodeTreeNode
  ): PChar;
begin

  // ToDo: ppu, ppw, dcu

  Result:=nil;
  if ProcNode=nil then exit;
  if ProcNode.Desc=ctnProcedure then begin
    ProcNode:=ProcNode.FirstChild;
    if ProcNode=nil then exit;
  end;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom;
  if not AtomIsIdentifier(false) then exit;
  Result:=@Src[CurPos.StartPos];
  ReadNextAtom;
  if not AtomIsChar('.') then exit;
  ReadNextAtom;
  Result:=@Src[CurPos.StartPos];
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
  Result:=GetExtraction;
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
  Result:=GetExtraction;
end;

function TPascalReaderTool.ExtractPropName(PropNode: TCodeTreeNode;
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
    Result:=GetUpAtom
  else
    Result:=GetAtom;
end;

function TPascalReaderTool.ExtractProperty(PropNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): string;
begin
  Result:='';
  ExtractProcHeadPos:=phepNone;
  if (PropNode=nil) or (PropNode.StartPos<1) or (PropNode.Desc<>ctnProperty)
  then exit;
  // start extraction
  InitExtraction;
  MoveCursorToNodeStart(PropNode);
  ExtractNextAtom(false,Attr);
  // parse 'property'
  ExtractNextAtom(phpWithStart in Attr,Attr);
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
    ExtractProcHeadPos:=phepResultType;
  end;

  // copy memorystream to Result string
  Result:=GetExtraction;
end;

function TPascalReaderTool.GetPropertyNameIdentifier(PropNode: TCodeTreeNode
  ): PChar;
begin

  // ToDo: ppu, ppw, dcu

  Result:=nil;
  if PropNode=nil then exit;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'propery'
  ReadNextAtom; // read name
  Result:=@Src[CurPos.StartPos];
end;

function TPascalReaderTool.ExtractIdentCharsFromStringConstant(StartPos,
  MaxLen: integer): string;
var
  APos: Integer;
  IdentStartPos: Integer;
  IdentStr: String;
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
        while (IdentStartPos<SrcLen) and (IsIdentChar[Src[IdentStartPos]]) do
          inc(IdentStartPos);
        if IdentStartPos>APos then begin
          if IdentStartPos-APos+length(Result)>MaxLen then
            IdentStartPos:=APos+MaxLen-length(Result);
          IdentStr:=copy(Src,APos,IdentStartPos-APos);
          if (Result<>'') and (IdentStr<>'') then
            IdentStr[1]:=UpChars[IdentStr[1]];
          Result:=Result+IdentStr;
        end;
        APos:=IdentStartPos;
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
  APos: Integer;
  Run: Integer;
  NumberStart: Integer;
  ResultLen: Integer;
  Number: Integer;
begin
  Result:='';
  // first read and calculate the resulting length, then copy the chars
  for Run:=1 to 2 do begin
    APos:=StartPos;
    ResultLen:=0;
    while APos<=SrcLen do begin
      if Src[APos]='''' then begin
        // read string
        inc(APos);
        while APos<=SrcLen do begin
          if (Src[APos]='''') then begin
            if (APos<SrcLen) and (Src[APos+1]='''') then begin
              // a double ' means a single '
              inc(ResultLen);
              if Run=2 then Result[ResultLen]:='''';
              inc(APos,2);
            end else begin
              // a single ' means end of string constant
              inc(APos);
              break;
            end;
          end else begin
            // normal char
            inc(ResultLen);
            if Run=2 then Result[ResultLen]:=Src[APos];
            inc(APos);
          end;
        end;
      end else if Src[APos]='#' then begin
        // read char constant
        inc(APos);
        NumberStart:=APos;
        if APos<=SrcLen then begin
          if IsNumberChar[Src[APos]] then begin
            // read decimal number
            while (APos<=SrcLen) and IsNumberChar[Src[APos]] do
              inc(APos);
            Number:=StrToIntDef(copy(Src,NumberStart,APos-NumberStart),-1);
          end else if Src[APos]='$' then begin
            // read hexnumber
            while (APos<=SrcLen) and IsHexNumberChar[Src[APos]] do
              inc(APos);
            Number:=StrToIntDef(copy(Src,NumberStart,APos-NumberStart),-1);
          end else
            Number:=-1;
          // add special character
          if (Number<0) or (Number>255) then break;
          inc(ResultLen);
          if Run=2 then Result[ResultLen]:=chr(Number);
        end;
      end else
        break;
    end;
    if Run=1 then SetLength(Result,ResultLen);
  end;
end;

function TPascalReaderTool.FindDeepestExpandedNodeAtPos(
  CleanCursorPos: integer; ExceptionOnNotFound: boolean): TCodeTreeNode;
begin
  Result:=FindDeepestNodeAtPos(CleanCursorPos,ExceptionOnNotFound);
  if Result=nil then exit;
  if Result.Desc in [ctnClass,ctnClassInterface] then begin
    BuildSubTreeForClass(Result);
    Result:=FindDeepestNodeAtPos(CleanCursorPos,true);
    if Result=nil then exit;
  end;
  if Result.Desc=ctnBeginBlock then begin
    BuildSubTreeForBeginBlock(Result);
    Result:=FindDeepestNodeAtPos(CleanCursorPos,true);
    if Result=nil then exit;
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
      if (Result<>nil) and (not (Result.Desc in AllPascalTypes)) then
        Result:=nil;
      exit;
    end;
    Result:=Result.NextBrother;
  end;
end;

function TPascalReaderTool.FindClassNode(StartNode: TCodeTreeNode;
  const UpperClassName: string; IgnoreForwards, IgnoreNonForwards: boolean
  ): TCodeTreeNode;
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
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)>0)))
        and (not (IgnoreNonForwards
                 and ((CurClassNode.SubDesc and ctnsForwardDeclaration)=0)))
        then begin
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

function TPascalReaderTool.FindClassNodeInInterface(
  const UpperClassName: string; IgnoreForwards, IgnoreNonForwards: boolean
  ): TCodeTreeNode;
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

function TPascalReaderTool.FindFirstIdentNodeInClass(ClassNode: TCodeTreeNode
  ): TCodeTreeNode;
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

function TPascalReaderTool.ClassSectionNodeStartsWithWord(ANode: TCodeTreeNode
  ): boolean;
var p: integer;
begin
  Result:=false;
  if ANode=nil then exit;
  p:=ANode.StartPos;
  while (p<ANode.EndPos) and (IsIdentChar[Src[p]]) do inc(p);
  if (p=ANode.StartPos) then exit;
  Result:=true;
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
  ReadNextAtom; // read name
  NamePos:=CurPos;
  Result:=(NamePos.StartPos<=SrcLen);
end;

function TPascalReaderTool.ExtractSourceName: string;
var
  NamePos: TAtomPosition;
begin
  GetSourceNamePos(NamePos);
  Result:=GetAtom;
end;

function TPascalReaderTool.FindInterfaceNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnInterface) do
    Result:=Result.NextBrother;
end;

function TPascalReaderTool.FindImplementationNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
end;

function TPascalReaderTool.FindInitializationNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  while (Result<>nil) and (Result.Desc<>ctnInitialization) do
    Result:=Result.NextBrother;
end;

function TPascalReaderTool.FindMainBeginEndNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if (Result=nil) then exit;
  if (Result.Desc=ctnProgram) then
    Result:=Result.LastChild
  else begin
    Result:=FindImplementationNode;
    if Result<>nil then
      Result:=Result.LastChild;
  end;
  if Result=nil then exit;
  if Result.Desc<>ctnBeginBlock then Result:=nil;
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
  if (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) then begin

    // ToDo: ppu, ppw, dcu

    MoveCursorToNodeStart(ProcNode.FirstChild); // ctnProcedureHead
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    ReadNextAtom;
    if (CurPos.Flag<>cafPoint) then exit;
    Result:=true;
    exit;
  end;
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

  // ToDo: ppu, ppw, dcu

  Result:=false;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'property'
  ReadNextAtom; // read name
  ReadNextAtom;
  Result:=(CurPos.Flag=cafEdgedBracketOpen);
end;

function TPascalReaderTool.PropNodeIsTypeLess(PropNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=false;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'property'
  ReadNextAtom; // read name
  ReadNextAtom;
  if CurPos.Flag=cafEdgedBracketOpen then begin
    ReadTilBracketClose(true);
    ReadNextAtom;
  end;
  Result:=(CurPos.Flag<>cafColon);
end;

function TPascalReaderTool.ProcNodeHasParamList(ProcNode: TCodeTreeNode
  ): boolean;
begin

  // ToDo: ppu, ppw, dcu

  Result:=false;
  if ProcNode.Desc=ctnProcedure then
    ProcNode:=ProcNode.FirstChild;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom; // read name
  ReadNextAtom;
  if AtomIsChar('.') then begin
    ReadNextAtom;
    ReadNextAtom;
  end;
  Result:=AtomIsChar('(');
end;

procedure TPascalReaderTool.MoveCursorToUsesEnd(UsesNode: TCodeTreeNode);
begin
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) then
    RaiseException('[TPascalParserTool.MoveCursorToUsesEnd] '
      +'internal error: invalid UsesNode');
  // search backwards through the uses section
  MoveCursorToCleanPos(UsesNode.EndPos);
  ReadPriorAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseExceptionFmt(ctsStrExpectedButAtomFound,[';',GetAtom]);
end;

procedure TPascalReaderTool.ReadPriorUsedUnit(var UnitNameAtom,
  InAtom: TAtomPosition);
begin
  ReadPriorAtom; // read unitname
  if AtomIsStringConstant then begin
    InAtom:=CurPos;
    ReadPriorAtom; // read 'in'
    if not UpAtomIs('IN') then
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[ctsKeywordIn,GetAtom]);
    ReadPriorAtom; // read unitname
  end else
    InAtom.StartPos:=-1;
  AtomIsIdentifier(true);
  UnitNameAtom:=CurPos;
end;

end.

