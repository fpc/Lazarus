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
    TStandardCodeTool enhances TIdentCompletionTool with many standard code
    editing functions for the following categories:
      - source name
      - uses sections
      - lazarus resources
      - Application.CreateForm statements
      - published variables
      - resource strings
      - compiler and IDE directives
      - code exploring
      - code blocks
}
unit StdCodeTools;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE IgnoreErrorAfterCursor}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, FileProcs, CodeTree, CodeAtom,
  FindDeclarationTool, IdentCompletionTool, PascalReaderTool, PascalParserTool,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, LFMTrees,
  SourceChanger, CustomCodeTool, CodeToolsStructs;

type
  TOnGetDefineProperties = procedure(Sender: TObject;
    const ClassContext: TFindContext; LFMNode: TLFMTreeNode;
    const IdentName: string; var DefineProperties: TStrings) of object;


  TStandardCodeTool = class(TIdentCompletionTool)
  private
    CachedSourceName: string;
    function ReadTilGuessedUnclosedBlock(MinCleanPos: integer;
      ReadOnlyOneBlock: boolean): boolean;
    function ReadForwardTilAnyBracketClose: boolean;
    function ReadBackwardTilAnyBracketClose: boolean;
  public
    // explore the code
    function Explore(WithStatements: boolean): boolean;
  
    // source name  e.g. 'unit UnitName;'
    function GetSourceName: string;
    function GetCachedSourceName: string;
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
    function FindUsedUnits(var MainUsesSection,
          ImplementationUsesSection: TStrings): boolean;
    function UsesSectionToFilenames(UsesNode: TCodeTreeNode): TStrings;

    // lazarus resources
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
    function CheckLFM(LFMBuf: TCodeBuffer; var LFMTree: TLFMTree;
                  const OnGetDefineProperties: TOnGetDefineProperties): boolean;

    // Application.Createform statements
    function FindCreateFormStatement(StartPos: integer;
          const UpperClassName, UpperVarName: string;
          var Position: TAtomPosition): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(const AClassName, AVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveCreateFormStatement(const UpperVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function ChangeCreateFormStatement(StartPos: integer;
          const OldClassName, OldVarName: string;
          const NewClassName, NewVarName: string;
          OnlyIfExists: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    function ListAllCreateFormStatements: TStrings;
    function SetAllCreateFromStatements(List: TStrings;
          SourceChangeCache: TSourceChangeCache): boolean;    
          
    // Application.Title:=<string const> statements
    function FindApplicationTitleStatement(var StartPos, StringConstStartPos,
          EndPos: integer): boolean;
    function GetApplicationTitleStatement(StringConstStartPos, EndPos: integer;
          var Title: string): boolean;
    function SetApplicationTitleStatement(const NewTitle: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveApplicationTitleStatement(
          SourceChangeCache: TSourceChangeCache): boolean;

    // forms
    function RenameForm(const OldFormName, OldFormClassName: string;
          const NewFormName, NewFormClassName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function FindFormAncestor(const UpperClassName: string;
          var AncestorClassName: string): boolean;

    // form components
    function FindPublishedVariable(const UpperClassName, UpperVarName: string;
          ExceptionOnClassNotFound: boolean): TCodeTreeNode;
    function AddPublishedVariable(const UpperClassName,VarName, VarType: string;
          SourceChangeCache: TSourceChangeCache): boolean; virtual;
    function RemovePublishedVariable(const UpperClassName, UpperVarName: string;
          ExceptionOnClassNotFound: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedVariable(const UpperClassName,
          UpperOldVarName: string; const NewVarName, VarType: shortstring;
          ExceptionOnClassNotFound: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;

    // blocks (e.g. begin..end)
    function FindBlockCounterPart(const CursorPos: TCodeXYPosition;
          var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindBlockStart(const CursorPos: TCodeXYPosition;
          var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function GuessUnclosedBlock(const CursorPos: TCodeXYPosition;
          var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindBlockCleanBounds(const CursorPos: TCodeXYPosition;
          var BlockCleanStart, BlockCleanEnd: integer): boolean;
      
    // compiler directives
    function GuessMisplacedIfdefEndif(const CursorPos: TCodeXYPosition;
          var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindEnclosingIncludeDirective(const CursorPos: TCodeXYPosition;
          var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindModeDirective(DoBuildTree: boolean;
          var ACleanPos: integer): boolean;
      
    // search & replace
    function ReplaceIdentifiers(IdentList: TStrings;
          SourceChangeCache: TSourceChangeCache): boolean;
    function FindNearestIdentifierNode(const CursorPos: TCodeXYPosition;
          IdentTree: TAVLTree): TAVLTreeNode;

    // expressions
    function GetStringConstBounds(const CursorPos: TCodeXYPosition;
          var StartPos, EndPos: TCodeXYPosition;
          ResolveComments: boolean): boolean;
    function ReplaceCode(const StartPos, EndPos: TCodeXYPosition;
          const NewCode: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function GetStringConstAsFormatString(StartPos, EndPos: integer;
          var FormatStringConstant,FormatParameters: string): boolean;

    // resource strings
    function GatherResourceStringSections(const CursorPos: TCodeXYPosition;
          PositionList: TCodeXYPositions): boolean;
    function IdentifierExistsInResourceStringSection(
          const CursorPos: TCodeXYPosition;
          const ResStrIdentifier: string): boolean;
    function GatherResourceStringsWithValue(const CursorPos: TCodeXYPosition;
          const StringValue: string;
          PositionList: TCodeXYPositions): boolean;
    function GatherResourceStringIdents(const SectionPos: TCodeXYPosition;
          var IdentTree: TAVLTree): boolean;
    function FindNearestResourceString(const CursorPos,
          SectionPos: TCodeXYPosition;
          var NearestPos: TCodeXYPosition): boolean;
    function AddResourceString(const SectionPos: TCodeXYPosition;
          const NewIdentifier, NewValue: string;
          InsertPolicy: TResourcestringInsertPolicy;
          const NearestPos: TCodeXYPosition;
          SourceChangeCache: TSourceChangeCache): boolean;
    function CreateIdentifierFromStringConst(
          const StartCursorPos, EndCursorPos: TCodeXYPosition;
          var Identifier: string; MaxLen: integer): boolean;
    function StringConstToFormatString(
          const StartCursorPos, EndCursorPos: TCodeXYPosition;
          var FormatStringConstant,FormatParameters: string): boolean;
          
    // register procedure
    function HasInterfaceRegisterProc(var HasRegisterProc: boolean): boolean;
    
    // Delphi to Lazarus conversion
    function ConvertDelphiToLazarusSource(AddLRSCode: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    
    // IDE % directives
    function GetIDEDirectives(DirectiveList: TStrings): boolean;
    function SetIDEDirectives(DirectiveList: TStrings;
          SourceChangeCache: TSourceChangeCache): boolean;
  end;


implementation


type
  TBlockKeyword = (bkwNone, bkwBegin, bkwAsm, bkwTry, bkwCase, bkwRepeat,
                   bkwRecord, bkwClass, bkwObject, bkwInterface,
                   bkwDispInterface, bkwEnd, bkwUntil, bkwFinally,
                   bkwExcept);

const
  BlockKeywords: array[TBlockKeyword] of string = (
      '(unknown)', 'BEGIN', 'ASM', 'TRY', 'CASE', 'REPEAT', 'RECORD', 'CLASS',
      'OBJECT', 'INTERFACE', 'DISPINTERFACE', 'END', 'UNTIL', 'FINALLY',
      'EXCEPT'
    );

var
  BlockKeywordFuncList: TKeyWordFunctionList;
  
procedure BuildBlockKeyWordFuncList;
var BlockWord: TBlockKeyword;
begin
  if BlockKeywordFuncList=nil then begin
    BlockKeywordFuncList:=TKeyWordFunctionList.Create;
    for BlockWord:=Low(TBlockKeyword) to High(TBlockKeyword) do
      with BlockKeywordFuncList do
        Add(BlockKeywords[BlockWord],{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;


{ TStandardCodeTool }

function TStandardCodeTool.GetSourceName: string;
var NamePos: TAtomPosition;
begin
  Result:='';
  BuildTree(true);
  if not GetSourceNamePos(NamePos) then exit;
  CachedSourceName:=copy(Src,NamePos.StartPos,NamePos.EndPos-NamePos.StartPos);
  Result:=CachedSourceName;
end;

{-------------------------------------------------------------------------------
  function TStandardCodeTool.GetCachedSourceName: string;
  Params: none
  Result: the source name (= e.g. the identifier behind 'program'/'unit' keyword)

  This function does neither check if source needs reparsing, nor does it check
  for errors in code. It simple checks if there is a first node, which is
  typically the source type and name.
  This function can therefore be used as a fast GetSourceName function.
-------------------------------------------------------------------------------}
function TStandardCodeTool.GetCachedSourceName: string;
begin
  if Tree.Root<>nil then begin
    MoveCursorToNodeStart(Tree.Root);
    ReadNextAtom; // read source type 'program', 'unit' ...
    ReadNextAtom; // read name
    if (CurPos.StartPos<=SrcLen) then
      CachedSourceName:=GetAtom;
  end;
  Result:=CachedSourceName;
end;

function TStandardCodeTool.RenameSource(const NewName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var NamePos: TAtomPosition;
begin
  Result:=false;
  BuildTree(true);
  if (not GetSourceNamePos(NamePos)) or (NamePos.StartPos<1) or (NewName='')
  or (Length(NewName)>255) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.Replace(gtNone,gtNone,NamePos.StartPos,NamePos.EndPos,
    NewName);
  if not SourceChangeCache.Apply then exit;
  CachedSourceName:=NewName;
  Result:=true;
end;

function TStandardCodeTool.FindUnitInUsesSection(UsesNode: TCodeTreeNode;
  const UpperUnitName: string;
  var NamePos, InPos: TAtomPosition): boolean;
begin
  Result:=false;
  NamePos.StartPos:=0;
  InPos.StartPos:=0;
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

function TStandardCodeTool.FindUnitInAllUsesSections(
  const UpperUnitName: string; var NamePos, InPos: TAtomPosition): boolean;
var SectionNode, UsesNode: TCodeTreeNode;
begin
  Result:=false;
  if (UpperUnitName='') or (length(UpperUnitName)>255) then exit;
  BuildTree(false);
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) and (SectionNode.Desc in [ctnProgram, ctnUnit,
    ctnPackage,ctnLibrary,ctnInterface,ctnImplementation]) do begin
    if SectionNode.Desc in [ctnProgram, ctnInterface, ctnImplementation] then
    begin
      UsesNode:=SectionNode.FirstChild;
      if (UsesNode.Desc=ctnUsesSection)
      and FindUnitInUsesSection(UsesNode,UpperUnitName,NamePos,InPos) then begin
        Result:=true;
        exit;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TStandardCodeTool.FindMainUsesSection: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  if Result.Desc=ctnUnit then begin
    Result:=Result.NextBrother;
    if Result=nil then exit;
  end;
  Result:=Result.FirstChild;
  if (Result=nil) then exit;
  if (Result.Desc<>ctnUsesSection) then Result:=nil;
end;

function TStandardCodeTool.FindImplementationUsesSection: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
  if (Result=nil) then exit;
  if (Result.Desc<>ctnUsesSection) then Result:=nil;
end;

function TStandardCodeTool.RenameUsedUnit(const OldUpperUnitName,
  NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var UnitPos, InPos: TAtomPosition;
  NewUsesTerm: string;
begin
  Result:=false;
  if (OldUpperUnitName='') or (length(OldUpperUnitName)>255) or (NewUnitName='')
  or (length(NewUnitName)>255) then exit;
  if not FindUnitInAllUsesSections(OldUpperUnitName,UnitPos,InPos) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  if InPos.StartPos>0 then
    UnitPos.EndPos:=InPos.EndPos;
  // build use unit term
  NewUsesTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUsesTerm:=NewUsesTerm+' in '''+NewUnitInFile+'''';
  //
  if ReplacementNeedsLineEnd(Src,UnitPos.StartPos,UnitPos.EndPos,
    length(NewUsesTerm),SourceChangeCache.BeautifyCodeOptions.LineLength) then
  begin
    if not SourceChangeCache.Replace(gtNewLine,gtNone,
      UnitPos.StartPos,UnitPos.EndPos,NewUsesTerm) then exit;
  end else begin
    if not SourceChangeCache.Replace(gtSpace,gtNone,
      UnitPos.StartPos,UnitPos.EndPos,NewUsesTerm) then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.AddUnitToUsesSection(UsesNode: TCodeTreeNode;
  const NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var LineStart, LineEnd, Indent, InsertPos: integer;
  NewUsesTerm: string;
begin
  Result:=false;
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) or (NewUnitName='')
  or (length(NewUnitName)>255) or (UsesNode.StartPos<1)
  or (UsesNode.EndPos<1) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  MoveCursorToNodeStart(UsesNode); // for nice error position
  InsertPos:=UsesNode.EndPos-1; // position of semicolon at end of uses section
  // build insert text  "unitname in 'file'"
  NewUsesTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUsesTerm:=NewUsesTerm+' in '''+NewUnitInFile+'''';
  // check if insertion would expand the line over the max LineLength
  GetLineStartEndAtPosition(Src,InsertPos,LineStart,LineEnd);
  if InsertPos-LineStart+length(NewUsesTerm)+2>=
    SourceChangeCache.BeautifyCodeOptions.LineLength then
  begin
    // split line
    // calculate the indent
    Indent:=GetLineIndent(Src,InsertPos);
    // if the 'uses' keyword is not in the same line of the insertion position,
    // then indent the new line
    // else keep the indentation.
    if (UsesNode.StartPos>=LineStart)
    and (UsesNode.StartPos<LineEnd) then
      inc(Indent,SourceChangeCache.BeautifyCodeOptions.Indent);
    NewUsesTerm:=','+SourceChangeCache.BeautifyCodeOptions.LineEnd+
      GetIndentStr(Indent)+NewUsesTerm;
  end else
    // simply insert
    NewUsesTerm:=', '+NewUsesTerm;
  if not SourceChangeCache.Replace(gtNone,gtNone,InsertPos,InsertPos,
                                   NewUsesTerm) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.AddUnitToMainUsesSection(const NewUnitName,
  NewUnitInFile: string; SourceChangeCache: TSourceChangeCache): boolean;
var UsesNode, SectionNode: TCodeTreeNode;
  NewUsesTerm: string;
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
    if not(FindUnitInUsesSection(UsesNode,Uppercase(NewUnitName),Junk,Junk))
    then
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
    NewUsesTerm:=SourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('uses')
         +' '+NewUnitName;
    if NewUnitInFile<>'' then
      NewUsesTerm:=NewUsesTerm+' in '''+NewUnitInFile+''';'
    else
      NewUsesTerm:=NewUsesTerm+';';
    InsertPos:=CurPos.EndPos;
    if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
      NewUsesTerm) then exit;
    if not SourceChangeCache.Apply then exit;
    Result:=true;
  end;
end;

function TStandardCodeTool.RemoveUnitFromUsesSection(UsesNode: TCodeTreeNode;
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
    if not AtomIsIdentifier(false) then exit;
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

function TStandardCodeTool.RemoveUnitFromAllUsesSections(
  const UpperUnitName: string; SourceChangeCache: TSourceChangeCache): boolean;
var SectionNode: TCodeTreeNode;
begin
  Result:=false;
  if (UpperUnitName='') or (length(UpperUnitName)>255)
  or (SourceChangeCache=nil) then exit;
  BuildTree(false);
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) do begin
    if (SectionNode.Desc in [ctnProgram,ctnInterface,ctnImplementation])
    and (SectionNode.FirstChild<>nil)
    and (SectionNode.FirstChild.Desc=ctnUsesSection) then begin
      if not RemoveUnitFromUsesSection(SectionNode.FirstChild,UpperUnitName,
         SourceChangeCache)
      then begin
        exit;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
  Result:=true;
end;

function TStandardCodeTool.FindUsedUnits(var MainUsesSection,
  ImplementationUsesSection: TStrings): boolean;
var
  MainUsesNode, ImplementatioUsesNode: TCodeTreeNode;
begin
  MainUsesSection:=nil;
  ImplementationUsesSection:=nil;
  // find the uses sections
  BuildTree(false);
  MainUsesNode:=FindMainUsesSection;
  ImplementatioUsesNode:=FindImplementationUsesSection;
  // create lists
  try
    MainUsesSection:=UsesSectionToFilenames(MainUsesNode);
    ImplementationUsesSection:=UsesSectionToFilenames(ImplementatioUsesNode);
  except
    FreeAndNil(MainUsesSection);
    FreeAndNil(ImplementationUsesSection);
    raise;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TStandardCodeTool.UsesSectionToFilenames(UsesNode: TCodeTreeNode
    ): TStrings;

  Reads the uses section backwards and tries to find each unit file
  The associated objects in the list will be the found codebuffers.
  If no codebuffer was found/created then the filename will be the unit name
  plus the 'in' extension.
------------------------------------------------------------------------------}
function TStandardCodeTool.UsesSectionToFilenames(UsesNode: TCodeTreeNode
  ): TStrings;
var
  InAtom, UnitNameAtom: TAtomPosition;
  AnUnitName, AnUnitInFilename: string;
  NewCode: TCodeBuffer;
  UnitFilename: string;
begin
  Result:=TStringList.Create;
  if UsesNode=nil then exit;
  MoveCursorToUsesEnd(UsesNode);
  repeat
    // read prior unit name
    ReadPriorUsedUnit(UnitNameAtom, InAtom);
    AnUnitName:=GetAtom(UnitNameAtom);
    if InAtom.StartPos>0 then
      AnUnitInFilename:=copy(Src,InAtom.StartPos+1,
                             InAtom.EndPos-InAtom.StartPos-2)
    else
      AnUnitInFilename:='';
    // find unit file
    NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename,false);
    if (NewCode=nil) then begin
      // no source found
      UnitFilename:=AnUnitName;
      if AnUnitInFilename<>'' then
        UnitFilename:=UnitFilename+' in '+AnUnitInFilename;
    end else begin
      // source found
      UnitFilename:=NewCode.Filename;
    end;
    // add filename to list
    Result.AddObject(UnitFilename,NewCode);
    // read keyword 'uses' or comma
    ReadPriorAtom;
  until not AtomIsChar(',');
end;

function TStandardCodeTool.FindNextIncludeInInitialization(
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

function TStandardCodeTool.FindLazarusResourceInBuffer(
  ResourceCode: TCodeBuffer; const ResourceName: string): TAtomPosition;
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

function TStandardCodeTool.FindLazarusResource(
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

function TStandardCodeTool.AddLazarusResource(ResourceCode: TCodeBuffer;
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
      ResourceCode,ResourceCode.SourceLength+1,ResourceCode.SourceLength+1,
      ResourceData)
    then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.RemoveLazarusResource(ResourceCode: TCodeBuffer;
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
    OldPosition.StartPos:=FindLineEndOrCodeInFrontOfPosition(
         OldPosition.StartPos);
    OldPosition.EndPos:=FindFirstLineEndAfterInCode(OldPosition.EndPos);
    if not SourceChangeCache.Replace(gtNone,gtNone,
      OldPosition.StartPos,OldPosition.EndPos,'') then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.RenameInclude(LinkIndex: integer;
  const NewFilename: string; KeepPath: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
{ change filename in an include directive
  if KeepPath is true and the include dircetive contains a path
  (relative or absolute), then this path is kept and only the filename is
  replaced
}
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
    while (FileNameStart>FileStart) and (Src[FileNameStart]<>PathDelim) do
      dec(FileNameStart);
    if Src[FileNameStart]=PathDelim then
      FileStart:=FileNameStart+1;
  end;
  if not SourceChangeCache.Replace(gtNone,GtNone,FileStart,FileEnd,
    NewFilename) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.CheckLFM(LFMBuf: TCodeBuffer; var LFMTree: TLFMTree;
  const OnGetDefineProperties: TOnGetDefineProperties): boolean;
var
  RootContext: TFindContext;
  
  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext): boolean; forward;

  function FindNonPublishedDefineProperty(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    const IdentName: string; const ClassContext: TFindContext): boolean;
  var
    PropertyNode: TLFMPropertyNode;
    ObjectNode: TLFMObjectNode;
  begin
    Result:=false;
    if (not (LFMNode is TLFMPropertyNode)) then exit;
    PropertyNode:=TLFMPropertyNode(LFMNode);
    if (PropertyNode.Parent=nil)
    or (not (PropertyNode.Parent is TLFMObjectNode)) then exit;
    ObjectNode:=TLFMObjectNode(PropertyNode.Parent);
    if ObjectNode.DefineProperties=nil then begin
      // fetch define properties
      if Assigned(OnGetDefineProperties) then begin
        OnGetDefineProperties(Self,ClassContext,LFMNode,IdentName,
          ObjectNode.DefineProperties);
      end else begin
        // create the default define properties for TComponent
        ObjectNode.DefineProperties:=TStringList.Create;
        ObjectNode.DefineProperties.Add('LEFT');
        ObjectNode.DefineProperties.Add('TOP');
      end;
    end;
    Result:=ObjectNode.DefineProperties.IndexOf(IdentName)>=0;
  end;

  function FindLFMIdentifier(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    const IdentName: string; const ClassContext: TFindContext;
    SearchAlsoInDefineProperties: boolean;
    var IdentContext: TFindContext): boolean;
  var
    Params: TFindDeclarationParams;
  begin
    Result:=false;
    IdentContext:=CleanFindContext;
    if (ClassContext.Node=nil) or (ClassContext.Node.Desc<>ctnClass) then begin
      writeln('TStandardCodeTool.CheckLFM.FindLFMIdentifier Internal error');
      exit;
    end;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
      Params.ContextNode:=ClassContext.Node;
      Params.SetIdentifier(ClassContext.Tool,PChar(IdentName),nil);
      try
        {writeln('FindLFMIdentifier A ',
          ' Ident=',
          '"',GetIdentifier(Params.Identifier),'"',
          ' Context="',ClassContext.Node.DescAsString,'" "',StringToPascalConst(copy(ClassContext.Tool.Src,ClassContext.Node.StartPos,20)),'"',
          ' File="',ExtractFilename(ClassContext.Tool.MainFilename)+'"',
          ' Flags=[',FindDeclarationFlagsAsString(Params.Flags),']'
          );}
        if ClassContext.Tool.FindIdentifierInContext(Params) then begin
          IdentContext:=CreateFindContext(Params);
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
    
    if IdentContext.Node<>nil then begin
      Result:=true;
    end else begin
      // no node found
      if SearchAlsoInDefineProperties then begin
        if FindNonPublishedDefineProperty(LFMNode,DefaultErrorPosition,
          IdentName,ClassContext)
        then begin
          Result:=true;
        end;
      end;
    end;
    if not Result then begin
      LFMTree.AddError(lfmeIdentifierNotFound,LFMNode,
                       'identifier '+IdentName+' not found',
                       DefaultErrorPosition);
      exit;
    end;
  end;
  
  function FindClassNodeForLFMObject(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    StartTool: TFindDeclarationTool; DefinitionNode: TCodeTreeNode): TFindContext;
  var
    Params: TFindDeclarationParams;
    Identifier: PChar;
    OldInput: TFindDeclarationInput;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
    Identifier:=@StartTool.Src[DefinitionNode.StartPos];
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
      Params.ContextNode:=DefinitionNode;
      Params.SetIdentifier(StartTool,Identifier,nil);
      try
        Params.Save(OldInput);
        if FindIdentifierInContext(Params) then begin
          Params.Load(OldInput);
          Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
          if (Result.Node=nil) or (Result.Node.Desc<>ctnClass) then
            Result:=CleanFindContext;
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
    if Result.Node=nil then begin
      LFMTree.AddError(lfmeIdentifierNotFound,LFMNode,
                       'identifier '+GetIdentifier(Identifier)+' not found',
                       DefaultErrorPosition);
      exit;
    end;
  end;

  function CreateFootNote(const Context: TFindContext): string;
  var
    Caret: TCodeXYPosition;
  begin
    Result:=' see '+Context.Tool.MainFilename;
    if Context.Tool.CleanPosToCaret(Context.Node.StartPos,Caret) then
      Result:=Result+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')';
  end;
  
  procedure CheckLFMChildObject(LFMObject: TLFMObjectNode;
    const ParentContext: TFindContext);
  var
    LFMObjectName: String;
    ChildContext: TFindContext;
    VariableTypeName: String;
    DefinitionNode: TCodeTreeNode;
    ClassContext: TFindContext;
  begin
    // find variable for object
    
    // find identifier in Lookup Root
    LFMObjectName:=LFMObject.Name;
    //writeln('CheckChildObject A LFMObjectName="',LFMObjectName,'"');
    if LFMObjectName='' then begin
      LFMTree.AddError(lfmeObjectNameMissing,LFMObject,'missing object name',
                       LFMObject.StartPos);
      exit;
    end;
    if not FindLFMIdentifier(LFMObject,LFMObject.NamePosition,
      LFMObjectName,RootContext,false,ChildContext) then exit;
    if ChildContext.Node=nil then begin
      // this is an extra entry, created via DefineProperties.
      // There is no generic way to test such things
      exit;
    end;

    // check if identifier is variable
    if not ChildContext.Node.Desc=ctnVarDefinition then begin
      LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                       LFMObjectName+' is not a variable'
                       +CreateFootNote(ChildContext),
                       LFMObject.NamePosition);
      exit;
    end;
    DefinitionNode:=ChildContext.Tool.FindTypeNodeOfDefinition(
                                                             ChildContext.Node);
    if DefinitionNode=nil then begin
      ChildContext.Node:=DefinitionNode;
      LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                       LFMObjectName+' is not a variable.'
                       +CreateFootNote(ChildContext),
                       LFMObject.NamePosition);
      exit;
    end;

    // check if variable has a compatible type
    if LFMObject.TypeName<>'' then begin
      VariableTypeName:=ChildContext.Tool.ExtractDefinitionNodeType(
                                                             ChildContext.Node);
      if (VariableTypeName='')
      or (AnsiCompareText(VariableTypeName,LFMObject.TypeName)<>0) then begin
        ChildContext.Node:=DefinitionNode;
        LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                       VariableTypeName+' expected, but '+LFMObject.TypeName+' found.'
                       +CreateFootNote(ChildContext),
                       LFMObject.NamePosition);
        exit;
      end;
    end;

    // check if variable is published
    if (ChildContext.Node.Parent=nil)
    or (ChildContext.Node.Parent.Desc<>ctnClassPublished) then begin
      LFMTree.AddError(lfmeIdentifierNotPublished,LFMObject,
                       LFMObjectName+' is not published',
                       LFMObject.NamePosition);
      exit;
    end;
    
    // find class node
    ClassContext:=FindClassNodeForLFMObject(LFMObject,LFMObject.TypeNamePosition,
                                            ChildContext.Tool,DefinitionNode);
    if ClassContext.Node=nil then exit;

    // check child LFM nodes
    CheckLFMObjectValues(LFMObject,ClassContext);
  end;
  
  function FindClassNodeForProperty(LFMProperty: TLFMPropertyNode;
    DefaultErrorPosition: integer; const PropertyContext: TFindContext
    ): TFindContext;
  var
    Params: TFindDeclarationParams;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
      Params.ContextNode:=PropertyContext.Node;
      Params.SetIdentifier(PropertyContext.Tool,nil,nil);
      try
        Result:=PropertyContext.Tool.FindBaseTypeOfNode(Params,
                                                        PropertyContext.Node);
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
    if Result.Node=nil then begin
      LFMTree.AddError(lfmePropertyHasNoSubProperties,LFMProperty,
                       'property has no sub properties',
                       DefaultErrorPosition);
      exit;
    end;

  end;

  procedure CheckLFMProperty(LFMProperty: TLFMPropertyNode;
    const ParentContext: TFindContext);
  // checks properties. For example lines like 'OnShow = FormShow'
  // or 'VertScrollBar.Range = 29'
  // LFMProperty is the property node
  // ParentContext is the context, where properties are searched.
  //               This can be a class or a property.
  var
    i: Integer;
    CurName: string;
    CurPropertyContext: TFindContext;
    SearchContext: TFindContext;
  begin
    // find complete property name
    //writeln('CheckLFMProperty A LFMProperty Name="',LFMProperty.CompleteName,'"');

    if LFMProperty.CompleteName='' then begin
      LFMTree.AddError(lfmePropertyNameMissing,LFMProperty,
                       'property without name',LFMProperty.StartPos);
      exit;
    end;
    
    // find every part of the property name
    SearchContext:=ParentContext;
    for i:=0 to LFMProperty.NameParts.Count-1 do begin
      if SearchContext.Node.Desc=ctnProperty then begin
        SearchContext:=FindClassNodeForProperty(LFMProperty,
          LFMProperty.NameParts.NamePositions[i],SearchContext);
        if SearchContext.Node=nil then exit;
      end;

      CurName:=LFMProperty.NameParts.Names[i];
      if not FindLFMIdentifier(LFMProperty,
                               LFMProperty.NameParts.NamePositions[i],
                               CurName,SearchContext,true,CurPropertyContext)
      then
        break;
      if CurPropertyContext.Node=nil then begin
        // this is an extra entry, created via DefineProperties.
        // There is no generic way to test such things
        break;
      end;
      SearchContext:=CurPropertyContext;
    end;
    
    // ToDo: check value
  end;

  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext): boolean;
  var
    CurLFMNode: TLFMTreeNode;
  begin
    //writeln('TStandardCodeTool.CheckLFM.CheckLFMObjectValues A ',LFMObject.Name,':',LFMObject.TypeName);
    CurLFMNode:=LFMObject.FirstChild;
    while CurLFMNode<>nil do begin
      //writeln('TStandardCodeTool.CheckLFM.CheckLFMObjectValues B ',CurLFMNode.ClassName);
      case CurLFMNode.TheType of
      
      lfmnObject:
        CheckLFMChildObject(TLFMObjectNode(CurLFMNode),ClassContext);

      lfmnProperty:
        CheckLFMProperty(TLFMPropertyNode(CurLFMNode),ClassContext);

      end;
      CurLFMNode:=CurLFMNode.NextSibling;
    end;
    Result:=true;
  end;

  function CheckLFMRoot: boolean;
  var
    LookupRootLFMNode: TLFMObjectNode;
    LookupRootTypeName: String;
    RootClassNode: TCodeTreeNode;
  begin
    Result:=false;
    
    //writeln('TStandardCodeTool.CheckLFM.CheckLFMRoot checking root ...');
    // get root object node
    if (LFMTree.Root=nil) or (not (LFMTree.Root is TLFMObjectNode)) then begin
      LFMTree.AddError(lfmeMissingRoot,nil,'missing root object',1);
      exit;
    end;
    LookupRootLFMNode:=TLFMObjectNode(LFMTree.Root);
    
    // get type name of root object
    LookupRootTypeName:=UpperCaseStr(LookupRootLFMNode.TypeName);
    if LookupRootTypeName='' then begin
      LFMTree.AddError(lfmeMissingRoot,nil,'missing type of root object',1);
      exit;
    end;
    
    // find root type
    RootClassNode:=FindClassNodeInInterface(LookupRootTypeName,true,false,false);
    if RootClassNode=nil then begin
      LFMTree.AddError(lfmeMissingRoot,LookupRootLFMNode,
                       'type '+LookupRootLFMNode.TypeName+' not found',
                       LookupRootLFMNode.TypeNamePosition);
      exit;
    end;

    RootContext:=CleanFindContext;
    RootContext.Node:=RootClassNode;
    RootContext.Tool:=Self;
    Result:=CheckLFMObjectValues(LookupRootLFMNode,RootContext);
  end;
  
begin
  Result:=false;
  //writeln('TStandardCodeTool.CheckLFM A');
  // create tree from LFM file
  LFMTree:=TLFMTree.Create;
  //writeln('TStandardCodeTool.CheckLFM parsing LFM ...');
  if not LFMTree.Parse(LFMBuf) then exit;
  // parse unit and find LookupRoot
  //writeln('TStandardCodeTool.CheckLFM parsing unit ...');
  BuildTree(true);
  // find every identifier
  //writeln('TStandardCodeTool.CheckLFM checking identifiers ...');
  if not CheckLFMRoot then exit;

  Result:=LFMTree.FirstError=nil;
end;

function TStandardCodeTool.FindCreateFormStatement(StartPos: integer;
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

function TStandardCodeTool.AddCreateFormStatement(const AClassName,
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
    FromPos:=FindLineEndOrCodeInFrontOfPosition(FromPos);
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,FromPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+');',Indent));
  end else begin
    // it exists -> replace it
    FromPos:=FindLineEndOrCodeInFrontOfPosition(OldPosition.StartPos);
    ToPos:=FindFirstLineEndAfterInCode(OldPosition.EndPos);
    SourceChangeCache.MainScanner:=Scanner;
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+');',
         SourceChangeCache.BeautifyCodeOptions.Indent));
  end;
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.RemoveCreateFormStatement(const UpperVarName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var Position: TAtomPosition;
  FromPos, ToPos: integer;
begin
  Result:=false;
  if FindCreateFormStatement(-1,'*',UpperVarName,Position)=-1 then
    exit;
  FromPos:=FindLineEndOrCodeInFrontOfPosition(Position.StartPos);
  ToPos:=FindFirstLineEndAfterInCode(Position.EndPos);
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.ChangeCreateFormStatement(StartPos: integer;
  const OldClassName, OldVarName: string;
  const NewClassName, NewVarName: string;
  OnlyIfExists: boolean; SourceChangeCache: TSourceChangeCache): boolean;
var MainBeginNode: TCodeTreeNode;
  OldPosition: TAtomPosition;
  FromPos, ToPos: integer;
begin
  Result:=false;
  if (OldClassName='') or (length(OldClassName)>255)
  or (OldVarName='') or (length(OldVarName)>255)
  or (NewClassName='') or (length(NewClassName)>255)
  or (NewVarName='') or (length(NewVarName)>255)
  then exit;
  BuildTree(false);
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode=nil then exit;
  FromPos:=-1;
  if FindCreateFormStatement(MainBeginNode.StartPos,UpperCaseStr(OldClassName),
    UpperCaseStr(OldVarName),OldPosition)=-1 then begin
    // does not exists
    if OnlyIfExists then begin
      Result:=true;
      exit;
    end;
    Result:=AddCreateFormStatement(NewClassName,NewVarName,SourceChangeCache);
  end else begin
    // replace
    FromPos:=FindLineEndOrCodeInFrontOfPosition(OldPosition.StartPos);
    ToPos:=FindFirstLineEndAfterInCode(OldPosition.EndPos);
    SourceChangeCache.MainScanner:=Scanner;
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+NewClassName+','+NewVarName+');',
         SourceChangeCache.BeautifyCodeOptions.Indent));
    Result:=SourceChangeCache.Apply;
  end;
end;

function TStandardCodeTool.ListAllCreateFormStatements: TStrings;
// list format: VarName:ClassName
var Position: integer;
  StatementPos: TAtomPosition;
  s:string;
  var MainBeginNode: TCodeTreeNode;
begin
  BuildTree(false);
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

function TStandardCodeTool.SetAllCreateFromStatements(List: TStrings;
  SourceChangeCache: TSourceChangeCache): boolean;
{ every string in the list has the format VarName:ClassName
  or simply VarName In the latter case it will be automatically expanded
  to VarName:TVarName
  
  ToDo: do it less destructable
}
var Position, InsertPos, i, ColonPos, Indent: integer;
  StatementPos: TAtomPosition;
  MainBeginNode: TCodeTreeNode;
  AClassName, AVarName: string;
begin
  Result:= false;
  if (List = nil) or (SourceChangeCache = nil) then exit;
  BuildTree(false);

  { first delete all CreateForm Statements }
  SourceChangeCache.MainScanner:= Scanner;
  MainBeginNode:= FindMainBeginEndNode;
  if MainBeginNode = nil then exit;
  Position:= MainBeginNode.StartPos;
  InsertPos:= -1;
  repeat
    if FindCreateFormStatement(Position, '*', '*', StatementPos) = -1 then break;

    Position:= StatementPos.EndPos;
    StatementPos.StartPos:= FindLineEndOrCodeInFrontOfPosition(StatementPos.StartPos);
    if InsertPos < 1 then InsertPos:= StatementPos.StartPos;

    StatementPos.EndPos:= FindFirstLineEndAfterInCode(StatementPos.EndPos);

    SourceChangeCache.Replace(gtNone,gtNone, StatementPos.StartPos, StatementPos.EndPos, '');
  until false;

  Result:= SourceChangeCache.Apply;

  { then add all CreateForm Statements }
  if InsertPos < 1 then begin

    { there was no createform statement -> insert in front of Application.Run }
    MoveCursorToCleanPos(MainBeginNode.StartPos);
    repeat
      if ReadNextUpAtomIs('APPLICATION') then begin
        InsertPos:=CurPos.StartPos;
        if ReadNextAtomIsChar('.') and ReadNextUpAtomIs('RUN') then begin
          InsertPos:=FindLineEndOrCodeInFrontOfPosition(InsertPos);
          break;
        end;
        InsertPos:=-1;
      end;
    until (CurPos.StartPos>SrcLen);
    if InsertPos < 1 then exit;
  end;

  for i:= 0 to List.Count - 1 do begin
    if Length(List[i]) <= 1 then continue;

    ColonPos:= Pos(List[i], ':');
    if (ColonPos > 1) then begin
      AVarName:= Copy(List[i], 1, ColonPos);
      AClassName:= Copy(List[i], ColonPos + 1, Length(List[i]) - ColonPos);
    end else begin
      AVarName:= List[i];  
      AClassName:= 'T' + AVarName;
    end;  
    Indent:= GetLineIndent(Src, InsertPos);

    SourceChangeCache.Replace(gtNewLine, gtNewLine, InsertPos, InsertPos,
      SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
        'Application.CreateForm('+AClassName+','+AVarName+');', Indent));
  end;
  Result:= Result and SourceChangeCache.Apply;
end;

function TStandardCodeTool.FindApplicationTitleStatement(var StartPos,
  StringConstStartPos, EndPos: integer): boolean;
var
  MainBeginNode: TCodeTreeNode;
  Position: Integer;
begin
  Result:=false;
  StartPos:=-1;
  StringConstStartPos:=-1;
  EndPos:=-1;
  BuildTree(false);
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode=nil then exit;
  Position:=MainBeginNode.StartPos;
  if Position<1 then exit;
  MoveCursorToCleanPos(Position);
  repeat
    ReadNextAtom;
    if UpAtomIs('APPLICATION') then begin
      StartPos:=CurPos.StartPos;
      if ReadNextAtomIsChar('.') and ReadNextUpAtomIs('TITLE')
      and ReadNextUpAtomIs(':=') then begin
        // read till semicolon or end
        repeat
          ReadNextAtom;
          if StringConstStartPos<1 then
            StringConstStartPos:=CurPos.StartPos;
          EndPos:=CurPos.EndPos;
          if CurPos.Flag in [cafEnd,cafSemicolon] then begin
            Result:=true;
            exit;
          end;
        until CurPos.StartPos>SrcLen;
      end;
    end;
  until (CurPos.StartPos>SrcLen);
end;

function TStandardCodeTool.GetApplicationTitleStatement(StringConstStartPos,
  EndPos: integer; var Title: string): boolean;
var
  FormatStringParams: string;
begin
  Result:=false;
  Title:='';
  if (StringConstStartPos<1) or (StringConstStartPos>SrcLen) then exit;
  MoveCursorToCleanPos(StringConstStartPos);
  ReadNextAtom;
  if not AtomIsStringConstant then exit;
  Result:=GetStringConstAsFormatString(StringConstStartPos,EndPos,Title,
                                       FormatStringParams);
end;

function TStandardCodeTool.SetApplicationTitleStatement(const NewTitle: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  StartPos, StringConstStartPos, EndPos: integer;
  OldExists: Boolean;
  NewStatement: String;
  Indent: Integer;
  MainBeginNode: TCodeTreeNode;
begin
  Result:=false;
  // search old Application.Title:= statement
  OldExists:=FindApplicationTitleStatement(StartPos,StringConstStartPos,EndPos);
  if OldExists then begin
    // replace old statement
    Indent:=0;
    Indent:=GetLineIndent(Src,StartPos)
  end else begin
    // insert as first line in program begin..end block
    MainBeginNode:=FindMainBeginEndNode;
    if MainBeginNode=nil then exit;
    MoveCursorToNodeStart(MainBeginNode);
    ReadNextAtom;
    StartPos:=CurPos.EndPos;
    EndPos:=StartPos;
    Indent:=GetLineIndent(Src,StartPos)
            +SourceChangeCache.BeautifyCodeOptions.Indent;
  end;
  // create statement
  NewStatement:='Application.Title:='+StringToPascalConst(NewTitle)+';';
  NewStatement:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
    NewStatement,Indent);
  SourceChangeCache.MainScanner:=Scanner;
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,StartPos,EndPos,
                                   NewStatement)
  then
    exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.RemoveApplicationTitleStatement(
  SourceChangeCache: TSourceChangeCache): boolean;
var
  StartPos, StringConstStartPos, EndPos: integer;
  OldExists: Boolean;
  FromPos: Integer;
  ToPos: Integer;
begin
  Result:=false;
  // search old Application.Title:= statement
  OldExists:=FindApplicationTitleStatement(StartPos,StringConstStartPos,EndPos);
  if not OldExists then begin
    Result:=true;
    exit;
  end;
  // -> delete whole line
  FromPos:=FindLineEndOrCodeInFrontOfPosition(StartPos);
  ToPos:=FindFirstLineEndAfterInCode(EndPos);
  SourceChangeCache.MainScanner:=Scanner;
  if not SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'') then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.RenameForm(const OldFormName,
  OldFormClassName: string; const NewFormName, NewFormClassName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  IdentList: TStringList;
begin
  Result:=false;
  if (OldFormName='') or (OldFormClassName='')
  or (NewFormName='') or (NewFormClassName='')
  or (SourceChangeCache=nil) then exit;
  if (OldFormName=NewFormName)
  and (OldFormClassName=NewFormClassName) then exit;
  IdentList:=TStringList.Create;
  try
    if (OldFormName<>NewFormName) then begin
      IdentList.Add(OldFormName);
      IdentList.Add(NewFormName);
    end;
    if (OldFormClassName<>NewFormClassName) then begin
      IdentList.Add(OldFormClassName);
      IdentList.Add(NewFormClassName);
    end;
    Result:=ReplaceIdentifiers(IdentList,SourceChangeCache);
  finally
    IdentList.Free;
  end;
end;

function TStandardCodeTool.FindFormAncestor(const UpperClassName: string;
  var AncestorClassName: string): boolean;
var
  ClassNode: TCodeTreeNode;
begin
  Result:=false;
  AncestorClassName:='';
  if UpperClassName='' then exit;
  BuildTree(true);
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,false);
  if (ClassNode=nil) then exit;
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  if UpAtomIs('PACKED') then ReadNextAtom;
  ReadNextAtom;
  if AtomIsChar('(') then begin
    ReadNextAtom;
    if AtomIsIdentifier(false) then
      AncestorClassName:=GetAtom;
  end;
  if AncestorClassName='' then
    AncestorClassName:='TObject';
  Result:=true;
end;

{-------------------------------------------------------------------------------
  function TStandardCodeTool.ReplaceIdentifiers(IdentList: TStrings;
    SourceChangeCache: TSourceChangeCache): boolean;
    
  Search in all used sources (not the cleaned source) for identifiers.
  It will find all identifiers, except identifiers in compiler directives.
  This includes identifiers in string constants and comments.
-------------------------------------------------------------------------------}
function TStandardCodeTool.ReplaceIdentifiers(IdentList: TStrings;
  SourceChangeCache: TSourceChangeCache): boolean;
  
  procedure ReplaceIdentifiersInSource(ACode: TCodeBuffer);
  var
    StartPos, EndPos, MaxPos, IdentStart, IdentEnd: integer;
    CurSource: string;
    i: integer;
  begin
    CurSource:=ACode.Source;
    MaxPos:=length(CurSource);
    StartPos:=1;
    // go through all source parts between compiler directives
    writeln('TStandardCodeTool.ReplaceIdentifiers ',ACode.Filename);
    repeat
      EndPos:=FindNextCompilerDirective(CurSource,StartPos,
                                        Scanner.NestedComments);
      if EndPos>MaxPos then EndPos:=MaxPos+1;
      // search all identifiers
      repeat
        IdentStart:=FindNextIdentifier(CurSource,StartPos,EndPos-1);
        if IdentStart<EndPos then begin
          i:=0;
          while i<IdentList.Count do begin
            if (IdentList[i]<>'')
            and (BasicCodeTools.CompareIdentifiers(PChar(IdentList[i]),
                                                   @CurSource[IdentStart])=0)
            and (IdentList[i]<>IdentList[i+1]) then
            begin
              // identifier found -> replace
              IdentEnd:=IdentStart+length(IdentList[i]);
              //writeln('TStandardCodeTool.ReplaceIdentifiers replacing: ',
              //' "',copy(CurSource,IdentStart,IdentEnd-IdentStart),'" -> "',IdentList[i+1],'" at ',IdentStart
              //);
              SourceChangeCache.ReplaceEx(gtNone,gtNone,1,1,
                ACode,IdentStart,IdentEnd,IdentList[i+1]);
              break;
            end;
            inc(i,2);
          end;
          // skip identifier
          StartPos:=IdentStart;
          while (StartPos<MaxPos) and IsIdentChar[CurSource[StartPos]] do
            inc(StartPos);
        end else begin
          break;
        end;
      until false;
      if EndPos<=MaxPos then begin
        // skip comment
        StartPos:=FindCommentEnd(CurSource,EndPos,Scanner.NestedComments);
        if StartPos>MaxPos then break;
      end else begin
        break;
      end;
    until false;
  end;
  
var
  SourceList: TList;
  i: integer;
begin
  Result:=false;
  if (IdentList=nil) or (IdentList.Count=0) or (SourceChangeCache=nil)
  or (Odd(IdentList.Count)) then exit;
  BuildTree(false);
  if Scanner=nil then exit;
  SourceChangeCache.MainScanner:=Scanner;
  SourceList:=TList.Create;
  try
    Scanner.FindCodeInRange(1,SrcLen,SourceList);
    for i:=0 to SourceList.Count-1 do begin
      ReplaceIdentifiersInSource(TCodeBuffer(SourceList[i]));
    end;
  finally
    SourceList.Free;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.FindNearestIdentifierNode(
  const CursorPos: TCodeXYPosition; IdentTree: TAVLTree): TAVLTreeNode;
var
  CleanCursorPos: integer;
  BestDiff: Integer;
  CurIdentNode: TAVLTreeNode;
  CurDiff: Integer;
begin
  Result:=nil;
  if IdentTree=nil then exit;
  BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,[]);
  BestDiff:=SrcLen+1;
  MoveCursorToCleanPos(1);
  repeat
    ReadNextAtom;
    if AtomIsIdentifier(false) then begin
      CurIdentNode:=
        IdentTree.FindKey(@Src[CurPos.StartPos],@CompareIdentifiers);
      if CurIdentNode<>nil then begin
        CurDiff:=CurPos.StartPos-CleanCursorPos;
        if CurDiff<0 then CurDiff:=-CurDiff;
        if (Result=nil) or (CurDiff<BestDiff) then begin
          BestDiff:=CurDiff;
          Result:=CurIdentNode;
        end;
      end;
    end;
  until CurPos.EndPos>SrcLen
end;

function TStandardCodeTool.GetStringConstBounds(
  const CursorPos: TCodeXYPosition;
  var StartPos, EndPos: TCodeXYPosition; ResolveComments: boolean): boolean;
// examples:
//   's1'+'s2'#13+AFunction(...)+inherited AMethod
{ $DEFINE VerboseGetStringConstBounds}
type
  TStrConstTokenType = (scatNone, scatStrConst, scatPlus, scatIdent,
    scatInherited, scatPoint, scatUp,
    scatEdgedBracketOpen, scatEdgedBracketClose,
    scatRoundBracketOpen, scatRoundBracketClose);
const
  StrConstTokenTypeName: array[TStrConstTokenType] of string = (
    'scatNone', 'scatStrConst', 'scatPlus', 'scatIdent',
    'scatInherited', 'scatPoint', 'scatUp',
    'scatEdgedBracketOpen', 'scatEdgedBracketClose',
    'scatRoundBracketOpen', 'scatRoundBracketClose');

  function GetCurrentTokenType: TStrConstTokenType;
  begin
    if AtomIsStringConstant then
      Result:=scatStrConst
    else if AtomIsChar('+') then
      Result:=scatPlus
    else if AtomIsIdentifier(false) then
      Result:=scatIdent
    else if UpAtomIs('INHERITED') then
      Result:=scatInherited
    else if CurPos.Flag=cafPoint then
      Result:=scatPoint
    else if AtomIsChar('^') then
      Result:=scatUp
    else if CurPos.Flag=cafRoundBracketOpen then
      Result:=scatRoundBracketOpen
    else if CurPos.Flag=cafRoundBracketClose then
      Result:=scatRoundBracketClose
    else if CurPos.Flag=cafEdgedBracketOpen then
      Result:=scatEdgedBracketOpen
    else if CurPos.Flag=cafEdgedBracketClose then
      Result:=scatEdgedBracketClose
    else
      Result:=scatNone;
  end;
  
var
  CleanCursorPos: integer;
  SameArea: TAtomPosition;
  LastToken, CurrentToken: TStrConstTokenType;
  StartCleanPos, EndCleanPos: integer;
  StringConstantFound: Boolean;
begin
  StartPos:=CursorPos;
  EndPos:=CursorPos;
  Result:=true;
  BuildTreeAndGetCleanPos(trAll,CursorPos,CleanCursorPos,[]);
  {$IFDEF VerboseGetStringConstBounds}
  writeln('TStandardCodeTool.GetStringConstBounds A ',CleanCursorPos,' "',copy(Src,CleanCursorPos-5,5),'" | "',copy(Src,CleanCursorPos,5),'"');
  {$ENDIF}
  GetCleanPosInfo(-1,CleanCursorPos,ResolveComments,SameArea);
  {$IFDEF VerboseGetStringConstBounds}
  writeln('TStandardCodeTool.GetStringConstBounds B ',SameArea.StartPos,'-',SameArea.EndPos,' "',copy(Src,SameArea.StartPos,SameArea.EndPos-SameArea.StartPos),'"');
  {$ENDIF}
  if (SameArea.EndPos=SameArea.StartPos) or (SameArea.StartPos>SrcLen) then
    exit;

  // read til end of string constant
  MoveCursorToCleanPos(SameArea.StartPos);
  ReadNextAtom;
  {$IFDEF VerboseGetStringConstBounds}
  writeln('TStandardCodeTool.GetStringConstBounds read til end of string  ',GetAtom);
  {$ENDIF}
  CurrentToken:=GetCurrentTokenType;
  if (CurrentToken=scatNone) then exit;
  StringConstantFound:=(CurrentToken=scatStrConst);
  repeat
    EndCleanPos:=CurPos.EndPos;
    ReadNextAtom;
    LastToken:=CurrentToken;
    CurrentToken:=GetCurrentTokenType;
    {$IFDEF VerboseGetStringConstBounds}
    writeln('TStandardCodeTool.GetStringConstBounds Read Forward: ',GetAtom,' EndCleanPos=',EndCleanPos,
    ' LastToken=',StrConstTokenTypeName[LastToken],
    ' CurrentToken=',StrConstTokenTypeName[CurrentToken],
    ' ',StrConstTokenTypeName[GetCurrentTokenType]);
    {$ENDIF}
    case CurrentToken of
    scatNone, scatEdgedBracketClose, scatRoundBracketClose:
      if not (LastToken in [scatStrConst,scatIdent,scatUp,
         scatEdgedBracketClose, scatRoundBracketClose])
      then
        exit
      else
        break;

    scatStrConst:
      if not (LastToken in [scatPlus]) then
        exit
      else
        StringConstantFound:=true;
      
    scatPlus:
      if not (LastToken in [scatStrConst, scatIdent, scatUp,
        scatEdgedBracketClose, scatRoundBracketClose]) then exit;

    scatIdent:
      if not (LastToken in [scatPlus, scatPoint, scatInherited]) then exit;

    scatInherited:
      if not (LastToken in [scatPlus, scatPoint]) then exit;

    scatPoint:
      if not (LastToken in [scatIdent, scatUp, scatRoundBracketClose]) then
        exit;

    scatEdgedBracketOpen,scatRoundBracketOpen:
      if not (LastToken in [scatIdent, scatUp]) then
        exit
      else begin
        ReadTilBracketClose(true);
        CurrentToken:=GetCurrentTokenType;
      end;
      
    end;
  until false;

  // read til start of string constant
  MoveCursorToCleanPos(SameArea.StartPos);
  ReadNextAtom;
  {$IFDEF VerboseGetStringConstBounds}
  writeln('TStandardCodeTool.GetStringConstBounds Read til start of string ',GetAtom);
  {$ENDIF}
  CurrentToken:=GetCurrentTokenType;
  repeat
    StartCleanPos:=CurPos.StartPos;
    ReadPriorAtom;
    {$IFDEF VerboseGetStringConstBounds}
    writeln('TStandardCodeTool.GetStringConstBounds Read backward: ',GetAtom,' StartCleanPos=',StartCleanPos);
    {$ENDIF}
    LastToken:=CurrentToken;
    CurrentToken:=GetCurrentTokenType;
    case CurrentToken of
    scatNone, scatEdgedBracketOpen, scatRoundBracketOpen:
      if not (LastToken in [scatStrConst,scatIdent,scatPlus]) then
        exit
      else
        break;

    scatStrConst:
      if not (LastToken in [scatPlus]) then
        exit
      else
        StringConstantFound:=true;

    scatPlus:
      if not (LastToken in [scatStrConst, scatIdent, scatRoundBracketOpen]) then
        exit;

    scatIdent:
      if not (LastToken in [scatPlus, scatPoint, scatUp, scatRoundBracketOpen,
        scatEdgedBracketOpen]) then exit;

    scatInherited:
      if not (LastToken in [scatIdent]) then exit;

    scatPoint:
      if not (LastToken in [scatIdent]) then exit;

    scatEdgedBracketClose,scatRoundBracketClose:
      if not (LastToken in [scatPlus, scatUp, scatPoint]) then
        exit
      else begin
        ReadBackTilBracketOpen(true);
        CurrentToken:=GetCurrentTokenType;
      end;

    end;
  until false;
  
  // convert start and end position
  {$IFDEF VerboseGetStringConstBounds}
  writeln('TStandardCodeTool.GetStringConstBounds END "',copy(Src,StartCleanPos,EndCleanPos-StartCleanPos),'" StringConstantFound=',StringConstantFound);
  {$ENDIF}
  if not StringConstantFound then begin
    EndCleanPos:=StartCleanPos;
  end;
  if not CleanPosToCaret(StartCleanPos,StartPos) then exit;
  if not CleanPosToCaret(EndCleanPos,EndPos) then exit;

  Result:=true;
end;

function TStandardCodeTool.ReplaceCode(const StartPos, EndPos: TCodeXYPosition;
  const NewCode: string; SourceChangeCache: TSourceChangeCache): boolean;
begin
  Result:=false;
  RaiseException('TStandardCodeTool.ReplaceCode not implemented yet');
end;

function TStandardCodeTool.GetStringConstAsFormatString(StartPos,
  EndPos: integer; var FormatStringConstant, FormatParameters: string
  ): boolean;
{ Converts a string constant into the parameters for a Format call of the
  system unit.

  Examples:

  'Hallo'           -> "Hallo", ""
  'A'+IntToStr(1)   -> "A%s", "IntToStr(1)"
  'A%B'#13#10       -> "A%sB%s", "'%', #13#10"
}
  procedure AddChar(c: char);
  begin
    FormatStringConstant:=FormatStringConstant+c;
  end;

  procedure AddParameter(const NewParam: string);
  begin
    FormatStringConstant:=FormatStringConstant+'%s';
    if FormatParameters<>'' then
      FormatParameters:=FormatParameters+',';
    FormatParameters:=FormatParameters+NewParam;
  end;

  procedure AddParameter(ParamStartPos,ParamEndPos: integer);
  begin
    AddParameter(copy(Src,ParamStartPos,ParamEndPos-ParamStartPos));
  end;

  procedure ConvertStringConstant;
  var
    APos: Integer;
    CharConstStart: Integer;
  begin
    APos:=CurPos.StartPos;
    while APos<EndPos do begin
      if Src[APos]='''' then begin
        // read string constant
        inc(APos);
        while APos<EndPos do begin
          case Src[APos] of
          '''':
            if (APos<EndPos-1) and (Src[APos+1]='''') then begin
              // a double ' means a single '
              AddChar('''');
              AddChar('''');
              inc(APos,2);
            end else begin
              // a single ' means end of string constant
              inc(APos);
              break;
            end;
          '"':
            begin
              AddParameter('''"''');
              inc(APos);
            end;
          else
            begin
              // normal char
              AddChar(Src[APos]);
              inc(APos);
            end;
          end;
        end;
      end else if Src[APos]='#' then begin
        CharConstStart:=APos;
        repeat
          // read char constant
          inc(APos);
          if APos<EndPos then begin
            if IsNumberChar[Src[APos]] then begin
              // read decimal number
              while (APos<EndPos) and IsNumberChar[Src[APos]] do
                inc(APos);
            end else if Src[APos]='$' then begin
              // read hexnumber
              while (APos<EndPos) and IsHexNumberChar[Src[APos]] do
                inc(APos);
            end;
          end;
        until (APos>=EndPos) or (Src[APos]<>'#');
        AddParameter(CharConstStart,APos);
      end else
        break;
    end;
  end;

  procedure ConvertOther;
  var
    ParamStartPos: Integer;
    ParamEndPos: Integer;
  begin
    // read till next string constant
    ParamStartPos:=CurPos.StartPos;
    ParamEndPos:=ParamStartPos;
    while (not AtomIsStringConstant) and (CurPos.EndPos<=EndPos) do begin
      if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
        ReadTilBracketClose(true);
      if not AtomIsChar('+') then ParamEndPos:=CurPos.EndPos;
      ReadNextAtom;
    end;
    if ParamEndPos>ParamStartPos then
      AddParameter(ParamStartPos,ParamEndPos);
    if AtomIsStringConstant then UndoReadNextAtom;
  end;

begin
  Result:=false;
  // read string constants and convert it
  FormatStringConstant:='';
  FormatParameters:='';
  MoveCursorToCleanPos(StartPos);
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  repeat
    ReadNextAtom;
    if (CurPos.EndPos>EndPos) then break;
    if AtomIsStringConstant then begin
      // a string constant
      ConvertStringConstant;
    end else if AtomIsChar('+') then begin
      // simply ignore
    end else if (CurPos.Flag=cafRoundBracketOpen) or AtomIsIdentifier(false)
    then begin
      // add as parameter
      ConvertOther;
    end else
      // string constant end
      break;
  until false;
  Result:=FormatStringConstant<>'';
end;

function TStandardCodeTool.GatherResourceStringSections(
  const CursorPos: TCodeXYPosition; PositionList: TCodeXYPositions): boolean;
  
  function SearchInUsesSection(UsesNode: TCodeTreeNode): boolean;
  var
    InAtom, UnitNameAtom: TAtomPosition;
    NewCodeTool: TPascalReaderTool;
    ANode: TCodeTreeNode;
    NewCaret: TCodeXYPosition;
  begin
    Result:=false;
    MoveCursorToUsesEnd(UsesNode);
    repeat
      ReadPriorUsedUnit(UnitNameAtom, InAtom);
      //writeln('TStandardCodeTool.GatherResourceStringSections Uses ',GetAtom(UnitNameAtom));
      // open the unit
      NewCodeTool:=OpenCodeToolForUnit(UnitNameAtom,InAtom,false);
      NewCodeTool.BuildTree(true);
      // search all resource string sections in the interface
      ANode:=NewCodeTool.FindInterfaceNode;
      if (ANode<>nil) and (ANode.LastChild<>nil) then begin
        ANode:=ANode.LastChild;
        while ANode<>nil do begin
          if ANode.Desc=ctnResStrSection then begin
            if not NewCodeTool.CleanPosToCaret(ANode.StartPos,NewCaret) then
              break;
            //writeln('TStandardCodeTool.GatherResourceStringSections Found Other ',NewCodeTool.MainFilename,' Y=',NewCaret.Y);
            PositionList.Add(NewCaret);
          end;
          ANode:=ANode.PriorBrother;
        end;
      end;
      // restore the cursor
      MoveCursorToCleanPos(UnitNameAtom.StartPos);
      ReadPriorAtom; // read keyword 'uses' or comma
      //writeln('TStandardCodeTool.GatherResourceStringSections Uses B ',GetAtom);
    until not AtomIsChar(',');
    Result:=true;
  end;
  
var
  CleanCursorPos: integer;
  CursorNode: TCodeTreeNode;
  NewCaret: TCodeXYPosition;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  //writeln('TStandardCodeTool.GatherResourceStringSections A ');
  BuildTreeAndGetCleanPos(trAll,CursorPos,CleanCursorPos,[]);
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  PositionList.Clear;
  ANode:=CursorNode;
  while ANode<>nil do begin
    case ANode.Desc of
    
    ctnResStrSection:
      begin
        if not CleanPosToCaret(ANode.StartPos,NewCaret) then exit;
        //writeln('TStandardCodeTool.GatherResourceStringSections Found Same Y=',NewCaret.Y);
        PositionList.Add(NewCaret);
      end;
      
    ctnUsesSection:
      if not SearchInUsesSection(ANode) then break;
      
    end;
    
    // go to next node
    if ANode.PriorBrother<>nil then begin
      ANode:=ANode.PriorBrother;
      if (ANode.Desc=ctnInterface) and (ANode.LastChild<>nil) then
        ANode:=ANode.LastChild;
    end else begin
      ANode:=ANode.Parent;
    end;
  end;
  Result:=true;
end;

function TStandardCodeTool.IdentifierExistsInResourceStringSection(
  const CursorPos: TCodeXYPosition; const ResStrIdentifier: string): boolean;
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  if ResStrIdentifier='' then exit;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(trAll,CursorPos,CleanCursorPos,[]);
  // find resource string section
  ANode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if (ANode=nil) then exit;
  ANode:=ANode.GetNodeOfType(ctnResStrSection);
  if ANode=nil then exit;
  // search identifier in section
  ANode:=ANode.FirstChild;
  while ANode<>nil do begin
    if (ANode.Desc=ctnConstDefinition)
    and CompareSrcIdentifier(ANode.StartPos,ResStrIdentifier) then begin
      Result:=true;
      exit;
    end;
    ANode:=ANode.NextBrother;
  end;
end;

function TStandardCodeTool.CreateIdentifierFromStringConst(const StartCursorPos,
  EndCursorPos: TCodeXYPosition; var Identifier: string;
  MaxLen: integer): boolean;
var
  StartPos, EndPos: integer;
  Dummy: Integer;
  IdentStr: String;
begin
  Result:=false;
  if MaxLen<=0 then exit;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(trAll,StartCursorPos,StartPos,[]);
  Dummy:=CaretToCleanPos(EndCursorPos, EndPos);
  if (Dummy<>0) and (Dummy<>-1) then exit;
  // read string constants and extract identifier characters
  Identifier:='';
  MoveCursorToCleanPos(StartPos);
  repeat
    ReadNextAtom;
    if CurPos.EndPos>EndPos then break;
    if AtomIsStringConstant then begin
      IdentStr:=ExtractIdentCharsFromStringConstant(CurPos.StartPos,
                                                     MaxLen-length(Identifier));
      if (Identifier<>'') and (IdentStr<>'') then
        IdentStr[1]:=UpChars[IdentStr[1]];
      Identifier:=Identifier+IdentStr;
    end;
  until length(Identifier)>=MaxLen;
  Result:=Identifier<>'';
end;

function TStandardCodeTool.StringConstToFormatString(const StartCursorPos,
  EndCursorPos: TCodeXYPosition; var FormatStringConstant,
  FormatParameters: string): boolean;
var
  StartPos,EndPos,Dummy: Integer;
begin
  Result:=false;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(trAll,StartCursorPos,StartPos,[]);
  Dummy:=CaretToCleanPos(EndCursorPos, EndPos);
  if (Dummy<>0) and (Dummy<>-1) then exit;
  Result:=GetStringConstAsFormatString(StartPos,EndPos,FormatStringConstant,
                                       FormatParameters);
end;

function TStandardCodeTool.HasInterfaceRegisterProc(var HasRegisterProc: boolean
  ): boolean;
var
  InterfaceNode: TCodeTreeNode;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  HasRegisterProc:=false;
  BuildTree(true);
  InterfaceNode:=FindInterfaceNode;
  if InterfaceNode=nil then exit;
  ANode:=InterfaceNode.FirstChild;
  while ANode<>nil do begin
    if (ANode.Desc=ctnProcedure) then begin
      MoveCursorToNodeStart(ANode);
      if ReadNextUpAtomIs('PROCEDURE')
      and ReadNextUpAtomIs('REGISTER')
      and ReadNextAtomIsChar(';')
      then begin
        HasRegisterProc:=true;
        break;
      end;
    end;
    ANode:=ANode.NextBrother;
  end;
  Result:=true;
end;

function TStandardCodeTool.ConvertDelphiToLazarusSource(AddLRSCode: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;

  function AddModeDelphiDirective: boolean;
  var
    ModeDirectivePos: integer;
    InsertPos: Integer;
  begin
    Result:=false;
    if not FindModeDirective(false,ModeDirectivePos) then begin
      // add {$MODE Delphi} behind source type
      if Tree.Root=nil then exit;
      MoveCursorToNodeStart(Tree.Root);
      ReadNextAtom; // 'unit', 'program', ..
      ReadNextAtom; // name
      ReadNextAtom; // semicolon
      InsertPos:=CurPos.EndPos;
      SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
        '{$MODE Delphi}');
    end;
    Result:=true;
  end;

  function ConvertUsedUnits: boolean;
  // replace unit 'Windows' with 'LCLIntf' and add 'LResources'
  var
    NamePos, InPos: TAtomPosition;
  begin
    Result:=false;
    if FindUnitInAllUsesSections('WINDOWS',NamePos,InPos)
    and (InPos.StartPos<1) then begin
      if not SourceChangeCache.Replace(gtNone,gtNone,
                           NamePos.StartPos,NamePos.EndPos,'LCLIntf') then exit;
    end;
    if AddLRSCode then
      if not AddUnitToMainUsesSection('LResources','',SourceChangeCache) then
        exit;
    if not RemoveUnitFromAllUsesSections('VARIANTS',SourceChangeCache) then
      exit;
    Result:=true;
  end;

  function RemoveDFMResourceDirective: boolean;
  // remove {$R *.dfm} or {$R *.xfm} directive
  var
    ParamPos: Integer;
    ACleanPos: Integer;
    StartPos: Integer;
  begin
    Result:=false;
    // find $R directive
    ACleanPos:=1;
    repeat
      ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
        Scanner.NestedComments,ParamPos);
      if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen) then break;
      if (Src[ACleanPos]='{')
      and ((copy(UpperSrc,ParamPos,6)='*.DFM}')
        or (copy(UpperSrc,ParamPos,6)='*.XFM}'))
      then begin
        StartPos:=FindLineEndOrCodeInFrontOfPosition(ACleanPos,true);
        if not SourceChangeCache.Replace(gtNone,gtNone,StartPos,ParamPos+6,'')
        then exit;
        break;
      end;
      ACleanPos:=FindCommentEnd(Src,ACleanPos,Scanner.NestedComments);
    until false;
    Result:=true;
  end;

  function AddLRSIncludeDirective: boolean;
  // add initialization and {$i unit.lrs} include directive
  var
    FirstInclude: TCodeBuffer;
    LRSFilename: String;
    InitializationNode: TCodeTreeNode;
    ImplementationNode: TCodeTreeNode;
    NewCode: String;
    InsertPos: Integer;
    LinkIndex: Integer;
  begin
    Result:=false;
    if AddLRSCode then begin
      LRSFilename:=ExtractFilenameOnly(MainFilename)+'.lrs';
      LinkIndex:=-1;
      FirstInclude:=FindNextIncludeInInitialization(LinkIndex);
      if (FirstInclude<>nil)
      and (CompareFilenames(FirstInclude.Filename,LRSFilename)=0) then begin
        // already there
        Result:=true;
        exit;
      end;
      if Tree.Root.Desc=ctnUnit then begin
        InitializationNode:=FindInitializationNode;
        NewCode:=GetIndentStr(SourceChangeCache.BeautifyCodeOptions.Indent)
                 +'{$i '+LRSFilename+'}';
        if InitializationNode=nil then begin
          // add also an initialization section
          ImplementationNode:=FindImplementationNode;
          InsertPos:=ImplementationNode.EndPos;
          NewCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord(
                     'initialization')
                   +SourceChangeCache.BeautifyCodeOptions.LineEnd
                   +NewCode;
          if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
                                           InsertPos,InsertPos,
                                           NewCode) then exit;
        end else begin
          InsertPos:=InitializationNode.StartPos+length('initialization');
          if not SourceChangeCache.Replace(gtNewLine,gtNewLine,
                                           InsertPos,InsertPos,
                                           NewCode) then exit;
        end;
      end else begin
        // only Units supported yet
        exit;
      end;
    end;
    Result:=true;
  end;

begin
  Result:=false;
  if SourceChangeCache=nil then exit;
  BuildTree(false);
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.BeginUpdate;
  try
    writeln('ConvertDelphiToLazarusSource AddModeDelphiDirective');
    if not AddModeDelphiDirective then exit;
    writeln('ConvertDelphiToLazarusSource RemoveDFMResourceDirective');
    if not RemoveDFMResourceDirective then exit;
    writeln('ConvertDelphiToLazarusSource AddLRSIncludeDirective');
    if not AddLRSIncludeDirective then exit;
    writeln('ConvertDelphiToLazarusSource ConvertUsedUnits');
    if not ConvertUsedUnits then exit;
    writeln('ConvertDelphiToLazarusSource Apply');
    if not SourceChangeCache.Apply then exit;
  finally
    SourceChangeCache.EndUpdate;
  end;
  writeln('ConvertDelphiToLazarusSource END');
  Result:=true;
end;

function TStandardCodeTool.GetIDEDirectives(DirectiveList: TStrings): boolean;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  Result:=false;
  DirectiveList.Clear;
  BuildTree(true);
  EndPos:=1;
  repeat
    StartPos:=FindNextIDEDirective(Src,EndPos,Scanner.NestedComments);
    if StartPos>SrcLen then break;
    EndPos:=FindCommentEnd(Src,StartPos,Scanner.NestedComments);
    DirectiveList.Add(copy(Src,StartPos,EndPos-StartPos));
    if EndPos>SrcLen then break;
    StartPos:=EndPos;
  until false;
  Result:=true;
end;

function TStandardCodeTool.SetIDEDirectives(DirectiveList: TStrings;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  InsertPos: Integer;
  EndPos: Integer;
  StartPos: Integer;
  InsertTxt: String;
  ImplementationNode: TCodeTreeNode;
begin
  Result:=false;
  if SourceChangeCache=nil then exit;
  SourceChangeCache.MainScanner:=Scanner;
  BuildTree(false);

  // find first old IDE directive
  InsertPos:=FindNextIDEDirective(Src,1,Scanner.NestedComments);
  if InsertPos>SrcLen then InsertPos:=0;

  // remove all old IDE directives
  if InsertPos>=1 then
    EndPos:=InsertPos
  else
    EndPos:=1;
  repeat
    // find next IDE directive
    StartPos:=FindNextIDEDirective(Src,EndPos,Scanner.NestedComments);
    if StartPos>SrcLen then break;
    EndPos:=FindCommentEnd(Src,StartPos,Scanner.NestedComments);
    // remove also space in front of directive
    while (StartPos>1) and (Src[StartPos-1] in [' ',#9]) do dec(StartPos);
    // remove also space behind directive
    while (EndPos<=SrcLen) and (Src[EndPos] in [' ',#9]) do inc(EndPos);
    if (EndPos<=SrcLen) and (Src[EndPos] in [#10,#13]) then begin
      inc(EndPos);
      if (EndPos<=SrcLen) and (Src[EndPos] in [#10,#13])
      and (Src[EndPos]<>Src[EndPos-1]) then
        inc(EndPos);
    end;
    // remove directive
    SourceChangeCache.Replace(gtNone,gtNone,StartPos,EndPos,'');
    if EndPos>SrcLen then break;
    StartPos:=EndPos;
  until false;
  
  // find a nice insert position
  ImplementationNode:=FindImplementationNode;
  if (ImplementationNode<>nil)
  and (ImplementationNode.StartPos<=InsertPos) then
    InsertPos:=0;
  if InsertPos<1 then begin
    // set default insert position
    InsertPos:=1;
    if (Tree<>nil) and (Tree.Root<>nil) then
      InsertPos:=Tree.Root.StartPos;
  end;
  
  // add directives
  InsertTxt:=ChompLineEndsAtEnd(DirectiveList.Text);
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
                            InsertTxt) then exit;
  if not SourceChangeCache.Apply then exit;

  Result:=true;
end;

function TStandardCodeTool.GatherResourceStringsWithValue(
  const CursorPos: TCodeXYPosition; const StringValue: string;
  PositionList: TCodeXYPositions): boolean;
  
  procedure CompareStringConst(ANode: TCodeTreeNode);
  var
    CurValue: String;
    NewCaret: TCodeXYPosition;
  begin
    MoveCursorToNodeStart(ANode);
    ReadNextAtom; // read identifier
    if not AtomIsIdentifier(false) then exit;
    ReadNextAtom; // read =
    if CurPos.Flag<>cafEqual then exit;
    ReadNextAtom; // read start of string constant
    if not AtomIsStringConstant then exit;
    // extract string constant value
    CurValue:=ReadStringConstantValue(CurPos.StartPos);
    if CurValue<>StringValue then exit;
    // values are the same
    // -> add it to position list
    // get x,y position
    if not CleanPosToCaret(ANode.StartPos,NewCaret) then exit;
    //writeln('TStandardCodeTool.GatherResourceStringsWithValue Found ',MainFilename,' Y=',NewCaret.Y);
    PositionList.Add(NewCaret);
  end;
  
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  if PositionList=nil then exit;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(trAll,CursorPos,CleanCursorPos,[]);
  // find resource string section
  ANode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if (ANode=nil) then exit;
  ANode:=ANode.GetNodeOfType(ctnResStrSection);
  if ANode=nil then exit;
  // search identifier in section
  ANode:=ANode.FirstChild;
  while ANode<>nil do begin
    if (ANode.Desc=ctnConstDefinition) then begin
      CompareStringConst(ANode);
    end;
    ANode:=ANode.NextBrother;
  end;
end;

function TStandardCodeTool.GatherResourceStringIdents(
  const SectionPos: TCodeXYPosition; var IdentTree: TAVLTree): boolean;
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  IdentTree:=nil;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(trAll,SectionPos,CleanCursorPos,[]);
  // find resource string section
  ANode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if (ANode=nil) then exit;
  ANode:=ANode.GetNodeOfType(ctnResStrSection);
  if ANode=nil then exit;
  // search identifier in section
  ANode:=ANode.FirstChild;
  while ANode<>nil do begin
    if (ANode.Desc=ctnConstDefinition) then begin
      if IdentTree=nil then
        IdentTree:=TAVLTree.Create(@BasicCodeTools.CompareIdentifiers);
      IdentTree.Add(@Src[ANode.StartPos]);
    end;
    ANode:=ANode.NextBrother;
  end;
  Result:=true;
end;

function TStandardCodeTool.FindNearestResourceString(const CursorPos,
  SectionPos: TCodeXYPosition; var NearestPos: TCodeXYPosition): boolean;
var
  CursorTool, SectionTool: TStandardCodeTool;
  IdentTree: TAVLTree;
  NearestNode: TAVLTreeNode;
  NearestCleanPos: Integer;
begin
  Result:=false;
  NearestPos.Code:=nil;
  // get both codetools
  if not Assigned(OnGetCodeToolForBuffer) then exit;
  CursorTool:=TStandardCodeTool(OnGetCodeToolForBuffer(Self,CursorPos.Code));
  SectionTool:=TStandardCodeTool(OnGetCodeToolForBuffer(Self,SectionPos.Code));
  if (CursorTool=nil) or (SectionTool=nil) then exit;
  // get all resourcestring identifiers
  IdentTree:=nil;
  Result:=SectionTool.GatherResourceStringIdents(SectionPos,IdentTree);
  if IdentTree=nil then exit;
  try
    // find nearest resourcestring identifier in the cursor source
    NearestNode:=CursorTool.FindNearestIdentifierNode(CursorPos,IdentTree);
    if NearestNode=nil then exit;
    // convert node to cleanpos
    NearestCleanPos:=Integer(NearestNode.Data)-Integer(@SectionTool.Src[1])+1;
    // convert cleanpos to caret
    CleanPosToCaret(NearestCleanPos,NearestPos);
  finally
    IdentTree.Free;
  end;
  Result:=true;
end;

function TStandardCodeTool.AddResourceString(const SectionPos: TCodeXYPosition;
  const NewIdentifier, NewValue: string;
  InsertPolicy: TResourcestringInsertPolicy;
  const NearestPos: TCodeXYPosition;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  CleanSectionPos: integer;
  ANode, SectionNode: TCodeTreeNode;
  Indent: Integer;
  InsertPos: Integer;
  InsertSrc: String;
  NearestCleanPos: integer;
begin
  Result:=false;
  //writeln('TStandardCodeTool.AddResourcestring A ',NewIdentifier,'=',NewValue,' ');
  if (NewIdentifier='') or (length(NewIdentifier)>255) then exit;
  if SourceChangeCache=nil then exit;
  SourceChangeCache.MainScanner:=Scanner;
  // parse source and find clean positions
  //writeln('TStandardCodeTool.AddResourcestring B');
  BuildTreeAndGetCleanPos(trAll,SectionPos,CleanSectionPos,[]);
  //writeln('TStandardCodeTool.AddResourcestring C');
  // find resource string section
  SectionNode:=FindDeepestNodeAtPos(CleanSectionPos,true);
  if (SectionNode=nil) then exit;
  SectionNode:=SectionNode.GetNodeOfType(ctnResStrSection);
  if SectionNode=nil then exit;
  
  //writeln('TStandardCodeTool.AddResourcestring D SectionChilds=',SectionNode.FirstChild<>nil);
  // find insert position
  if SectionNode.FirstChild=nil then begin
    // no resourcestring in this section yet -> append as first child
    Indent:=GetLineIndent(Src,SectionNode.StartPos)
            +SourceChangeCache.BeautifyCodeOptions.Indent;
    InsertPos:=SectionNode.StartPos+length('RESOURCESTRING');
  end else begin
    // search insert position
    case InsertPolicy of
    rsipAlphabetically:
      begin
        // insert new identifier alphabetically
        ANode:=SectionNode.FirstChild;
        while (ANode<>nil) do begin
          if (ANode.Desc=ctnConstDefinition)
          and (CompareIdentifiers(@Src[ANode.StartPos],PChar(NewIdentifier))<0)
          then
            break;
          ANode:=ANode.NextBrother;
        end;
        if ANode=nil then begin
          // append new identifier as last
          Indent:=GetLineIndent(Src,SectionNode.LastChild.StartPos);
          InsertPos:=FindLineEndOrCodeAfterPosition(SectionNode.LastChild.EndPos);
        end else begin
          // insert in front of node
          Indent:=GetLineIndent(Src,ANode.StartPos);
          InsertPos:=FindLineEndOrCodeInFrontOfPosition(ANode.StartPos);
        end;
      end;
      
    rsipContext:
      begin
        // find nearest
        ANode:=nil;
        if (NearestPos.Code<>nil)
        and (CaretToCleanPos(NearestPos,NearestCleanPos)=0) then begin
          ANode:=SectionNode.FirstChild;
          while (ANode<>nil) do begin
            if (ANode.Desc=ctnConstDefinition)
            and (ANode.StartPos<=NearestCleanPos)
            and (ANode.EndPos>NearestCleanPos)
            then begin
              break;
            end;
            ANode:=ANode.NextBrother;
          end;
        end;
        if ANode=nil then begin
          // append new identifier as last
          Indent:=GetLineIndent(Src,SectionNode.LastChild.StartPos);
          InsertPos:=FindLineEndOrCodeAfterPosition(SectionNode.LastChild.EndPos);
        end else begin
          // insert behind node
          Indent:=GetLineIndent(Src,ANode.StartPos);
          InsertPos:=FindLineEndOrCodeAfterPosition(ANode.EndPos);
        end;
      end;
      
    else
      begin
        // append new identifier
        Indent:=GetLineIndent(Src,SectionNode.LastChild.StartPos);
        InsertPos:=FindLineEndOrCodeAfterPosition(SectionNode.LastChild.EndPos);
      end;
    end;
  end;

  //writeln('TStandardCodeTool.AddResourcestring E Indent=',Indent,' InsertPos=',InsertPos,' ',copy(Src,InsertPos-9,8),'|',copy(Src,InsertPos,8));
  // insert
  InsertSrc:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                     NewIdentifier+' = '+NewValue+';',Indent);
  //writeln('TStandardCodeTool.AddResourcestring F "',InsertSrc,'"');
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,InsertSrc);
  SourceChangeCache.Apply;
  Result:=true;
  //writeln('TStandardCodeTool.AddResourcestring END ',Result);
end;

function TStandardCodeTool.FindPublishedVariable(const UpperClassName,
  UpperVarName: string; ExceptionOnClassNotFound: boolean): TCodeTreeNode;
var ClassNode, SectionNode: TCodeTreeNode;
begin
  Result:=nil;
  if (UpperClassName='') or (length(UpperClassName)>255) then
    RaiseException(Format(ctsinvalidClassName, ['"', UpperClassName, '"']));
  BuildTree(true);
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,false);
  if ClassNode=nil then begin
    if ExceptionOnClassNotFound then
      RaiseException(Format(ctsclassNotFound, ['"', UpperClassName, '"']))
    else
      exit;
  end;
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

function TStandardCodeTool.AddPublishedVariable(const UpperClassName,
  VarName, VarType: string; SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode, SectionNode: TCodeTreeNode;
  Indent, InsertPos: integer;
begin
  Result:=false;
  if (UpperClassName='') or (length(UpperClassName)>255) then
    RaiseException(Format(ctsinvalidClassName2, ['"', UpperClassName, '"']));
  if (VarName='') or (length(VarName)>255) then
    RaiseException(Format(ctsinvalidVariableName, ['"', VarName, '"']));
  if (VarType='') or (length(VarType)>255) then
    RaiseException(Format(ctsinvalidVariableType, ['"', VarType, '"']));
  if (SourceChangeCache=nil) then
    RaiseException('missing SourceChangeCache');
  if FindPublishedVariable(UpperClassName,UpperCaseStr(VarName),true)<>nil then
  begin
    Result:=true;
    exit;
  end;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
  if ClassNode=nil then
    RaiseException(Format(ctsclassNotFound, ['"', UpperClassName, '"']));
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
  InsertPos:=FindLineEndOrCodeInFrontOfPosition(SectionNode.EndPos);
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
          SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                     VarName+':'+VarType+';',Indent)
       );
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.RemovePublishedVariable(const UpperClassName,
  UpperVarName: string; ExceptionOnClassNotFound: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
var VarNode: TCodeTreeNode;
  FromPos, ToPos: integer;
begin
  Result:=false;
  VarNode:=FindPublishedVariable(UpperClassName,UpperVarName,
                                 ExceptionOnClassNotFound);
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
      FromPos:=FindLineEndOrCodeInFrontOfPosition(VarNode.StartPos);
      ToPos:=FindFirstLineEndAfterInCode(VarNode.EndPos);
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

function TStandardCodeTool.RenamePublishedVariable(const UpperClassName,
  UpperOldVarName: string; const NewVarName, VarType: shortstring;
  ExceptionOnClassNotFound: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  TypeNode, VarNode: TCodeTreeNode;
begin
  Result:=false;
  VarNode:=FindPublishedVariable(UpperClassName,UpperOldVarName,
                                 ExceptionOnClassNotFound);
  if VarNode<>nil then begin
    // old variable found
    // check type
    TypeNode:=FindTypeNodeOfDefinition(VarNode);
    MoveCursorToNodeStart(TypeNode);
    ReadNextAtom;
    if UpAtomIs(UpperCaseStr(VarType)) then begin
      // rename the identifier
      MoveCursorToNodeStart(VarNode);
      ReadNextAtom;
      SourceChangeCache.MainScanner:=Scanner;
      if not SourceChangeCache.Replace(gtNone,gtNone,
        CurPos.StartPos,CurPos.EndPos,NewVarName)
      then begin
        RaiseException('Unable to replace name');
      end;
    end else begin
      // auto correct type

      // ToDo: auto correct
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[VarType,GetAtom]);
      
    end;
    Result:=SourceChangeCache.Apply;
  end else begin
    // old variable not found -> add it
    Result:=AddPublishedVariable(UpperClassName,NewVarName,VarType,
                                 SourceChangeCache);
  end;
end;

function TStandardCodeTool.FindBlockCounterPart(
  const CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
// jump from bracket-open to bracket-close or 'begin' to 'end'
// or 'until' to 'repeat' ...
var CleanCursorPos: integer;
begin
  Result:=false;
  BeginParsingAndGetCleanPos(true,false,CursorPos,CleanCursorPos);
  // read word at cursor
  MoveCursorToCleanPos(CleanCursorPos);
  if Src[CurPos.StartPos] in ['(','[','{'] then begin
    // jump forward to matching bracket
    ReadNextAtom;
    if not ReadForwardTilAnyBracketClose then exit;
  end else if Src[CurPos.StartPos] in [')',']','}'] then begin
    // jump backward to matching bracket
    ReadNextAtom;
    if not ReadBackwardTilAnyBracketClose then exit;
  end else begin
    if Src[CurPos.StartPos] in [';','.'] then dec(CurPos.StartPos);
    while (CurPos.StartPos>2) and IsWordChar[Src[CurPos.StartPos-1]] do
      dec(CurPos.StartPos);
    MoveCursorToCleanPos(CurPos.StartPos);
    ReadNextAtom;
    if CurPos.EndPos=CurPos.StartPos then exit;
    // read till block keyword counterpart
    if UpAtomIs('BEGIN') or UpAtomIs('CASE') or UpAtomIs('ASM')
    or UpAtomIs('RECORD') or UpAtomIs('TRY') or UpAtomIs('REPEAT') then begin
      // read forward till END, FINALLY, EXCEPT
      ReadTilBlockEnd(true,false);
    end else if UpAtomIs('END') or UpAtomIs('FINALLY') or UpAtomIs('EXCEPT')
    or UpAtomIs('UNTIL') then
    begin
      // read backward till BEGIN, CASE, ASM, RECORD, REPEAT
      ReadBackTilBlockEnd(true);
    end else
      exit;
  end;
  // CursorPos now contains the counter block keyword
  Result:=CleanPosToCaretAndTopLine(CurPos.StartPos,NewPos,NewTopLine);
end;

function TStandardCodeTool.FindBlockStart(const CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
// jump to beginning of current block
// e.g. bracket open, 'begin', 'repeat', ...
var CleanCursorPos: integer;
begin
  Result:=false;
  // scan code
  BeginParsingAndGetCleanPos(true,false,CursorPos,CleanCursorPos);
  // read word at cursor
  MoveCursorToCleanPos(CleanCursorPos);
  while (CurPos.StartPos>2) and IsWordChar[Src[CurPos.StartPos-1]] do
    dec(CurPos.StartPos);
  MoveCursorToCleanPos(CurPos.StartPos);
  ReadNextAtom;
  try
    repeat
      ReadPriorAtom;
      if (CurPos.StartPos<0) then begin
        // start of source found -> this is always a block start
        CurPos.StartPos:=1;
        Result:=true;
        exit;
      end
      else if Src[CurPos.StartPos] in [')',']','}'] then begin
        // jump backward to matching bracket
        if not ReadBackwardTilAnyBracketClose then exit;
      end
      else if WordIsLogicalBlockStart.DoItUpperCase(UpperSrc,
        CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
      begin
        // block start found
        Result:=true;
        exit;
      end else if UpAtomIs('END') or UpAtomIs('FINALLY') or UpAtomIs('EXCEPT')
      or UpAtomIs('UNTIL') then
      begin
        // read backward till BEGIN, CASE, ASM, RECORD, REPEAT
        ReadBackTilBlockEnd(true);
      end;
    until false;
  finally
    if Result then begin
      // CursorPos now contains the counter block keyword
      Result:=CleanPosToCaretAndTopLine(CurPos.StartPos,NewPos,NewTopLine);
    end;
  end;
end;

function TStandardCodeTool.GuessUnclosedBlock(const CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
{ search a block (e.g. begin..end) that looks unclosed, i.e. 'begin'
  without 'end' or 'begin' with 'end' in a different column.
  This function can be used as GuessNextUnclosedBlock, because it ignores blocks
  in front of CursorPos.
  
  Examples for good blocks:
    
    repeat
    until
    
    begin end           // start and end of block in the same line
    
    if expr then begin  // first char in line is relevant, not the block keyword
    end
    
    class;

        
  Examples for bad blocks:
    
    begin               // block start and end has different indenting
      end
      
    asm                 // 'end.' is source end, never asm end
    end.
    
      try               // different indenting
    finally
    
    repeat              // keywords do not match
    end
    
}
var CleanCursorPos: integer;
begin
  Result:=false;
  BeginParsingAndGetCleanPos(true,false,CursorPos,CleanCursorPos);
  // start reading at beginning of code
  MoveCursorToCleanPos(1);
  BuildBlockKeyWordFuncList;
  if ReadTilGuessedUnclosedBlock(CleanCursorPos,false) then
    Result:=CleanPosToCaretAndTopLine(CurPos.StartPos,NewPos,NewTopLine);
  //WriteDebugTreeReport;
end;

function TStandardCodeTool.FindBlockCleanBounds(
  const CursorPos: TCodeXYPosition; var BlockCleanStart, BlockCleanEnd: integer
  ): boolean;
var
  CleanCursorPos: integer;
  BlockStartFound: Boolean;
begin
  Result:=false;
  // scan code
  BeginParsingAndGetCleanPos(true,false,CursorPos,CleanCursorPos);
  // read word at cursor
  MoveCursorToCleanPos(CleanCursorPos);
  while (CurPos.StartPos>2) and IsWordChar[Src[CurPos.StartPos-1]] do
    dec(CurPos.StartPos);
  MoveCursorToCleanPos(CurPos.StartPos);
  ReadNextAtom;
  BlockStartFound:=false;
  repeat
    ReadPriorAtom;
    if (CurPos.StartPos<0) then begin
      // start of source found -> this is always a block start
      CurPos.StartPos:=1;
      BlockStartFound:=true;
      break;
    end
    else if Src[CurPos.StartPos] in [')',']','}'] then begin
      // jump backward to matching bracket
      if not ReadBackwardTilAnyBracketClose then exit;
    end
    else if WordIsLogicalBlockStart.DoItUpperCase(UpperSrc,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
    begin
      // block start found
      BlockStartFound:=true;
      break;
    end else if UpAtomIs('END') or UpAtomIs('FINALLY') or UpAtomIs('EXCEPT')
    or UpAtomIs('UNTIL') then
    begin
      // read backward till BEGIN, CASE, ASM, RECORD, REPEAT
      ReadBackTilBlockEnd(true);
    end;
  until false;
  if not BlockStartFound then exit;
  BlockCleanStart:=CurPos.StartPos;
  
  // read word at cursor
  MoveCursorToCleanPos(BlockCleanStart);
  if Src[CurPos.StartPos] in ['(','[','{'] then begin
    // jump forward to matching bracket
    ReadNextAtom;
    if not ReadForwardTilAnyBracketClose then exit;
  end else begin
    if Src[CurPos.StartPos] in [';','.'] then dec(CurPos.StartPos);
    while (CurPos.StartPos>2) and IsWordChar[Src[CurPos.StartPos-1]] do
      dec(CurPos.StartPos);
    MoveCursorToCleanPos(CurPos.StartPos);
    ReadNextAtom;
    if CurPos.EndPos=CurPos.StartPos then exit;
    // read till block keyword counterpart
    if UpAtomIs('BEGIN') or UpAtomIs('CASE') or UpAtomIs('ASM')
    or UpAtomIs('RECORD') or UpAtomIs('TRY') or UpAtomIs('REPEAT') then begin
      // read forward till END, FINALLY, EXCEPT
      ReadTilBlockEnd(true,false);
    end else
      exit;
  end;
  BlockCleanEnd:=CurPos.StartPos;
  Result:=true;
end;

function TStandardCodeTool.GuessMisplacedIfdefEndif(
  const CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var
  StartCursorPos, EndCursorPos: integer;
  StartCode, EndCode: Pointer;
begin
  Result:=false;
  try
    BeginParsing(true,false);
  except
    // ignore scanner and parser errors
    on e: ELinkScannerError do ;
    on e: ECodeToolError do ;
  end;
  if Scanner<>nil then begin
    CursorPos.Code.LineColToPosition(CursorPos.Y,CursorPos.X,StartCursorPos);
    StartCode:=CursorPos.Code;
    Result:=Scanner.GuessMisplacedIfdefEndif(StartCursorPos,StartCode,
                                             EndCursorPos,EndCode);
    if Result then begin
      NewPos.Code:=TCodeBuffer(EndCode);
      NewPos.Code.AbsoluteToLineCol(EndCursorPos,NewPos.Y,NewPos.X);
      if JumpCentered then begin
        NewTopLine:=NewPos.Y-(VisibleEditorLines shr 1);
        if NewTopLine<1 then NewTopLine:=1;
      end else
        NewTopLine:=NewPos.Y;
    end;
  end;
end;

function TStandardCodeTool.FindEnclosingIncludeDirective(
  const CursorPos: TCodeXYPosition; var NewPos: TCodeXYPosition;
  var NewTopLine: integer): boolean;
var
  CleanCursorPos, LinkIndex, NewCleanPos: integer;
begin
  Result:=false;
  try
    BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
             [{$IFDEF IgnoreErrorAfterCursor}btSetIgnoreErrorPos{$ENDIF}]);
    LinkIndex:=Scanner.LinkIndexAtCleanPos(CleanCursorPos);
    LinkIndex:=Scanner.FindParentLink(LinkIndex);
    if LinkIndex<0 then
      // this is no include file
      exit;
    NewPos.Code:=TCodeBuffer(Scanner.Links[LinkIndex].Code);
    // calculate the directive end bracket
    NewCleanPos:=Scanner.Links[LinkIndex].CleanedPos+Scanner.LinkSize(LinkIndex)-1;
    Result:=CleanPosToCaretAndTopLine(NewCleanPos,NewPos,NewTopLine);
  finally
    ClearIgnoreErrorAfter;
  end;
end;

function TStandardCodeTool.FindModeDirective(DoBuildTree: boolean;
  var ACleanPos: integer): boolean;
var
  ParamPos: Integer;
begin
  Result:=false;
  if DoBuildTree then BuildTree(true);
  ACleanPos:=FindNextCompilerDirectiveWithName(Src,1,'Mode',
    Scanner.NestedComments,ParamPos);
  Result:=(ACleanPos>0) and (ACleanPos<=SrcLen);
end;

function TStandardCodeTool.ReadTilGuessedUnclosedBlock(
  MinCleanPos: integer;  ReadOnlyOneBlock: boolean): boolean;
// returns true if unclosed block found
var BlockType, CurBlockWord: TBlockKeyword;
  BlockStart: integer;
begin
  Result:=false;
  BlockType:=bkwNone;
  BlockStart:=-1;
  // read til this block is closed
  while (CurPos.StartPos<=SrcLen) do begin
    if BlockKeywordFuncList.DoItUppercase(UpperSrc,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
    begin
      for CurBlockWord:=Low(TBlockKeyword) to High(TBlockKeyword) do
        if UpAtomIs(BlockKeywords[CurBlockWord]) then
          break;
      if (CurBlockWord=bkwInterface) and (not LastAtomIs(0,'=')) then
        CurBlockWord:=bkwNone;
        
      if (CurBlockWord=bkwEnd) then begin
        ReadNextAtom;
        if AtomIsChar('.') then begin
          // source end found
          if BlockType in [bkwBegin,bkwNone] then begin
            CurPos.StartPos:=SrcLen+1;
            exit;
          end else begin
            MoveCursorToCleanPos(BlockStart);
            Result:=true;
            exit;
          end;
        end else
          UndoReadNextAtom;
      end;
      
      if BlockType=bkwNone then begin
        case CurBlockWord of

        bkwBegin, bkwAsm, bkwRepeat, bkwCase, bkwTry, bkwRecord:
          begin
            BlockType:=CurBlockWord;
            BlockStart:=CurPos.StartPos;
          end;
          
        bkwClass, bkwObject, bkwInterface, bkwDispInterface:
          begin
            ReadNextAtom;
            if AtomIsChar(';')
            or ((CurBlockWord=bkwClass) and UpAtomIs('OF'))
            or ((CurBlockWord=bkwClass)
                and (UpAtomIs('FUNCTION') or UpAtomIs('PROCEDURE')))
            or ((CurBlockWord=bkwObject) and LastUpAtomIs(0,'OF')) then
            begin
              // forward class or 'class of' or class method or 'of object'
            end else begin
              UndoReadNextAtom;
              BlockType:=CurBlockWord;
              BlockStart:=CurPos.StartPos;
            end;
          end;

        bkwEnd, bkwUntil:
          begin
            // close block keywords found, but no block was opened
            //  -> unclosed block found
            Result:=true;
            exit;
          end;
          
        end;
      end
      else
      if ((BlockType in [bkwBegin, bkwAsm, bkwCase, bkwRecord, bkwClass,
        bkwObject, bkwFinally, bkwExcept, bkwInterface, bkwDispInterface])
        and (CurBlockWord=bkwEnd))
      or ((BlockType=bkwRepeat) and (CurBlockWord=bkwUntil)) then begin
        // block end found
        if (MinCleanPos<=CurPos.StartPos)
        and (GetLineIndent(Src,CurPos.StartPos)<>GetLineIndent(Src,BlockStart))
        then begin
          // different indent -> unclosed block found
          if GetLineIndent(Src,BlockStart)>=GetLineIndent(Src,CurPos.StartPos)
          then begin
            // the current block is more or equal indented than the next block
            // -> probably the current block misses a block end
            MoveCursorToCleanPos(BlockStart);
          end;
          Result:=true;
          exit;
        end;
        // end block
        if (BlockType=bkwRecord) and (CurBlockWord=bkwCase) then begin
          // the 'end' keyword is the end for the case block and the record block
          UndoReadNextAtom;
        end;
        BlockType:=bkwNone;
        if ReadOnlyOneBlock then break;
      end
      else
      if (BlockType=bkwTry) and (CurBlockWord in [bkwFinally,bkwExcept]) then
      begin
        // try..finally, try..except found
        if (MinCleanPos<=CurPos.StartPos)
        and (GetLineIndent(Src,CurPos.StartPos)<>GetLineIndent(Src,BlockStart))
        then begin
          // different indent -> unclosed block found
          //   probably a block start is missing, so the error position is
          //   here at block end
          Result:=true;
          exit;
        end;
        // change blocktype
        BlockType:=CurBlockWord;
        BlockStart:=CurPos.StartPos;
      end
      else
      if ((BlockType in [bkwBegin,bkwRepeat,bkwTry,bkwFinally,bkwExcept,
          bkwCase])
        and (CurBlockWord in [bkwBegin,bkwRepeat,bkwTry,bkwCase,bkwAsm]))
      or ((BlockType in [bkwClass,bkwInterface,bkwDispInterface,bkwObject,
          bkwRecord])
        and (CurBlockWord in [bkwRecord])) then
      begin
        // sub blockstart found -> read recursively
        Result:=ReadTilGuessedUnclosedBlock(MinCleanPos,true);
        if Result then exit;
      end
      else
      if (BlockType=bkwRecord) and (CurBlockWord=bkwCase) then begin
        // variant record
      end
      else
      if (BlockType=bkwClass) and (CurBlockWord=bkwClass) then begin
        // class method
      end
      else
      begin
        // unexpected keyword found
        if GetLineIndent(Src,BlockStart)>=GetLineIndent(Src,CurPos.StartPos)
        then begin
          // the current block is more or equal indented than the next block
          // -> probably the current block misses a block end
          MoveCursorToCleanPos(BlockStart);
        end;
        Result:=true;
        exit;
      end;
    end;
    ReadNextAtom;
  end;
end;

function TStandardCodeTool.ReadForwardTilAnyBracketClose: boolean;
// this function reads any bracket
// (the ReadTilBracketClose function reads only brackets in code, not comments)
var OpenBracket: char;
  CommentLvl: integer;
begin
  Result:=false;
  OpenBracket:=Src[CurPos.StartPos];
  if OpenBracket='{' then begin
    // read til end of comment
    CommentLvl:=1;
    inc(CurPos.StartPos);
    while (CurPos.StartPos<=SrcLen) and (CommentLvl>0) do begin
      case Src[CurPos.StartPos] of
      '{': if Scanner.NestedComments then inc(CommentLvl);
      '}':
        if CommentLvl=1 then begin
          Result:=true;
          break;
        end else
          dec(CommentLvl);
      end;
      inc(CurPos.StartPos);
    end;
  end else if OpenBracket='(' then begin
    if (CurPos.StartPos<SrcLen) and (Src[CurPos.StartPos+1]='*') then begin
      // read til end of comment
      inc(CurPos.StartPos,3);
      while true do begin
        if (CurPos.StartPos<=SrcLen)
        and ((Src[CurPos.StartPos-1]='*') and (Src[CurPos.StartPos]=')')) then
        begin
          Result:=true;
          exit;
        end;
        inc(CurPos.StartPos);
      end;
    end else begin
      Result:=ReadTilBracketClose(false);
    end;
  end else if OpenBracket='[' then begin
    Result:=ReadTilBracketClose(false);
  end;
end;

function TStandardCodeTool.ReadBackwardTilAnyBracketClose: boolean;
// this function reads any bracket
// (the ReadBackTilBracketClose function reads only brackets in code,
//  not comments)
var OpenBracket: char;
  CommentLvl: integer;
begin
  Result:=false;
  OpenBracket:=Src[CurPos.StartPos];
  if OpenBracket='}' then begin
    // read backwards til end of comment
    CommentLvl:=1;
    dec(CurPos.StartPos);
    while (CurPos.StartPos>=1) and (CommentLvl>0) do begin
      case Src[CurPos.StartPos] of
      '}': if Scanner.NestedComments then inc(CommentLvl);
      '{':
        if CommentLvl=1 then begin
          Result:=true;
          break;
        end else
          dec(CommentLvl);
      end;
      dec(CurPos.StartPos);
    end;
  end else if OpenBracket=')' then begin
    if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]='*') then begin
      // read til end of comment
      dec(CurPos.StartPos,3);
      while true do begin
        if (CurPos.StartPos>=1)
        and ((Src[CurPos.StartPos+11]='*') and (Src[CurPos.StartPos]='(')) then
        begin
          Result:=true;
          exit;
        end;
        dec(CurPos.StartPos);
      end;
    end else begin
      Result:=ReadBackTilBracketOpen(false);
    end;
  end else if OpenBracket=']' then begin
    Result:=ReadBackTilBracketOpen(false);
  end;
end;

function TStandardCodeTool.Explore(WithStatements: boolean): boolean;

  procedure ExploreNode(ANode: TCodeTreeNode);
  begin
    if ANode=nil then exit;
    case ANode.Desc of
    ctnClass,ctnClassInterface:
      BuildSubTreeForClass(ANode);
    ctnProcedure,ctnProcedureHead:
      BuildSubTreeForProcHead(ANode);
    ctnBeginBlock:
      if WithStatements then
        BuildSubTreeForBeginBlock(ANode);
    end;
    ExploreNode(ANode.FirstChild);
    ExploreNode(ANode.NextBrother);
  end;

begin
  Result:=true;
  BuildTree(false);
  ExploreNode(Tree.Root);
end;


end.


