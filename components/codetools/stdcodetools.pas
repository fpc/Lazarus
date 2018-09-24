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

{ $DEFINE VerboseGetStringConstBounds}
{ $DEFINE VerboseCompleteBlock}
{ $DEFINE VerboseCheckLFM}
{ $DEFINE VerboseFindUnusedUnits}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, TypInfo, Laz_AVL_Tree,
  // Codetools
  CodeToolsStrConsts, FileProcs, CodeTree, CodeAtom,
  FindDeclarationTool, IdentCompletionTool, PascalReaderTool, PascalParserTool,
  ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, LFMTrees, SourceChanger, CustomCodeTool, CodeToolsStructs,
  // LazUtils
  LazFileUtils, LazFileCache, AvgLvlTree;

type
  TStandardCodeTool = class;

  TInsertStatementPosDescription = class
  public
    InsertPos: integer;
    Indent: integer;
    CodeXYPos: TCodeXYPosition;
    FrontGap, AfterGap: TGapTyp;
    Description: string;
  end;

  TUsesSection = (usMain, usImplementation);

  TOnFindDefinePropertyForContext = procedure(Sender: TObject;
    const ClassContext, AncestorClassContext: TFindContext;
    LFMNode: TLFMTreeNode;
    const IdentName: string; var IsDefined: boolean) of object;
  TOnIDEDirectiveFilter = function(Tool: TStandardCodeTool;
    StartPos, EndPos: integer): boolean of object; // true = use

  { TStandardCodeTool }

  TStandardCodeTool = class(TIdentCompletionTool)
  private
    function ReadTilGuessedUnclosedBlock(MinCleanPos: integer;
      ReadOnlyOneBlock: boolean): boolean;
    function ReadForwardTilAnyBracketClose: boolean;
    function ReadBackwardTilAnyBracketClose: boolean;
    function FindApplicationStatement(const APropertyUpCase: string;
          out StartPos, ConstStartPos, EndPos: integer): boolean;
    function SetApplicationStatement(const APropertyName, NewCode: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveApplicationStatement(const APropertyUpCase: string;
          SourceChangeCache: TSourceChangeCache): boolean;
  public
    // explore the code
    function Explore(WithStatements: boolean; Range: TLinkScannerRange): boolean;
    function Explore(WithStatements: boolean;
          OnlyInterface: boolean = false): boolean;
  
    // source name  e.g. 'unit UnitName;'
    function GetCachedSourceName: string;
    function RenameSource(const NewName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
        
    // uses sections
    function RenameUsedUnit(const OldUnitName, NewUnitName,
          NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function ReplaceUsedUnits(UnitNamePairs: TStringToStringTree; // ToDo: dotted
          SourceChangeCache: TSourceChangeCache): boolean;
    function AddUnitToUsesSection(UsesNode: TCodeTreeNode;
          const NewUnitName, NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache;
          AsLast: boolean = false; CheckSpecialUnits: boolean = true): boolean;
    function AddUnitToSpecificUsesSection(UsesSection: TUsesSection;
          const NewUnitName, NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache;
          AsLast: boolean = false; CheckSpecialUnits: boolean = true): boolean;
    function AddUnitToMainUsesSection(const NewUnitName, NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache;
          AsLast: boolean = false; CheckSpecialUnits: boolean = true): boolean;
    function AddUnitToImplementationUsesSection(const NewUnitName,
          NewUnitInFile: string;
          SourceChangeCache: TSourceChangeCache;
          AsLast: boolean = false; CheckSpecialUnits: boolean = true): boolean;
    function UnitExistsInUsesSection(UsesSection: TUsesSection;
          const AnUnitName: string): boolean;
    function UnitExistsInUsesSection(UsesNode: TCodeTreeNode;
                                const AnUnitName: string): boolean;
    function RemoveUnitFromUsesSection(UsesNode: TCodeTreeNode;
                                const AnUnitName: string;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveUnitFromAllUsesSections(const AnUnitName: string;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FixUsedUnitCase(SourceChangeCache: TSourceChangeCache): boolean;
    function FixUsedUnitCaseInUsesSection(UsesNode: TCodeTreeNode;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FindUsedUnitNames(var MainUsesSection,
                               ImplementationUsesSection: TStrings): boolean;
    function FindUsedUnitNames(var List: TStringToStringTree): boolean;
    function FindUsedUnitFiles(var MainUsesSection: TStrings): boolean;
    function FindUsedUnitFiles(var MainUsesSection,
                               ImplementationUsesSection: TStrings): boolean;
    function FindDelphiProjectUnits(out FoundInUnits, MissingInUnits,
                                    NormalUnits: TStrings;
                                    UseContainsSection: boolean = false;
                                    IgnoreNormalUnits: boolean = false): boolean;
    function UsesSectionToFilenames(UsesNode: TCodeTreeNode): TStrings;
    function UsesSectionToUnitnames(UsesNode: TCodeTreeNode): TStrings;
    function FindMissingUnits(var MissingUnits: TStrings; FixCase: boolean;
                              SearchImplementation: boolean;
                              SourceChangeCache: TSourceChangeCache): boolean;
    function CommentUnitsInUsesSection(MissingUnits: TStrings;
      SourceChangeCache: TSourceChangeCache; UsesNode: TCodeTreeNode): boolean;
    function CommentUnitsInUsesSections(MissingUnits: TStrings;
                                SourceChangeCache: TSourceChangeCache): boolean;
    function FindUnusedUnits(Units: TStrings): boolean;

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
    function CheckLFM(LFMBuf: TCodeBuffer; out LFMTree: TLFMTree;
          const OnFindDefineProperty: TOnFindDefinePropertyForContext;
          RootMustBeClassInUnit: boolean; RootMustBeClassInIntf: boolean;
          ObjectsMustExist: boolean): boolean;

    // Application.Createform statements
    function FindCreateFormStatement(StartPos: integer;
          const AClassName, AVarName: string;
          out Position: TAtomPosition): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(const AClassName, AVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveCreateFormStatement(const AVarName: string;
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
    function FindApplicationTitleStatement(out StartPos, StringConstStartPos,
          EndPos: integer): boolean;
    function GetApplicationTitleStatement(StringConstStartPos, EndPos: integer;
          var Title: string): boolean;
    function SetApplicationTitleStatement(const NewTitle: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveApplicationTitleStatement(
          SourceChangeCache: TSourceChangeCache): boolean;

    // Application.Scaled:=<boolean const> statements
    function FindApplicationScaledStatement(out StartPos, BooleanConstStartPos,
          EndPos: integer): boolean;
    function GetApplicationScaledStatement(BooleanConstStartPos, EndPos: integer;
          var AScaled: boolean): boolean;
    function SetApplicationScaledStatement(const NewScaled: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemoveApplicationScaledStatement(
          SourceChangeCache: TSourceChangeCache): boolean;

    // forms
    function RenameForm(const OldFormName, OldFormClassName: string;
          const NewFormName, NewFormClassName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function FindFormAncestor(const AClassName: string;
          var AncestorClassName: string): boolean;

    // published variables
    function FindPublishedVariable(const AClassName, AVarName: string;
          ExceptionOnClassNotFound: boolean): TCodeTreeNode;
    function AddPublishedVariable(const AClassName,VarName, VarType: string;
          SourceChangeCache: TSourceChangeCache): boolean; virtual;
    function RemovePublishedVariable(const AClassName, AVarName: string;
          ExceptionOnClassNotFound: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedVariable(const AClassName,
          AOldVarName: string; const NewVarName, VarType: shortstring;
          ExceptionOnClassNotFound: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    function GatherPublishedClassElements(const TheClassName: string;
          ExceptionOnClassNotFound, WithVariables, WithMethods,
          WithProperties, WithAncestors: boolean;
          out TreeOfCodeTreeNodeExtension: TAVLTree): boolean;
    function RetypeClassVariables(const AClassName: string;
          ListOfTypes: TStringToStringTree; ExceptionOnClassNotFound: boolean;
          SourceChangeCache: TSourceChangeCache;
          SearchImplementationToo: boolean = false): boolean;
    function FindDanglingComponentEvents(const TheClassName: string;
          RootComponent: TComponent; ExceptionOnClassNotFound,
          SearchInAncestors: boolean;
          out ListOfPInstancePropInfo: TFPList;
          const OverrideGetMethodName: TOnGetMethodname = nil): boolean;

    // variables, constants, types
    function RemoveIdentifierDefinition(const CursorPos: TCodeXYPosition;
          SourceChangeCache: TSourceChangeCache): boolean;

    function InsertStatements(InsertPos: TInsertStatementPosDescription;
          Statements: string; SourceChangeCache: TSourceChangeCache): boolean;
    function InsertStatements(CleanPos: integer;
          Statements: string; Indent: integer; FrontGap, AfterGap: TGapTyp;
          SourceChangeCache: TSourceChangeCache): boolean;

    // blocks (e.g. begin..end)
    function FindBlockCounterPart(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
    function FindBlockStart(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer;
          SkipStart: boolean = true): boolean;
    function GuessUnclosedBlock(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
    function FindBlockCleanBounds(const CursorPos: TCodeXYPosition;
          out BlockCleanStart, BlockCleanEnd: integer): boolean;
    function CompleteBlock(const CursorPos: TCodeXYPosition;
          SourceChangeCache: TSourceChangeCache;
          OnlyIfCursorBlockIndented: boolean;
          out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;

    // compiler directives
    function GuessMisplacedIfdefEndif(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
    function FindEnclosingIncludeDirective(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
    function FindModeDirective(DoBuildTree: boolean;
          out ACleanPos: integer): boolean;
    function FindResourceDirective(DoBuildTree: boolean;
          var ACleanPos: integer; const Filename: string = ''): boolean;
    function FindResourceDirective(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer;
          const Filename: string = ''): boolean;
    function AddResourceDirective(const Filename: string;
          SourceChangeCache: TSourceChangeCache; const NewSrc: string = ''
          ): boolean;
    function FindIncludeDirective(DoBuildTree: boolean;
          var ACleanPos: integer; const Filename: string = ''): boolean;
    function FindIncludeDirective(const CursorPos: TCodeXYPosition;
          out NewPos: TCodeXYPosition; out NewTopLine: integer;
          const Filename: string = ''): boolean;
    function AddIncludeDirective(const Filename: string;
          SourceChangeCache: TSourceChangeCache; const NewSrc: string = ''
          ): boolean; deprecated;
    function AddIncludeDirectiveForInit(const Filename: string;
          SourceChangeCache: TSourceChangeCache; const NewSrc: string = ''
          ): boolean;
    function AddUnitWarnDirective(WarnID, Comment: string; TurnOn: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    function FixIncludeFilenames(Code: TCodeBuffer;
          SourceChangeCache: TSourceChangeCache;
          out FoundIncludeFiles: TStrings;
          var MissingIncludeFilesCodeXYPos: TFPList): boolean;

    // search & replace
    function ReplaceWords(IdentList: TStrings; ChangeStrings: boolean;
          SourceChangeCache: TSourceChangeCache;
          SkipPointWords: boolean = false): boolean;
    function FindNearestIdentifierNode(const CursorPos: TCodeXYPosition;
          IdentTree: TAVLTree): TAVLTreeNode;
    function ReplaceWord(const OldWord, NewWord: string; ChangeStrings: boolean;
          SourceChangeCache: TSourceChangeCache;
          SkipPointWords: boolean = false): boolean;

    // comments
    function CommentCode(const StartPos, EndPos: integer;
          SourceChangeCache: TSourceChangeCache; Apply: boolean): boolean;

    // expressions
    function GetStringConstBounds(const CursorPos: TCodeXYPosition;
          out StartPos, EndPos: TCodeXYPosition;
          ResolveComments: boolean): boolean;
    function GetStringConstAsFormatString(StartPos, EndPos: integer;
          out FormatStringConstant, FormatParameters: string;
          out StartInStringConst, EndInStringConst: boolean): boolean;
    function GetStringConstAsFormatString(StartPos, EndPos: integer;
          out FormatStringConstant, FormatParameters: string): boolean;
    function ExtractOperand(const CursorPos: TCodeXYPosition;
          out Operand: string; WithPostTokens, WithAsOperator,
          WithoutTrailingPoints: boolean): boolean;

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
          out Identifier: string; MaxLen: integer): boolean;
    function StringConstToFormatString(
          const StartCursorPos, EndCursorPos: TCodeXYPosition;
          out FormatStringConstant,FormatParameters: string;
          out StartInStringConst, EndInStringConst: boolean): boolean;
          
    // register procedure
    function HasInterfaceRegisterProc(out HasRegisterProc: boolean): boolean;
    
    // Delphi to Lazarus conversion
    function ConvertDelphiToLazarusSource(AddLRSCode: boolean;
          SourceChangeCache: TSourceChangeCache): boolean;
    
    // IDE % directives
    function GetIDEDirectives(DirectiveList: TStrings;
          const Filter: TOnIDEDirectiveFilter = nil): boolean;
    function SetIDEDirectives(DirectiveList: TStrings;
          SourceChangeCache: TSourceChangeCache;
          const Filter: TOnIDEDirectiveFilter = nil): boolean;

    procedure CalcMemSize(Stats: TCTMemStats); override;
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
    BlockKeywordFuncList:=TKeyWordFunctionList.Create('StdCodeTools.BlockKeywordFuncList');
    for BlockWord:=Low(TBlockKeyword) to High(TBlockKeyword) do
      with BlockKeywordFuncList do
        Add(BlockKeywords[BlockWord],{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;


{ TStandardCodeTool }

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
  Result:=GetSourceName(false);
end;

function TStandardCodeTool.RenameSource(const NewName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var NamePos: TAtomPosition;
begin
  Result:=false;
  BuildTree(lsrSourceName);
  //debugln(['TStandardCodeTool.RenameSource NewName=',NewName]);
  if (not GetSourceNamePos(NamePos)) or (NamePos.StartPos<1) or (NewName='')
  or (Length(NewName)>255) then exit;
  //debugln(['TStandardCodeTool.RenameSource OldName="',dbgstr(copy(Src,NamePos.StartPos,NamePos.EndPos-NamePos.StartPos)),'"']);
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.Replace(gtNone,gtNone,NamePos.StartPos,NamePos.EndPos,
    NewName);
  if not SourceChangeCache.Apply then exit;
  CachedSourceName:=NewName;
  Result:=true;
end;

function TStandardCodeTool.RenameUsedUnit(const OldUnitName,
  NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var UnitPos, InPos: TAtomPosition;
  NewUsesTerm: string;
begin
  Result:=false;
  if (not IsDottedIdentifier(OldUnitName))
  or (not IsDottedIdentifier(NewUnitName)) then
    exit;
  if not FindUnitInAllUsesSections(OldUnitName,UnitPos,InPos) then begin
    //debugln('TStandardCodeTool.RenameUsedUnit not found: ',OldUnitName,' ');
    exit;
  end;
  SourceChangeCache.MainScanner:=Scanner;
  if InPos.StartPos>0 then
    UnitPos.EndPos:=InPos.EndPos;
  // build use unit term
  NewUsesTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUsesTerm:=NewUsesTerm+' in '''+NewUnitInFile+'''';
  // Note: do not use beautifier, unit names are case sensitive
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

function TStandardCodeTool.ReplaceUsedUnits(UnitNamePairs: TStringToStringTree;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  ExistingUnits: TStringToStringTree;
  
  procedure CleanNewUnits(const AnUnitName: string; var NewText: string);
  var
    StartPos: Integer;
    EndPos: LongInt;
    CommaBehind: LongInt;
    CommaInFront: Integer;
    NewUnitName: String;
  begin
    // remove all units, that already exists
    StartPos:=1;
    CommaInFront:=-1;
    while StartPos<=length(NewText) do begin
      EndPos:=StartPos;
      while (EndPos<=length(NewText)) and (IsIdentChar[NewText[EndPos]]) do
        inc(EndPos);
      if EndPos<=StartPos then break;
      NewUnitName:=copy(NewText,StartPos,EndPos-StartPos);
      // set EndPos to start of next unit
      CommaBehind:=-1;
      while (EndPos<=length(NewText)) do begin
        if NewText[EndPos]='''' then begin
          inc(EndPos);
          while (EndPos<=length(NewText)) and (NewText[EndPos]<>'''') do
            inc(EndPos);
        end else if NewText[EndPos]=',' then begin
          CommaBehind:=EndPos;
          while (EndPos<=length(NewText))
          and (not IsIdentStartChar[NewText[EndPos]]) do
            inc(EndPos);
          break;
        end;
        inc(EndPos);
      end;
      if (SysUtils.CompareText(AnUnitName,NewUnitName)=0) then begin
        // this is the old unit or
        //DebugLn('Replace: keep old unit "',NewUnitName,'"');
      end else if ExistingUnits.Contains(NewUnitName) then begin
        // this unit already exists and should not be added
        //DebugLn('Replace: already exists: "',NewUnitName,'"="',ExistingUnits[NewUnitName],'" CommaInFront=',dbgs(CommaInFront),' CommaBehind=',dbgs(CommaBehind));
        if CommaBehind>0 then
          System.Delete(NewText,StartPos,EndPos-StartPos)
        else if CommaInFront>0 then
          System.Delete(NewText,CommaInFront,EndPos-CommaInFront)
        else
          System.Delete(NewText,StartPos,EndPos-StartPos);
        EndPos:=StartPos;
        CommaBehind:=-1;
      end else begin
        // this unit does not exist yet
        //DebugLn('Replace new unit with "',NewUnitName,'"');
      end;
      if CommaBehind>0 then
        CommaInFront:=CommaBehind;
      StartPos:=EndPos;
    end;
  end;

  function Replace(UsesNode: TCodeTreeNode): boolean;
  var
    UnitNameAtom: TAtomPosition;
    InAtom: TAtomPosition;
    NewText: string;
    CommaInFront: LongInt;
    FromPos: LongInt;
    ToPos: LongInt;
    CommaBehind: Integer;
    AnUnitName: String;
  begin
    if UsesNode=nil then exit(true);
    MoveCursorToUsesStart(UsesNode);
    CommaInFront:=-1;
    repeat
      // read next unit name
      ReadNextUsedUnit(UnitNameAtom, InAtom);
      if CurPos.Flag=cafComma then
        CommaBehind:=CurPos.StartPos
      else
        CommaBehind:=-1;
      AnUnitName:=GetAtom(UnitNameAtom);
      if UnitNamePairs.Contains(AnUnitName) then begin
        // replace
        NewText:=UnitNamePairs[AnUnitName];
        //DebugLn('Replace Unit="',AnUnitName,'" NewText="',NewText,'"');
        
        CleanNewUnits(AnUnitName,NewText);
        
        if NewText='' then begin
          // comment unit
          if CommaInFront>0 then begin
            // example:  uses a{, b};
            FromPos:=CommaInFront;
            ToPos:=UnitNameAtom.EndPos;
            if InAtom.StartPos>0 then
              ToPos:=InAtom.EndPos;
          end else if CommaBehind>0 then begin
            // example:  uses {a,} b;
            //           uses {a,} {b};
            FromPos:=UnitNameAtom.StartPos;
            ToPos:=CommaBehind+1;
          end else begin
            // examples:  uses {b};
            FromPos:=UnitNameAtom.StartPos;
            ToPos:=UnitNameAtom.EndPos;
            if InAtom.StartPos>0 then
              ToPos:=InAtom.EndPos;
          end;
          if not CommentCode(FromPos,ToPos,SourceChangeCache,false) then
            exit(false);
        end else begin
          // replace
          FromPos:=UnitNameAtom.StartPos;
          ToPos:=UnitNameAtom.EndPos;
          if InAtom.StartPos>0 then
            ToPos:=InAtom.EndPos;
          if not SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,NewText)
          then exit(false);
        end;
      end;

      if CurPos.Flag=cafComma then begin
        // read next unit name
        CommaInFront:=CurPos.StartPos;
        ReadNextAtom;
      end else if CurPos.Flag=cafSemicolon then begin
        break;
      end else
        RaiseExceptionFmt(20170421201056,ctsStrExpectedButAtomFound,[';',GetAtom]);
    until false;
    Result:=true;
  end;
  
begin
  Result:=false;
  BuildTree(lsrImplementationUsesSectionEnd);
  SourceChangeCache.MainScanner:=Scanner;
  ExistingUnits:=nil;
  try
    // first collect all units
    if not FindUsedUnitNames(ExistingUnits) then exit;
    // then change uses sections
    Replace(FindMainUsesNode);
    Replace(FindImplementationUsesNode);
  finally
    ExistingUnits.Free;
  end;
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.AddUnitToUsesSection(UsesNode: TCodeTreeNode;
  const NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache; AsLast: boolean;
  CheckSpecialUnits: boolean): boolean;
const
  SpecialUnits: array[1..5] of string = (
    'cmem',
    'sharedmem',
    'lineinfo',
    'heaptrc',
    'cthreads'
    );
var
  Beauty: TBeautifyCodeOptions;

  function SpecialUnitPriority(Identifier: PChar): integer;
  begin
    Result:=Low(SpecialUnits);
    while Result<=High(SpecialUnits) do begin
      if CompareIdentifierPtrs(Pointer(Identifier),Pointer(SpecialUnits[Result]))=0 then
        exit;
      inc(Result);
    end;
  end;

  function NextUseUnitNodeInSameBlock(Node: TCodeTreeNode): boolean;
  var
    p: LongInt;
  begin
    if Node.NextBrother=nil then exit(false);
    if PositionsInSameLine(Src,Node.EndPos,Node.NextBrother.StartPos) then
    begin
      // uses on same line belongs to the same formatting block
      exit(true);
    end;
    // check that there is no comment/directive between
    p:=FindPrevNonSpace(Src,Node.NextBrother.StartPos-1);
    if Src[p]<>',' then exit(false);
    p:=FindPrevNonSpace(Src,p-1);
    if p>Node.EndPos then exit(false);
    if LineEndCount(Src,Node.EndPos,Node.NextBrother.StartPos,p)>1 then exit(false);
    Result:=true;
  end;

  procedure AddUseUnit(Lines: TStrings; FirstIndent, Indent: integer;
    const NewUses: string);
  var
    Line: string;
    l: Integer;
  begin
    if Lines.Count=0 then begin
      Lines.Add(NewUses);
      exit;
    end;
    Line:=Lines[Lines.Count-1];
    if (atIdentifier in Beauty.DoInsertSpaceAfter)
    or (atComma in Beauty.DoInsertSpaceInFront) then
      Line:=Line+' ';
    Line:=Line+',';
    l:=length(Line)+length(NewUses)+1; // +1 for the following , or ;
    if (atComma in Beauty.DoInsertSpaceAfter)
    or (atIdentifier in Beauty.DoInsertSpaceInFront) then
      inc(l);
    if Lines.Count=1 then
      inc(l,FirstIndent);
    //DebugLn(['AddUseUnit Lines.Count=',Lines.Count,' l=',l,' Line="',Line,'" NewUses=',NewUses,' FirstIndent=',FirstIndent]);
    if l<=Beauty.LineLength then begin
      // append to last line
      if (atComma in Beauty.DoInsertSpaceAfter)
      or (atIdentifier in Beauty.DoInsertSpaceInFront) then
        Line:=Line+' ';
      Line:=Line+NewUses;
      Lines[Lines.Count-1]:=Line;
    end else begin
      // add new line
      Lines[Lines.Count-1]:=Line;
      Line:=Beauty.GetIndentStr(Indent)+NewUses;
      Lines.Add(Line);
    end;
  end;

var
  LineStart, LineEnd, Indent, InsertPos, InsertToPos, InsertLen: integer;
  NewUsesTerm: string;
  InsertBehind: Boolean;
  InsertNode: TCodeTreeNode;
  Node: TCodeTreeNode;
  NewCode: TCodeBuffer;
  DiffPath: String;
  DiffCnt: Integer;
  BestDiffCnt: LongInt;
  AnUnitName: String;
  AnUnitInFilename: String;
  i: Integer;
  NewFilename: String;
  NewComma: string;
  Lines: TStringList;
  FirstIndent: Integer;
  InsertCode: String;
  UsesInsertPolicy: TUsesInsertPolicy;
  Prio: LongInt;
  FirstNormalUsesNode: TCodeTreeNode;
  InsertPosFound: Boolean;
begin
  Result:=false;
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection)
  or (UsesNode.StartPos<1) or (UsesNode.EndPos<1)
  or (not IsDottedIdentifier(NewUnitName))
  then exit;
  SourceChangeCache.MainScanner:=Scanner;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;

  // find nice insert position

  Prio:=SpecialUnitPriority(PChar(NewUnitName));
  UsesInsertPolicy:=Beauty.UsesInsertPolicy;
  if AsLast then
    UsesInsertPolicy:=uipLast;
  InsertPosFound:=false;
  if CheckSpecialUnits and (Prio<=High(SpecialUnits)) then begin
    // this is a special unit, insert at the beginning
    InsertBehind:=false;
    InsertNode:=UsesNode.FirstChild;
    while (InsertNode<>nil)
    and (Prio>SpecialUnitPriority(@Src[InsertNode.StartPos])) do
      InsertNode:=InsertNode.NextBrother;
    InsertPosFound:=true;
    if InsertNode=nil then begin
      InsertBehind:=true;
      InsertNode:=UsesNode.LastChild;
    end;
  end;
  if not InsertPosFound then begin
    FirstNormalUsesNode:=UsesNode.FirstChild;
    if CheckSpecialUnits and (UsesInsertPolicy<>uipLast) then begin
      while (FirstNormalUsesNode<>nil)
      and (SpecialUnitPriority(@Src[FirstNormalUsesNode.StartPos])<Prio) do
        FirstNormalUsesNode:=FirstNormalUsesNode.NextBrother;
      if FirstNormalUsesNode=nil then
        UsesInsertPolicy:=uipLast;
    end;

    case UsesInsertPolicy of

    uipFirst:
      begin
        InsertBehind:=false;
        InsertNode:=FirstNormalUsesNode;
      end;

    uipInFrontOfRelated,uipBehindRelated:
      begin
        if UsesInsertPolicy=uipBehindRelated then begin
          InsertNode:=UsesNode.LastChild;
          InsertBehind:=true;
        end else begin
          InsertBehind:=false;
          InsertNode:=FirstNormalUsesNode;
        end;
        NewCode:=FindUnitSource(NewUnitName,'',false);
        if NewCode<>nil then begin
          NewFilename:=NewCode.Filename;
          BestDiffCnt:=High(integer);
          Node:=FirstNormalUsesNode;
          while Node<>nil do begin
            AnUnitName:=ExtractUsedUnitName(Node,@AnUnitInFilename);
            // search unit
            //DebugLn(['TStandardCodeTool.AddUnitToUsesSection Unit=',AnUnitName,' in "',AnUnitInFilename,'"']);
            NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename,false);
            if NewCode<>nil then begin
              // used unit found -> compute distance
              DiffPath:=CreateRelativePath(NewCode.Filename,ExtractFilePath(NewFilename));
              DiffCnt:=0;
              for i:=1 to length(DiffPath) do
                if DiffPath[i]=PathDelim then
                  inc(DiffCnt);
              //DebugLn(['TStandardCodeTool.AddUnitToUsesSection DiffCnt=',DiffCnt,' "',NewCode.Filename,'" "',NewFilename,'"']);
              if UsesInsertPolicy=uipInFrontOfRelated then begin
                // insert in front of the first node with the lowest DiffCnt
                if BestDiffCnt>DiffCnt then begin
                  BestDiffCnt:=DiffCnt;
                  InsertNode:=Node;
                  InsertBehind:=false;
                end;
              end else begin
                // insert behind the last node with the lowest DiffCnt
                if BestDiffCnt>=DiffCnt then begin
                  BestDiffCnt:=DiffCnt;
                  InsertNode:=Node;
                  InsertBehind:=true;
                end;
              end;
            end;
            Node:=Node.NextBrother;
          end;
        end;
      end;

    uipLast:
      begin
        InsertNode:=UsesNode.LastChild;
        InsertBehind:=true;
      end;

    uipAlphabetically:
      begin
        InsertNode:=FirstNormalUsesNode;
        InsertBehind:=false;
        while (InsertNode<>nil)
        and (CompareIdentifiers(PChar(NewUnitName),@Src[InsertNode.StartPos])<0) do
          InsertNode:=InsertNode.NextBrother;
        if InsertNode=nil then begin
          InsertNode:=UsesNode.LastChild;
          InsertBehind:=true;
        end;
      end;

    end;
  end;

  // build insert text  "newunitname in 'file'"
  NewUsesTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUsesTerm:=NewUsesTerm+' '
      +Beauty.BeautifyKeyWord('in')
      +' '''+NewUnitInFile+'''';

  NewComma:=',';
  if (atComma in Beauty.DoInsertSpaceInFront)
    or (atIdentifier in Beauty.DoInsertSpaceAfter)
  then
    NewComma:=' '+NewComma;
  if (atComma in Beauty.DoInsertSpaceAfter)
    or (atIdentifier in Beauty.DoInsertSpaceInFront)
  then
    NewComma:=NewComma+' ';

  if InsertBehind then begin
    // insert behind unit name, in front of semicolon or comma
    // for example: uses unit1|, unit2 in 'unit2.pp'|;
    InsertPos:=InsertNode.EndPos;
    InsertCode:=NewComma+NewUsesTerm;
  end else begin
    // insert in front of unit name, behind 'uses' or comma
    // for example: uses |unit1, |unit2;
    InsertPos:=InsertNode.StartPos;
    InsertCode:=NewUsesTerm+NewComma;
  end;
  InsertToPos:=InsertPos;

  //DebugLn(['TStandardCodeTool.AddUnitToUsesSection InsertNode=',ExtractNode(InsertNode,[]),' InsertBehind=',InsertBehind]);

  // check if addition fits into the line
  // if not, rebuild the uses section
  GetLineStartEndAtPosition(Src,InsertPos,LineStart,LineEnd);
  InsertLen:=length(NewUsesTerm)+length(NewComma);
  //DebugLn(['TStandardCodeTool.AddUnitToUsesSection Line=',copy(Src,LineStart,InsertPos-LineStart),'<InsertPos>',copy(Src,InsertPos,LineEnd-InsertPos),' NewLen=',LineEnd-LineStart+InsertLen,' Max=',Beauty.LineLength,' Addition=',NewUsesTerm]);
  if (LineEnd-LineStart+InsertLen > Beauty.LineLength) then begin
    // line too long => reformat block of used units
    // find start of block of used units
    Node:=InsertNode;
    while (Node.PriorBrother<>nil)
    and NextUseUnitNodeInSameBlock(Node.PriorBrother) do
      Node:=Node.PriorBrother;
    InsertPos:=Node.StartPos;
    GetLineStartEndAtPosition(Src,InsertPos,LineStart,LineEnd);
    FirstIndent:=InsertPos-LineStart;
    Indent:=Beauty.GetLineIndent(Src,InsertPos);
    if PositionsInSameLine(Src,UsesNode.StartPos,InsertPos) then begin
      // for example: uses |unit1;
      inc(Indent,Beauty.Indent);
    end;
    // create new block of used units
    Lines:=TStringList.Create;
    try
      while Node<>nil do begin
        InsertToPos:=Node.EndPos;
        if (Node=InsertNode) and (not InsertBehind) then
          AddUseUnit(Lines,FirstIndent,Indent,NewUsesTerm);
        InsertCode:=ExtractUsedUnitName(Node);
        if UpAtomIs('IN') then begin
          ReadNextAtom;
          InsertCode:=InsertCode+' '+Beauty.BeautifyKeyWord('in')+' '+GetAtom;
        end;
        AddUseUnit(Lines,FirstIndent,Indent,InsertCode);
        if (Node=InsertNode) and InsertBehind then
          AddUseUnit(Lines,FirstIndent,Indent,NewUsesTerm);
        if not NextUseUnitNodeInSameBlock(Node) then break;
        Node:=Node.NextBrother;
      end;
      InsertCode:='';
      for i:=0 to Lines.Count-1 do begin
        if i>0 then
          InsertCode:=InsertCode+Beauty.LineEnd;
        InsertCode:=InsertCode+Lines[i];
      end;
    finally
      Lines.Free;
    end;
  end;

  //DebugLn(['TStandardCodeTool.AddUnitToUsesSection Replace="',copy(Src,InsertPos,InsertToPos-InsertPos),'" with "',InsertCode,'"']);
  if not SourceChangeCache.Replace(gtNone,gtNone,InsertPos,InsertToPos,
                                   InsertCode) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.AddUnitToMainUsesSection(const NewUnitName,
  NewUnitInFile: string; SourceChangeCache: TSourceChangeCache;
  AsLast: boolean; CheckSpecialUnits: boolean): boolean;
begin
  Result:=AddUnitToSpecificUsesSection(usMain, NewUnitName, NewUnitInFile, SourceChangeCache,
    AsLast, CheckSpecialUnits);
end;

function TStandardCodeTool.AddUnitToImplementationUsesSection(const NewUnitName,
  NewUnitInFile: string; SourceChangeCache: TSourceChangeCache;
  AsLast: boolean; CheckSpecialUnits: boolean): boolean;
begin
  Result:=AddUnitToSpecificUsesSection(usImplementation, NewUnitName, NewUnitInFile, SourceChangeCache,
    AsLast, CheckSpecialUnits);
end;

function TStandardCodeTool.AddUnitToSpecificUsesSection(UsesSection: TUsesSection;
  const NewUnitName, NewUnitInFile: string; SourceChangeCache: TSourceChangeCache;
  AsLast: boolean; CheckSpecialUnits: boolean): boolean;
var
  UsesNode, OtherUsesNode, SectionNode, Node: TCodeTreeNode;
  NewUsesTerm: string;
  InsertPos: integer;
  Junk: TAtomPosition;
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  if not IsDottedIdentifier(NewUnitName) then exit;
  if UsesSection=usMain then begin
    // quick check using only the main uses section
    BuildTree(lsrMainUsesSectionEnd);
    UsesNode:=FindMainUsesNode;
    if (UsesNode<>nil)
    and (FindUnitInUsesSection(UsesNode,NewUnitName,Junk,Junk)) then
      exit(true); // unit already in main uses section
  end;
  if GetSourceType=ctnUnit then
    BuildTree(lsrImplementationUsesSectionEnd)
  else if UsesSection=usImplementation then begin
    MoveCursorToNodeStart(Tree.Root);
    RaiseException(20170421201102,
      ctsCanNotAddAUnitToTheImplementationBecauseOnlyAUnitH);
  end;
  SourceChangeCache.MainScanner:=Scanner;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  SourceChangeCache.BeginUpdate;
  try
    UsesNode:=FindMainUsesNode;
    OtherUsesNode:=FindImplementationUsesNode;
    if UsesSection=usImplementation then begin
      SectionNode:=UsesNode;
      UsesNode:=OtherUsesNode;
      OtherUsesNode:=SectionNode;
    end;
    // remove unit from other uses section
    if (OtherUsesNode<>nil) then
      RemoveUnitFromUsesSection(OtherUsesNode,NewUnitName,SourceChangeCache);

    if UsesNode<>nil then begin
      // add unit to existing uses section
      if not (FindUnitInUsesSection(UsesNode,NewUnitName,Junk,Junk))
      then begin
        if not AddUnitToUsesSection(UsesNode,NewUnitName,NewUnitInFile,
                                    SourceChangeCache,AsLast,CheckSpecialUnits)
        then
          exit;
      end;
    end else begin
      // create a new uses section
      if Tree.Root=nil then exit;
      SectionNode:=Tree.Root;
      InsertPos:=0;
      NewUsesTerm:='';
      if SectionNode.Desc=ctnUnit then begin
        // unit
        case UsesSection of
        usMain: SectionNode:=FindInterfaceNode;
        usImplementation: SectionNode:=FindImplementationNode;
        end;
        if SectionNode<>nil then begin
          // add uses to existing interface/implementation before any content
          MoveCursorToNodeStart(SectionNode);
          ReadNextAtom;
          InsertPos := CurPos.EndPos;
        end else begin
          // section is missing => add it
          SectionNode:=Tree.Root;
          case UsesSection of
          usMain: NewUsesTerm:='interface';
          usImplementation: NewUsesTerm:='implementation';
          end;
          NewUsesTerm:=Beauty.BeautifyKeyWord(NewUsesTerm)
                      +Beauty.LineEnd;
          if SectionNode.FirstChild<>nil then begin
            // unit not empty => add in front of first node
            InsertPos:=FindLineEndOrCodeInFrontOfPosition(SectionNode.FirstChild.StartPos,
              true);
          end else begin
            // unit empty => add at end
            InsertPos:=FindLineEndOrCodeInFrontOfPosition(SectionNode.EndPos,true);
          end;
        end;
      end;
      if InsertPos<1 then begin
        // not a unit (i.e. program)
        // => insert after title and directives
        Node:=SectionNode.Next;
        if (Node<>nil) and (Node.Desc=ctnSrcName) then
          Node:=Node.NextSkipChilds;
        if Node<>nil then begin
          InsertPos:=FindLineEndOrCodeInFrontOfPosition(Node.StartPos,
            true);
        end else begin
          // program empty => add at end
          InsertPos:=FindLineEndOrCodeInFrontOfPosition(SectionNode.EndPos,true);
        end;
      end;
      NewUsesTerm:=NewUsesTerm+Beauty.BeautifyKeyWord('uses')+' '+NewUnitName;
      if NewUnitInFile<>'' then
        NewUsesTerm:=NewUsesTerm+' in '''+NewUnitInFile+''';'
      else
        NewUsesTerm:=NewUsesTerm+';';
      if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
        NewUsesTerm) then exit;
    end;
    Result:=true;
  finally
    if not Result then
      SourceChangeCache.Clear;
    if not SourceChangeCache.EndUpdate then
      Result:=false;
  end;
end;

function TStandardCodeTool.UnitExistsInUsesSection(UsesSection: TUsesSection;
  const AnUnitName: string): boolean;
var
  UsesNode: TCodeTreeNode;
begin
  Result:=false;
  if not IsDottedIdentifier(AnUnitName) then
    exit;
  if UsesSection=usMain then
    BuildTree(lsrMainUsesSectionEnd)
  else
    BuildTree(lsrImplementationUsesSectionEnd);
  case UsesSection Of
    usMain: UsesNode:=FindMainUsesNode;
    usImplementation: UsesNode:=FindImplementationUsesNode;
  end;
  Result:=UnitExistsInUsesSection(UsesNode,AnUnitName);
end;

function TStandardCodeTool.UnitExistsInUsesSection(UsesNode: TCodeTreeNode;
  const AnUnitName: string): boolean;
begin
  Result:=false;
  if (UsesNode=nil) or (not IsDottedIdentifier(AnUnitName)) then
    exit;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read 'uses'
  repeat
    ReadNextAtom; // read name
    if not AtomIsIdentifier then exit;
    if ReadAndCompareUsedUnit(AnUnitName) then begin
      // unit found
      exit(true);
    end;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      ReadNextAtom;
    end;
    if CurPos.Flag=cafSemicolon then break;
    if CurPos.Flag<>cafComma then break;
  until (CurPos.StartPos>UsesNode.EndPos) or (CurPos.StartPos>SrcLen);
end;

function TStandardCodeTool.RemoveUnitFromUsesSection(UsesNode: TCodeTreeNode;
  const AnUnitName: string; SourceChangeCache: TSourceChangeCache): boolean;
var UnitPos, StartPos, EndPos: integer;
  Found: Boolean;
begin
  Result:=false;
  if (UsesNode=nil) or (not IsDottedIdentifier(AnUnitName)) then
    exit;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read 'uses'
  UnitPos:=0;
  repeat
    EndPos:=CurPos.StartPos;
    ReadNextAtom; // read name
    if not AtomIsIdentifier then exit;
    inc(UnitPos);
    StartPos:=CurPos.StartPos;
    Found:=ReadAndCompareUsedUnit(AnUnitName);
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      ReadNextAtom;
    end;
    if Found then begin
      // unit found
      SourceChangeCache.MainScanner:=Scanner;
      if UnitPos=1 then begin
        // first unit in uses section
        if AtomIsChar(';') then begin
          // last unit in uses section -> delete whole uses section
          StartPos:=FindLineEndOrCodeInFrontOfPosition(UsesNode.StartPos,true,true);
          EndPos:=UsesNode.EndPos;
        end else begin
          // not last unit -> delete with comma behind
          EndPos:=FindLineEndOrCodeAfterPosition(CurPos.EndPos);
          if (EndPos>SrcLen) or (Src[EndPos] in [#10,#13]) then
            StartPos:=FindLineEndOrCodeInFrontOfPosition(StartPos);// delete space in front or even the empty line
        end;
      end else begin
        // not first unit in uses section
        if AtomIsChar(';') then begin
          // last unit -> delete with comma in front
          StartPos:=EndPos;
          EndPos:=CurPos.StartPos;
        end else if PositionsInSameLine(Src,EndPos,StartPos) then begin
          // not first unit in line -> delete with comma in front
          StartPos:=EndPos;
          EndPos:=CurPos.StartPos;
        end else begin
          // first unit in line -> delete with comma behind
          EndPos:=FindLineEndOrCodeAfterPosition(CurPos.EndPos);
          if (EndPos>SrcLen) or (Src[EndPos] in [#10,#13]) then
            StartPos:=FindLineEndOrCodeInFrontOfPosition(StartPos);// delete space in front or even the empty line
        end;
      end;
      if not SourceChangeCache.Replace(gtNone,gtNone,StartPos,EndPos,'') then
        exit;
      if not SourceChangeCache.Apply then exit;
      Result:=true;
      exit;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then break;
  until (CurPos.StartPos>UsesNode.EndPos) or (CurPos.StartPos>SrcLen);
  Result:=true;
end;

function TStandardCodeTool.RemoveUnitFromAllUsesSections(
  const AnUnitName: string; SourceChangeCache: TSourceChangeCache): boolean;

  function RemoveFromSection(UsesNode: TCodeTreeNode): boolean;
  begin
    Result:=(UsesNode=nil)
      or (RemoveUnitFromUsesSection(UsesNode,AnUnitName,SourceChangeCache));
  end;

begin
  Result:=false;
  if (AnUnitName='') or (SourceChangeCache=nil) then exit;
  BuildTree(lsrImplementationUsesSectionEnd);

  SourceChangeCache.BeginUpdate;
  try
    if not RemoveFromSection(FindMainUsesNode) then exit;
    if not RemoveFromSection(FindImplementationUsesNode) then exit;
  finally
    Result:=SourceChangeCache.EndUpdate;
  end;
end;

function TStandardCodeTool.FixUsedUnitCase(
  SourceChangeCache: TSourceChangeCache): boolean;

  function FixUsesSection(UsesNode: TCodeTreeNode): boolean;
  begin
    Result:=(UsesNode=nil) or FixUsedUnitCaseInUsesSection(UsesNode,SourceChangeCache);
  end;

begin
  //debugln('TStandardCodeTool.FixUsedUnitCase ',MainFilename);
  Result:=false;
  BuildTree(lsrImplementationUsesSectionEnd);
  if not FixUsesSection(FindMainUsesNode) then exit;
  if not FixUsesSection(FindImplementationUsesNode) then exit;
  Result:=true;
end;

function TStandardCodeTool.FixUsedUnitCaseInUsesSection(
  UsesNode: TCodeTreeNode; SourceChangeCache: TSourceChangeCache): boolean;
  
  function FindUnit(AFilename: string): string;
  var
    CurDir: String;
    MakeRelative: Boolean;
  begin
    Result:='';
    AFilename:=TrimFilename(AFilename);
    CurDir:='';
    if FilenameIsAbsolute(AFilename) then begin
      MakeRelative:=false;
    end else begin
      MakeRelative:=true;
      if TCodeBuffer(Scanner.MainCode).IsVirtual then exit;
      CurDir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename);
      AFilename:=CurDir+AFilename;
    end;
    Result:=DirectoryCache.Pool.FindDiskFilename(AFilename,true);
    if Result='' then exit;
    if MakeRelative then
      Result:=CreateRelativePath(Result,CurDir);
  end;
  
var
  UnitInFilename: String;
  Changed: Boolean;
  RealUnitInFilename: String;
  UnitNameRange: TAtomPosition;
  InAtom: TAtomPosition;
begin
  Result:=false;
  if (UsesNode=nil) then exit;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read 'uses'
  Changed:=false;
  repeat
    ReadNextAtom; // read name
    if not ReadNextUsedUnit(UnitNameRange,InAtom,false) then exit;
    if InAtom.StartPos>1 then begin
      UnitInFilename:=GetAtom(InAtom);
      //debugln('TStandardCodeTool.FixUsedUnitCaseInUsesSection A UnitInFilename="',UnitInFilename,'"');
      if (UnitInFilename<>'') and (UnitInFilename[1]='''') then begin
        UnitInFilename:=copy(UnitInFilename,2,length(UnitInFilename)-2);
        RealUnitInFilename:=FindUnit(UnitInFilename);
        //debugln('TStandardCodeTool.FixUsedUnitCaseInUsesSection B RealUnitInFilename="',RealUnitInFilename,'"');
        if (RealUnitInFilename<>'')
        and (RealUnitInFilename<>UnitInFilename) then begin
          if not Changed then begin
            SourceChangeCache.MainScanner:=Scanner;
            Changed:=true;
          end;
          debugln('TStandardCodeTool.FixUsedUnitCaseInUsesSection Replacing UnitInFilename="',UnitInFilename,'" with "',RealUnitInFilename,'"');
          if not SourceChangeCache.Replace(gtNone,gtNone,
            InAtom.StartPos,InAtom.EndPos,''''+RealUnitInFilename+'''') then exit;
        end;
      end;
    end;
    if CurPos.Flag=cafSemicolon then break;
    if CurPos.Flag<>cafComma then exit;
  until (CurPos.StartPos>UsesNode.EndPos) or (CurPos.StartPos>SrcLen);
  if Changed and (not SourceChangeCache.Apply) then exit;
  Result:=true;
end;

function TStandardCodeTool.FindUsedUnitNames(var MainUsesSection,
  ImplementationUsesSection: TStrings): boolean;
var
  MainUsesNode, ImplementatioUsesNode: TCodeTreeNode;
begin
  MainUsesSection:=nil;
  ImplementationUsesSection:=nil;
  // find the uses sections
  BuildTree(lsrImplementationUsesSectionEnd);
  MainUsesNode:=FindMainUsesNode;
  ImplementatioUsesNode:=FindImplementationUsesNode;
  // create lists
  try
    MainUsesSection:=UsesSectionToUnitNames(MainUsesNode);
    ImplementationUsesSection:=UsesSectionToUnitNames(ImplementatioUsesNode);
  except
    FreeAndNil(MainUsesSection);
    FreeAndNil(ImplementationUsesSection);
    raise;
  end;
  Result:=true;
end;

function TStandardCodeTool.FindUsedUnitNames(var List: TStringToStringTree
  ): boolean;
  
  procedure Collect(UsesNode: TCodeTreeNode; const Tag: string);
  var
    UnitNameAtom: TAtomPosition;
    InAtom: TAtomPosition;
    OldTag: string;
    AnUnitName: String;
  begin
    if UsesNode=nil then exit;
    MoveCursorToUsesStart(UsesNode);
    repeat
      // read next unit name
      ReadNextUsedUnit(UnitNameAtom, InAtom);
      AnUnitName:=GetAtom(UnitNameAtom);
      // tag unit in list
      OldTag:=List[AnUnitName];
      if System.Pos(Tag,OldTag)<1 then
        List[AnUnitName]:=OldTag+Tag;
      if CurPos.Flag=cafComma then begin
        // read next unit name
        ReadNextAtom;
      end else if CurPos.Flag=cafSemicolon then begin
        break;
      end else
        RaiseExceptionFmt(20170421201120,ctsStrExpectedButAtomFound,[';',GetAtom]);
    until false;
    Result:=true;
  end;
  
begin
  // find the uses sections
  List:=TStringToStringTree.Create(false);
  BuildTree(lsrImplementationUsesSectionEnd);
  Collect(FindMainUsesNode,'Main');
  Collect(FindImplementationUsesNode,'Implementation');
  Result:=true;
end;

function TStandardCodeTool.FindUsedUnitFiles(var MainUsesSection: TStrings
  ): boolean;
var
  MainUsesNode: TCodeTreeNode;
begin
  MainUsesSection:=nil;
  // find the uses sections
  BuildTree(lsrMainUsesSectionEnd);
  MainUsesNode:=FindMainUsesNode;
  // create lists
  try
    MainUsesSection:=UsesSectionToFilenames(MainUsesNode);
  except
    FreeAndNil(MainUsesSection);
    raise;
  end;
  Result:=true;
end;

function TStandardCodeTool.FindUsedUnitFiles(var MainUsesSection,
  ImplementationUsesSection: TStrings): boolean;
var
  MainUsesNode, ImplementatioUsesNode: TCodeTreeNode;
begin
  MainUsesSection:=nil;
  ImplementationUsesSection:=nil;
  // find the uses sections
  BuildTree(lsrImplementationUsesSectionEnd);
  MainUsesNode:=FindMainUsesNode;
  ImplementatioUsesNode:=FindImplementationUsesNode;
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
  function TStandardCodeTool.FindDelphiProjectUnits(var FoundInUnits,
    MissingInUnits, NormalUnits: TStrings): boolean;

  Reads the main uses section and tries to find each unit file having
  an 'in' modifier.
  The associated objects in the list will be the found codebuffers.
  FoundInUnits returns the list of found 'in' unitnames plus TCodeBuffer
  MissingInUnits returns the list of missing 'in' unitnames
  NormalUnits returns the list of unitnames plus TCodeBuffer (if found)

  If no codebuffer was found/created then the filename will be the unit name
  plus the 'in' extension.
------------------------------------------------------------------------------}
function TStandardCodeTool.FindDelphiProjectUnits(out FoundInUnits,
  MissingInUnits, NormalUnits: TStrings; UseContainsSection: boolean;
  IgnoreNormalUnits: boolean): boolean;
var
  AnUnitName, AnUnitInFilename: string;
  NewCode: TCodeBuffer;
  UsesNode: TCodeTreeNode;
  Node: TCodeTreeNode;
begin
  Result:=false;
  FoundInUnits:=nil;
  MissingInUnits:=nil;
  NormalUnits:=nil;
  // find the uses sections
  BuildTree(lsrMainUsesSectionEnd);
  UsesNode:=FindMainUsesNode(UseContainsSection);
  if UsesNode=nil then exit;
  FoundInUnits:=TStringList.Create;
  MissingInUnits:=TStringList.Create;
  if IgnoreNormalUnits then
    NormalUnits:=nil
  else
    NormalUnits:=TStringList.Create;
  Node:=UsesNode.FirstChild;
  while Node<>nil do begin
    // read next unit name
    AnUnitName:=ExtractUsedUnitName(Node,@AnUnitInFilename);
    // find unit file
    if AnUnitInFilename<>'' then begin
      // An 'in' unit => Delphi project file
      NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename,false,Node.StartPos);
      if (NewCode=nil) then begin
        // no source found
        MissingInUnits.Add(AnUnitName+' in '+AnUnitInFilename);
      end else begin
        // source found => add filename to list
        FoundInUnits.AddObject(AnUnitName+' in '+AnUnitInFilename,NewCode);
      end;
    end else if AnUnitName<>'' then begin
      // the units without 'in' are 'Forms' or units added by the user
      if not IgnoreNormalUnits then begin
        NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename,false,Node.StartPos);
        NormalUnits.AddObject(AnUnitName,NewCode);
      end;
    end;
    Node:=Node.NextBrother;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TStandardCodeTool.UsesSectionToFilenames(UsesNode: TCodeTreeNode
    ): TStrings;

  Reads the uses section backwards and tries to find each unit file.
  The associated objects in the list will be the found codebuffers.
  If no codebuffer was found/created then the filename will be the unit name
  plus the 'in' extension.
------------------------------------------------------------------------------}
function TStandardCodeTool.UsesSectionToFilenames(UsesNode: TCodeTreeNode
  ): TStrings;
var
  AnUnitName, AnUnitInFilename: string;
  NewCode: TCodeBuffer;
  UnitFilename: string;
  Node: TCodeTreeNode;
begin
  Result:=TStringList.Create;
  if UsesNode=nil then exit;
  Node:=UsesNode.LastChild;
  while Node<>nil do begin
    // read unit name
    AnUnitName:=ExtractUsedUnitName(Node,@AnUnitInFilename);
    if AnUnitName<>'' then begin
      // find unit file
      NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename,false,Node.StartPos);
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
    end;
    Node:=Node.PriorBrother;
  end;
end;

function TStandardCodeTool.UsesSectionToUnitnames(UsesNode: TCodeTreeNode
  ): TStrings;
var
  AnUnitName: string;
  Node: TCodeTreeNode;
begin
  Result:=TStringList.Create;
  if UsesNode=nil then exit;
  Node:=UsesNode.LastChild;
  while Node<>nil do begin
    // read unit name
    AnUnitName:=ExtractUsedUnitName(Node);
    if AnUnitName<>'' then
      Result.Add(AnUnitName);
    Node:=Node.PriorBrother;
  end;
end;

function TStandardCodeTool.FindMissingUnits(var MissingUnits: TStrings;
  FixCase: boolean; SearchImplementation: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
const
  FPCSrcSearchRequiresPPU = true;

  function CheckUsesSection(UsesNode: TCodeTreeNode): boolean;
  var
    OldUnitName: String;
    OldInFilename: String;
    AFilename: String;
    s: String;
    NewUnitName: String;
    NewInFilename: String;
    FromPos: LongInt;
    ToPos: LongInt;
    Node: TCodeTreeNode;
  begin
    if UsesNode=nil then exit(true);

    Node:=UsesNode.FirstChild;
    while Node<>nil do begin
      // read next unit name
      OldUnitName:=ExtractUsedUnitName(Node,@OldInFilename);
      // find unit file
      NewUnitName:=OldUnitName;
      NewInFilename:=OldInFilename;
      //debugln(['CheckUsesSection NewUnitName="',NewUnitName,'" NewInFilename="',NewInFilename,'"']);
      AFilename:=DirectoryCache.FindUnitSourceInCompletePath(
         NewUnitName,NewInFilename,true,FPCSrcSearchRequiresPPU,AddedNameSpace);
      s:=NewUnitName;
      if NewInFilename<>'' then
        s:=s+' in '''+NewInFilename+'''';
      if AFilename<>'' then begin
        // unit found
        if FixCase
        and ((NewUnitName<>OldUnitName) or (NewInFilename<>OldInFilename)) then
        begin
          // fix case
          FromPos:=Node.StartPos;
          ToPos:=Node.EndPos;
          SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,s);
          DebugLn('TStandardCodeTool.FindMissingUnits.CheckUsesSection fix case Unit Name(',OldUnitName,'->',NewUnitName,') InFile(',OldInFilename,'->',NewInFilename,')');
        end;
      end else begin
        // unit not found
        if MissingUnits=nil then MissingUnits:=TStringList.Create;
        MissingUnits.Add(s);
      end;
      Node:=Node.NextBrother;
    end;
    Result:=true;
  end;
  
begin
  Result:=false;
  BuildTree(lsrImplementationUsesSectionEnd);
  if FixCase then
    SourceChangeCache.MainScanner:=Scanner;
  try
    if not CheckUsesSection(FindMainUsesNode) then exit;
    if SearchImplementation
    and not CheckUsesSection(FindImplementationUsesNode) then exit;
  except
    FreeAndNil(MissingUnits);
    raise;
  end;
  if FixCase then
    Result:=SourceChangeCache.Apply
  else
    Result:=true;
end;

function TStandardCodeTool.CommentUnitsInUsesSection(MissingUnits: TStrings;
  SourceChangeCache: TSourceChangeCache; UsesNode: TCodeTreeNode): boolean;
// Examples:
// 1. uses {a,} b, c;    commenting one unit not at end
// 2. uses a, {b,} c;    commenting one unit not at end
// 3. uses {a, b,} c;    commenting several units not at end
// 4. uses a{, b, c} ;   commenting units at end
// 5. {uses a, b, c;}    commenting all units
// 6. uses {a,} b{, c};  commenting several units

  procedure Comment(StartPos, EndPos: integer);
  begin
    //debugln(['Comment ',dbgstr(copy(Src,StartPos,EndPos-StartPos))]);
    CommentCode(StartPos,EndPos,SourceChangeCache,false);
  end;

var
  i: Integer;
  CurUnitName: String;
  CommentCurUnit: Boolean;
  FirstCommentUnitStart: Integer;
  LastCommaAfterCommentUnitsStart: Integer;
  LastNormalUnitEnd: Integer;
  LastCommentUnitEnd: Integer;
  Node: TCodeTreeNode;
begin
  Result:=true;
  if UsesNode=nil then exit;
  FirstCommentUnitStart:=-1;
  LastCommaAfterCommentUnitsStart:=-1;
  LastNormalUnitEnd:=-1;
  LastCommentUnitEnd:=-1;
  Node:=UsesNode.FirstChild;
  while Node<>nil do begin
    // check if unit should be commented
    CurUnitName:=ExtractUsedUnitName(Node);
    // Note: CurPos is now on atom behind used unit, i.e. comma or semicolon
    i:=MissingUnits.Count-1;
    while (i>=0)
    and (CompareIdentifiers(PChar(Pointer(MissingUnits[i])),
                            PChar(Pointer(CurUnitName)))<>0) do
      dec(i);
    CommentCurUnit:=i>=0;
    //debugln('CommentUnitsInUsesSection CurUnitName="',CurUnitName,'" CommentCurUnit=',dbgs(CommentCurUnit));

    if CommentCurUnit then begin
      // unit should be commented
      if FirstCommentUnitStart<1 then FirstCommentUnitStart:=Node.StartPos;
      LastCommentUnitEnd:=Node.EndPos;
    end else begin
      // unit should be kept
      LastNormalUnitEnd:=Node.EndPos;
      if FirstCommentUnitStart>=1 then begin
        // there are some units to be commented
        // See examples: 1., 2., 3. and 6.
        Comment(FirstCommentUnitStart,LastCommaAfterCommentUnitsStart);
        FirstCommentUnitStart:=-1;
        LastCommentUnitEnd:=-1;
        LastCommaAfterCommentUnitsStart:=-1;
      end;
    end;

    if CommentCurUnit then
      LastCommaAfterCommentUnitsStart:=CurPos.EndPos;

    if CurPos.Flag<>cafComma then begin
      if CommentCurUnit then begin
        // last unit must be commented
        if LastNormalUnitEnd>=1 then begin
          // comment last unit and keep some units in front
          // See example: 4.
          Comment(LastNormalUnitEnd,LastCommentUnitEnd);
        end else begin
          // all units should be commented
          // See example: 5.
          Comment(UsesNode.StartPos,CurPos.EndPos);
        end;
      end;
      break;
    end;

    Node:=Node.NextBrother;
  end;
end;

function TStandardCodeTool.CommentUnitsInUsesSections(MissingUnits: TStrings;
  SourceChangeCache: TSourceChangeCache): boolean;
begin
  if (MissingUnits=nil) or (MissingUnits.Count=0) then
    exit(true);
  Result:=false;
  BuildTree(lsrInitializationStart);
  SourceChangeCache.MainScanner:=Scanner;
  if not CommentUnitsInUsesSection(MissingUnits, SourceChangeCache, FindMainUsesNode) then exit;
  if not CommentUnitsInUsesSection(MissingUnits, SourceChangeCache, FindImplementationUsesNode) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.FindUnusedUnits(Units: TStrings): boolean;
// returns a list of unitname=flags
// flags are a comma separated list of words:
//   'implementation': unit is in implementation uses section
//   'used': an identifier of the interface is used
//   'code': unit has non empty initialization/finalization section
var
  Identifiers: TAVLTree;// all identifiers used in this unit

  function IsUnitAlreadyChecked(const AnUnitName: string): boolean;
  var
    i: Integer;
  begin
    for i:=0 to Units.Count-1 do
      if SysUtils.CompareText(Units.Names[i],AnUnitName)=0 then exit(true);
    Result:=false;
  end;

  procedure GatherIdentifiersInRange(StartPos, EndPos: integer);
  // gather all used identifiers from this unit in the range Startpos..EndPos
  var
    Identifier: PChar;
  begin
    if (StartPos<1) or (StartPos>=EndPos) then exit;
    MoveCursorToCleanPos(StartPos);
    repeat
      ReadNextAtom;
      if CurPos.StartPos>=EndPos then break;
      if IsIdentStartChar[Src[CurPos.StartPos]] then begin
        Identifier:=@Src[CurPos.StartPos];
        if Identifiers.Find(Identifier)=nil then begin
          {$IFDEF VerboseFindUnusedUnits}
          DebugLn(['Used Identifier=',GetIdentifier(Identifier)]);
          {$ENDIF}
          Identifiers.Add(Identifier);
        end;
      end;
    until false;
  end;

  procedure GatherIdentifiers;
  // gather all used identifiers from this unit
  var
    StartPos: Integer;

    procedure Gather(EndPos: integer);
    begin
      if StartPos<1 then exit;
      GatherIdentifiersInRange(StartPos,EndPos);
      StartPos:=-1;
    end;

  var
    Node: TCodeTreeNode;
  begin
    if Identifiers<>nil then exit;
    Identifiers:=TAVLTree.Create(@CompareIdentifierPtrs);
    {$IFDEF VerboseFindUnusedUnits}
    DebugLn(['GatherIdentifiers ']);
    {$ENDIF}
    StartPos:=-1;
    Node:=Tree.Root;
    while Node<>nil do begin
      case Node.Desc of
      ctnUseUnit,ctnUsesSection,ctnUseUnitNamespace,ctnUseUnitClearName,
      ctnProgram,ctnUnit,ctnPackage,ctnLibrary,ctnEndPoint:
        begin
          // skip node
          Gather(Node.StartPos);
        end;
      ctnEnumIdentifier,
      ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition:
        begin
          // start reading behind identifier
          Gather(Node.StartPos);
          MoveCursorToCleanPos(Node.StartPos);
          ReadNextAtom;
          StartPos:=CurPos.EndPos;
        end;
      else
        if StartPos<1 then
          StartPos:=Node.StartPos;
      end;
      Node:=Node.Next;
    end;
  end;

  function InterfaceIsUsed(Tool: TFindDeclarationTool;
    IntfNode: TCodeTreeNode): boolean;

    function IsIdentifierUsed(StartPos: integer): boolean;
    begin
      {$IFDEF VerboseFindUnusedUnits}
      if CompareIdentifiers(PChar(GetIdentifier(@Tool.Src[StartPos])),'TComponent')=0 then
        DebugLn(['IsIdentifierUsed ',GetIdentifier(@Tool.Src[StartPos])]);
      {$ENDIF}
      Result:=Identifiers.Find(@Tool.Src[StartPos])<>nil;
    end;

    function IsNodeVisible(Node: TCodeTreeNode): boolean;
    begin
      Result:=false;
      Node:=Node.Parent;
      while Node<>nil do begin
        if Node.Desc=ctnInterface then exit(true);
        if Node.Desc in AllClasses then exit;
        Node:=Node.Parent;
      end;
    end;

    function FindProceduresInHelper(Node: TCodeTreeNode): boolean;
    begin
      Result := False;
      Node := Node.FirstChild;
      while (Node<>nil) and (Node.Desc <> ctnEndPoint) do
      begin
        if (Node.Desc = ctnProcedure) and (Node.FirstChild<>nil) then
        begin
          if IsIdentifierUsed(Node.FirstChild.StartPos) then
            Exit(True);
        end;
        Node:=Node.Next;
      end;
    end;

  var
    Node: TCodeTreeNode;
  begin
    Result:=true;
    Node:=IntfNode.FirstChild;
    while Node<>nil do begin
      case Node.Desc of
      ctnTypeHelper, ctnRecordHelper, ctnClassHelper:
        if FindProceduresInHelper(Node) then exit;
      ctnEnumIdentifier:
        if IsIdentifierUsed(Node.StartPos) then exit;
      ctnVarDefinition,ctnConstDefinition,ctnTypeDefinition,ctnGenericName:
        if IsNodeVisible(Node) and IsIdentifierUsed(Node.StartPos) then exit;
      ctnProcedure:
        if (Node.Parent.Desc=ctnInterface)
        and (Node.FirstChild<>nil)
        and (Node.FirstChild.Desc=ctnProcedureHead)
        and IsIdentifierUsed(Node.FirstChild.StartPos) then exit;
      ctnGlobalProperty:
        if Tool.MoveCursorToPropName(Node)
        and IsIdentifierUsed(Tool.CurPos.StartPos) then exit;
      end;
      Node:=Node.Next;
    end;
    Result:=false;
  end;

  procedure CheckUnit(Tool: TFindDeclarationTool;
    out HasCode, UseInterface: boolean);
  var
    Node: TCodeTreeNode;
    Identifier: String;
  begin
    GatherIdentifiers;
    HasCode:=false;
    UseInterface:=false;
    // parse used unit
    Tool.BuildTree(lsrEnd);
    Node:=Tool.Tree.Root;
    while (Node<>nil) do begin
      case Node.Desc of
      ctnUnit,ctnPackage,ctnLibrary:
        begin
          Identifier:=Tool.ExtractSourceName;
          if Identifiers.Find(PChar(Identifier))<>nil then
            UseInterface:=true;
        end;
      ctnInterface:
        if not UseInterface then
          UseInterface:=InterfaceIsUsed(Tool,Node);
      ctnInitialization,ctnFinalization,ctnBeginBlock:
        begin
          HasCode:=true;
          break;
        end;
      end;
      Node:=Node.NextBrother;
    end;
  end;

  procedure CheckUsesSection(UsesNode: TCodeTreeNode; InImplementation: boolean);
  var
    Unit_Name: String;
    UnitInFilename: String;
    Tool: TFindDeclarationTool;
    HasCode: boolean;
    UseInterface: boolean;
    Flags: String;
    Node: TCodeTreeNode;
  begin
    HasCode:=false;
    UseInterface:=false;
    if UsesNode=nil then exit;
    Node:=UsesNode.FirstChild;
    while Node<>nil do begin
      Unit_Name:=ExtractUsedUnitName(Node,@UnitInFilename);
      if not IsUnitAlreadyChecked(Unit_Name) then begin
        // try to load the used unit
        {$IFDEF VerboseFindUnusedUnits}
        DebugLn(['CheckUsesSection ',Unit_Name,' in ',UnitInFilename]);
        {$ENDIF}
        Tool:=FindCodeToolForUsedUnit(Unit_Name,UnitInFilename,true);
        // parse the used unit
        CheckUnit(Tool,HasCode,UseInterface);
        Flags:='';
        if InImplementation then
          Flags:=Flags+',implementation';
        if HasCode then
          Flags:=Flags+',code';
        if UseInterface then
          Flags:=Flags+',used';
        {$IFDEF VerboseFindUnusedUnits}
        DebugLn(['CheckUsesSection ',Unit_Name,'=',Flags]);
        {$ENDIF}
        Units.Add(Unit_Name+'='+Flags);
      end;
      Node:=Node.NextBrother;
    end;
  end;

begin
  Result:=false;
  {$IFDEF VerboseFindUnusedUnits}
  DebugLn(['TStandardCodeTool.FindUnusedUnits START']);
  {$ENDIF}
  BuildTree(lsrEnd);
  Identifiers:=nil;
  try
    CheckUsesSection(FindMainUsesNode,false);
    CheckUsesSection(FindImplementationUsesNode,true);
  finally
    Identifiers.Free;
  end;
  {$IFDEF VerboseFindUnusedUnits}
  DebugLn(['TStandardCodeTool.FindUnusedUnits END']);
  {$ENDIF}
  Result:=true;
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
    BuildTree(lsrEnd);
    InitializationNode:=FindInitializationNode;
    if InitializationNode=nil then exit;
    LinkIndex:=Scanner.LinkIndexAtCleanPos(InitializationNode.StartPos);
  end else begin
    InitializationNode:=nil;
    inc(LinkIndex);
  end;
  if (LinkIndex<0) or (LinkIndex>=Scanner.LinkCount) then exit;
  StartCode:=TCodeBuffer(Scanner.Links[LinkIndex].Code);
  // ToDo: Fix the test for InitializationNode, it can be Nil.
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
  Result.EndPos:=-1;
  Result.Flag:=cafNone;
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
  BuildTree(lsrEnd);
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
  BuildTree(lsrEnd);
  SourceChangeCache.MainScanner:=Scanner;
  OldPosition:=FindLazarusResourceInBuffer(ResourceCode,ResourceName);
  if OldPosition.StartPos>0 then begin
    OldPosition.StartPos:=FindLineEndOrCodeInFrontOfPosition(
         OldPosition.StartPos);
    OldPosition.EndPos:=FindLineEndOrCodeAfterPosition(OldPosition.EndPos);
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
  FileEnd:=0;
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

function TStandardCodeTool.CheckLFM(LFMBuf: TCodeBuffer; out LFMTree: TLFMTree;
  const OnFindDefineProperty: TOnFindDefinePropertyForContext;
  RootMustBeClassInUnit: boolean; RootMustBeClassInIntf: boolean;
  ObjectsMustExist: boolean): boolean;
var
  RootContext: TFindContext;

  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext; ContextIsDefault: boolean): boolean; forward;

  function FindNonPublishedDefineProperty(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    const IdentName: string; const ClassContext: TFindContext): boolean;
  // properties can be defined via DefineProperties
  var
    PropertyNode: TLFMPropertyNode;
    ObjectNode: TLFMObjectNode;
    AncestorClassContext: TFindContext;
    Params: TFindDeclarationParams;
    IsDefined: Boolean;
  begin
    Result:=false;
    if (not (LFMNode is TLFMPropertyNode)) then exit;
    PropertyNode:=TLFMPropertyNode(LFMNode);
    if (PropertyNode.Parent=nil)
    or (not (PropertyNode.Parent is TLFMObjectNode)) then exit;
    ObjectNode:=TLFMObjectNode(PropertyNode.Parent);
    // find define property
    IsDefined:=false;
    if Assigned(OnFindDefineProperty) then begin
      AncestorClassContext:=CleanFindContext;
      if ClassContext.Tool=Self then begin
        // the class is defined in this source
        // -> try to find the ancestor class
        if ObjectNode.AncestorContextValid then begin
          AncestorClassContext:=CreateFindContext(
                                  TFindDeclarationTool(ObjectNode.AncestorTool),
                                  TCodeTreeNode(ObjectNode.AncestorNode));
        end else begin
          {$IFDEF VerboseCheckLFM}
          debugln('FindNonPublishedDefineProperty Class is defined in this source: search ancestor ... ');
          {$ENDIF}
          Params:=TFindDeclarationParams.Create;
          try
            Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
                           fdfExceptionOnPredefinedIdent];
            Params.ContextNode:=ClassContext.Node;
            try
              if ClassContext.Tool.FindAncestorOfClass(ClassContext.Node,
                Params,true) then
              begin
                {$IFDEF VerboseCheckLFM}
                debugln('FindNonPublishedDefineProperty Ancestor found');
                {$ENDIF}
                AncestorClassContext:=CreateFindContext(Params);
                ObjectNode.AncestorTool:=AncestorClassContext.Tool;
                ObjectNode.AncestorNode:=AncestorClassContext.Node;
              end;
            except
              // ignore search/parse errors
              on E: ECodeToolError do ;
            end;
          finally
            Params.Free;
          end;
          ObjectNode.AncestorContextValid:=true;
        end;
      end;
      OnFindDefineProperty(Self,ClassContext,AncestorClassContext,LFMNode,
        IdentName,IsDefined);
      if IsDefined then begin
        //debugln('FindNonPublishedDefineProperty Path=',LFMNode.GetPath,' IdentName="',IdentName,'"');
      end else begin
        {$IFDEF VerboseCheckLFM}
        debugln('FindNonPublishedDefineProperty Path=',LFMNode.GetPath,' NO DEFINE PROPERTIES');
        {$ENDIF}
        if DefaultErrorPosition=0 then ;
      end;
    end;
    Result:=IsDefined;
  end;

  function FindLFMIdentifier(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    const IdentName: string; const ClassContext: TFindContext;
    SearchInDefinePropertiesToo, ErrorOnNotFound: boolean;
    out IdentContext: TFindContext): boolean;
  var
    Params: TFindDeclarationParams;
    IsPublished, IsMissingInCode: Boolean;
    CurContext: TFindContext;
  begin
    Result:=false;
    IdentContext:=CleanFindContext;
    IsPublished:=false;
    if (ClassContext.Node=nil)
    or (not (ClassContext.Node.Desc in AllClasses)) then begin
      DebugLn('TStandardCodeTool.CheckLFM.FindLFMIdentifier Internal error');
      exit;
    end;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
                     fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
                     fdfIgnoreOverloadedProcs];
      Params.ContextNode:=ClassContext.Node;
      Params.SetIdentifier(ClassContext.Tool,PChar(Pointer(IdentName)),nil);
      try
        {DebugLn('FindLFMIdentifier A ',
          ' Ident=',
          '"'+GetIdentifier(Params.Identifier)+'"',
          ' Context="'+ClassContext.Node.DescAsString,'" "',StringToPascalConst(copy(ClassContext.Tool.Src,ClassContext.Node.StartPos,20))+'"',
          ' File="'+ExtractFilename(ClassContext.Tool.MainFilename)+'"',
          ' Flags=['+FindDeclarationFlagsAsString(Params.Flags)+']'
          );}
        if ClassContext.Tool.FindIdentifierInContext(Params) then begin
          IdentContext:=CleanFindContext;
          repeat
            CurContext:=CreateFindContext(Params);
            if (not IsPublished)
            and (CurContext.Node.HasParentOfType(ctnClassPublished)) then
              IsPublished:=true;

            if (IdentContext.Node=nil) then begin
              if (LFMNode.TheType<>lfmnProperty)
              or ((CurContext.Node.Desc=ctnProperty)
                  and (not CurContext.Tool.PropNodeIsTypeLess(CurContext.Node)))
              then
                IdentContext:=CurContext;
            end;

            if (IdentContext.Node<>nil) and IsPublished then break;

            // search further
            Params.Clear;
            Params.Flags:=[fdfSearchInAncestors,
                           fdfIgnoreMissingParams,
                           fdfIgnoreCurContextNode,
                           fdfIgnoreOverloadedProcs];
            Params.ContextNode:=CurContext.Node.Parent;
            while (Params.ContextNode<>nil)
            and (not (Params.ContextNode.Desc in AllClasses)) do
              Params.ContextNode:=Params.ContextNode.Parent;
            if Params.ContextNode=nil then break;
            Params.SetIdentifier(ClassContext.Tool,PChar(Pointer(IdentName)),nil);
            if not CurContext.Tool.FindIdentifierInContext(Params) then
            begin
              DebugLn(['FindLFMIdentifier ERROR ancestor of '+LFMNode.GetPath+' not found: ',FindContextToString(IdentContext),' IdentName=',IdentName]);
              break;
            end;
          until Params.NewNode=nil;
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;

    IsMissingInCode := False;
    if (IdentContext.Node<>nil) and IsPublished then begin
      Result:=true;
    end else begin
      // no proper node found -> search in DefineProperties
      if SearchInDefinePropertiesToo then begin
        if FindNonPublishedDefineProperty(LFMNode,DefaultErrorPosition,IdentName,ClassContext)
        then begin
          //debugln(['FindLFMIdentifier "',IdentName,'" is defined via DefineProperties']);
          Result:=true;
        end;
      end
      else
        IsMissingInCode := True;
    end;
    if (not Result) and ErrorOnNotFound then begin
      if (IdentContext.Node<>nil) and (not IsPublished) then begin
        LFMTree.AddError(lfmeIdentifierNotPublished,LFMNode,
                         'identifier '+IdentName+' is not published in class '
                         +'"'+ClassContext.Tool.ExtractClassName(ClassContext.Node,false,true)+'"',
                         DefaultErrorPosition);
      end
      else if IsMissingInCode then begin
        LFMTree.AddError(lfmeIdentifierMissingInCode,LFMNode,
                         'identifier '+IdentName+' not found in pascal code '
                         +'"'+ClassContext.Tool.ExtractClassName(ClassContext.Node,false,true)+'"',
                         DefaultErrorPosition);
      end
      else begin
        LFMTree.AddError(lfmeIdentifierNotFound,LFMNode,
                         'identifier '+IdentName+' not found in class '
                         +'"'+ClassContext.Tool.ExtractClassName(ClassContext.Node,false,true)+'"',
                         DefaultErrorPosition);
      end;
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
    if (DefinitionNode.Desc=ctnIdentifier) then
      Identifier:=@StartTool.Src[DefinitionNode.StartPos]
    else if DefinitionNode.Desc=ctnProperty then
      Identifier:=StartTool.GetPropertyTypeIdentifier(DefinitionNode)
    else
      Identifier:=nil;
    if Identifier=nil then begin
      {$IFDEF VerboseCheckLFM}
      debugln(['FindClassNodeForLFMObject LFMNode=',LFMNode.GetPath,' definition node has no identifier: ',FindContextToString(CreateFindContext(StartTool,DefinitionNode))]);
      {$ENDIF}
      exit;
    end;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs,fdfIgnoreCurContextNode];
      Params.ContextNode:=DefinitionNode;
      Params.SetIdentifier(StartTool,Identifier,nil);
      try
        Params.Save(OldInput);
        if StartTool.FindIdentifierInContext(Params) then begin
          Params.Load(OldInput,true);
          Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
          if (Result.Node=nil) then begin
            {$IFDEF VerboseCheckLFM}
            debugln(['FindClassNodeForLFMObject FindBaseTypeOfNode failed. LFMNode=',LFMNode.GetPath,' ',FindContextToString(CreateFindContext(StartTool,DefinitionNode))]);
            {$ENDIF}
            Result:=CleanFindContext;
          end else if (not (Result.Node.Desc in AllClasses)) then begin
            {$IFDEF VerboseCheckLFM}
            debugln(['FindClassNodeForLFMObject base type is not a class. LFMNode=',LFMNode.GetPath,' ',FindContextToString(Result)]);
            {$ENDIF}
            Result:=CleanFindContext;
          end;
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do begin
          {$IFDEF VerboseCheckLFM}
          debugln(['FindClassNodeForLFMObject ',E.Message]);
          {$ENDIF}
        end;
      end;
    finally
      Params.Free;
    end;
    if Result.Node=nil then begin
      // FindClassNodeForLFMObject
      LFMTree.AddError(lfmeIdentifierNotFound,LFMNode,
                       'class '+GetIdentifier(Identifier)+' not found',
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

  function FindClassContext(const ClassName: string): TFindContext;
  var
    Params: TFindDeclarationParams;
    Identifier: PChar;
    OldInput: TFindDeclarationInput;
    StartTool: TStandardCodeTool;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
    StartTool:=Self;
    Identifier:=PChar(Pointer(ClassName));
    try
      Params.Flags:=[fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
      Params.ContextNode:=FindInterfaceNode;
      if Params.ContextNode=nil then
        Params.ContextNode:=FindMainUsesNode;
      Params.SetIdentifier(StartTool,Identifier,nil);
      try
        Params.Save(OldInput);
        if FindIdentifierInContext(Params) then begin
          Params.Load(OldInput,true);
          Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
          if (Result.Node=nil)
          or (not (Result.Node.Desc in AllClasses)) then
            Result:=CleanFindContext;
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
  end;

  procedure CheckLFMChildObject(LFMObject: TLFMObjectNode;
    const ParentContext: TFindContext; ContextIsDefault: boolean);
  var
    LFMObjectName: String;
    ChildContext: TFindContext;
    VariableTypeName: String;
    DefinitionNode: TCodeTreeNode;
    ClassContext: TFindContext;
    IdentifierFound: Boolean;
  begin
    // find variable for object

    // find identifier in Lookup Root
    LFMObjectName:=LFMObject.Name;
    //DebugLn('CheckChildObject A LFMObjectName="',LFMObjectName,'"');
    if LFMObjectName='' then begin
      LFMTree.AddError(lfmeObjectNameMissing,LFMObject,'missing object name',
                       LFMObject.StartPos);
      exit;
    end;

    ChildContext:=CleanFindContext;
    IdentifierFound:=(not ContextIsDefault) and
      FindLFMIdentifier(LFMObject,LFMObject.NamePosition,LFMObjectName,RootContext,
          false,ObjectsMustExist,ChildContext);

    //debugln(['CheckLFMChildObject LFMObjectName="',LFMObjectName,'" IdentifierFound=',IdentifierFound,' ObjectsMustExist=',ObjectsMustExist,' ',FindContextToString(ChildContext)]);
    if IdentifierFound and (ObjectsMustExist or (ChildContext.Node<>nil)) then
    begin
      if ChildContext.Node=nil then begin
        // this is an extra entry, created via DefineProperties.
        // There is no generic way to test such things
        exit;
      end;

      // check if identifier is a variable or property
      VariableTypeName:='';
      if (ChildContext.Node.Desc=ctnVarDefinition) then begin
        DefinitionNode:=ChildContext.Tool.FindTypeNodeOfDefinition(ChildContext.Node);
        if DefinitionNode<>nil then
          VariableTypeName:=ChildContext.Tool.ExtractDefinitionNodeType(ChildContext.Node);
      end else if (ChildContext.Node.Desc=ctnProperty) then begin
        DefinitionNode:=ChildContext.Node;
        VariableTypeName:=ChildContext.Tool.ExtractPropType(ChildContext.Node,false,false);
      end
      else
        DefinitionNode:=nil;
      if DefinitionNode=nil then begin
        LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                         LFMObjectName+' is not a variable'
                         +CreateFootNote(ChildContext),
                         LFMObject.NamePosition);
        exit;
      end;

      // check if variable/property has a compatible type
      if (VariableTypeName<>'') then begin
        if (LFMObject.TypeName<>'')
        and (CompareIdentifiers(PChar(VariableTypeName),
                                PChar(LFMObject.TypeName))<>0)
        then begin
          ChildContext.Node:=DefinitionNode;
          LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                         VariableTypeName+' expected, but '+LFMObject.TypeName+' found.'
                         +CreateFootNote(ChildContext),
                         LFMObject.NamePosition);
          exit;
        end;

        // ToDo: check if variable/property type exists

      end;


      // find class node
      //debugln(['CheckLFMChildObject searching class node: LFMObjectName="',LFMObjectName,'" ',FindContextToString(CreateFindContext(ChildContext.Tool,DefinitionNode))]);
      ClassContext:=FindClassNodeForLFMObject(LFMObject,LFMObject.TypeNamePosition,
                                              ChildContext.Tool,DefinitionNode);
      //debugln(['CheckLFMChildObject LFMObjectName="',LFMObjectName,'" class context: ',FindContextToString(ClassContext)]);
    end else begin
      // try the object type
      ClassContext:=FindClassContext(LFMObject.TypeName);
      if ClassContext.Node=nil then begin
        // object type not found
        LFMTree.AddError(lfmeIdentifierNotFound,LFMObject,
            'type '+LFMObject.TypeName+' not found',LFMObject.TypeNamePosition);
      end;
    end;
    // check child LFM nodes
    if ClassContext.Node<>nil then
      CheckLFMObjectValues(LFMObject,ClassContext,false)
    else
      CheckLFMObjectValues(LFMObject,ParentContext,true);
  end;

  function FindClassNodeForPropertyType(LFMProperty: TLFMPropertyNode;
    DefaultErrorPosition: integer; const PropertyContext: TFindContext): TFindContext;
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
    //DebugLn('CheckLFMProperty A LFMProperty Name="',LFMProperty.CompleteName,'" ParentContext=',FindContextToString(ParentContext));

    if LFMProperty.CompleteName='' then begin
      LFMTree.AddError(lfmePropertyNameMissing,LFMProperty,
                       'property without name',LFMProperty.StartPos);
      exit;
    end;

    // find every part of the property name
    SearchContext:=ParentContext;
    for i:=0 to LFMProperty.NameParts.Count-1 do begin
      if SearchContext.Node.Desc=ctnProperty then begin
        // get the type of the property and search the class node
        SearchContext:=FindClassNodeForPropertyType(LFMProperty,
          LFMProperty.NameParts.NamePositions[i],SearchContext);
        if SearchContext.Node=nil then exit;
      end;

      CurName:=LFMProperty.NameParts.Names[i];
      if not FindLFMIdentifier(LFMProperty,
                               LFMProperty.NameParts.NamePositions[i],
                               CurName,SearchContext,true,true,
                               CurPropertyContext)
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
    const ClassContext: TFindContext; ContextIsDefault: boolean): boolean;
  var
    CurLFMNode: TLFMTreeNode;
  begin
    //DebugLn('TStandardCodeTool.CheckLFM.CheckLFMObjectValues A ',LFMObject.Name,':',LFMObject.TypeName);
    CurLFMNode:=LFMObject.FirstChild;
    while CurLFMNode<>nil do begin
      //DebugLn('TStandardCodeTool.CheckLFM.CheckLFMObjectValues B ',CurLFMNode.ClassName);
      case CurLFMNode.TheType of
        lfmnObject:
          CheckLFMChildObject(TLFMObjectNode(CurLFMNode),ClassContext,ContextIsDefault);
        lfmnProperty:
          if not ContextIsDefault then
            CheckLFMProperty(TLFMPropertyNode(CurLFMNode),ClassContext);
      end;
      CurLFMNode:=CurLFMNode.NextSibling;
    end;
    Result:=true;
  end;

  function CheckLFMRoot(RootLFMNode: TLFMTreeNode): boolean;
  var
    LookupRootLFMNode: TLFMObjectNode;
    LookupRootTypeName: String;
    RootClassNode: TCodeTreeNode;
  begin
    Result:=false;

    //DebugLn('TStandardCodeTool.CheckLFM.CheckLFMRoot checking root ...');
    // get root object node
    if (RootLFMNode=nil) or (not (RootLFMNode is TLFMObjectNode)) then begin
      LFMTree.AddError(lfmeMissingRoot,nil,'missing root object',1);
      exit;
    end;
    LookupRootLFMNode:=TLFMObjectNode(RootLFMNode);

    // get type name of root object
    LookupRootTypeName:=LookupRootLFMNode.TypeName;
    if LookupRootTypeName='' then begin
      LFMTree.AddError(lfmeMissingRoot,nil,'missing type of root object',1);
      exit;
    end;

    // find root type
    if RootMustBeClassInIntf then begin
      RootClassNode:=FindClassNodeInInterface(LookupRootTypeName,true,false,false);
      RootContext:=CleanFindContext;
      RootContext.Node:=RootClassNode;
      RootContext.Tool:=Self;
    end else if RootMustBeClassInUnit then begin
      RootClassNode:=FindClassNodeInUnit(LookupRootTypeName,true,false,false,false);
      RootContext:=CleanFindContext;
      RootContext.Node:=RootClassNode;
      RootContext.Tool:=Self;
    end else begin
      RootContext:=FindClassContext(LookupRootTypeName);
      RootClassNode:=RootContext.Node;
    end;
    if RootClassNode=nil then begin
      LFMTree.AddError(lfmeMissingRoot,LookupRootLFMNode,
                       'type '+LookupRootLFMNode.TypeName+' not found',
                       LookupRootLFMNode.TypeNamePosition);
      exit;
    end;
    Result:=CheckLFMObjectValues(LookupRootLFMNode,RootContext,false);
  end;

var
  CurRootLFMNode: TLFMTreeNode;
begin
  Result:=false;
  //DebugLn('TStandardCodeTool.CheckLFM A');
  // create tree from LFM file
  LFMTree:=DefaultLFMTrees.GetLFMTree(LFMBuf,true);
  ActivateGlobalWriteLock;
  try
    //DebugLn('TStandardCodeTool.CheckLFM parsing LFM ...');
    if not LFMTree.ParseIfNeeded then exit;
    // parse unit and find LookupRoot
    //DebugLn('TStandardCodeTool.CheckLFM parsing unit ...');
    BuildTree(lsrImplementationUsesSectionEnd);
    // find every identifier
    //DebugLn('TStandardCodeTool.CheckLFM checking identifiers ...');
    CurRootLFMNode:=LFMTree.Root;
    while CurRootLFMNode<>nil do begin
      if not CheckLFMRoot(CurRootLFMNode) then exit;
      CurRootLFMNode:=CurRootLFMNode.NextSibling;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
  Result:=LFMTree.FirstError=nil;
end;

function TStandardCodeTool.FindCreateFormStatement(StartPos: integer;
  const AClassName, AVarName: string;
  out Position: TAtomPosition): integer;
// 0=found, -1=not found, 1=found, but wrong classname
var MainBeginNode: TCodeTreeNode;
  ClassNameFits: boolean;
begin
  Result:=-1;
  Position.StartPos:=-1;
  if (AClassName='') or (AVarName='') or (length(AClassName)>255)
  or (length(AVarName)>255) then exit;
  if StartPos<1 then begin
    BuildTree(lsrEnd);
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
        ClassNameFits:=AtomIsIdentifier(PChar(AClassName));
        if ReadNextAtomIsChar(',')
        and (ReadNextAtomIsIdentifier(PChar(AVarName)) or (AVarName='*')) then begin
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
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  if (AClassName='') or (length(AClassName)>255) or (AVarName='')
  or (length(AVarName)>255) then exit;
  BuildTree(lsrEnd);
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode=nil then exit;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  FromPos:=-1;
  if FindCreateFormStatement(MainBeginNode.StartPos,AClassName,
    AVarName,OldPosition)=-1
  then begin
    // does not exist -> create as last in front of 'Application.Run'
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
    Indent:=Beauty.GetLineIndent(Src,FromPos);
    FromPos:=FindLineEndOrCodeInFrontOfPosition(FromPos);
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,FromPos,
       Beauty.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+');',Indent));
  end else begin
    // it exists -> replace it
    FromPos:=FindLineEndOrCodeInFrontOfPosition(OldPosition.StartPos);
    ToPos:=FindLineEndOrCodeAfterPosition(OldPosition.EndPos);
    SourceChangeCache.MainScanner:=Scanner;
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
       Beauty.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+');',
         Beauty.Indent));
  end;
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.RemoveCreateFormStatement(const AVarName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var Position: TAtomPosition;
  FromPos, ToPos: integer;
begin
  Result:=false;
  if FindCreateFormStatement(-1,'*',AVarName,Position)=-1 then
    exit;
  FromPos:=FindLineEndOrCodeInFrontOfPosition(Position.StartPos);
  ToPos:=FindLineEndOrCodeAfterPosition(Position.EndPos);
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
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  if (OldClassName='') or (length(OldClassName)>255)
  or (OldVarName='') or (length(OldVarName)>255)
  or (NewClassName='') or (length(NewClassName)>255)
  or (NewVarName='') or (length(NewVarName)>255)
  then exit;
  BuildTree(lsrEnd);
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  if StartPos<1 then begin
    MainBeginNode:=FindMainBeginEndNode;
    if MainBeginNode=nil then exit;
    StartPos:=MainBeginNode.StartPos;
    if StartPos<1 then exit;
  end;
  FromPos:=-1;
  if FindCreateFormStatement(StartPos,OldClassName,
    OldVarName,OldPosition)=-1 then begin
    // does not exist
    if OnlyIfExists then begin
      Result:=true;
      exit;
    end;
    Result:=AddCreateFormStatement(NewClassName,NewVarName,SourceChangeCache);
  end else begin
    // replace
    FromPos:=FindLineEndOrCodeInFrontOfPosition(OldPosition.StartPos);
    ToPos:=FindLineEndOrCodeAfterPosition(OldPosition.EndPos);
    SourceChangeCache.MainScanner:=Scanner;
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
       Beauty.BeautifyStatement(
         'Application.CreateForm('+NewClassName+','+NewVarName+');',
         Beauty.Indent));
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
  BuildTree(lsrEnd);
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
}
var Position, InsertPos, i, ColonPos, Indent: integer;
  StatementPos: TAtomPosition;
  MainBeginNode: TCodeTreeNode;
  AClassName, AVarName: string;
  LastEndPos: Integer;
  Beauty: TBeautifyCodeOptions;
begin
  Result:= false;
  if (List = nil) or (SourceChangeCache = nil) then exit;
  BuildTree(lsrEnd);

  { first delete all CreateForm Statements }
  SourceChangeCache.MainScanner:= Scanner;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  MainBeginNode:=FindMainBeginEndNode;
  if MainBeginNode = nil then exit;
  Position:=MainBeginNode.StartPos;
  InsertPos:=-1;
  LastEndPos:=-1;
  repeat
    if FindCreateFormStatement(Position, '*', '*', StatementPos) = -1 then break;

    Position:=StatementPos.EndPos;
    StatementPos.StartPos:=FindLineEndOrCodeInFrontOfPosition(StatementPos.StartPos);
    if (LastEndPos>0) and (StatementPos.StartPos<LastEndPos) then
      StatementPos.StartPos:=LastEndPos;
    if InsertPos < 1 then InsertPos:= StatementPos.StartPos;

    StatementPos.EndPos:=FindLineEndOrCodeAfterPosition(StatementPos.EndPos);
    LastEndPos:=StatementPos.EndPos;

    if not SourceChangeCache.Replace(gtNone,gtNone, StatementPos.StartPos, StatementPos.EndPos, '') then
      exit;
  until false;

  Result:=SourceChangeCache.Apply;
  if not Result then exit;

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
    Indent:=Beauty.GetLineIndent(Src, InsertPos);

    SourceChangeCache.Replace(gtNewLine, gtNewLine, InsertPos, InsertPos,
      SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
        'Application.CreateForm('+AClassName+','+AVarName+');', Indent));
  end;
  Result:= Result and SourceChangeCache.Apply;
end;

function TStandardCodeTool.SetApplicationScaledStatement(
  const NewScaled: boolean; SourceChangeCache: TSourceChangeCache): boolean;
begin
  Result := SetApplicationStatement('Scaled', BoolToStr(NewScaled, True), SourceChangeCache);
end;

function TStandardCodeTool.SetApplicationStatement(const APropertyName,
  NewCode: string; SourceChangeCache: TSourceChangeCache): boolean;
var
  StartPos, ConstStartPos, EndPos: integer;
  NewStatement: String;
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  // search old Application.APropertyName:=XYZ statement
  FindApplicationStatement(UpperCase(APropertyName),StartPos,ConstStartPos,EndPos);
  // create statement. FindApplicationStatement always returns an insert point.
  NewStatement:='Application.'+APropertyName+':='+NewCode+';';
  NewStatement:=Beauty.BeautifyStatement(NewStatement,Beauty.Indent);
  SourceChangeCache.MainScanner:=Scanner;
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,StartPos,EndPos,NewStatement) then
    exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.FindApplicationTitleStatement(out StartPos,
  StringConstStartPos, EndPos: integer): boolean;
begin
  Result := FindApplicationStatement('TITLE', StartPos, StringConstStartPos, EndPos);
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
  if FormatStringParams='' then ;
end;

function TStandardCodeTool.SetApplicationTitleStatement(const NewTitle: string;
  SourceChangeCache: TSourceChangeCache): boolean;
begin
  Result := SetApplicationStatement('Title', StringToPascalConst(NewTitle), SourceChangeCache);
end;

function TStandardCodeTool.RemoveApplicationTitleStatement(
  SourceChangeCache: TSourceChangeCache): boolean;
begin
  Result := RemoveApplicationStatement('TITLE', SourceChangeCache);
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
    Result:=ReplaceWords(IdentList,false,SourceChangeCache);
  finally
    IdentList.Free;
  end;
end;

function TStandardCodeTool.FindFormAncestor(const AClassName: string;
  var AncestorClassName: string): boolean;
var
  ClassNode: TCodeTreeNode;
begin
  Result:=false;
  AncestorClassName:='';
  if AClassName='' then exit;
  BuildTree(lsrImplementationStart);
  ClassNode:=FindClassNodeInInterface(AClassName,true,false,false);
  if (ClassNode=nil) then exit;
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  while UpAtomIs('SEALED') or UpAtomIs('ABSTRACT') do ReadNextAtom;    
  if UpAtomIs('PACKED') or UpAtomIs('BITPACKED') then ReadNextAtom;
  ReadNextAtom;
  if AtomIsChar('(') then begin
    ReadNextAtom;
    if AtomIsIdentifier then
      AncestorClassName:=GetAtom;
  end;
  if AncestorClassName='' then
    AncestorClassName:='TObject';
  Result:=true;
end;

{-------------------------------------------------------------------------------
  function TStandardCodeTool.ReplaceWords(IdentList: TStrings;
    ChangeStrings: boolean; SourceChangeCache: TSourceChangeCache): boolean;
    
  Search in all used sources (not only the cleaned source) for identifiers.
  It will find all identifiers, except identifiers in compiler directives.
  This includes identifiers in string constants and comments.
  
  ChangeStrings = true, means to replace in string constants too
-------------------------------------------------------------------------------}
function TStandardCodeTool.ReplaceWords(IdentList: TStrings;
  ChangeStrings: boolean; SourceChangeCache: TSourceChangeCache;
  SkipPointWords: boolean): boolean;

  function CheckIdentifier(const CurSource: string;
    IdentStart: integer): boolean;
  var
    p: integer;
  begin
    if not SkipPointWords then exit(true);
    p:=IdentStart-1;
    while (p>0) and (IsSpaceChar[CurSource[p]]) do dec(p);
    Result:=(p<1) or (CurSource[p]<>'.');
  end;

  procedure ReplaceWordsInSource(ACode: TCodeBuffer);
  var
    StartPos, EndPos, MaxPos, IdentStart, IdentEnd: integer;
    CurSource: string;
    i: integer;
  begin
    CurSource:=ACode.Source;
    MaxPos:=length(CurSource);
    StartPos:=1;
    // go through all source parts between compiler directives
    //DebugLn('TStandardCodeTool.ReplaceWords ',ACode.Filename);
    repeat
      EndPos:=FindNextCompilerDirective(CurSource,StartPos,
                                        Scanner.NestedComments);
      if EndPos>MaxPos then EndPos:=MaxPos+1;
      // search all identifiers
      repeat
        if ChangeStrings then
          IdentStart:=FindNextIdentifier(CurSource,StartPos,EndPos-1)
        else
          IdentStart:=FindNextIdentifierSkipStrings(CurSource,StartPos,EndPos-1);
        if IdentStart>=EndPos then
          break;
        i:=0;
        while i<IdentList.Count do begin
          if (IdentList[i]<>'')
          and (BasicCodeTools.CompareIdentifiers(PChar(Pointer(IdentList[i])),
                                                 @CurSource[IdentStart])=0)
          and CheckIdentifier(CurSource,IdentStart)
          and (IdentList[i]<>IdentList[i+1])
          then begin
            // identifier found -> replace
            IdentEnd:=IdentStart+length(IdentList[i]);
            //DebugLn('TStandardCodeTool.ReplaceWords replacing: ',
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
  SourceList: TFPList;
  i: integer;
begin
  Result:=false;
  if (IdentList=nil) or (IdentList.Count=0) or (SourceChangeCache=nil)
  or (Odd(IdentList.Count)) then exit;
  BuildTree(lsrEnd);
  if Scanner=nil then exit;
  SourceChangeCache.MainScanner:=Scanner;
  SourceList:=TFPList.Create;
  try
    Scanner.FindCodeInRange(1,SrcLen,SourceList);
    for i:=0 to SourceList.Count-1 do begin
      ReplaceWordsInSource(TCodeBuffer(SourceList[i]));
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
  BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,[]);
  BestDiff:=SrcLen+1;
  MoveCursorToCleanPos(1);
  repeat
    ReadNextAtom;
    if AtomIsIdentifier then begin
      CurIdentNode:=
        IdentTree.FindKey(@Src[CurPos.StartPos],
                          TListSortCompare(@CompareIdentifiers));
      if CurIdentNode<>nil then begin
        CurDiff:=CurPos.StartPos-CleanCursorPos;
        if CurDiff<0 then CurDiff:=-CurDiff;
        if (Result=nil) or (CurDiff<BestDiff) then begin
          BestDiff:=CurDiff;
          Result:=CurIdentNode;
        end;
      end;
    end;
  until CurPos.EndPos>SrcLen;
end;

function TStandardCodeTool.ReplaceWord(const OldWord, NewWord: string;
  ChangeStrings: boolean; SourceChangeCache: TSourceChangeCache;
  SkipPointWords: boolean): boolean;
var
  IdentList: TStringList;
begin
  Result:=false;
  if OldWord='' then exit;
  if OldWord=NewWord then exit(true);
  if (SourceChangeCache=nil) then exit;
  IdentList:=TStringList.Create;
  try
    IdentList.Add(OldWord);
    IdentList.Add(NewWord);
    Result:=ReplaceWords(IdentList,ChangeStrings,SourceChangeCache,SkipPointWords);
  finally
    IdentList.Free;
  end;
end;

function TStandardCodeTool.CommentCode(const StartPos, EndPos: integer;
  SourceChangeCache: TSourceChangeCache; Apply: boolean): boolean;
var
  i: LongInt;
  CurStartPos: LongInt;
  CommentNeeded: Boolean;
  CurEndPos: LongInt;
begin
  if StartPos>=EndPos then
    RaiseException(20170421201123,'TStandardCodeTool CommentCode');

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

function TStandardCodeTool.GetStringConstBounds(
  const CursorPos: TCodeXYPosition;
  out StartPos, EndPos: TCodeXYPosition; ResolveComments: boolean): boolean;
// examples:
//   's1'+'s2'#13+AFunction(...)+inherited AMethod
{ $DEFINE VerboseGetStringConstBounds}
type
  TStrConstTokenType = (scatNone, scatStrConst, scatPlus, scatIdent,
    scatInherited, scatPoint, scatUp,
    scatEdgedBracketOpen, scatEdgedBracketClose,
    scatRoundBracketOpen, scatRoundBracketClose);

  {$IFDEF VerboseGetStringConstBounds}
  function EnumToStr(TokenType: TStrConstTokenType): string;
  begin
    WriteStr(Result, TokenType);
  end;
  {$ENDIF}

  function GetCurrentTokenType: TStrConstTokenType;
  begin
    if (CurPos.StartPos<1) or (CurPos.StartPos>SrcLen) then
      Result:=scatNone
    else if AtomIsStringConstant then
      Result:=scatStrConst
    else if AtomIsChar('+') then
      Result:=scatPlus
    else if AtomIsIdentifier then
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
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
  {$IFDEF VerboseGetStringConstBounds}
  DebugLn('TStandardCodeTool.GetStringConstBounds A Start at ',CleanPosToStr(CleanCursorPos),' "',copy(Src,CleanCursorPos-5,5),'" | "',copy(Src,CleanCursorPos,5),'"');
  {$ENDIF}
  GetCleanPosInfo(-1,CleanCursorPos,ResolveComments,SameArea);
  {$IFDEF VerboseGetStringConstBounds}
  DebugLn('TStandardCodeTool.GetStringConstBounds B Same Area: ',CleanPosToStr(SameArea.StartPos),'-',CleanPosToStr(SameArea.EndPos),' "',copy(Src,SameArea.StartPos,SameArea.EndPos-SameArea.StartPos),'"');
  {$ENDIF}
  if (SameArea.EndPos=SameArea.StartPos) or (SameArea.StartPos>SrcLen) then
    exit;

  // read til end of string constant
  MoveCursorToCleanPos(SameArea.StartPos);
  ReadNextAtom;
  {$IFDEF VerboseGetStringConstBounds}
  DebugLn('TStandardCodeTool.GetStringConstBounds read til end of string Atom=',GetAtom);
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
    DebugLn('TStandardCodeTool.GetStringConstBounds Read Forward: ',GetAtom,' EndCleanPos=',dbgs(EndCleanPos),
      ' LastToken=',EnumToStr(LastToken),
      ' CurrentToken=',EnumToStr(CurrentToken),' ',EnumToStr(GetCurrentTokenType));
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
      if not (LastToken in [scatIdent, scatUp, scatRoundBracketClose,
                            scatEdgedBracketClose]) then
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
  DebugLn('TStandardCodeTool.GetStringConstBounds Read til start of string ',GetAtom);
  {$ENDIF}
  CurrentToken:=GetCurrentTokenType;
  repeat
    StartCleanPos:=CurPos.StartPos;
    ReadPriorAtom;
    {$IFDEF VerboseGetStringConstBounds}
    DebugLn('TStandardCodeTool.GetStringConstBounds Read backward: ',GetAtom,' StartCleanPos=',dbgs(StartCleanPos));
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
  DebugLn('TStandardCodeTool.GetStringConstBounds END "',copy(Src,StartCleanPos,EndCleanPos-StartCleanPos),'" StringConstantFound=',dbgs(StringConstantFound));
  {$ENDIF}
  if not StringConstantFound then begin
    EndCleanPos:=StartCleanPos;
  end;
  if not CleanPosToCaret(StartCleanPos,StartPos) then exit;
  if not CleanPosToCaret(EndCleanPos,EndPos) then exit;

  Result:=true;
end;

function TStandardCodeTool.GetStringConstAsFormatString(StartPos,
  EndPos: integer; out FormatStringConstant, FormatParameters: string;
  out StartInStringConst, EndInStringConst: boolean): boolean;
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
    InRange: Boolean;
  begin
    if (CurPos.StartPos<StartPos) and (CurPos.EndPos>StartPos) then
      StartInStringConst:=true;
    if (CurPos.StartPos<EndPos) and (CurPos.EndPos>EndPos) then
      EndInStringConst:=true;

    APos:=CurPos.StartPos;
    while APos<EndPos do begin
      InRange:=(APos>=StartPos);
      //debugln('ConvertStringConstant InRange=',dbgs(InRange),' Src[APos]=',Src[APos]);
      if Src[APos]='''' then begin
        // read string constant
        inc(APos);
        while APos<EndPos do begin
          InRange:=(APos>=StartPos);
          case Src[APos] of
          '''':
            if (APos<EndPos-1) and (Src[APos+1]='''') then begin
              // a double ' means a single '
              if InRange then begin
                AddChar('''');
                AddChar('''');
              end;
              inc(APos,2);
            end else begin
              // a single ' means end of string constant
              inc(APos);
              break;
            end;
          else
            begin
              // normal char
              if InRange then
                AddChar(Src[APos]);
              inc(APos);
            end;
          end;
        end;
      end else if Src[APos]='#' then begin
        CharConstStart:=APos;
        InRange:=(APos+1>=StartPos);
        repeat
          // read char constant
          inc(APos);
          if APos<EndPos then begin
            if Src[APos-1]='#' then begin
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
          end;
        until (APos>=EndPos) or (Src[APos]<>'#');
        if InRange then
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

var
  ANode: TCodeTreeNode;
  CodePosInFront: LongInt;
begin
  Result:=false;
  // read string constants and convert it
  FormatStringConstant:='';
  FormatParameters:='';
  StartInStringConst:=false;
  EndInStringConst:=false;
  ANode:=FindDeepestNodeAtPos(StartPos,True);
  CodePosInFront:=ANode.StartPos;
  MoveCursorToCleanPos(CodePosInFront);
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  repeat
    ReadNextAtom;
    //debugln('GetStringConstAsFormatString Atom=',GetAtom);
    if (CurPos.StartPos>=EndPos) then break;
    if CurPos.EndPos>StartPos then begin
      //debugln('GetStringConstAsFormatString Parsing...');
      if AtomIsStringConstant then begin
        // a string constant
        ConvertStringConstant;
      end else if AtomIsChar('+') then begin
        // simply ignore
      end else if (CurPos.Flag=cafRoundBracketOpen) or AtomIsIdentifier
      then begin
        // add as parameter
        ConvertOther;
      end else
        // string constant end
        break;
    end;
  until false;
  Result:=FormatStringConstant<>'';
end;

function TStandardCodeTool.GetStringConstAsFormatString(StartPos,
  EndPos: integer; out FormatStringConstant, FormatParameters: string
    ): boolean;
var
  StartInStringConst, EndInStringConstant: boolean;
begin
  Result:=GetStringConstAsFormatString(StartPos,EndPos,FormatStringConstant,
                       FormatParameters,StartInStringConst,EndInStringConstant);
  if StartInStringConst then ;
  if EndInStringConstant then ;
end;

function TStandardCodeTool.ExtractOperand(const CursorPos: TCodeXYPosition; out
  Operand: string; WithPostTokens, WithAsOperator,
  WithoutTrailingPoints: boolean): boolean;
var
  CleanPos: integer;
  StartPos: LongInt;
  EndPos: LongInt;
  Node: TCodeTreeNode;
begin
  Result:=false;
  Operand:='';
  if CursorPos.Code.LineColIsSpace(CursorPos.Y,CursorPos.X) then exit;
  BuildTreeAndGetCleanPos(CursorPos,CleanPos);
  Node:=FindDeepestNodeAtPos(CleanPos,true);
  StartPos:=FindStartOfTerm(CleanPos,NodeTermInType(Node));
  if StartPos<1 then exit;
  StartPos:=FindNextNonSpace(Src,StartPos);
  if StartPos>CleanPos then exit;
  EndPos:=FindEndOfTerm(CleanPos,false,WithAsOperator);
  if not WithPostTokens then begin
    MoveCursorToCleanPos(CleanPos);
    ReadNextAtom;
    if CurPos.EndPos<EndPos then
      EndPos:=CurPos.EndPos;
  end;
  if EndPos<1 then exit;
  //DebugLn(['TStandardCodeTool.ExtractOperand "',dbgstr(copy(Src,StartPos,EndPos-StartPos)),'"']);
  Operand:=ExtractCode(StartPos,EndPos,[phpCommentsToSpace]);
  if WithoutTrailingPoints then begin
    while (Operand<>'') and (Operand[length(Operand)]='.') do
      Operand:=copy(Operand,1,length(Operand)-1);
  end;
  Result:=true;
end;

function TStandardCodeTool.FindApplicationScaledStatement(out StartPos,
  BooleanConstStartPos, EndPos: integer): boolean;
begin
  Result := FindApplicationStatement('SCALED', StartPos, BooleanConstStartPos, EndPos);
end;

function TStandardCodeTool.FindApplicationStatement(const APropertyUpCase: string;
  out StartPos, ConstStartPos, EndPos: integer): boolean;
// Find statement "Application.APropertyUpCase:=XYZ;" and return True if found.
//  Also return its positions (Start, const "XYZ" and End) in out parameters.
// If not found, out parameters get a good position to insert such a statement.
var
  MainBeginNode: TCodeTreeNode;
  AppPos, FirstAppPos: Integer;
begin
  Result:=false;
  StartPos:=-1;
  ConstStartPos:=-1;
  EndPos:=-1;
  FirstAppPos:=-1;
  BuildTree(lsrEnd);
  MainBeginNode:=FindMainBeginEndNode;
  if (MainBeginNode=nil) or (MainBeginNode.StartPos<1) then exit;
  MoveCursorToCleanPos(MainBeginNode.StartPos);
  repeat
    ReadNextAtom;
    if UpAtomIs('APPLICATION') then
    begin
      AppPos:=CurPos.StartPos;
      if FirstAppPos=-1 then
        FirstAppPos:=AppPos;
      ReadNextAtom;
      if AtomIsChar('.') then
      begin                    // Application.APropertyUpCase:=XYZ;
        if ReadNextUpAtomIs(APropertyUpCase) and ReadNextUpAtomIs(':=') then
        begin
          StartPos:=AppPos;
          repeat               // read till semicolon or end
            ReadNextAtom;
            if ConstStartPos<1 then
              ConstStartPos:=CurPos.StartPos;
            EndPos:=CurPos.EndPos;
            if CurPos.Flag in [cafEnd,cafSemicolon] then
              exit(true);
          until CurPos.StartPos>SrcLen;
        end;
      end
      else                     // Application:=TMyApplication.Create(nil);
      if UpAtomIs(':=') and ReadNextUpAtomIs('TMYAPPLICATION')
      and ReadNextAtomIsChar('.') and ReadNextUpAtomIs('CREATE') then
        repeat                 // read till semicolon or end
          ReadNextAtom;
          StartPos:=CurPos.EndPos; // Insert point behind the TMyApplication.Create line.
          if CurPos.Flag in [cafEnd,cafSemicolon] then
            break;
        until CurPos.StartPos>SrcLen;
    end;  // UpAtomIs('APPLICATION')
  until (CurPos.StartPos>SrcLen);
  // The statement was not found. Return a good place for insertion.
  if StartPos=-1 then
    if FirstAppPos <> -1 then
      StartPos:=FirstAppPos // Before first Application statement if there is one
    else begin
      MoveCursorToNodeStart(MainBeginNode);
      ReadNextAtom;
      StartPos:=CurPos.EndPos; // or after the main Begin.
    end;
  EndPos:=StartPos;     // Both StartPos and EndPos return the same insert point.
end;

function TStandardCodeTool.GatherResourceStringSections(
  const CursorPos: TCodeXYPosition; PositionList: TCodeXYPositions): boolean;
  
  function SearchInUsesSection(UsesNode: TCodeTreeNode): boolean;
  var
    NewCodeTool: TPascalReaderTool;
    IntfNode: TCodeTreeNode;
    NewCaret: TCodeXYPosition;
    Node: TCodeTreeNode;
    AnUnitName, InFilename: string;
  begin
    Result:=false;
    if UsesNode=nil then exit(true);
    Node:=UsesNode.LastChild;
    while Node<>nil do begin
      AnUnitName:=ExtractUsedUnitName(Node,@InFilename);
      // open the unit
      NewCodeTool:=FindCodeToolForUsedUnit(AnUnitName,InFilename,true);
      NewCodeTool.BuildTree(lsrImplementationStart);
      // search all resource string sections in the interface
      IntfNode:=NewCodeTool.FindInterfaceNode;
      if (IntfNode<>nil) and (IntfNode.LastChild<>nil) then begin
        IntfNode:=IntfNode.LastChild;
        while IntfNode<>nil do begin
          if IntfNode.Desc=ctnResStrSection then begin
            if not NewCodeTool.CleanPosToCaret(IntfNode.StartPos,NewCaret) then
              break;
            //DebugLn('TStandardCodeTool.GatherResourceStringSections Found Other ',NewCodeTool.MainFilename,' Y=',NewCaret.Y);
            PositionList.Add(NewCaret);
          end;
          IntfNode:=IntfNode.PriorBrother;
        end;
      end;
      Node:=Node.PriorBrother;
    end;
    Result:=true;
  end;
  
var
  CleanCursorPos: integer;
  CursorNode: TCodeTreeNode;
  NewCaret: TCodeXYPosition;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  //DebugLn('TStandardCodeTool.GatherResourceStringSections A ');
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  PositionList.Clear;
  ANode:=CursorNode;
  while ANode<>nil do begin
    case ANode.Desc of
    
    ctnResStrSection:
      begin
        if not CleanPosToCaret(ANode.StartPos,NewCaret) then exit;
        //DebugLn('TStandardCodeTool.GatherResourceStringSections Found Same Y=',NewCaret.Y);
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
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
  // find resource string section
  ANode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if (ANode=nil) then exit;
  ANode:=ANode.GetNodeOfType(ctnResStrSection);
  if ANode=nil then exit;
  // search identifier in section
  ANode:=ANode.FirstChild;
  while ANode<>nil do begin
    if (ANode.Desc=ctnConstDefinition)
    and CompareSrcIdentifiers(ANode.StartPos,PChar(ResStrIdentifier)) then begin
      Result:=true;
      exit;
    end;
    ANode:=ANode.NextBrother;
  end;
end;

function TStandardCodeTool.CreateIdentifierFromStringConst(const StartCursorPos,
  EndCursorPos: TCodeXYPosition; out Identifier: string;
  MaxLen: integer): boolean;
var
  StartPos, EndPos: integer;
  Dummy: Integer;
  IdentStr: String;
  ANode: TCodeTreeNode;
  CodePosInFront: LongInt;
begin
  Result:=false;
  if MaxLen<=0 then exit;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(StartCursorPos,StartPos);
  Dummy:=CaretToCleanPos(EndCursorPos, EndPos);
  if (Dummy<>0) and (Dummy<>-1) then exit;
  ANode:=FindDeepestNodeAtPos(StartPos,True);
  CodePosInFront:=ANode.StartPos;
  // read string constants and extract identifier characters
  Identifier:='';
  MoveCursorToCleanPos(CodePosInFront);
  repeat
    ReadNextAtom;
    //debugln('TStandardCodeTool.CreateIdentifierFromStringConst Atom=',GetAtom);
    if (CurPos.StartPos>=EndPos) then break;
    if AtomIsStringConstant then begin
      IdentStr:=ExtractIdentCharsFromStringConstant(CurPos.StartPos,
                                     StartPos,EndPos,MaxLen-length(Identifier));
      //debugln('TStandardCodeTool.CreateIdentifierFromStringConst IdentStr=',IdentStr);
      if (IdentStr<>'') then begin
        IdentStr[1]:=UpChars[IdentStr[1]];
        Identifier:=Identifier+IdentStr;
      end;
    end;
  until length(Identifier)>=MaxLen;
  Result:=Identifier<>'';
end;

function TStandardCodeTool.StringConstToFormatString(const StartCursorPos,
  EndCursorPos: TCodeXYPosition;
  out FormatStringConstant, FormatParameters: string;
  out StartInStringConst, EndInStringConst: boolean): boolean;
var
  StartPos,EndPos,Dummy: Integer;
begin
  Result:=false;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(StartCursorPos,StartPos);
  Dummy:=CaretToCleanPos(EndCursorPos, EndPos);
  if (Dummy<>0) and (Dummy<>-1) then exit;
  Result:=GetStringConstAsFormatString(StartPos,EndPos,FormatStringConstant,
                       FormatParameters,StartInStringConst,EndInStringConst);
end;

function TStandardCodeTool.HasInterfaceRegisterProc(out HasRegisterProc: boolean
  ): boolean;

  function IsRegisterProc(ANode: TCodeTreeNode): boolean;
  begin
    Result:=false;
    if ANode=nil then exit;
    if ANode.Desc=ctnProcedureHead then
      ANode:=Anode.Parent;
    if (ANode.Desc<>ctnProcedure) then exit;
    MoveCursorToNodeStart(ANode);
    if not ReadNextUpAtomIs('PROCEDURE') then exit;
    if not ReadNextUpAtomIs('REGISTER') then exit;
    if CurPos.Flag<>cafSemicolon then exit;
    HasRegisterProc:=true;
    Result:=true;
  end;

var
  InterfaceNode: TCodeTreeNode;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  HasRegisterProc:=false;
  ANode:=FindDeclarationNodeInInterface('Register',true);
  if ANode=nil then exit;
  if IsRegisterProc(ANode) then
    exit(true);
  // there may be multiple register
  InterfaceNode:=FindInterfaceNode;
  ANode:=InterfaceNode.FirstChild;
  while ANode<>nil do begin
    if IsRegisterProc(ANode) then
      exit(true);
    ANode:=ANode.NextBrother;
  end;
end;

function TStandardCodeTool.ConvertDelphiToLazarusSource(AddLRSCode: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  Beauty: TBeautifyCodeOptions;

  function AddModeDelphiDirective: boolean;
  var
    ModeDirectivePos: integer;
    InsertPos: Integer;
  begin
    Result:=false;
    BuildTree(lsrInterfaceStart);
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
      if not SourceChangeCache.Apply then exit;
    end;
    // changing mode requires rescan
    BuildTree(lsrImplementationStart);
    Result:=true;
  end;

  function ConvertUsedUnits: boolean;
  // replace unit 'Windows' with 'LCLIntf' and add 'LResources'
  // rename 'in' filenames to case sensitive filename
  var
    NamePos, InPos: TAtomPosition;
  begin
    Result:=false;
    if FindUnitInAllUsesSections('WINDOWS',NamePos,InPos)
    and (InPos.StartPos<1) then begin
      if not SourceChangeCache.Replace(gtNone,gtNone,
                           NamePos.StartPos,NamePos.EndPos,'LCLIntf') then
      begin
        debugln('ConvertUsedUnits Unable to replace Windows with LCLIntf unit');
        exit;
      end;
    end;
    if AddLRSCode then
      if not AddUnitToMainUsesSection('LResources','',SourceChangeCache) then
      begin
        debugln('ConvertUsedUnits Unable to add LResources to main uses section');
        exit;
      end;
    if not RemoveUnitFromAllUsesSections('VARIANTS',SourceChangeCache) then
    begin
      debugln('ConvertUsedUnits Unable to remove Variants from all uses sections');
      exit;
    end;
    if not FixUsedUnitCase(SourceChangeCache) then
    begin
      debugln('ConvertUsedUnits Unable to fix unit filename case sensitivity in all uses sections');
      exit;
    end;
    Result:=true;
  end;

  function RemoveDFMResourceDirective: boolean;
  // remove {$R *.dfm} or {$R *.xfm} directive
  var
    ParamPos: Integer;
    ACleanPos: Integer;
    StartPos: Integer;
    s: String;
  begin
    Result:=false;
    // find $R directive
    ACleanPos:=1;
    repeat
      ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
        Scanner.NestedComments,ParamPos);
      if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen) then break;
      s:=UpperCaseStr(copy(Src,ParamPos,6));
      if (Src[ACleanPos]='{')
      and ((s='*.DFM}') or (s='*.XFM}'))
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
        NewCode:=Beauty.GetIndentStr(Beauty.Indent)
                 +'{$i '+LRSFilename+'}';
        if InitializationNode=nil then begin
          // add also an initialization section
          ImplementationNode:=FindImplementationNode;
          InsertPos:=ImplementationNode.EndPos;
          NewCode:=Beauty.BeautifyKeyWord(
                     'initialization')
                   +Beauty.LineEnd
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
  SourceChangeCache.MainScanner:=Scanner;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  DebugLn('ConvertDelphiToLazarusSource AddModeDelphiDirective');
  if not AddModeDelphiDirective then exit;
  DebugLn('ConvertDelphiToLazarusSource RemoveDFMResourceDirective');
  if not RemoveDFMResourceDirective then exit;
  DebugLn('ConvertDelphiToLazarusSource AddLRSIncludeDirective');
  if not AddLRSIncludeDirective then exit;
  DebugLn('ConvertDelphiToLazarusSource ConvertUsedUnits');
  if not ConvertUsedUnits then exit;
  DebugLn('ConvertDelphiToLazarusSource Apply');
  if not SourceChangeCache.Apply then exit;
  DebugLn('ConvertDelphiToLazarusSource END');
  Result:=true;
end;

function TStandardCodeTool.GetIDEDirectives(DirectiveList: TStrings;
  const Filter: TOnIDEDirectiveFilter): boolean;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  Result:=false;
  DirectiveList.Clear;
  BuildTree(lsrImplementationStart);
  EndPos:=1;
  repeat
    StartPos:=FindNextIDEDirective(Src,EndPos,Scanner.NestedComments);
    if StartPos<1 then break;
    EndPos:=FindCommentEnd(Src,StartPos,Scanner.NestedComments);
    if (Filter=nil) or Filter(Self,StartPos,EndPos) then
      DirectiveList.Add(copy(Src,StartPos,EndPos-StartPos));
    if EndPos>SrcLen then break;
    StartPos:=EndPos;
  until false;
  Result:=true;
end;

function TStandardCodeTool.SetIDEDirectives(DirectiveList: TStrings;
  SourceChangeCache: TSourceChangeCache; const Filter: TOnIDEDirectiveFilter
  ): boolean;
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
  BuildTree(lsrEnd);

  // find first old IDE directive
  InsertPos:=FindNextIDEDirective(Src,1,Scanner.NestedComments);
  if InsertPos>=1 then begin
    EndPos:=FindCommentEnd(Src,InsertPos,Scanner.NestedComments);
    if (Filter<>nil) and (not Filter(Self,InsertPos,EndPos)) then
      InsertPos:=0;
  end else
    InsertPos:=0;

  // remove all old IDE directives
  if InsertPos>=1 then
    EndPos:=InsertPos
  else
    EndPos:=1;
  repeat
    // find next IDE directive
    StartPos:=FindNextIDEDirective(Src,EndPos,Scanner.NestedComments);
    if StartPos<1 then break;
    EndPos:=FindCommentEnd(Src,StartPos,Scanner.NestedComments);
    if (Filter=nil) or Filter(Self,StartPos,EndPos) then begin
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
      if not SourceChangeCache.Replace(gtNone,gtNone,StartPos,EndPos,'') then
        exit;
    end;
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

procedure TStandardCodeTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
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
    if not AtomIsIdentifier then exit;
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
    //DebugLn('TStandardCodeTool.GatherResourceStringsWithValue Found ',MainFilename,' Y=',NewCaret.Y);
    PositionList.Add(NewCaret);
  end;
  
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  if PositionList=nil then exit;
  // parse source and find clean positions
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
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

function TStandardCodeTool.GetApplicationScaledStatement(BooleanConstStartPos,
  EndPos: integer; var AScaled: boolean): boolean;
begin
  Result:=false;
  AScaled:=false;
  if (BooleanConstStartPos<1) or (BooleanConstStartPos>SrcLen) then exit;
  MoveCursorToCleanPos(BooleanConstStartPos);
  ReadNextAtom;
  if (EndPos>0) and (CurPos.EndPos>EndPos) then exit;
  if UpAtomIs('TRUE') then
  begin
    AScaled := True;
    Result := True;
  end;
  if UpAtomIs('FALSE') then
  begin
    AScaled := False;
    Result := True;
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
  BuildTreeAndGetCleanPos(SectionPos,CleanCursorPos);
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
        IdentTree:=TAVLTree.Create(TListSortCompare(@BasicCodeTools.CompareIdentifiers));
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
  CursorTool:=
            TStandardCodeTool(OnGetCodeToolForBuffer(Self,CursorPos.Code,true));
  SectionTool:=
           TStandardCodeTool(OnGetCodeToolForBuffer(Self,SectionPos.Code,true));
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
    NearestCleanPos:={%H-}PtrUInt(NearestNode.Data)-{%H-}PtrUInt(@SectionTool.Src[1])+1;
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
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  //DebugLn('TStandardCodeTool.AddResourcestring A ',NewIdentifier,'=',NewValue,' ');
  if (NewIdentifier='') or (length(NewIdentifier)>255) then exit;
  if SourceChangeCache=nil then exit;
  SourceChangeCache.MainScanner:=Scanner;
  // parse source and find clean positions
  //DebugLn('TStandardCodeTool.AddResourcestring B');
  BuildTreeAndGetCleanPos(SectionPos,CleanSectionPos);
  //DebugLn('TStandardCodeTool.AddResourcestring C');
  // find resource string section
  SectionNode:=FindDeepestNodeAtPos(CleanSectionPos,true);
  if (SectionNode=nil) then exit;
  SectionNode:=SectionNode.GetNodeOfType(ctnResStrSection);
  if SectionNode=nil then exit;

  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  //DebugLn('TStandardCodeTool.AddResourcestring D SectionChilds=',SectionNode.FirstChild<>nil);
  // find insert position
  if SectionNode.FirstChild=nil then begin
    // no resourcestring in this section yet -> append as first child
    Indent:=Beauty.GetLineIndent(Src,SectionNode.StartPos)+Beauty.Indent;
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
          and (CompareIdentifiers(@Src[ANode.StartPos],
            PChar(Pointer(NewIdentifier)))<0)
          then
            break;
          ANode:=ANode.NextBrother;
        end;
        if ANode=nil then begin
          // append new identifier as last
          Indent:=Beauty.GetLineIndent(Src,SectionNode.LastChild.StartPos);
          InsertPos:=FindLineEndOrCodeAfterPosition(SectionNode.LastChild.EndPos);
        end else begin
          // insert in front of node
          Indent:=Beauty.GetLineIndent(Src,ANode.StartPos);
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
          Indent:=Beauty.GetLineIndent(Src,SectionNode.LastChild.StartPos);
          InsertPos:=FindLineEndOrCodeAfterPosition(SectionNode.LastChild.EndPos);
        end else begin
          // insert behind node
          Indent:=Beauty.GetLineIndent(Src,ANode.StartPos);
          InsertPos:=FindLineEndOrCodeAfterPosition(ANode.EndPos);
        end;
      end;
      
    else
      begin
        // append new identifier
        Indent:=Beauty.GetLineIndent(Src,SectionNode.LastChild.StartPos);
        InsertPos:=FindLineEndOrCodeAfterPosition(SectionNode.LastChild.EndPos);
      end;
    end;
  end;

  //DebugLn('TStandardCodeTool.AddResourcestring E Indent=',Indent,' InsertPos=',InsertPos,' ',copy(Src,InsertPos-9,8),'|',copy(Src,InsertPos,8));
  // insert
  InsertSrc:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                     NewIdentifier+' = '+NewValue+';',Indent);
  //DebugLn('TStandardCodeTool.AddResourcestring F "',InsertSrc,'"');
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,InsertSrc);
  SourceChangeCache.Apply;
  Result:=true;
  //DebugLn('TStandardCodeTool.AddResourcestring END ',Result);
end;

function TStandardCodeTool.FindPublishedVariable(const AClassName,
  AVarName: string; ExceptionOnClassNotFound: boolean): TCodeTreeNode;
var ClassNode, SectionNode: TCodeTreeNode;
begin
  Result:=nil;
  if (AClassName='') or (length(AClassName)>255) then
    RaiseExceptionFmt(20170421201129,ctsinvalidClassName, [AClassName]);
  if AVarName='' then exit;
  BuildTree(lsrImplementationStart);
  ClassNode:=FindClassNodeInInterface(AClassName,true,false,false);
  if ClassNode=nil then begin
    if ExceptionOnClassNotFound then
      RaiseExceptionFmt(20170421201136,ctsclassNotFound, [AClassName])
    else
      exit;
  end;
  SectionNode:=ClassNode.FirstChild;
  while (SectionNode<>nil) do begin
    if SectionNode.Desc=ctnClassPublished then begin
      Result:=SectionNode.FirstChild;
      while Result<>nil do begin
        if (Result.Desc=ctnVarDefinition) then begin
          MoveCursorToNodeStart(Result);
          if ReadNextAtomIsIdentifier(PChar(AVarName)) then
            exit;
        end;
        Result:=Result.NextBrother;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TStandardCodeTool.AddPublishedVariable(const AClassName,
  VarName, VarType: string; SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode, SectionNode: TCodeTreeNode;
  Indent, InsertPos: integer;
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  if (AClassName='') or (length(AClassName)>255) then
    RaiseExceptionFmt(20170421201143,ctsinvalidClassName2, [AClassName]);
  if (VarName='') or (length(VarName)>255) then
    RaiseExceptionFmt(20170421201152,ctsinvalidVariableName, [VarName]);
  if (VarType='') or (length(VarType)>255) then
    RaiseExceptionFmt(20170421201158,ctsinvalidVariableType, [VarType]);
  if (SourceChangeCache=nil) then
    RaiseException(20170421201203,'missing SourceChangeCache');
  if FindPublishedVariable(AClassName,VarName,true)<>nil then
  begin
    Result:=true;
    exit;
  end;
  ClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
  if ClassNode=nil then
    RaiseExceptionFmt(20170421201208,ctsclassNotFound, [AClassName]);
  SectionNode:=ClassNode.FirstChild;
  if (SectionNode.NextBrother<>nil)
  and (SectionNode.NextBrother.Desc=ctnClassPublished) then
    SectionNode:=SectionNode.NextBrother;
  SourceChangeCache.MainScanner:=Scanner;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  if SectionNode.FirstChild<>nil then begin
    Indent:=Beauty.GetLineIndent(Src,SectionNode.FirstChild.StartPos);
  end else begin
    Indent:=Beauty.GetLineIndent(Src,SectionNode.StartPos)+Beauty.Indent;
  end;
  InsertPos:=FindLineEndOrCodeInFrontOfPosition(SectionNode.EndPos);
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
          Beauty.BeautifyStatement(VarName+':'+VarType+';',Indent)
       );
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.RemovePublishedVariable(const AClassName,
  AVarName: string; ExceptionOnClassNotFound: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
var VarNode: TCodeTreeNode;
  FromPos, ToPos: integer;
begin
  Result:=false;
  VarNode:=FindPublishedVariable(AClassName,AVarName,
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
      ToPos:=FindLineEndOrCodeAfterPosition(VarNode.EndPos);
      //debugln(['TStandardCodeTool.RemovePublishedVariable ',dbgstr(copy(Src,FromPos,ToPos-FromPos))]);
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

function TStandardCodeTool.RenamePublishedVariable(const AClassName,
  AOldVarName: string; const NewVarName, VarType: shortstring;
  ExceptionOnClassNotFound: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  TypeNode, VarNode: TCodeTreeNode;
  ApplyNeeded: Boolean;
begin
  Result:=false;
  if (NewVarName='') or (VarType='') then exit;
  BuildTree(lsrEnd);
  VarNode:=FindPublishedVariable(AClassName,AOldVarName,
                                 ExceptionOnClassNotFound);
  if VarNode<>nil then begin
    // old variable found
    // check type
    TypeNode:=FindTypeNodeOfDefinition(VarNode);
    MoveCursorToNodeStart(TypeNode);
    ReadNextAtom;
    SourceChangeCache.MainScanner:=Scanner;
    ApplyNeeded:=false;
    if (not AtomIsIdentifier(@VarType[1])) then begin
      // change the type
      ApplyNeeded:=true;
      if not SourceChangeCache.Replace(gtNone,gtNone,
        CurPos.StartPos,CurPos.EndPos,VarType)
      then begin
        RaiseException(20170421201215,'Unable to replace type');
      end;
    end;
    // rename variable in source
    if not ReplaceWord(AOldVarName,NewVarName,false,SourceChangeCache,true)
    then
      exit;
    Result:=(not ApplyNeeded) or SourceChangeCache.Apply;
  end else begin
    // old variable not found -> add it
    Result:=AddPublishedVariable(AClassName,NewVarName,VarType,
                                 SourceChangeCache);
  end;
end;

function TStandardCodeTool.GatherPublishedClassElements(
  const TheClassName: string;
  ExceptionOnClassNotFound, WithVariables, WithMethods, WithProperties,
  WithAncestors: boolean;
  out TreeOfCodeTreeNodeExtension: TAVLTree): boolean;
  
  function Add(AFindContext: PFindContext): boolean;
  var
    ClassNode: TCodeTreeNode;
    CurTool: TFindDeclarationTool;
    SectionNode: TCodeTreeNode;
    ANode: TCodeTreeNode;
    CurProcName: String;
    NewNodeExt: TCodeTreeNodeExtension;
    CurPropName: String;
    CurVarName: String;
  begin
    Result:=false;
    ClassNode:=AFindContext^.Node;
    if (ClassNode=nil)
    or (not (ClassNode.Desc in AllClasses)) then exit;
    CurTool:=AFindContext^.Tool;
    SectionNode:=ClassNode.FirstChild;
    while (SectionNode<>nil) do begin
      if SectionNode.Desc=ctnClassPublished then begin
        ANode:=SectionNode.FirstChild;
        while ANode<>nil do begin
          if (ANode.Desc=ctnProcedure) and WithMethods then begin
            CurProcName:=CurTool.ExtractProcName(ANode,[]);
            {$IFDEF VerboseDanglingComponentEvents}
            debugln('TStandardCodeTool.GatherPublishedClassElements CurProcName="',CurProcName,'"');
            {$ENDIF}
            NewNodeExt:=TCodeTreeNodeExtension.Create;
            with NewNodeExt do begin
              Node:=ANode;
              Txt:=CurProcName;
            end;
            TreeOfCodeTreeNodeExtension.Add(NewNodeExt);
          end
          else if (ANode.Desc=ctnVarDefinition) and WithVariables then begin
            CurVarName:=CurTool.ExtractDefinitionName(ANode);
            NewNodeExt:=TCodeTreeNodeExtension.Create;
            with NewNodeExt do begin
              Node:=ANode;
              Txt:=CurVarName;
            end;
            TreeOfCodeTreeNodeExtension.Add(NewNodeExt);
          end
          else if (ANode.Desc=ctnProperty) and WithProperties then begin
            CurPropName:=CurTool.ExtractPropName(ANode,false);
            NewNodeExt:=TCodeTreeNodeExtension.Create;
            with NewNodeExt do begin
              Node:=ANode;
              Txt:=CurPropName;
            end;
            TreeOfCodeTreeNodeExtension.Add(NewNodeExt);
          end;
          ANode:=ANode.NextBrother;
        end;
      end;
      SectionNode:=SectionNode.NextBrother;
    end;
    Result:=true;
  end;
  
var
  ClassNode: TCodeTreeNode;
  AncestorList: TFPList;// of PFindContext
  i: Integer;
begin
  Result:=false;
  TreeOfCodeTreeNodeExtension:=nil;
  if (TheClassName='') or (length(TheClassName)>255) then
    RaiseExceptionFmt(20170421201221,ctsInvalidClassName, [TheClassName]);
  {$IFDEF VerboseDanglingComponentEvents}
  DebugLn(['TStandardCodeTool.GatherPublishedClassElements BEFORE buildtree']);
  {$ENDIF}
  BuildTree(lsrImplementationStart);
  {$IFDEF VerboseDanglingComponentEvents}
  DebugLn(['TStandardCodeTool.GatherPublishedClassElements after buildtree']);
  {$ENDIF}
  ClassNode:=FindClassNodeInInterface(TheClassName,true,false,
    ExceptionOnClassNotFound);
  if ClassNode=nil then exit;
  AncestorList:=nil;
  try
    if WithAncestors then begin
      if not FindClassAndAncestors(ClassNode,AncestorList,true) then exit;
    end else begin
      AddFindContext(AncestorList,CreateFindContext(Self,ClassNode));
    end;
    TreeOfCodeTreeNodeExtension:=TAVLTree.Create(@CompareCodeTreeNodeExt);
    for i:=0 to AncestorList.Count-1 do begin
      if not Add(PFindContext(AncestorList[i])) then exit;
    end;
    //debugln(['TStandardCodeTool.GatherPublishedClassElements END']);
  finally
    FreeListOfPFindContext(AncestorList);
  end;
  Result:=true;
end;

function TStandardCodeTool.RetypeClassVariables(const AClassName: string;
  ListOfTypes: TStringToStringTree; ExceptionOnClassNotFound: boolean;
  SourceChangeCache: TSourceChangeCache; SearchImplementationToo: boolean): boolean;
var
  ClassNode: TCodeTreeNode;
  Node: TCodeTreeNode;
  TypeNode: TCodeTreeNode;
  OldType: String;
  NewType: string;
  HasChanged: Boolean;
begin
  Result:=false;
  if SearchImplementationToo then begin
    BuildTree(lsrEnd);
    ClassNode:=FindClassNodeInUnit(AClassName,true,false,false,
                                   ExceptionOnClassNotFound)
  end
  else begin
    BuildTree(lsrImplementationStart);
    ClassNode:=FindClassNodeInInterface(AClassName,true,false,
                                        ExceptionOnClassNotFound);
  end;
  if ClassNode=nil then exit;
  if (ListOfTypes=nil) or (ListOfTypes.Tree.Count=0) then exit(true);

  HasChanged:=false;
  Node:=ClassNode.FirstChild;
  while (Node<>nil) and (Node.HasAsParent(ClassNode)) do begin
    if (Node.Desc=ctnVarDefinition) and (Node.FirstChild<>nil) then begin
      TypeNode:=Node.FirstChild;
      if TypeNode.Desc=ctnIdentifier then begin
        MoveCursorToNodeStart(TypeNode);
        ReadNextAtom;
        ReadNextAtom;
        if CurPos.Flag=cafPoint then begin
          // skip unitname
          ReadNextAtom;
        end else begin
          UndoReadNextAtom;
        end;
        // cursor is now on identifier
        OldType:=GetAtom;
        if ListOfTypes.Contains(OldType) then begin
          NewType:=ListOfTypes[OldType];
          if OldType<>NewType then begin
            // change type (or case)
            if not HasChanged then begin
              HasChanged:=true;
              SourceChangeCache.MainScanner:=Scanner;
            end;
            if not SourceChangeCache.Replace(gtNone,gtNone,
              CurPos.StartPos,CurPos.EndPos,NewType)
            then
              exit(false);
          end;
        end;
      end;
      Node:=Node.NextSkipChilds;
    end else
      Node:=Node.Next;
  end;
  if HasChanged then begin
    if not SourceChangeCache.Apply then exit;
  end;
  Result:=true;
end;

function TStandardCodeTool.FindDanglingComponentEvents(
  const TheClassName: string; RootComponent: TComponent;
  ExceptionOnClassNotFound, SearchInAncestors: boolean; out
  ListOfPInstancePropInfo: TFPList;
  const OverrideGetMethodName: TOnGetMethodname): boolean;
var
  PublishedMethods: TAVLTree;
  
  procedure AddDanglingEvent(Instance: TPersistent; PropInfo: PPropInfo);
  var
    NewItem: PInstancePropInfo;
  begin
    New(NewItem);
    NewItem^.Instance:=Instance;
    NewItem^.PropInfo:=PropInfo;
    if ListOfPInstancePropInfo=nil then ListOfPInstancePropInfo:=TFPList.Create;
    ListOfPInstancePropInfo.Add(NewItem);
    {$IFDEF VerboseDanglingComponentEvents}
    debugln('AddDanglingEvent ',DbgSName(Instance),' ',PropInfo^.Name);
    {$ENDIF}
  end;

  procedure CheckMethodsInPersistent(APersistent: TPersistent);
  var
    TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    PropInfo: PPropInfo;
    PropList: PPropList;
    CurCount,i: integer;
    PropType: PTypeInfo;
    NodeExt: TCodeTreeNodeExtension;
    CurMethod: TMethod;
    CurMethodName: String;
    ObjValue: TObject;
  begin
    if APersistent=nil then exit;
    {$IFDEF VerboseDanglingComponentEvents}
    debugln('TStandardCodeTool.FindDanglingComponentEvents.CheckMethodsInPersistent Checking ',DbgSName(APersistent));
    {$ENDIF}
    // read all properties and remove doubles
    TypeInfo:=APersistent.ClassInfo;
    repeat
      // read all property infos of current class
      TypeData:=GetTypeData(TypeInfo);
      // read property count
      CurCount:=GetPropList(TypeInfo,PropList);
      try
        {$IFDEF VerboseDanglingComponentEvents}
        debugln('    UnitName=',TypeData^.UnitName,' Type=',TypeInfo^.Name,' CurPropCount=',dbgs(CurCount));
        {$ENDIF}
        // read properties
        for i:=0 to CurCount-1 do begin
          PropInfo:=PropList^[i];
          {$IFDEF VerboseDanglingComponentEvents}
          debugln('      Property ',PropInfo^.Name,' Type=',PropInfo^.PropType^.Name);
          {$ENDIF}
          PropType:=PropInfo^.PropType;

          if (PropType^.Kind=tkMethod) then begin
            // RTTI property is method
            // -> search method in source
            CurMethod:=GetMethodProp(APersistent,PropInfo);
            if (CurMethod.Data<>nil) or (CurMethod.Code<>nil) then begin
              if Assigned(OverrideGetMethodName) then
                CurMethodName:=OverrideGetMethodName(CurMethod,RootComponent)
              else
                CurMethodName:=OnGetMethodName(CurMethod,RootComponent);
              {$IFDEF VerboseDanglingComponentEvents}
              debugln('      Persistent ',DbgSName(APersistent),' Property ',PropInfo^.Name,' Type=',PropInfo^.PropType^.Name,' CurMethodName="',CurMethodName,'"');
              {$ENDIF}
              if CurMethodName<>'' then begin
                NodeExt:=FindCodeTreeNodeExt(PublishedMethods,CurMethodName);
                if NodeExt=nil then begin
                  // method not found -> dangling event
                  AddDanglingEvent(APersistent,PropInfo);
                end;
              end;
            end;
          end else if (PropType^.Kind=tkClass) then begin
            // RTTI property is class instance
            ObjValue := TObject(GetObjectProp(APersistent, PropInfo));
            if ObjValue is TCollection then begin
              // collection

            end else if (ObjValue is TPersistent)
            and (not (ObjValue is TComponent)
                 or (csSubComponent in TComponent(ObjValue).ComponentStyle))
            then begin
              // sub persistent (e.g. Canvas.Font)
              //debugln(['CheckMethodsInPersistent sub persistent: ',DbgSName(ObjValue)]);
              CheckMethodsInPersistent(TPersistent(ObjValue));
            end;
          end;
       end;
      finally
        FreeMem(PropList);
      end;
      TypeInfo:=TypeData^.ParentInfo;
    until TypeInfo=nil;
  end;

var
  i: Integer;
  Collector: TComponentChildCollector;
  AllComponents: TFPList;
begin
  PublishedMethods:=nil;
  ListOfPInstancePropInfo:=nil;
  Collector:=nil;
  AllComponents:=nil;
  try
    // search all available published methods
    {$IFDEF VerboseDanglingComponentEvents}
    debugln('TStandardCodeTool.FindDanglingComponentEvents A ',MainFilename,' ',DbgSName(RootComponent));
    {$ENDIF}
    Result:=GatherPublishedClassElements(TheClassName,ExceptionOnClassNotFound,
                                         false,true,false,SearchInAncestors,
                                         PublishedMethods);
    if not Result then exit;
    // go through all components
    Collector:=TComponentChildCollector.Create;
    AllComponents:=Collector.GetComponents(RootComponent,true);
    for i:=0 to AllComponents.Count-1 do
      CheckMethodsInPersistent(TComponent(AllComponents[i]));
  finally
    Collector.Free;
    DisposeAVLTree(PublishedMethods);
  end;
end;

function TStandardCodeTool.RemoveIdentifierDefinition(
  const CursorPos: TCodeXYPosition; SourceChangeCache: TSourceChangeCache
  ): boolean;
var
  CleanCursorPos: integer;
  Node: TCodeTreeNode;
  PrevSibling: TCodeTreeNode;
  NextSibling: TCodeTreeNode;
  DeleteStartPos: LongInt;
  DeleteEndPos: LongInt;
  DeleteFirstTokenOfLine: Boolean;
begin
  Result:=false;
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
  Node:=BuildSubTreeAndFindDeepestNodeAtPos(CleanCursorPos,true);
  if Node.Desc in AllIdentifierDefinitions then begin
    // Examples:
    //   var i, X: integer;     ->  var i[, X]: integer;
    //   var i, X, j: integer;  ->  var i, [X, ]j: integer;
    //   var X, i: integer;     ->  var [X, ]i: integer;
    //   type X = integer;
    //   const X = 0;
    //   const X : integer = 0;
    PrevSibling:=nil;
    NextSibling:=nil;
    if (Node.PriorBrother<>nil) and (Node.PriorBrother.FirstChild=nil) then
      PrevSibling:=Node.PriorBrother;
    if (Node.FirstChild=nil) and (Node.NextBrother<>nil) then
      NextSibling:=Node.NextBrother;
    DeleteStartPos:=Node.StartPos;
    DeleteEndPos:=Node.StartPos+GetIdentLen(@Src[Node.StartPos]);
    if NextSibling<>nil then begin
      //   var i, X, j: integer;  ->  var i, [X, ]j: integer;
      //   var X, i: integer;     ->  var [X, ]i: integer;
      MoveCursorToCleanPos(Node.StartPos);
      ReadNextAtom;
      AtomIsIdentifierE;
      if not ReadNextAtomIsChar(',') then
        RaiseCharExpectedButAtomFound(20170421201227,',');
      DeleteEndPos:=CurPos.EndPos;
    end else if PrevSibling<>nil then begin
      // var i, X: integer;     ->  var i[, X]: integer;
      MoveCursorToCleanPos(PrevSibling.StartPos);
      ReadNextAtom;
      AtomIsIdentifierE;
      if not ReadNextAtomIsChar(',') then
        RaiseCharExpectedButAtomFound(20170421201233,',');
      DeleteStartPos:=CurPos.StartPos;
    end else begin
      // delete whole declaration
      if (Node.Parent.Desc in AllDefinitionSections)
      and (Node.PriorBrother=nil) and (Node.NextBrother=nil) then begin
        // delete whole section
        DeleteStartPos:=Node.Parent.StartPos;
        DeleteEndPos:=Node.Parent.EndPos;
      end else if Node.Parent.Desc=ctnParameterList then begin
        // delete whole parameter including modifier, type and default value
        if Node.PriorBrother<>nil then begin
          // ... var i: integer; var X: ... -> ... var i: integer[; var X: ...
          MoveCursorToCleanPos(Node.PriorBrother.EndPos);
          repeat
            ReadNextAtom;
            if CurPos.Flag=cafSemicolon then begin
              DeleteStartPos:=CurPos.EndPos;
              break;
            end;
          until CurPos.StartPos>=Node.StartPos;
        end else begin
          // (var X: ... -> ([; X: ...
          MoveCursorToCleanPos(Node.Parent.StartPos);
          ReadNextAtom;
          if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
            DeleteStartPos:=CurPos.EndPos;
        end;
        if Node.NextBrother<>nil then begin
          // ... var X: integer; var i: ... -> .. var X: integer;] var i: ...
          DeleteEndPos:=Node.PriorBrother.EndPos;
        end else begin
          // ... var X: integer) -> .. var X: integer])
          DeleteEndPos:=Node.EndPos;
        end;
      end else begin
        // keep section, delete whole declaration
        DeleteEndPos:=Node.EndPos;
      end;
    end;
    // include corresponding comments
    DeleteFirstTokenOfLine:=FindFirstNonSpaceCharInLine(Src,DeleteStartPos)=DeleteStartPos;
    //DebugLn(['TStandardCodeTool.RemoveIdentifierDefinition ',dbgstr(copy(Src,FindFirstNonSpaceCharInLine(Src,DeleteStartPos),10))]);
    DeleteEndPos:=FindLineEndOrCodeAfterPosition(DeleteEndPos,true,DeleteFirstTokenOfLine);
    if DeleteFirstTokenOfLine and (Src[DeleteEndPos-1] in [#10,#13]) then begin
      // delete first and last token of line
      // => remove the entire line
      DeleteStartPos:=GetLineStartPosition(Src,DeleteStartPos);
    end;
    //DebugLn(['TStandardCodeTool.RemoveIdentifierDefinition "',dbgstr(copy(Src,DeleteStartPos,DeleteEndPos-DeleteStartPos)),'" IncludeLineEnd=',DeleteFirstTokenOfLine]);

    // delete
    SourceChangeCache.MainScanner:=Scanner;
    if not SourceChangeCache.Replace(gtNone,gtNone,DeleteStartPos,DeleteEndPos,'')
    then exit;
    Result:=SourceChangeCache.Apply;
  end;
end;

function TStandardCodeTool.InsertStatements(
  InsertPos: TInsertStatementPosDescription; Statements: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  CleanCursorPos: integer;
begin
  BeginParsingAndGetCleanPos(lsrEnd,InsertPos.CodeXYPos,CleanCursorPos);
  Result:=InsertStatements(CleanCursorPos,Statements,InsertPos.Indent,
    InsertPos.FrontGap,InsertPos.AfterGap,SourceChangeCache);
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.InsertStatements(CleanPos: integer;
  Statements: string; Indent: integer; FrontGap, AfterGap: TGapTyp;
  SourceChangeCache: TSourceChangeCache): boolean;
{
  ToDo: check for "uses" in Statements and extend uses section
        e.g. "uses unit1, unit2 in 'filename'; statements
  ToDo: check for single statement (e.g. for .. do | dosome;) and add begin/end

 }
var
  Node: TCodeTreeNode;
  SameArea: TAtomPosition;
  BeautifyFlags: TBeautifyCodeFlags;
begin
  Node:=FindDeepestNodeAtPos(CleanPos,true);
  if not (Node.Desc in AllPascalStatements) then begin
    MoveCursorToCleanPos(CleanPos);
    RaiseException(20170421201247,ctsInvalidPositionForInsertionOfStatements);
  end;
  if Node.Desc=ctnBeginBlock then
    Node:=BuildSubTreeAndFindDeepestNodeAtPos(Node,CleanPos,true);

  GetCleanPosInfo(Node.StartPos,CleanPos,false,SameArea);
  if (SameArea.StartPos>SrcLen) or (not IsSpaceChar[Src[SameArea.StartPos]])
  then begin
    MoveCursorToCleanPos(CleanPos);
    RaiseException(20170421201255,ctsInvalidPositionForInsertionOfStatements);
  end;

  SourceChangeCache.MainScanner:=Scanner;
  BeautifyFlags:=[bcfIndentExistingLineBreaks];
  if FrontGap in [gtNone,gtSpace] then
    include(BeautifyFlags,bcfDoNotIndentFirstLine);
  Statements:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
    Statements,Indent,BeautifyFlags);

  Result:=SourceChangeCache.Replace(FrontGap,AfterGap,CleanPos,CleanPos,Statements);
end;

function TStandardCodeTool.FindBlockCounterPart(
  const CursorPos: TCodeXYPosition;
  out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
// jump from bracket-open to bracket-close or 'begin' to 'end'
// or 'until' to 'repeat' ...
var CleanCursorPos: integer;
begin
  Result:=false;
  BeginParsingAndGetCleanPos(lsrEnd,CursorPos,CleanCursorPos);
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

function TStandardCodeTool.FindBlockStart(const CursorPos: TCodeXYPosition; out
  NewPos: TCodeXYPosition; out NewTopLine: integer; SkipStart: boolean
  ): boolean;
// jump to beginning of current block
// e.g. bracket open, 'begin', 'repeat', ...
var CleanCursorPos: integer;
  CursorOnStart: Boolean;
  Node: TCodeTreeNode;
  MinPos: Integer;
begin
  Result:=false;
  // scan code
  BeginParsingAndGetCleanPos(lsrEnd,CursorPos,CleanCursorPos);
  // read word at cursor
  MoveCursorToCleanPos(CleanCursorPos);
  while (CurPos.StartPos>2) and IsWordChar[Src[CurPos.StartPos-1]] do
    dec(CurPos.StartPos);
  MoveCursorToCleanPos(CurPos.StartPos);
  ReadNextAtom;
  try
    if CurPos.StartPos>=SrcLen then begin
      ReadPriorAtom;
      if CurPos.StartPos<1 then begin
        MoveCursorToCleanPos(1);
        exit(true);
      end;
    end;
    Node:=FindDeepestNodeAtPos(CleanCursorPos,false);
    //if Node<>nil then debugln(['TStandardCodeTool.FindBlockStart ',Node.DescAsString]);
    if (Node=nil)
    or (Node.Desc in (AllPascalStatements+AllPascalTypes-AllClasses))
    or (Src[CurPos.StartPos] in [')',']','}'])
    then begin
      MinPos:=1;
      if Node<>nil then MinPos:=Node.StartPos;
      repeat
        //debugln(['TStandardCodeTool.FindBlockStart atom ',CleanPosToStr(CurPos.StartPos),' ',GetAtom]);
        if (CurPos.StartPos<0) then begin
          // start of source found -> this is always a block start
          MoveCursorToCleanPos(1);
          exit(true);
        end
        else if Src[CurPos.StartPos] in [')',']','}'] then begin
          // jump backward to matching bracket
          CursorOnStart:=(CleanCursorPos=CurPos.StartPos);
          if not ReadBackwardTilAnyBracketClose then exit;
          if CursorOnStart then exit(true);
        end
        else if WordIsBlockStatementStart.DoItCaseInsensitive(Src,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos) then
        begin
          // block start found
          if (CurPos.StartPos<CleanCursorPos) or (not SkipStart) then
            exit(true);
        end else if UpAtomIs('END') or UpAtomIs('FINALLY') or UpAtomIs('EXCEPT')
        or UpAtomIs('UNTIL') then
        begin
          // read backward till BEGIN, CASE, ASM, RECORD, REPEAT
          CursorOnStart:=(CleanCursorPos>=CurPos.StartPos)
                     and (CleanCursorPos<CurPos.EndPos);
          ReadBackTilBlockEnd(true);
          if CursorOnStart then exit(true);
        end;
        ReadPriorAtom;
      until CurPos.StartPos<MinPos;
    end;
    if Node<>nil then begin
      if SkipStart and (CleanCursorPos=Node.StartPos) then begin
        while (Node<>nil) and (Node.StartPos=CleanCursorPos) do
          Node:=Node.Parent;
        if Node<>nil then
          MoveCursorToCleanPos(Node.StartPos)
        else
          MoveCursorToCleanPos(1);
        exit(true);
      end;
      if CleanCursorPos>=Node.StartPos then begin
        MoveCursorToCleanPos(Node.StartPos);
        exit(true);
      end;
    end;
  finally
    if Result then begin
      // CursorPos now contains the block start atom
      Result:=CleanPosToCaretAndTopLine(CurPos.StartPos,NewPos,NewTopLine);
    end;
  end;
end;

function TStandardCodeTool.GuessUnclosedBlock(const CursorPos: TCodeXYPosition;
  out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
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
  BeginParsingAndGetCleanPos(lsrEnd,CursorPos,CleanCursorPos);
  // start reading at beginning of code
  MoveCursorToCleanPos(1);
  BuildBlockKeyWordFuncList;
  if ReadTilGuessedUnclosedBlock(CleanCursorPos,false) then
    Result:=CleanPosToCaretAndTopLine(CurPos.StartPos,NewPos,NewTopLine);
  //WriteDebugTreeReport;
end;

function TStandardCodeTool.FindBlockCleanBounds(
  const CursorPos: TCodeXYPosition; out BlockCleanStart, BlockCleanEnd: integer
  ): boolean;
var
  CleanCursorPos: integer;
  BlockStartFound: Boolean;
begin
  Result:=false;
  BlockCleanStart:=0;
  BlockCleanEnd:=0;
  // scan code
  BeginParsingAndGetCleanPos(lsrEnd,CursorPos,CleanCursorPos);
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
      MoveCursorToCleanPos(1);
      BlockStartFound:=true;
      break;
    end
    else if Src[CurPos.StartPos] in [')',']','}'] then begin
      // jump backward to matching bracket
      if not ReadBackwardTilAnyBracketClose then exit;
    end
    else if WordIsBlockStatementStart.DoItCaseInsensitive(Src,
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

function TStandardCodeTool.CompleteBlock(const CursorPos: TCodeXYPosition;
  SourceChangeCache: TSourceChangeCache; OnlyIfCursorBlockIndented: boolean;
  out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
{ For example:
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    begin
      |
      ...
  something
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if then begin
      |
      ...
  something
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  begin
    |

  procedure
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Statements:
    begin: end;
    asm: end;
    try: finally end;
    finally: end;
    except: end;
    repeat: until ;
    case of: end;
    case :: ;
    case else: end;
    (: )
    [: ]

  Types:
    (: )
    [: ]
    record: end;
    class: end;
    object: end;
    interface: end;
}
type
  TBlockType = (
    btNone,
    btBegin,
    btAsm,
    btEdgedBracket,
    btRoundBracket,
    btTry,
    btFinally,
    btExcept,
    btCase,
    btCaseOf,
    btCaseColon,
    btCaseElse,
    btRepeat,
    btIf,
    btIfElse,
    btClass,
    btInterface,
    btObject,
    btRecord
    );
  TBlock = record
    Typ: TBlockType;
    StartPos: integer;
    InnerIndent: integer;
    InnerStartPos: integer;
  end;
  PBlock = ^TBlock;
  TBlockStack = record
    Stack: PBlock;
    Capacity: integer;
    Top: integer;
  end;
  TExBool = (ebNone, ebTrue, ebFalse);
var
  CleanCursorPos: integer;
  StartNode: TCodeTreeNode;
  InternalCursorAtEmptyLine: TExBool;
  Beauty: TBeautifyCodeOptions;

  function CursorAtEmptyLine: Boolean;
  // true if cursor in empty line or at line end in front of an empty line
  var
    p: LongInt;
  begin
    if InternalCursorAtEmptyLine=ebNone then begin
      if (CleanCursorPos>SrcLen) or InEmptyLine(Src,CleanCursorPos) then
        InternalCursorAtEmptyLine:=ebTrue
      else begin
        p:=CleanCursorPos;
        while (p<=SrcLen) do begin
          case Src[p] of
          ' ',#9: inc(p);
          #10,#13:
            begin
              // after cursor the rest of the line is blank
              // check the next line
              inc(p);
              if (p<=SrcLen) and (Src[p] in [#10,#13]) and (Src[p]<>Src[p-1]) then
                inc(p);
              if (p>SrcLen) or InEmptyLine(Src,p) then
                InternalCursorAtEmptyLine:=ebTrue
              else
                InternalCursorAtEmptyLine:=ebFalse;
              break;
            end;
          else
            InternalCursorAtEmptyLine:=ebFalse;
            break;
          end;
        end;
      end;
    end;
    Result:=InternalCursorAtEmptyLine=ebTrue;
  end;

  procedure InitStack(out Stack: TBlockStack);
  begin
    FillByte(Stack{%H-},SizeOf(Stack),0);
    Stack.Top:=-1;
  end;

  procedure FreeStack(var Stack: TBlockStack);
  begin
    ReAllocMem(Stack.Stack,0);
    Stack.Capacity:=0;
    Stack.Top:=-1;
  end;

  procedure BeginBlock(var Stack: TBlockStack; Typ: TBlockType;
    StartPos: integer);
  var
    Block: PBlock;
  begin
    inc(Stack.Top);
    if Stack.Top>=Stack.Capacity then begin
      if Stack.Capacity=0 then
        Stack.Capacity:=16
      else
        Stack.Capacity:=Stack.Capacity*2;
      ReAllocMem(Stack.Stack,SizeOf(TBlock)*Stack.Capacity);
    end;
    {$IFDEF VerboseCompleteBlock}
    DebugLn([GetIndentStr(Stack.Top*2),'BeginBlock ',CleanPosToStr(StartPos),' ',GetAtom]);
    {$ENDIF}
    Block:=@Stack.Stack[Stack.Top];
    Block^.Typ:=Typ;
    Block^.StartPos:=StartPos;
    Block^.InnerIndent:=-1;
    Block^.InnerStartPos:=-1;
  end;

  procedure EndBlock(var Stack: TBlockStack);
  begin
    {$IFDEF VerboseCompleteBlock}
    DebugLn([GetIndentStr(Stack.Top*2),'EndBlock ',GetAtom,' ',CleanPosToStr(CurPos.StartPos),', started at ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
    {$ENDIF}
    dec(Stack.Top);
  end;

  function TopBlockType(const Stack: TBlockStack): TBlockType;
  begin
    if Stack.Top>=0 then
      Result:=Stack.Stack[Stack.Top].Typ
    else
      Result:=btNone;
  end;

  function Replace(NewCode: string; FromPos, ToPos, Indent: integer;
    FrontGap, AfterGap: TGapTyp; BeautifyFlags: TBeautifyCodeFlags): boolean;
  var
    p: LongInt;
  begin
    Result:=false;
    if NewCode='' then exit(true);
    // try to avoid changing current line
    if (FrontGap=gtEmptyLine) then begin
      p:=FromPos;
      while (p>1) and (Src[p-1] in [' ',#9]) do dec(p);
      if (p=1) or (Src[p] in [#10,#13]) then begin
        while (p<=SrcLen) and (Src[p] in [' ',#9]) do inc(p);
        if (p>SrcLen) or (Src[p] in [#10,#13]) then begin
          // inserting in an empty line
          inc(p);
          if (p<=SrcLen) and (Src[p] in [#10,#13]) and (Src[p]<>Src[p-1]) then
            inc(p);
          FrontGap:=gtNewLine;
          FromPos:=p;
          if ToPos<FromPos then ToPos:=FromPos;
        end;
      end;
    end;
    // replace trailing spaces
    while (ToPos<=SrcLen) and (Src[ToPos] in [' ',#9]) do inc(ToPos);
    // use existing semicolon
    if (NewCode[length(NewCode)]=';')
    and (ToPos<=SrcLen) and (Src[ToPos]=';') then begin
      AfterGap:=gtNone;
      inc(ToPos);
    end;
    // use existing "else"
    if (NewCode[length(NewCode)]=';') then begin
      MoveCursorToCleanPos(ToPos);
      ReadNextAtom;
      if UpAtomIs('ELSE') then
        NewCode:=copy(NewCode,1,length(NewCode)-1);
    end;

    // adjust indent of first line
    if FrontGap in [gtNone,gtSpace] then begin
      BeautifyFlags:=BeautifyFlags+[bcfDoNotIndentFirstLine];
      NewCode:=Beauty.GetIndentStr(Indent-GetPosInLine(Src,FromPos))+NewCode;
    end;
    // beautify
    NewCode:=Beauty.BeautifyStatement(
                     NewCode,Indent,BeautifyFlags);

    if AfterGap=gtNewLine then begin
      // do not reuse existing newline, but always add newline
      NewCode:=NewCode+Beauty.LineEnd;
      if (ToPos<SrcLen) and (not (Src[ToPos] in [#10,#13])) then
        NewCode:=NewCode+Beauty.GetIndentStr(Beauty.GetLineIndent(Src,ToPos));
      AfterGap:=gtNone;
    end;
    {$IFDEF VerboseCompleteBlock}
    debugln(['Replace Indent=',Indent,' NewCode="',dbgstr(NewCode),'" Replace: InFront="',DbgStr(copy(Src,FromPos-15,15)),'",Replace="',dbgstr(copy(Src,FromPos,ToPos-FromPos)),'",Behind="',dbgstr(copy(Src,ToPos,15)),'" FrontGap=',dbgs(FrontGap),' AfterGap=',dbgs(AfterGap)]);
    {$ENDIF}
    // insert
    if not SourceChangeCache.Replace(FrontGap,AfterGap,
      FromPos,ToPos,NewCode) then exit;
    if not SourceChangeCache.Apply then exit;
    Result:=true;
  end;

  function CompleteStatements(var Stack: TBlockStack): Boolean;
  var
    CursorBlockLvl: Integer; // the stack level of the cursor
    LastPos: Integer;
    LineStart: boolean; // Atom is first atom of a line in cursor block (not in sub block)
    Indent: Integer;
    CursorBlockInnerIndent, CursorBlockOuterIndent: LongInt;
    CursorBlock: TBlock;
    AtomInFrontOfCursor: TAtomPosition;
    BehindCursorBlock: Boolean; // atom is behind cursor block
    InCursorBlock: Boolean;
    NeedCompletion: integer;
    InsertPos: LongInt;
    NewCode: String;
    AfterGap: TGapTyp;
    FrontGap: TGapTyp;
    BeautifyFlags: TBeautifyCodeFlags;
    BehindPos: LongInt;
    CursorInEmptyStatement: Boolean;
    FromPos: LongInt;
    ToPos: LongInt;
    WasInCursorBlock: Boolean;

    function EndBlockIsOk: boolean;
    begin
      //DebugLn(['EndBlockIsOk ']);
      if (NeedCompletion>0) and (CursorBlockLvl>=0)
      and (Stack.Top=CursorBlockLvl)
      and (Beauty.GetLineIndent(Src,CurPos.StartPos)=CursorBlockOuterIndent) then begin
        // cursor block is properly closed => do not complete
        {$IFDEF VerboseCompleteBlock}
        debugln(['EndBlockIsOk cursor block is properly closed at ',CleanPosToStr(CurPos.StartPos)]);
        {$ENDIF}
        NeedCompletion:=0;
      end;
      EndBlock(Stack);
      Result:=true;
      if (not BehindCursorBlock) and (Stack.Top<CursorBlockLvl) then
        BehindCursorBlock:=true;
    end;

    function CloseBrackets: boolean;
    begin
      while TopBlockType(Stack) in [btRoundBracket,btEdgedBracket] do begin
        if not EndBlockIsOk then exit(false);
      end;
      Result:=true;
    end;

    function InsertPosAtCursor: integer;
    begin
      Result:=BasicCodeTools.FindLineEndOrCodeInFrontOfPosition(Src,
                         CurPos.StartPos,CleanCursorPos,Scanner.NestedComments);
    end;

  begin
    Result:=false;
    MoveCursorToNodeStart(StartNode);
    CursorBlockLvl:=-2;
    LastPos:=-1;
    CursorBlockOuterIndent:=0;
    CursorBlockInnerIndent:=0;
    Indent:=0;
    CursorBlock.StartPos:=0;
    BehindCursorBlock:=false;
    NeedCompletion:=0;
    AtomInFrontOfCursor.StartPos:=0;
    CursorInEmptyStatement:=false;
    repeat
      ReadNextAtom;

      //DebugLn(['ReadStatements Atom=',GetAtom,' TopTyp=',ord(TopBlockType(Stack)),' Top=',Stack.Top]);
      if (Stack.Top>=0) and (Stack.Stack[Stack.Top].InnerIndent<0)
      and (not PositionsInSameLine(Src,Stack.Stack[Stack.Top].StartPos,CurPos.StartPos))
      then begin
        // the first atom of this block is on a new line
        Stack.Stack[Stack.Top].InnerIndent:=Beauty.GetLineIndent(Src,CurPos.StartPos);
        Stack.Stack[Stack.Top].InnerStartPos:=CurPos.StartPos;
      end;

      // check if cursor reached
      if (CurPos.StartPos>=CleanCursorPos) and (CursorBlockLvl<0) then begin
        // reached cursor
        CursorBlockLvl:=Stack.Top;
        if CursorBlockLvl<0 then begin
          // cursor outside blocks or on first atom of first block
          {$IFDEF VerboseCompleteBlock}
          DebugLn(['ReadStatements no completion: cursor outside blocks or on first atom of first block ',CleanPosToStr(CurPos.StartPos)]);
          {$ENDIF}
          exit;
        end else begin
          CursorBlock:=Stack.Stack[CursorBlockLvl];
          CursorBlockOuterIndent:=Beauty.GetLineIndent(Src,CursorBlock.StartPos);
          CursorBlockInnerIndent:=Stack.Stack[Stack.Top].InnerIndent;
          if (CursorBlockInnerIndent<=CursorBlockOuterIndent)
          and OnlyIfCursorBlockIndented then begin
            // cursor block not indented
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements no completion: cursor block not indented ',CleanPosToStr(CurPos.StartPos),' CursorBlockOuterIndent=',CursorBlockOuterIndent,' CursorBlockInnerIndent=',CursorBlockInnerIndent]);
            {$ENDIF}
            exit;
          end;
          AtomInFrontOfCursor:=LastAtoms.GetPriorAtom;
          {$IFDEF VerboseCompleteBlock}
          DebugLn(['ReadStatements reached cursor: ',CleanPosToStr(CurPos.StartPos),' CursorBlockOuterIndent=',CursorBlockOuterIndent,' CursorBlockInnerIndent=',CursorBlockInnerIndent,' LastAtom=',GetAtom(AtomInFrontOfCursor),' CurAtom=',GetAtom]);
          {$ENDIF}
          if (CurPos.Flag=cafSemicolon)
          and ((AtomInFrontOfCursor.Flag=cafSemicolon)
            or (CursorBlock.StartPos=AtomInFrontOfCursor.StartPos))
          and (FindNextNonSpace(Src,AtomInFrontOfCursor.EndPos)=CurPos.StartPos)
          then begin
            // cursor in empty statement
            CursorInEmptyStatement:=true;
          end;
        end;
        //DebugLn(['ReadStatements CursorBlockLvl=',CursorBlockLvl,' Indent=',CursorBlockIndent]);
      end;

      InCursorBlock:=(CursorBlockLvl>=0) and (CursorBlockLvl=Stack.Top)
                     and (not BehindCursorBlock);
      WasInCursorBlock:=InCursorBlock;

      // check if end of node
      if (CurPos.StartPos>SrcLen) or (CurPos.StartPos>=StartNode.EndPos) then
      begin
        if InCursorBlock and (NeedCompletion=0) then begin
          {$IFDEF VerboseCompleteBlock}
          DebugLn(['ReadStatements NeedCompletion: source end found at ',CleanPosToStr(CurPos.StartPos)]);
          {$ENDIF}
          NeedCompletion:=CleanCursorPos;
        end;
        break;
      end;

      // check if line start vs outer indent
      LineStart:=InCursorBlock and (LastPos>0)
                 and not PositionsInSameLine(Src,LastPos,CurPos.StartPos);
      if LineStart then
        Indent:=Beauty.GetLineIndent(Src,CurPos.StartPos);
      if LineStart and (NeedCompletion=0) then begin
        // atom is in same block as cursor (not sub block)
        // and first atom of a line
        // => check indent
        //debugln(['CompleteStatements first atom of line in cursor block: ',GetAtom,' Indent=',Indent,' CursorBlockOuterIndent=',CursorBlockOuterIndent,' CursorBlockOuterIndent=',CursorBlockOuterIndent]);
        if (Indent=CursorBlockOuterIndent) then begin
          if (CursorBlockLvl>0)
          and (Stack.Stack[CursorBlockLvl-1].InnerIndent=Indent)
          and (Stack.Stack[CursorBlockLvl-1].InnerStartPos<CurPos.StartPos)
          then begin
            { for example:
                Code;
                begin|
                Code;
            }
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements NeedCompletion: between same indented ',CleanPosToStr(CurPos.StartPos),' Indent=',Indent,' < CursorBlockOuterIndent=',CursorBlockOuterIndent,' < CursorBlockInnerIndent=',CursorBlockInnerIndent,' Parent.InnerStartPos=',CleanPosToStr(Stack.Stack[CursorBlockLvl-1].InnerStartPos)]);
            {$ENDIF}
            NeedCompletion:=InsertPosAtCursor;
          end;
        end else if (Indent<CursorBlockOuterIndent) then begin
          // for example:
          //    begin
          //    Code;
          //  |end;
          //DebugLn(['ReadStatements Indent=',Indent,' < CursorBlockOuterIndent=',CursorBlockOuterIndent,' CursorBlockInnerIndent=',CursorBlockInnerIndent,' CursorAtEmptyLine=',CursorAtEmptyLine,' CursorInEmptyStatement=',CursorInEmptyStatement]);
          if CursorBlockOuterIndent<CursorBlockInnerIndent then begin
            // for example:
            //    begin
            //      Code;
            //  |end;
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements NeedCompletion: at out indented ',CleanPosToStr(CurPos.StartPos),' Indent=',Indent,' < CursorBlockOuterIndent=',CursorBlockOuterIndent,' < CursorBlockInnerIndent=',CursorBlockInnerIndent]);
            {$ENDIF}
            NeedCompletion:=InsertPosAtCursor;
          end else if CursorAtEmptyLine or CursorInEmptyStatement
          or (FindNextNonSpace(Src,CleanCursorPos)=CurPos.StartPos) then begin
            { for example:
                  begin
                  |
                  Code;
                end;

                  begin
                  Code;
                  |
                end;
            }
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements NeedCompletion: at empty line ',CleanPosToStr(CleanCursorPos),' Indent=',Indent,' < CursorBlockOuterIndent=',CursorBlockOuterIndent,' < CursorBlockInnerIndent=',CursorBlockInnerIndent]);
            {$ENDIF}
            NeedCompletion:=CleanCursorPos;
          end else begin
            { It needs completion, but where?
              for example:
                begin
                  begin|
                  Code;
                end;
            }
          end;
        end;
      end;

      // check block starts/ends
      case CurPos.Flag of
      cafEnd:
        if (CurPos.EndPos<=SrcLen) and (Src[CurPos.EndPos]='.') then begin
          { end. of source found
            The parsing started in a begin block, valid cases:

              program a;
              begin|
              end.

              implementation
              begin|
              end.
          }
          if (Stack.Top=0) and (TopBlockType(Stack)=btBegin)
          and (StartNode.Desc=ctnBeginBlock)
          and ((StartNode.Parent=nil)
            or (StartNode.Parent.Desc in AllSourceTypes+[ctnInterface,ctnImplementation]))
          then begin
            if not EndBlockIsOk then exit; // close main begin
          end else begin
            // unexpected end of source
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements unexpected end. at ',CleanPosToStr(CurPos.StartPos)]);
            {$ENDIF}
            if InCursorBlock and (NeedCompletion=0) then begin
              {$IFDEF VerboseCompleteBlock}
              DebugLn(['ReadStatements NeedCompletion: unexpected end. at ',CleanPosToStr(CurPos.StartPos)]);
              {$ENDIF}
              NeedCompletion:=CleanCursorPos;
            end;
          end;
          break;
        end else begin
          case TopBlockType(Stack) of
          btCaseOf,btCaseElse:
            begin
              if not EndBlockIsOk then exit; // close btCaseOf,btCaseElse
              if not EndBlockIsOk then exit; // close btCase
            end;
          btBegin,btFinally,btExcept,btCase:
            if not EndBlockIsOk then exit;
          btCaseColon,btRepeat:
            begin
              // missing semicolon or until
              DebugLn(['ReadStatements CursorBlockLvl=',CursorBlockLvl,' Stack.Top=',Stack.Top,' BehindCursorBlock=',BehindCursorBlock]);
              DebugLn(['ReadStatements unexpected end at ',CleanPosToStr(CurPos.StartPos),': missing finally ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
              if InCursorBlock and (NeedCompletion=0) then begin
                {$IFDEF VerboseCompleteBlock}
                DebugLn(['ReadStatements NeedCompletion: unexpected end at ',CleanPosToStr(CurPos.StartPos),': missing semicolon or until ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
                {$ENDIF}
                NeedCompletion:=CleanCursorPos;
              end;
              break;
            end;
          btTry:
            begin
              // missing finally/except
              DebugLn(['ReadStatements CursorBlockLvl=',CursorBlockLvl,' Stack.Top=',Stack.Top,' BehindCursorBlock=',BehindCursorBlock]);
              DebugLn(['ReadStatements unexpected end at ',CleanPosToStr(CurPos.StartPos),': missing finally ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
              if InCursorBlock and (NeedCompletion=0) then begin
                {$IFDEF VerboseCompleteBlock}
                DebugLn(['ReadStatements NeedCompletion: unexpected end at ',CleanPosToStr(CurPos.StartPos),': missing finally ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
                {$ENDIF}
                NeedCompletion:=CleanCursorPos;
              end;
              break;
            end;
          btAsm:
            if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]<>'@') then begin
              if not EndBlockIsOk then exit;
            end;
          else
            // missing begin
            exit;
          end;
        end;
      cafEdgedBracketOpen:
        BeginBlock(Stack,btEdgedBracket,CurPos.StartPos);
      cafEdgedBracketClose:
        if TopBlockType(Stack)=btEdgedBracket then begin
          if not EndBlockIsOk then exit;
        end else begin
          // missing [
          exit;
        end;
      cafRoundBracketOpen:
        BeginBlock(Stack,btRoundBracket,CurPos.StartPos);
      cafRoundBracketClose:
        if TopBlockType(Stack)=btRoundBracket then begin
          if not EndBlockIsOk then exit;
        end else begin
          // missing (
          exit;
        end;
      cafColon:
        if TopBlockType(Stack)=btCaseOf then
          BeginBlock(Stack,btCaseColon,CurPos.StartPos);
      cafSemicolon:
        while TopBlockType(Stack)
        in [btCaseColon,btIf,btIfElse,btRoundBracket,btEdgedBracket] do begin
          if not EndBlockIsOk then exit;
        end;
      cafWord:
        if TopBlockType(Stack)<>btAsm then begin
          if UpAtomIs('BEGIN') then
            BeginBlock(Stack,btBegin,CurPos.StartPos)
          else if UpAtomIs('TRY') then
            BeginBlock(Stack,btTry,CurPos.StartPos)
          else if UpAtomIs('FINALLY') then begin
            if TopBlockType(Stack)=btTry then
              if not EndBlockIsOk then exit;
            BeginBlock(Stack,btFinally,CurPos.StartPos)
          end else if UpAtomIs('EXCEPT') then begin
            if TopBlockType(Stack)=btTry then
              if not EndBlockIsOk then exit;
            BeginBlock(Stack,btExcept,CurPos.StartPos)
          end else if UpAtomIs('REPEAT') then
            BeginBlock(Stack,btRepeat,CurPos.StartPos)
          else if UpAtomIs('UNTIL') then begin
            if TopBlockType(Stack)=btRepeat then begin
              if not EndBlockIsOk then exit;
            end else begin
              // until without repeat
              DebugLn(['ReadStatements CursorBlockLvl=',CursorBlockLvl,' Stack.Top=',Stack.Top,' BehindCursorBlock=',BehindCursorBlock,' Block=',ord(TopBlockType(Stack))]);
              DebugLn(['ReadStatements unexpected until at ',CleanPosToStr(CurPos.StartPos)]);
              exit;
            end;
          end else if UpAtomIs('ASM') then begin
            BeginBlock(Stack,btAsm,CurPos.StartPos);
          end else if UpAtomIs('IF') then begin
            BeginBlock(Stack,btIf,CurPos.StartPos);
          end else if UpAtomIs('THEN') then begin
            CloseBrackets;
            if TopBlockType(Stack)=btIf then begin
              Stack.Stack[Stack.Top].InnerIndent:=-1;
              Stack.Stack[Stack.Top].InnerStartPos:=-1;
            end;
          end else if UpAtomIs('CASE') then begin
            BeginBlock(Stack,btCase,CurPos.StartPos)
          end else if UpAtomIs('OF') then begin
            CloseBrackets;
            if TopBlockType(Stack)=btCase then
              BeginBlock(Stack,btCaseOf,CurPos.StartPos);
          end else if UpAtomIs('ELSE') then begin
            CloseBrackets;
            case TopBlockType(Stack) of
            btIf:
              begin
                if not EndBlockIsOk then exit;
                BeginBlock(Stack,btIfElse,CurPos.StartPos);
              end;
            btCaseOf:
              begin
                if not EndBlockIsOk then exit;
                BeginBlock(Stack,btCaseElse,CurPos.StartPos);
              end;
            btBegin:
              begin
                // missing end
                if InCursorBlock and (NeedCompletion=0) then begin
                  {$IFDEF VerboseCompleteBlock}
                  DebugLn(['ReadStatements NeedCompletion: unexpected else at ',CleanPosToStr(CurPos.StartPos),': missing end. block start: ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
                  {$ENDIF}
                  NeedCompletion:=InsertPosAtCursor;
                end;
                break;
              end;
            btCaseColon,btRepeat:
              begin
                // missing semicolon
                if InCursorBlock and (NeedCompletion=0) then begin
                  {$IFDEF VerboseCompleteBlock}
                  DebugLn(['ReadStatements NeedCompletion: unexpected else at ',CleanPosToStr(CurPos.StartPos),': missing semicolon or until. block start: ',CleanPosToStr(Stack.Stack[Stack.Top].StartPos)]);
                  {$ENDIF}
                  NeedCompletion:=InsertPosAtCursor;
                end;
                break;
              end;
            end;
          end else if UpAtomIs('PROCEDURE') or UpAtomIs('FUNCTION')
          or UpAtomIs('CONSTRUCTOR') or UpAtomIs('DESTRUCTOR')
          or UpAtomIs('VAR') or UpAtomIs('TYPE') or UpAtomIs('CONST')
          or UpAtomIs('RESOURCESTRING') or UpAtomIs('LABEL') or UpAtomIs('CLASS')
          or UpAtomIs('INITIALIZATION') or UpAtomIs('FINALIZATION')
          then begin
            // unexpected keyword => block not closed
            if InCursorBlock and (NeedCompletion=0) then begin
              {$IFDEF VerboseCompleteBlock}
              DebugLn(['ReadStatements NeedCompletion: unexpected keyword ',GetAtom,' at ',CleanPosToStr(CurPos.StartPos)]);
              {$ENDIF}
              NeedCompletion:=CleanCursorPos;
            end;
            break;
          end;
        end;
      end;

      // check if line start
      if LineStart and WasInCursorBlock and (not BehindCursorBlock) then begin
        // atom is first atom of a line
        // and atom is in same block as cursor (not sub block)
        // (maybe the atom started a new sub block, but it did not close it)
        // => check indent
        //debugln(['CompleteStatements ',CleanPosToStr(CurPos.StartPos),' Indent=',Indent,' CursorBlockInnerIndent=',CursorBlockInnerIndent,' CursorBlockOuterIndent=',CursorBlockOuterIndent]);
        if (Indent<CursorBlockInnerIndent) and (NeedCompletion=0) then begin
          if CursorBlockOuterIndent<CursorBlockInnerIndent then begin
            // for example:
            //  begin
            //    Code;
            //    |
            //    Code;
            //  Code;
            //DebugLn(['ReadStatements Indent=',Indent,' < CursorBlockIndent=',CursorBlockIndent]);
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements NeedCompletion: at ',CleanPosToStr(CurPos.StartPos),' Indent=',Indent,' < CursorBlockInnerIndent=',CursorBlockInnerIndent]);
            {$ENDIF}
            NeedCompletion:=InsertPosAtCursor;
          end else begin
            // for example:
            // begin
            // |
            // Code;
            {$IFDEF VerboseCompleteBlock}
            DebugLn(['ReadStatements NeedCompletion: at ',CleanPosToStr(CleanCursorPos),' Indent=',Indent,' CursorBlockInnerIndent=',CursorBlockInnerIndent]);
            {$ENDIF}
            NeedCompletion:=CleanCursorPos;
            // Note: if the end is coming later, NeedCompletion is disabled
          end;
        end;
      end;

      LastPos:=CurPos.StartPos;
    until Stack.Top<0;

    {$IFDEF VerboseCompleteBlock}
    DebugLn(['ReadStatements END Stack.Top=',Stack.Top,' CursorBlockLvl=',CursorBlockLvl,' BehindCursorBlock=',BehindCursorBlock]);
    {$ENDIF}

    if Stack.Top<0 then begin
      // all blocks closed
      {$IFDEF VerboseCompleteBlock}
      if NeedCompletion>0 then
        DebugLn(['ReadStatements all blocks closed: no completion needed']);
      {$ENDIF}
      NeedCompletion:=0;
    end;

    if (NeedCompletion>0) then begin
      InsertPos:=NeedCompletion;
      while (InsertPos>CleanCursorPos) and (IsSpaceChar[Src[InsertPos-1]]) do
        dec(InsertPos);
      Indent:=CursorBlockOuterIndent;

      // check code behind
      BehindPos:=FindNextNonSpace(Src,InsertPos);
      if BehindPos<=SrcLen then begin
        if (not CursorInEmptyStatement)
        and PositionsInSameLine(Src,InsertPos,BehindPos) then begin
          // target line not empty
          {$IFDEF VerboseCompleteBlock}
          DebugLn(['CompleteStatements target line not empty => skip']);
          {$ENDIF}
          exit;
        end;
        if (Beauty.GetLineIndent(Src,BehindPos)>Indent) then begin
          // code behind is more indented
          // for example
          //   repeat
          //   |
          //     DoSomething;
          debugln(['CompleteStatements BehindPos ',dbgstr(copy(Src,BehindPos-8,8)),'|',dbgstr(copy(Src,BehindPos,8))]);
          {$IFDEF VerboseCompleteBlock}
          DebugLn(['CompleteStatements code behind is indented more (Behind=',Beauty.GetLineIndent(Src,BehindPos),' > Indent=',Indent,') => skip']);
          {$ENDIF}
          exit;
        end;
      end;

      NewCode:=';';
      FrontGap:=gtEmptyLine;
      AfterGap:=gtNewLine;
      FromPos:=InsertPos;
      ToPos:=InsertPos;
      BeautifyFlags:=[bcfIndentExistingLineBreaks];
      if CursorInEmptyStatement and (BehindPos<=SrcLen) then begin
        // replace the empty statement
        FrontGap:=gtNewLine;
        ToPos:=BehindPos;
      end;
      case CursorBlock.Typ of
      btBegin,btFinally,btExcept,btAsm,btCaseOf,btCaseElse:
        NewCode:='end'+NewCode;
      btRepeat:
        NewCode:='until '+NewCode;
      btTry:
        NewCode:='finally'+SourceChangeCache.BeautifyCodeOptions.LineEnd
           +'end'+NewCode;
      btCaseColon:
        begin
          FrontGap:=gtNone;
          AfterGap:=gtNone;
        end;
      else
        exit;
      end;
      if (CursorBlockLvl=0) and (AfterGap=gtNewLine) then begin
        // top level => insert empty lines between top level structures
        AfterGap:=gtEmptyLine;
      end;
      if not Replace(NewCode,FromPos,ToPos,Indent,FrontGap,AfterGap,
        BeautifyFlags) then exit;
    end;
    Result:=true;
  end;

  function CompleteClassSection: Boolean;
  {  type
       TMyClass = class
         |
  }
  var
    LastIndent: LongInt;
    Indent: LongInt;
    InsertPos: LongInt;
    NeedCompletion: Integer;
  begin
    Result:=false;
    if CleanCursorPos<StartNode.StartPos then exit;
    LastIndent:=Beauty.GetLineIndent(Src,StartNode.Parent.StartPos);
    MoveCursorToNodeStart(StartNode);
    //debugln(['CompleteClassSection ',dbgstr(copy(Src,StartNode.StartPos-10,10)),'|',dbgstr(copy(Src,StartNode.StartPos,10))]);
    Indent:=Beauty.GetLineIndent(Src,CurPos.StartPos);
    if Indent<LastIndent then
      LastIndent:=Indent;
    ReadNextAtom;
    NeedCompletion:=0;
    if (CurPos.StartPos>SrcLen) then begin
      { For example:
          TMyClass = class
          <EOF>
      }
      NeedCompletion:=CleanCursorPos;
    end else if CurPos.Flag=cafWord then begin
      if AtomIsIdentifier then begin
        ReadNextAtom;
        if CurPos.Flag=cafEqual then begin
          { For example:
              TMyClass = class

              TIdentifier =
          }
          NeedCompletion:=CleanCursorPos;
        end else
          exit(true);
      end else begin
        Indent:=Beauty.GetLineIndent(Src,CurPos.StartPos);
        if Indent<LastIndent then begin
          { For example:
                TMyClass = class

              type
          }
          NeedCompletion:=CleanCursorPos;
        end;
      end;
    end else
      exit(true);
    //debugln(['CompleteClassSection NeedCompletion=',NeedCompletion]);
    if NeedCompletion>0 then begin
      InsertPos:=NeedCompletion;
      Result:=Replace('end;',InsertPos,InsertPos,LastIndent,
        gtNewLine,gtEmptyLine,
        [bcfIndentExistingLineBreaks]);
    end else
      Result:=true;
  end;

  function CompleteClassInterface: Boolean;
  {  type
       TMyClass = interface
         |
  }
  var
    LastIndent: LongInt;
    Indent: LongInt;
    InsertPos: LongInt;
  begin
    Result:=false;
    if CleanCursorPos<StartNode.StartPos then exit;
    LastIndent:=Beauty.GetLineIndent(Src,StartNode.StartPos);
    MoveCursorToNodeStart(StartNode);
    ReadNextAtom;
    if CleanCursorPos<CurPos.EndPos then exit(true);
    ReadNextAtom;
    if CurPos.Flag=cafEnd then exit(true);
    if CleanCursorPos<=CurPos.StartPos then begin
      Indent:=Beauty.GetLineIndent(Src,CurPos.StartPos);
      InsertPos:=CleanCursorPos;
      if Indent<LastIndent then begin
        if not Replace('end;',InsertPos,InsertPos,LastIndent,
          gtNewLine,gtEmptyLine,
          [bcfIndentExistingLineBreaks])
        then
          exit;
      end;
    end;
    Result:=true;
  end;

  function CompleteRecord: Boolean;
  {  type
       TMyClass = record
         |
  }
  var
    LastIndent: LongInt;
    Indent: LongInt;
    InsertPos: LongInt;
  begin
    Result:=false;
    if CleanCursorPos<StartNode.StartPos then exit;
    LastIndent:=Beauty.GetLineIndent(Src,StartNode.StartPos);
    MoveCursorToNodeStart(StartNode);
    ReadNextAtom; // record
    if CleanCursorPos<CurPos.EndPos then exit(true);
    ReadNextAtom;
    if CurPos.Flag=cafEnd then exit(true);
    if CleanCursorPos<=CurPos.StartPos then begin
      Indent:=Beauty.GetLineIndent(Src,CurPos.StartPos);
      InsertPos:=CleanCursorPos;
      if Indent<=LastIndent then begin
        if not Replace('end;',InsertPos,InsertPos,LastIndent,
          gtNewLine,gtEmptyLine,
          [bcfIndentExistingLineBreaks])
        then
          exit;
      end;
    end;
    Result:=true;
  end;

var
  Stack: TBlockStack;
  CommentStart, CommentEnd: integer;
begin
  Result:=false;
  NewPos:=CursorPos;
  NewTopLine:=-1;
  BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,
                          [btSetIgnoreErrorPos]);
  StartNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if CleanPosIsInComment(CleanCursorPos,StartNode.StartPos,CommentStart,CommentEnd) then begin
    {$IFDEF VerboseCompleteBlock}
    debugln(['TStandardCodeTool.CompleteBlock cursor (',CursorPos.Y,',',CursorPos.X,') cleanpos=[',CleanPosToStr(CleanCursorPos),'] in comment Start=[',CleanPosToStr(CommentStart),'] End=[',CleanPosToStr(CommentEnd),']']);
    {$ENDIF}
    exit;
  end;

  InternalCursorAtEmptyLine:=ebNone;
  SourceChangeCache.MainScanner:=Scanner;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  InitStack(Stack);
  try
    {$IFDEF VerboseCompleteBlock}
    DebugLn(['TStandardCodeTool.CompleteBlock ',StartNode.DescAsString]);
    {$ENDIF}

    if StartNode.Desc in AllPascalStatements then begin
      while (StartNode.Parent<>nil)
      and (StartNode.Parent.Desc in AllPascalStatements) do
        StartNode:=StartNode.Parent;
      if not CompleteStatements(Stack) then exit;
    end
    else if (StartNode.Desc in AllClassSections)
    or ((StartNode.Desc in AllClassSubSections) and (StartNode.Parent.Desc in AllClassSections))
    then begin
      if not CompleteClassSection then exit;
    end
    else if StartNode.Desc in AllClassInterfaces then begin
      if not CompleteClassInterface then exit;
    end
    else if StartNode.Desc=ctnRecordType then begin
      if not CompleteRecord then exit;
    end;
  finally
    FreeStack(Stack);
  end;

  Result:=true;
end;

function TStandardCodeTool.GuessMisplacedIfdefEndif(
  const CursorPos: TCodeXYPosition;
  out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
var
  StartCursorPos, EndCursorPos: integer;
  StartCode, EndCode: Pointer;
begin
  Result:=false;
  try
    BeginParsing(lsrEnd);
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
      if JumpSingleLinePos>0 then begin
        NewTopLine:=NewPos.Y-(VisibleEditorLines*JumpSingleLinePos div 100);
        if NewTopLine<1 then NewTopLine:=1;
      end else
        NewTopLine:=NewPos.Y;
    end;
  end;
end;

function TStandardCodeTool.FindEnclosingIncludeDirective(
  const CursorPos: TCodeXYPosition; out NewPos: TCodeXYPosition; out
  NewTopLine: integer): boolean;
var
  CleanCursorPos, LinkIndex, NewCleanPos: integer;
begin
  Result:=false;
  NewPos:=CleanCodeXYPosition;
  NewTopLine:=-1;
  try
    BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,
                            [btSetIgnoreErrorPos]);
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
  out ACleanPos: integer): boolean;
var
  ParamPos: Integer;
begin
  Result:=false;
  ACleanPos:=0;
  if DoBuildTree then BuildTree(lsrMainUsesSectionStart);
  ACleanPos:=FindNextCompilerDirectiveWithName(Src,1,'Mode',
                                               Scanner.NestedComments,ParamPos);
  if ParamPos=0 then ;
  Result:=(ACleanPos>0) and (ACleanPos<=SrcLen);
end;

function TStandardCodeTool.FindResourceDirective(DoBuildTree: boolean;
  var ACleanPos: integer; const Filename: string): boolean;
var
  ParamPos: Integer;
  FilenameStartPos: Integer;
  FilenameEndPos: LongInt;
begin
  Result:=false;
  if DoBuildTree then BuildTree(lsrEnd);
  ACleanPos:=1;
  repeat
    ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
      Scanner.NestedComments,ParamPos);
    if ParamPos=0 then ;
    if (ACleanPos<1) or (ACleanPos>SrcLen) then
      exit(false);
    if Filename='' then begin
      // searching any filename -> found
      exit(true);
    end;
    FilenameStartPos:=ACleanPos+length('{$R ');
    FilenameEndPos:=FilenameStartPos;
    while (FilenameEndPos<=SrcLen) and (Src[FilenameEndPos]<>'}') do
      inc(FilenameEndPos);
    if CompareText(PChar(Pointer(Filename)),length(Filename),
                   @Src[FilenameStartPos],FilenameEndPos-FilenameStartPos,
                   true,false)=0
    then begin
      // filename found
      exit(true);
    end;
    ACleanPos:=FilenameEndPos+1;
  until ACleanPos>SrcLen;
end;

function TStandardCodeTool.FindResourceDirective(
  const CursorPos: TCodeXYPosition; out NewPos: TCodeXYPosition;
  out NewTopLine: integer; const Filename: string): boolean;
var
  CleanCursorPos: integer;
begin
  Result:=false;
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
  if not FindResourceDirective(false,CleanCursorPos,Filename) then begin
    //DebugLn('TStandardCodeTool.FindResourceDirective resource directive not found');
    exit;
  end;
  Result:=CleanPosToCaretAndTopLine(CleanCursorPos,NewPos,NewTopLine);
end;

function TStandardCodeTool.AddResourceDirective(const Filename: string;
  SourceChangeCache: TSourceChangeCache; const NewSrc: string): boolean;
var
  ANode: TCodeTreeNode;
  Indent: LongInt;
  InsertPos: Integer;
  AddSrc: String;
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  BuildTree(lsrEnd);
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  // find an insert position
  ANode:=FindImplementationNode;
  if ANode<>nil then begin
    Indent:=Beauty.GetLineIndent(Src,ANode.StartPos);
    InsertPos:=ANode.StartPos+length('implementation');
  end else begin
    ANode:=FindMainBeginEndNode;
    if ANode<>nil then begin
      Indent:=Beauty.GetLineIndent(Src,ANode.StartPos);
      InsertPos:=ANode.StartPos;
    end else begin
      ANode:=FindMainUsesNode;
      if ANode<>nil then begin
        Indent:=Beauty.GetLineIndent(Src,ANode.StartPos);
        InsertPos:=ANode.StartPos;
      end else begin
        Indent:=0;
        InsertPos:=1;
      end;
    end;
  end;

  // insert directive
  SourceChangeCache.MainScanner:=Scanner;
  if NewSrc<>'' then
    AddSrc:=NewSrc
  else
    AddSrc:=Beauty.GetIndentStr(Indent)+'{$R '+Filename+'}';
  if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
    AddSrc) then exit;
  if not SourceChangeCache.Apply then exit;

  Result:=true;
end;

function TStandardCodeTool.FindIncludeDirective(DoBuildTree: boolean;
  var ACleanPos: integer; const Filename: string): boolean;
var
  FilenameStartPos: Integer;
  FilenameEndPos: LongInt;
  CommentStart: integer;
  CommentEnd: integer;
begin
  Result:=false;
  if DoBuildTree then BuildTree(lsrEnd);
  ACleanPos:=1;
  repeat
    ACleanPos:=FindNextIncludeDirective(Src,ACleanPos,Scanner.NestedComments,
                       FilenameStartPos,FilenameEndPos,CommentStart,CommentEnd);
    if (ACleanPos<1) or (ACleanPos>SrcLen) then
      exit(false);
    if Filename='' then begin
      // searching any filename -> found
      exit(true);
    end;
    if CompareText(PChar(Pointer(Filename)),length(Filename),
                   @Src[FilenameStartPos],FilenameEndPos-FilenameStartPos,
                   true,false)=0
    then begin
      // filename found
      exit(true);
    end;
    ACleanPos:=FilenameEndPos+1;
  until ACleanPos>SrcLen;
end;

function TStandardCodeTool.FindIncludeDirective(
  const CursorPos: TCodeXYPosition; out NewPos: TCodeXYPosition; out
  NewTopLine: integer; const Filename: string): boolean;
var
  CleanCursorPos: integer;
begin
  Result:=false;
  BuildTreeAndGetCleanPos(CursorPos,CleanCursorPos);
  if not FindIncludeDirective(false,CleanCursorPos,Filename) then begin
    //DebugLn('TStandardCodeTool.FindIncludeDirective resource directive not found');
    exit;
  end;
  Result:=CleanPosToCaretAndTopLine(CleanCursorPos,NewPos,NewTopLine);
end;

function TStandardCodeTool.AddIncludeDirective(const Filename: string;
  SourceChangeCache: TSourceChangeCache; const NewSrc: string): boolean;
begin
  Result:=AddIncludeDirectiveForInit(Filename,SourceChangeCache,NewSrc);
end;

function TStandardCodeTool.AddIncludeDirectiveForInit(const Filename: string;
  SourceChangeCache: TSourceChangeCache; const NewSrc: string): boolean;
var
  ANode: TCodeTreeNode;
  Indent: LongInt;
  InsertPos: Integer;
  AddSrc: String;
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  BuildTree(lsrEnd);
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  // find an insert position
  ANode:=FindInitializationNode;
  if ANode<>nil then begin
    Indent:=Beauty.GetLineIndent(Src,ANode.StartPos)+Beauty.Indent;
    InsertPos:=ANode.StartPos+length('initialization');
  end else begin
    ANode:=FindMainBeginEndNode;
    if ANode<>nil then begin
      MoveCursorToNodeStart(ANode);
      ReadNextAtom;
      //debugln(['TStandardCodeTool.AddIncludeDirective ',GetAtom]);
      Indent:=Beauty.GetLineIndent(Src,CurPos.StartPos)+Beauty.Indent;
      InsertPos:=CurPos.EndPos;
    end else begin
      debugln(['TStandardCodeTool.AddIncludeDirective ToDo: add initialization / begin..end']);
      exit;
    end;
  end;

  // insert directive
  SourceChangeCache.MainScanner:=Scanner;
  if NewSrc<>'' then
    AddSrc:=NewSrc
  else
    AddSrc:=Beauty.GetIndentStr(Indent)+'{$I '+Filename+'}';
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
    AddSrc) then exit;
  if not SourceChangeCache.Apply then exit;

  Result:=true;
end;

function TStandardCodeTool.AddUnitWarnDirective(WarnID, Comment: string;
  TurnOn: boolean; SourceChangeCache: TSourceChangeCache): boolean;
const
  DirectiveFlagValue: array[boolean] of string = ('off','on');
var
  ACleanPos, DirEndPos, InsertStartPos, MaxPos: Integer;
  Node: TCodeTreeNode;
  p, IDStartPos, IDEndPos, ParamPos: PChar;
  NewCode: String;
begin
  Result:=false;
  if WarnID='' then
    raise Exception.Create('TStandardCodeTool.AddUnitWarnDirective missing WarnID');
  InsertStartPos:=0;
  BuildTree(lsrMainUsesSectionStart);
  SourceChangeCache.MainScanner:=Scanner;

  // fix comment
  if Comment<>'' then begin
    for ACleanPos:=1 to length(Comment) do
      if Comment[ACleanPos] in [#0..#8,#11,#12,#14..#31,'{','}'] then
        Comment[ACleanPos]:='?';
    if not (Comment[1] in [' ',#9,#10,#13]) then Comment:=' '+Comment;
  end;

  // insert in front of first node after source name
  Node:=Tree.Root;
  MaxPos:=Node.StartPos;
  if Node.Desc in AllSourceTypes then
    Node:=Node.Next;
  if (Node<>nil) and (Node.Desc=ctnSrcName) then begin
    MaxPos:=Node.EndPos;
    Node:=Node.NextSkipChilds;
  end;
  if Node<>nil then
    MaxPos:=Node.StartPos;
  MaxPos:=FindLineEndOrCodeAfterPosition(MaxPos,true,true);

  // find existing directive for replacement
  ACleanPos:=1;
  repeat
    ACleanPos:=FindNextCompilerDirective(Src,ACleanPos,Scanner.NestedComments);
    if (ACleanPos<1) or (ACleanPos>MaxPos) then
      break;
    DirEndPos:=FindCommentEnd(Src,ACleanPos,Scanner.NestedComments)+1;
    p:=@Src[ACleanPos+2];
    if CompareIdentifiers(p,'warn')=0 then begin
      IDStartPos:=p+4;
      while IDStartPos^ in [' ',#9,#10,#13] do
        inc(IDStartPos);
      IDEndPos:=IDStartPos;
      while IDEndPos^ in ['0'..'9','A'..'Z','a'..'z','_'] do
        inc(IDEndPos);
      if CompareText(PChar(Pointer(WarnID)),length(WarnID),
                     IDStartPos,IDEndPos-IDStartPos,false)=0
      then begin
        // warn directive found
        p:=IDEndPos;
        while p^ in [' ',#9,#10,#13] do
          inc(p);
        ParamPos:=p;
        while p^ in ['+','-','a'..'z','A'..'Z'] do
          inc(p);
        if not SourceChangeCache.Replace(gtSpace,gtNone,
          ParamPos-PChar(Src)+1,p-PChar(Src)+1,DirectiveFlagValue[TurnOn])
        then
          exit;
        Result:=SourceChangeCache.Apply;
        exit;
      end;
    end else if (CompareIdentifiers(p,'i')=0) or (CompareIdentifiers(p,'include')=0)
    then begin
      // insert before include file
      if MaxPos>ACleanPos then MaxPos:=ACleanPos;
      break;
    end;
    ACleanPos:=DirEndPos;
  until ACleanPos>MaxPos;

  // there was no such directive yet -> find nice insert pos
  InsertStartPos:=FindLineEndOrCodeInFrontOfPosition(MaxPos,true,true);
  NewCode:='{$WARN '+WarnID+' '+DirectiveFlagValue[TurnOn]+Comment+'}';
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,
    InsertStartPos,InsertStartPos,NewCode)
  then
    exit;
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.FixIncludeFilenames(Code: TCodeBuffer;
  SourceChangeCache: TSourceChangeCache;
  out FoundIncludeFiles: TStrings;
  var MissingIncludeFilesCodeXYPos: TFPList): boolean;
var
  ASource: String;
  
  {procedure WriteMissingIncludeFilesCodeXYPos;
  var
    CodePos: PCodeXYPosition;
    i: Integer;
  begin
    if MissingIncludeFilesCodeXYPos<>nil then begin
      for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
        CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
        DebugLn('TStandardCodeTool.FixMissingUnits ',dbgs(CodePos));
        DebugLn('TStandardCodeTool.FixMissingUnits ',CodePos^.Code.Filename);
        debugln(CodePos^.Code.Filename
             +'('+IntToStr(CodePos^.y)+','+IntToStr(CodePos^.x)+')'
             +' missing include file');
      end;
    end;
  end;}
  
  procedure Add(FilenameSrcPos: integer; const AFilename: string; Found: boolean);
  var
    NewFilename: String;
    p: PCodeXYPosition;
  begin
    if Found then begin
      if FoundIncludeFiles=nil then
        FoundIncludeFiles:=TStringList.Create;
      NewFilename:=TrimFilename(AFilename);
      if FoundIncludeFiles.IndexOf(NewFilename)<0 then
        FoundIncludeFiles.Add(NewFilename);
    end else begin
      if MissingIncludeFilesCodeXYPos=nil then
        MissingIncludeFilesCodeXYPos:=TFPList.Create;
      New(p);
      p^.Code:=Code;
      Code.AbsoluteToLineCol(FilenameSrcPos,p^.y,p^.x);
      MissingIncludeFilesCodeXYPos.Add(p);
      ///DebugLn('TStandardCodeTool.FixIncludeFilenames.Add "',p^.Code.Filename,'" ',dbgs(p),' X=',dbgs(p^.X),' Y=',dbgs(p^.Y));
      //WriteMissingIncludeFilesCodeXYPos;
    end;
  end;
  
  function SearchIncludeFilename(FilenameSrcPos: integer; const AFilename: string): string;
  var
    AFilePath: String;
    BaseDir: String;
    CurFilename: String;
    IncludePath: String;
    PathDivider: String;
    ACodeBuf: TCodeBuffer;
  begin
    Result:=TrimFilename(AFilename);
    if FilenameIsAbsolute(Result) then begin
      Result:=DirectoryCache.Pool.FindDiskFilename(Result,true);
      Add(FilenameSrcPos,Result,FileExistsCached(Result));
      //DebugLn('SearchIncludeFilename AbsoluteFilename="',Result,'"');
    end else begin
      BaseDir:=ExtractFilePath(MainFilename);
      //DebugLn('SearchIncludeFilename BaseDir="',BaseDir,'"');
      if FilenameIsAbsolute(BaseDir) then begin
        // unit has normal path -> not virtual
        AFilePath:=ExtractFilePath(Result);
        if AFilePath<>'' then begin
          // search relative to unit
          //debugln(['SearchIncludeFilename BaseDir+Result=',BaseDir+Result]);
          CurFilename:=DirectoryCache.Pool.FindDiskFilename(BaseDir+Result,true);
          //debugln(['SearchIncludeFilename DiskFilename=',CurFilename]);
          Result:=CreateRelativePath(CurFilename,BaseDir);
          //debugln(['SearchIncludeFilename RelativeDiskFilename=',Result]);
          if FileExistsCached(CurFilename) then
            Add(FilenameSrcPos,CurFilename,true)
          else
            Add(FilenameSrcPos,Result,false);
          //DebugLn('SearchIncludeFilename relative filename="',CurFilename,'"');
        end else begin
          // search in path
          IncludePath:='';
          PathDivider:=':;';
          if (Scanner.Values<>nil) then begin
            IncludePath:=Scanner.Values.Variables[ExternalMacroStart+'INCPATH'];
            if Scanner.Values.IsDefined('DELPHI') then
              PathDivider:=':'
          end;
          CurFilename:=SearchFileInPath(Result,BaseDir,IncludePath,PathDivider, ctsfcAllCase);
          if CurFilename<>'' then begin
            // found
            Result:=CreateRelativePath(CurFilename,BaseDir);
            Add(FilenameSrcPos,CurFilename,true);
          end else begin
            // not found
            Add(FilenameSrcPos,Result,false);
          end;
          //DebugLn('SearchIncludeFilename search in include path="',IncludePath,'" Result="',Result,'"');
        end;
      end else begin
        // unit has no path -> virtual unit -> search in virtual files
        ACodeBuf:=TCodeBuffer(Scanner.LoadSourceCaseLoUp(Result));
        if ACodeBuf<>nil then begin
          Result:=ACodeBuf.Filename;
          Add(FilenameSrcPos,Result,true);
        end else begin
          Add(FilenameSrcPos,Result,false);
        end;
      end;
    end;
  end;

  procedure FixFilename(StartPos, EndPos: integer);
  var
    OldFilename: String;
    AFilename: String;
  begin
    OldFilename:=GetForcedPathDelims(copy(ASource,StartPos,EndPos-StartPos));
    //DebugLn('FixFilename ',dbgs(StartPos),' ',dbgs(EndPos),' ',OldFilename);
    AFilename:=OldFilename;
    if ExtractFileExt(AFilename)='' then begin
      // add default extension
      if (Scanner.CompilerMode=cmDELPHI) then
        AFilename:=AFilename+'.pas'
      else
        AFilename:=AFilename+'.pp';
    end;
    AFilename:=SearchIncludeFilename(StartPos,AFilename);
    if OldFilename<>AFilename then begin
      DebugLn('TStandardCodeTool.FixIncludeFilenames.FixFilename replacing in '+Code.Filename+' include directive "',OldFilename,'" with "',AFilename,'"');
      SourceChangeCache.ReplaceEx(gtNone,gtNone,0,0,Code,StartPos,EndPos,AFilename);
    end;
  end;
  
var
  p: Integer;
  NestedComments: Boolean;
  FilenameStartPos, FileNameEndPos, CommentStartPos, CommentEndPos: integer;
begin
  Result:=false;
  FoundIncludeFiles:=nil;
  if (Scanner=nil) or (Scanner.MainCode=nil) then exit;
  ASource:=Code.Source;
  Scanner.Scan(lsrInit,false);
  SourceChangeCache.MainScanner:=Scanner;
  
  Result:=true;
  NestedComments:=Scanner.NestedComments;
  p:=1;
  repeat
    p:=BasicCodeTools.FindNextIncludeDirective(ASource,p,NestedComments,
              FilenameStartPos, FileNameEndPos, CommentStartPos, CommentEndPos);
    if (p<1) or (p>length(ASource)) then break;
    if (CommentStartPos=0) and (CommentEndPos=0) then ;
    FixFilename(FilenameStartPos,FilenameEndPos);
    p:=FindCommentEnd(ASource,p,NestedComments);
    //DebugLn('TStandardCodeTool.FixIncludeFilenames ',dbgs(p));
  until false;
  //WriteMissingIncludeFilesCodeXYPos;

  Result:=SourceChangeCache.Apply;
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
    if BlockKeywordFuncList.DoItCaseInsensitive(Src,
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
            MoveCursorToCleanPos(SrcLen+1);
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
            if AtomIsChar(';') then begin
              // forward class
            end else if ((CurBlockWord=bkwClass) and UpAtomIs('OF')) then begin
              // 'class of'
            end else if ((CurBlockWord=bkwClass)
            and (UpAtomIs('FUNCTION') or UpAtomIs('PROCEDURE'))) then begin
              // 'class procedure'
            end else if ((CurBlockWord=bkwObject) and LastUpAtomIs(0,'OF')) then
            begin
              // or 'of object'
            end else begin
              BlockType:=CurBlockWord;
              BlockStart:=LastAtoms.GetPriorAtom.StartPos;
              // read ancestor list  class(...)
              if CurPos.Flag=cafRoundBracketOpen then begin
                repeat
                  ReadNextAtom;
                  if AtomIsIdentifier then begin
                    ReadNextAtom;
                    if CurPos.Flag=cafPoint then begin
                      ReadNextAtom;
                      AtomIsIdentifierE;
                    end;
                  end;
                  if CurPos.Flag=cafRoundBracketClose then break;
                  if CurPos.Flag<>cafComma then begin
                    exit(false);
                  end;
                until false;
                ReadNextAtom;
              end;
              // a semicolon directly behind the ancestor list ends the class
              if (CurPos.Flag in [cafEnd,cafSemicolon]) then begin
                // class ends
                BlockType:=bkwNone;
              end else begin
                // class continues
                UndoReadNextAtom;
              end;
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
        and (Beautifier.GetLineIndent(Src,CurPos.StartPos)<>Beautifier.GetLineIndent(Src,BlockStart))
        then begin
          // different indent -> unclosed block found
          if Beautifier.GetLineIndent(Src,BlockStart)>Beautifier.GetLineIndent(Src,CurPos.StartPos)
          then begin
            // the current block is more indented than the next block
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
        and (Beautifier.GetLineIndent(Src,CurPos.StartPos)<>Beautifier.GetLineIndent(Src,BlockStart))
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
        if Beautifier.GetLineIndent(Src,BlockStart)>=Beautifier.GetLineIndent(Src,CurPos.StartPos)
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

function TStandardCodeTool.RemoveApplicationScaledStatement(
  SourceChangeCache: TSourceChangeCache): boolean;
begin
  Result := RemoveApplicationStatement('SCALED', SourceChangeCache);
end;

function TStandardCodeTool.RemoveApplicationStatement(const APropertyUpCase: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  StartPos, ConstStartPos, EndPos: integer;
  OldExists: Boolean;
  FromPos: Integer;
  ToPos: Integer;
begin
  Result:=false;
  // search old Application.XYZ:= statement
  OldExists:=FindApplicationStatement(APropertyUpCase,StartPos,ConstStartPos,EndPos);
  if not OldExists then
    exit(true);
  // -> delete whole line
  FromPos:=FindLineEndOrCodeInFrontOfPosition(StartPos);
  ToPos:=FindLineEndOrCodeAfterPosition(EndPos);
  SourceChangeCache.MainScanner:=Scanner;
  if not SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'') then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.ReadForwardTilAnyBracketClose: boolean;
// this function reads any bracket
// (the ReadTilBracketClose function reads only brackets in code, not comments)
var OpenBracket: char;
begin
  Result:=false;
  OpenBracket:=Src[CurPos.StartPos];
  if OpenBracket='{' then begin
    // read til end of comment
    MoveCursorToCleanPos(FindCommentEnd(Src,CurPos.StartPos,Scanner.NestedComments));
    Result:=CurPos.StartPos<=SrcLen;
  end else if OpenBracket='(' then begin
    if (CurPos.StartPos<SrcLen) and (Src[CurPos.StartPos+1]='*') then begin
      // read til end of comment
      MoveCursorToCleanPos(FindCommentEnd(Src,CurPos.StartPos,Scanner.NestedComments));
      Result:=CurPos.StartPos<=SrcLen;
    end else begin
      // round bracket operator
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
    dec(CurPos.StartPos);
    if (CurPos.StartPos>0) and (Src[CurPos.StartPos]=#3) then begin
      // codetools skip comment
      dec(CurPos.StartPos);
      while (CurPos.StartPos>=1) do begin
        if (Src[CurPos.StartPos]=#3) and (CurPos.StartPos>1)
        and (Src[CurPos.StartPos-1]='}') then begin
          dec(CurPos.StartPos,2);
          break;
        end;
        dec(CurPos.StartPos);
      end;
    end else begin
      // pascal comment
      CommentLvl:=1;
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
    end;
  end else if OpenBracket=')' then begin
    if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]='*') then begin
      // read til end of comment
      dec(CurPos.StartPos,3);
      while true do begin
        if (CurPos.StartPos>=1)
        and ((Src[CurPos.StartPos+1]='*') and (Src[CurPos.StartPos]='(')) then
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

function TStandardCodeTool.Explore(WithStatements: boolean;
  Range: TLinkScannerRange): boolean;
var
  Node: TCodeTreeNode;
begin
  Result:=true;
  BuildTree(Range);
  Node:=Tree.Root;
  while Node<>nil do begin
    case Node.Desc of
    ctnProcedure,ctnProcedureHead:
      BuildSubTreeForProcHead(Node);
    ctnBeginBlock:
      if WithStatements then
        BuildSubTreeForBeginBlock(Node);
    ctnImplementation:
      if ord(Range)<ord(lsrImplementationStart) then exit;
    end;
    Node:=Node.Next;
  end;
end;

function TStandardCodeTool.Explore(WithStatements: boolean;
  OnlyInterface: boolean): boolean;
begin
  if OnlyInterface then
    Result:=Explore(WithStatements,lsrImplementationStart)
  else
    Result:=Explore(WithStatements,lsrEnd);
end;

finalization
  FreeAndNil(BlockKeywordFuncList);

end.


