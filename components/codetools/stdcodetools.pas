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
    TStandardCodeTool enhances TFindDeclarationTool with many standard code
    editing functions for the following categories:
      - source name
      - uses sections
      - lazarus resources
      - Application.CreateForm statements
      - published variables

}
unit StdCodeTools;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom,
  FindDeclarationTool, PascalParserTool, SourceLog, KeywordFuncLists,
  BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo, SourceChanger,
  CustomCodeTool;

type
  TStandardCodeTool = class(TFindDeclarationTool)
  private
    function ReadTilGuessedUnclosedBlock(MinCleanPos: integer;
      ReadOnlyOneBlock: boolean): boolean;
    function ReadForwardTilAnyBracketClose: boolean;
    function ReadBackwardTilAnyBracketClose: boolean;
  public
    // search & replace
    function ReplaceIdentifiers(IdentList: TStrings;
          SourceChangeCache: TSourceChangeCache): boolean;

    // source name  e.g. 'unit UnitName;'
    function GetSourceNamePos(var NamePos: TAtomPosition): boolean;
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

    // createform
    function FindCreateFormStatement(StartPos: integer;
          const UpperClassName, UpperVarName: string;
          var Position: TAtomPosition): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(const AClassName,
          AVarName: string; SourceChangeCache: TSourceChangeCache): boolean;
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

    // forms
    function RenameForm(const OldFormName, OldFormClassName: string;
          const NewFormName, NewFormClassName: string;
          SourceChangeCache: TSourceChangeCache): boolean;

    // form components
    function FindPublishedVariable(const UpperClassName,
          UpperVarName: string): TCodeTreeNode;
    function AddPublishedVariable(const UpperClassName,VarName, VarType: string;
          SourceChangeCache: TSourceChangeCache): boolean; virtual;
    function RemovePublishedVariable(const UpperClassName, UpperVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedVariable(const UpperClassName,
          UpperOldVarName: string; const NewVarName, VarType: shortstring;
          SourceChangeCache: TSourceChangeCache): boolean;

    // blocks (e.g. begin..end)
    function FindBlockCounterPart(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindBlockStart(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function GuessUnclosedBlock(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
      
    // compiler directives
    function GuessMisplacedIfdefEndif(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindEnclosingIncludeDirective(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
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

function TStandardCodeTool.GetSourceNamePos(
  var NamePos: TAtomPosition): boolean;
begin
  Result:=false;
  BuildTree(true);
  NamePos.StartPos:=-1;
  if Tree.Root=nil then exit;
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read source type 'program', 'unit' ...
  ReadNextAtom; // read name
  NamePos:=CurPos;
  Result:=(NamePos.StartPos<=SrcLen);
end;

function TStandardCodeTool.GetSourceName: string;
var NamePos: TAtomPosition;
begin
  Result:='';
  if not GetSourceNamePos(NamePos) then exit;
  Result:=copy(Src,NamePos.StartPos,NamePos.EndPos-NamePos.StartPos);
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
  Result:='';
  if Tree.Root=nil then exit;
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read source type 'program', 'unit' ...
  ReadNextAtom; // read name
  if (CurPos.StartPos<=SrcLen) then
    Result:=GetAtom;
end;

function TStandardCodeTool.RenameSource(const NewName: string;
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

function TStandardCodeTool.FindUnitInUsesSection(UsesNode: TCodeTreeNode;
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
  if (Result<>nil) and (Result.Desc<>ctnUsesSection) then Result:=nil;
end;

function TStandardCodeTool.FindImplementationUsesSection: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then exit;
  while (Result<>nil) and (Result.Desc<>ctnImplementation) do
    Result:=Result.NextBrother;
  if Result=nil then exit;
  Result:=Result.FirstChild;
  if (Result=nil) or (Result.Desc<>ctnUsesSection) then exit;
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
  Result:=true;
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) do begin
    if (SectionNode.Desc in [ctnProgram,ctnInterface,ctnImplementation]) then
    begin
      if RemoveUnitFromUsesSection(SectionNode.FirstChild,UpperUnitName,
         SourceChangeCache) then begin
        Result:=RemoveUnitFromAllUsesSections(UpperUnitName,SourceChangeCache);
        exit;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
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
  ResourceCode: TCodeBuffer;  const ResourceName: string): TAtomPosition;
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
  var MainBeginNode: TCodeTreeNode;
  AClassName, AVarName: string;
begin
  Result:=false;
  if (List=nil) or (SourceChangeCache=nil) then exit;
  BuildTree(false);
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
    StatementPos.StartPos:=FindLineEndOrCodeInFrontOfPosition(
       StatementPos.StartPos);
    InsertPos:=StatementPos.StartPos;
    StatementPos.EndPos:=FindFirstLineEndAfterInCode(StatementPos.EndPos);
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
  IdentList:=TStringList.Create;
  try
    IdentList.Add(OldFormName);
    IdentList.Add(NewFormName);
    IdentList.Add(OldFormClassName);
    IdentList.Add(NewFormClassName);
    Result:=ReplaceIdentifiers(IdentList,SourceChangeCache);
  finally
    IdentList.Free;
  end;
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
            if BasicCodeTools.CompareIdentifiers(PChar(IdentList[i]),
                                                 @CurSource[IdentStart])=0 then
            begin
              // identifier found -> replace
              IdentEnd:=IdentStart+length(IdentList[i]);
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

function TStandardCodeTool.FindPublishedVariable(const UpperClassName,
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

function TStandardCodeTool.AddPublishedVariable(const UpperClassName,
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
  InsertPos:=FindLineEndOrCodeInFrontOfPosition(SectionNode.EndPos);
  SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
          SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                     VarName+':'+VarType+';',Indent)
       );
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.RemovePublishedVariable(const UpperClassName,
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
  SourceChangeCache: TSourceChangeCache): boolean;
var TypeNode, VarNode: TCodeTreeNode;
begin
  Result:=false;
  VarNode:=FindPublishedVariable(UpperClassName,UpperOldVarName);
  if VarNode<>nil then begin
    // old variable found
    // check type
    TypeNode:=FindTypeNodeOfDefinition(VarNode);
    MoveCursorToNodeStart(TypeNode);
    ReadNextAtom;
    if AtomIs(VarType) then begin
      // rename the identifier
      MoveCursorToNodeStart(VarNode);
      ReadNextAtom;
      SourceChangeCache.MainScanner:=Scanner;
      if not SourceChangeCache.Replace(gtNone,gtNone,
        CurPos.StartPos,CurPos.EndPos,NewVarName)
      then
        exit;
    end else begin
      // auto correct type

      // ToDo: auto correct
      RaiseExceptionFmt(ctsStrExpectedButAtomFound,[VarType,GetAtom]);
      
    end;
    Result:=SourceChangeCache.Apply;
  end else begin
    // old variable not found -> add it
    Result:=AddPublishedVariable(UpperClassName,NewVarName, VarType,
                                 SourceChangeCache);
  end;
end;

function TStandardCodeTool.FindBlockCounterPart(CursorPos: TCodeXYPosition;
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
  end else begin;
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

function TStandardCodeTool.FindBlockStart(CursorPos: TCodeXYPosition;
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

function TStandardCodeTool.GuessUnclosedBlock(CursorPos: TCodeXYPosition;
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

function TStandardCodeTool.GuessMisplacedIfdefEndif(CursorPos: TCodeXYPosition;
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
  CursorPos: TCodeXYPosition; var NewPos: TCodeXYPosition;
  var NewTopLine: integer): boolean;
var
  CleanCursorPos, LinkIndex, NewCleanPos: integer;
begin
  Result:=false;
  BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos);
  LinkIndex:=Scanner.LinkIndexAtCleanPos(CleanCursorPos);
  LinkIndex:=Scanner.FindParentLink(LinkIndex);
  if LinkIndex<0 then
    // this is no include file
    exit;
  NewPos.Code:=TCodeBuffer(Scanner.Links[LinkIndex].Code);
  // calculate the directive end bracket
  NewCleanPos:=Scanner.Links[LinkIndex].CleanedPos+Scanner.LinkSize(LinkIndex)-1;
  Result:=CleanPosToCaretAndTopLine(NewCleanPos,NewPos,NewTopLine);
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


end.


