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

  ToDo:
    -Insert class method body in pipClassOrder
    
}
unit StdCodeTools;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, FindDeclarationTool, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  SourceChanger;

type
  TStandardCodeTool = class(TFindDeclarationTool)
  public
    // source name  e.g. 'unit UnitName;'
    function GetSourceNamePos(var NamePos: TAtomPosition): boolean;
    function GetSourceName: string;
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
    function ListAllCreateFormStatements: TStrings;
    function SetAllCreateFromStatements(List: TStrings;
          SourceChangeCache: TSourceChangeCache): boolean;    

    // form components
    function FindPublishedVariable(const UpperClassName,
          UpperVarName: string): TCodeTreeNode;
    function AddPublishedVariable(const UpperClassName,VarName, VarType: string;
          SourceChangeCache: TSourceChangeCache): boolean;
    function RemovePublishedVariable(const UpperClassName, UpperVarName: string;
          SourceChangeCache: TSourceChangeCache): boolean;
  end;


implementation


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
  Result:=(NamePos.StartPos<SrcLen);
end;

function TStandardCodeTool.GetSourceName: string;
var NamePos: TAtomPosition;
begin
  Result:='';
  if not GetSourceNamePos(NamePos) then exit;
  Result:=copy(Src,NamePos.StartPos,NamePos.EndPos-NamePos.StartPos);
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
    if SectionNode.Desc in [ctnProgram, ctnPackage,ctnLibrary, ctnInterface,
       ctnImplementation] then
    begin
      UsesNode:=SectionNode.FirstChild;
      if FindUnitInUsesSection(UsesNode,UpperUnitName,NamePos,InPos) then begin
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
  NewUnitTerm: string;
begin
  Result:=false;
  if (OldUpperUnitName='') or (length(OldUpperUnitName)>255) or (NewUnitName='')
  or (length(NewUnitName)>255) then exit;
  if not FindUnitInAllUsesSections(OldUpperUnitName,UnitPos,InPos) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  if InPos.StartPos>0 then UnitPos.EndPos:=InPos.EndPos;
  NewUnitTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUnitTerm:=NewUnitTerm+' in '''+NewUnitInFile+'''';
  if ReplacementNeedsLineEnd(Src,UnitPos.StartPos,UnitPos.EndPos,
    length(NewUnitTerm),SourceChangeCache.BeautifyCodeOptions.LineLength) then
  begin
    if not SourceChangeCache.Replace(gtNewLine,gtNone,
      UnitPos.StartPos,UnitPos.EndPos,NewUnitTerm) then exit;
  end else begin
    if not SourceChangeCache.Replace(gtSpace,gtNone,
      UnitPos.StartPos,UnitPos.EndPos,NewUnitTerm) then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.AddUnitToUsesSection(UsesNode: TCodeTreeNode;
  const NewUnitName, NewUnitInFile: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var LineStart, LineEnd, Indent, InsertPos: integer;
  NewUnitTerm: string;
begin
  Result:=false;
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) or (NewUnitName='')
  or (length(NewUnitName)>255) or (UsesNode.StartPos<1)
  or (UsesNode.EndPos<1) then exit;
  SourceChangeCache.MainScanner:=Scanner;
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom; // read first name
  Indent:=GetLineIndent(Src,CurPos.StartPos);
  if Indent<SourceChangeCache.BeautifyCodeOptions.Indent then
    Indent:=SourceChangeCache.BeautifyCodeOptions.Indent;
  InsertPos:=UsesNode.EndPos-1;
  NewUnitTerm:=NewUnitName;
  if NewUnitInFile<>'' then
    NewUnitTerm:=NewUnitTerm+' in '''+NewUnitInFile+'''';
  GetLineStartEndAtPosition(Src,InsertPos,LineStart,LineEnd);
  if InsertPos-LineStart+length(NewUnitTerm)+2>=
    SourceChangeCache.BeautifyCodeOptions.LineLength then begin
    NewUnitTerm:=','+SourceChangeCache.BeautifyCodeOptions.LineEnd+
      GetIndentStr(Indent)+NewUnitTerm;
  end else
    NewUnitTerm:=', '+NewUnitTerm;
  if not SourceChangeCache.Replace(gtNone,gtNone,InsertPos,InsertPos,
                                    NewUnitTerm) then exit;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.AddUnitToMainUsesSection(const NewUnitName,
  NewUnitInFile: string; SourceChangeCache: TSourceChangeCache): boolean;
var UsesNode, SectionNode: TCodeTreeNode;
  NewUnitTerm: string;
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
    NewUnitTerm:=SourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('uses')
         +' '+NewUnitName;
    if NewUnitInFile<>'' then
      NewUnitTerm:=NewUnitTerm+' in '''+NewUnitInFile+''';'
    else
      NewUnitTerm:=NewUnitTerm+';';
    InsertPos:=CurPos.EndPos;
    if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
      NewUnitTerm) then exit;
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
    if not AtomIsWord then exit;
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
    if (SectionNode.Desc in [ctnProgram,ctnPackage,ctnLibrary,ctnInterface,
         ctnImplementation]) then begin
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
      ResourceCode,ResourceCode.SourceLength+1,ResourceData) then exit;
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
    OldPosition.StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,
         OldPosition.StartPos,Scanner.NestedComments);
    OldPosition.EndPos:=FindFirstLineEndAfterInCode(Src,OldPosition.EndPos,
         Scanner.NestedComments);
    if not SourceChangeCache.Replace(gtNone,gtNone,
      OldPosition.StartPos,OldPosition.EndPos,'') then exit;
  end;
  if not SourceChangeCache.Apply then exit;
  Result:=true;
end;

function TStandardCodeTool.RenameInclude(LinkIndex: integer;
  const NewFilename: string; KeepPath: boolean;
  SourceChangeCache: TSourceChangeCache): boolean;
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
    while (FileNameStart>FileStart) and (Src[FileNameStart]<>OSDirSeparator) do
      dec(FileNameStart);
    if Src[FileNameStart]=OSDirSeparator then
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
    FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,FromPos,
                    Scanner.NestedComments);
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,FromPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+');',Indent));
  end else begin
    FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,OldPosition.StartPos,
                                         Scanner.NestedComments);
    ToPos:=FindFirstLineEndAfterInCode(Src,OldPosition.EndPos,
                                       Scanner.NestedComments);
    SourceChangeCache.MainScanner:=Scanner;
    SourceChangeCache.Replace(gtNewLine,gtNewLine,FromPos,ToPos,
       SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
         'Application.CreateForm('+AClassName+','+AVarName+')',2));
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
  FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,Position.StartPos,
                                       Scanner.NestedComments);
  ToPos:=FindFirstLineEndAfterInCode(Src,Position.EndPos,
                                     Scanner.NestedComments);
  SourceChangeCache.MainScanner:=Scanner;
  SourceChangeCache.Replace(gtNone,gtNone,FromPos,ToPos,'');
  Result:=SourceChangeCache.Apply;
end;

function TStandardCodeTool.ListAllCreateFormStatements: TStrings;
// list format: VarName:ClassName
var Position: integer;
  StatementPos: TAtomPosition;
  s:string;
  var MainBeginNode: TCodeTreeNode;
begin
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
    StatementPos.StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,
       StatementPos.StartPos,Scanner.NestedComments);
    InsertPos:=StatementPos.StartPos;
    StatementPos.EndPos:=FindFirstLineEndAfterInCode(Src,
       StatementPos.EndPos,Scanner.NestedComments);
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
  InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,SectionNode.EndPos,
               Scanner.NestedComments);
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
      FromPos:=FindLineEndOrCodeInFrontOfPosition(Src,VarNode.StartPos,
                      Scanner.NestedComments);
      ToPos:=FindFirstLineEndAfterInCode(Src,VarNode.EndPos,
                      Scanner.NestedComments);
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



end.

