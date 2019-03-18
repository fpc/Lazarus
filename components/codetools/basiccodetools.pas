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
    Basic pascal code functions. Many of the functions have counterparts in the
    code tools, which are faster, more flexible and aware of compiler settings
    and directives.
}
unit BasicCodeTools;

{$ifdef FPC}
  {$mode objfpc}
{$else}
  {$ifdef MSWindows}
    {$define Windows}
  {$endif}
{$endif}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, strutils, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils, LazUTF8,
  // Codetools
  SourceLog, KeywordFuncLists, FileProcs;

//----------------------------------------------------------------------------
{ These functions are used by the codetools }

// comments
function FindNextNonSpace(const ASource: string; StartPos: integer): integer;
function FindPrevNonSpace(const ASource: string; StartPos: integer): integer;
function FindCommentEnd(const ASource: string; StartPos: integer;
    NestedComments: boolean): integer; overload;
function FindCommentEnd(Src: PChar; NestedComments: boolean): PChar; overload;
function IsCommentEnd(const ASource: string; EndPos: integer): boolean;
function FindNextComment(const ASource: string;
    StartPos: integer; MaxPos: integer = 0): integer;
procedure FindCommentsInRange(const Src: string; StartPos, EndPos: integer;
    out FirstCommentStart, FirstAtomStart, LastCommentEnd, LastAtomEnd: integer;
    NestedComments: boolean = false);
function FindNextCompilerDirective(const ASource: string; StartPos: integer;
    NestedComments: boolean): integer;
function FindNextCompilerDirectiveWithName(const ASource: string;
    StartPos: integer; const DirectiveName: string;
    NestedComments: boolean; out ParamPos: integer): integer;
function FindNextIncludeDirective(const ASource: string;
    StartPos: integer; NestedComments: boolean;
    out FilenameStartPos, FileNameEndPos,
    CommentStartPos, CommentEndPos: integer): integer;
function FindNextIDEDirective(const ASource: string; StartPos: integer;
    NestedComments: boolean; EndPos: integer = 0): integer;
function CleanCodeFromComments(const Src: string;
    NestedComments: boolean; KeepDirectives: boolean = false;
    KeepVerbosityDirectives: boolean = false): string;
function ExtractCommentContent(const ASource: string; CommentStart: integer;
    NestedComments: boolean;
    TrimStart: boolean = false; TrimEnd: boolean = false;
    TrimPasDoc: boolean = false): string;
function FindMainUnitHint(const ASource: string; out Filename: string): boolean;
function InEmptyLine(const ASource: string; StartPos: integer): boolean;
function SkipResourceDirective(const ASource: string; StartPos, EndPos: integer;
    NestedComments: boolean): integer;

// indent
function GetLineIndent(const Source: string; Position: integer): integer;
function GetLineIndentWithTabs(const Source: string; Position: integer;
                               TabWidth: integer): integer;
function GetPosInLine(const Source: string; Position: integer): integer; // 0 based
function GetBlockMinIndent(const Source: string;
    StartPos, EndPos: integer): integer;
function GetIndentStr(Indent: integer; TabWidth: integer = 0): string;
procedure IndentText(const Source: string; Indent, TabWidth: integer;
    out NewSource: string);
function FindFirstNonSpaceCharInLine(const Source: string;
    Position: integer): integer;
function IsFirstNonSpaceCharInLine(const Source: string;
    Position: integer): boolean;
procedure GuessIndentSize(const Source: string;
  var IndentSize: integer; TabWidth: integer = 2; MaxLineCount: integer = 10000);
function ReIndent(const Source: string; OldIndent, OldTabWidth,
  NewIndent, NewTabWidth: integer): string;

// identifiers
procedure GetIdentStartEndAtPosition(const Source:string; Position:integer;
    out IdentStart,IdentEnd:integer);
function GetIdentStartPosition(const Source:string; Position:integer): integer;
function GetIdentLen(Identifier: PChar): integer;
function GetIdentifier(Identifier: PChar): string;
function FindNextIdentifier(const Source: string; StartPos, MaxPos: integer): integer;
function FindNextIdentifierSkipStrings(const Source: string;
    StartPos, MaxPos: integer): integer;
function IsValidIdentPair(const NamePair: string): boolean;
function IsValidIdentPair(const NamePair: string; out First, Second: string): boolean;
function ExtractPasIdentifier(const Ident: string; AllowDots: Boolean): string;

// line/code ends
function SrcPosToLineCol(const s: string; Position: integer;
    out Line, Col: integer): boolean;
procedure GetLineStartEndAtPosition(const Source: string; Position:integer;
    out LineStart,LineEnd:integer); // LineEnd at first line break character
function GetLineStartPosition(const Source: string; Position:integer): integer;
function GetLineInSrc(const Source: string; Position:integer): string;
function LineEndCount(const Txt: string): integer; inline;
function LineEndCount(const Txt: string; out LengthOfLastLine:integer): integer; inline;
function LineEndCount(const Txt: string; StartPos, EndPos: integer;
                      out LengthOfLastLine:integer): integer;
function EmptyCodeLineCount(const Source: string; StartPos, EndPos: integer;
    NestedComments: boolean): integer;
function PositionsInSameLine(const Source: string;
    Pos1, Pos2: integer): boolean;
function FindLineEndOrCodeInFrontOfPosition(const Source: string;
    Position, MinPosition: integer; NestedComments: boolean;
    StopAtDirectives: boolean = true; SkipSemicolonComma: boolean = true;
    SkipEmptyLines: boolean = false): integer;
function FindLineEndOrCodeAfterPosition(const Source: string;
    Position, MaxPosition: integer; NestedComments: boolean;
    StopAtDirectives: boolean = true; SkipEmptyLines: boolean = false;
    IncludeLineEnd: boolean = false): integer;
function FindFirstLineEndInFrontOfInCode(const Source: string;
    Position, MinPosition: integer; NestedComments: boolean): integer;
function FindFirstLineEndAfterInCode(const Source: string;
    Position, MaxPosition: integer; NestedComments: boolean): integer;
function ChompLineEndsAtEnd(const s: string): string;
function ChompOneLineEndAtEnd(const s: string): string;
function TrimLineEnds(const s: string; TrimStart, TrimEnd: boolean): string;

// brackets
function GetBracketLvl(const Src: string; StartPos, EndPos: integer;
    NestedComments: boolean): integer;

// replacements
function ReplacementNeedsLineEnd(const Source: string;
    FromPos, ToPos, NewLength, MaxLineLength: integer): boolean;
function CountNeededLineEndsToAddForward(const Src: string;
    StartPos, MinLineEnds: integer): integer;
function CountNeededLineEndsToAddBackward(const Src: string;
    StartPos, MinLineEnds: integer): integer;
procedure AdjustPositionAfterInsert(var p: integer; IsStart: boolean;
                                    FromPos, ToPos, DiffPos: integer);

// comparison
function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
    CaseSensitive: boolean): integer; overload;
function CompareTextCT(const Txt1, Txt2: string;
    CaseSensitive: boolean = false): integer; overload;
function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
    CaseSensitive, IgnoreSpace: boolean): integer; overload;
function CompareTextIgnoringSpace(const Txt1, Txt2: string;
    CaseSensitive: boolean): integer;
function CompareTextIgnoringSpace(Txt1: PChar; Len1: integer;
    Txt2: PChar; Len2: integer; CaseSensitive: boolean): integer;
function CompareAnsiStringIgnoringSpaceIgnoreCase(Txt1, Txt2: pointer): integer;
function CompareSubStrings(const Find, Txt: string;
    FindStartPos, TxtStartPos, Len: integer; CaseSensitive: boolean): integer;
function CompareIdentifiers(Identifier1, Identifier2: PChar): integer; {$IFDEF HasInline}inline;{$ENDIF}
function CompareIdentifiersCaseSensitive(Identifier1, Identifier2: PChar): integer;
function CompareIdentifierPtrs(Identifier1, Identifier2: Pointer): integer; {$IFDEF HasInline}inline;{$ENDIF}
function ComparePrefixIdent(PrefixIdent, Identifier: PChar): boolean;
function TextBeginsWith(Txt: PChar; TxtLen: integer; StartTxt: PChar;
    StartTxtLen: integer; CaseSensitive: boolean): boolean;
function StrBeginsWith(const s, Prefix: string): boolean;
function IdentifierPos(Search, Identifier: PChar): PtrInt; // search Search in Identifier
function CompareAtom(p1, p2: PChar; NestedComments: boolean): integer;
function CompareStringConstants(p1, p2: PChar): integer; // compare case sensitive
function CompareComments(p1, p2: PChar; NestedComments: boolean): integer; // compare case insensitive
function FindDiff(const s1, s2: string): integer;
function dbgsDiff(Expected, Actual: string): string; overload;

// dotted identifiers
function DottedIdentifierLength(Identifier: PChar): integer;
function GetDottedIdentifier(Identifier: PChar): string;
function IsDottedIdentifier(const Identifier: string): boolean;
function CompareDottedIdentifiers(Identifier1, Identifier2: PChar): integer;
function ChompDottedIdentifier(const Identifier: string): string;

// space and special chars
function TrimCodeSpace(const ACode: string): string;
function CodeIsOnlySpace(const ACode: string; FromPos, ToPos: integer): boolean;
function StringToPascalConst(const s: string): string;

// string constants
function SplitStringConstant(const StringConstant: string;
    FirstLineLength, OtherLineLengths, Indent: integer;
    const aLineBreak: string): string;
procedure ImproveStringConstantStart(const ACode: string; var StartPos: integer);
procedure ImproveStringConstantEnd(const ACode: string; var EndPos: integer);
function HexStrToIntDef(p: PChar; Def: integer): integer;

// search
function SearchNextInText(Search: PChar; SearchLen: PtrInt;
    Src: PChar; SrcLen: PtrInt;
    StartPos: PtrInt;// 0 based
    out MatchStart, MatchEnd: PtrInt;// 0 based
    WholeWords: boolean = false; MultiLine: boolean = false): boolean;
procedure HasTxtWord(SearchWord, Txt: PChar; out WholeWord: boolean;
  out Count: SizeInt);

// misc
function SubString(p: PChar; Count: SizeInt): string; overload;

// files
type

  { TUnitFileInfo }

  TUnitFileInfo = class
  private
    FFilename: string;
    FUnitName: string;
    function GetFileUnitNameWithoutNamespace: string;
    function GetIdentifierStartInUnitName: Integer;
  public
    constructor Create(const TheUnitName, TheFilename: string);
    property FileUnitName: string read FUnitName;
    property FileUnitNameWithoutNamespace: string read GetFileUnitNameWithoutNamespace;
    property Filename: string read FFilename;
    property IdentifierStartInUnitName: Integer read GetIdentifierStartInUnitName;
  end;

  { TNameSpaceInfo }

  TNameSpaceInfo = class
  private
    FUnitName: string;
    FNamespace: string;
    FIdentifierStartInUnitName: Integer;
  public
    constructor Create(const TheNamespace, TheUnitName: string; TheIdentifierStartInUnitName: Integer);
    property UnitName: string read FUnitName;
    property Namespace: string read FNamespace;
    property IdentifierStartInUnitName: Integer read FIdentifierStartInUnitName;
  end;

function ExtractFileNamespace(const Filename: string): string;
procedure AddToTreeOfUnitFilesOrNamespaces(
  var TreeOfUnitFiles, TreeOfNameSpaces: TAVLTree;
  const NameSpacePath, Filename: string;
  CaseInsensitive, KeepDoubles: boolean);
function GatherUnitFiles(const BaseDir, SearchPath,
    Extensions, NameSpacePath: string; KeepDoubles, CaseInsensitive: boolean;
    var TreeOfUnitFiles, TreeOfNamespaces: TAVLTree): boolean;
procedure FreeTreeOfUnitFiles(TreeOfUnitFiles: TAVLTree);
procedure AddToTreeOfUnitFiles(var TreeOfUnitFiles: TAVLTree;
  const Filename, Unitname: string;
  KeepDoubles: boolean);
procedure AddToTreeOfNamespaces(var TreeOfNameSpaces: TAVLTree;
  const UnitName, ParentNameSpacePath: string;
  KeepDoubles: boolean);
function CompareUnitFileInfos(Data1, Data2: Pointer): integer;
function CompareNameSpaceInfos(Data1, Data2: Pointer): integer;
function CompareUnitNameAndUnitFileInfo(UnitnamePAnsiString,
                                        UnitFileInfo: Pointer): integer;
function CompareNameSpaceAndNameSpaceInfo(NamespacePAnsiString,
                                        NamespaceInfo: Pointer): integer;

// function filled by CodeToolManager to find inc files
var
  FindIncFileInCfgCache: function(const Name: string; out ExpFilename: string): boolean;

//-----------------------------------------------------------------------------
// functions / procedures

{ These functions are not context sensitive. Especially they ignore compiler
  settings and compiler directives. They exist only for basic usage.
}

// source type
function FindSourceType(const Source: string;
  var SrcNameStart, SrcNameEnd: integer; NestedComments: boolean = false): string;

// identifier
function ReadDottedIdentifier(const Source: string; var Position: integer;
  NestedComments: boolean = false): string;
function ReadDottedIdentifier(var Position: PChar; SrcEnd: PChar;
  NestedComments: boolean = false): string;

// program name
function RenameProgramInSource(Source:TSourceLog;
   const NewProgramName:string):boolean;
function FindProgramNameInSource(const Source:string;
   out ProgramNameStart,ProgramNameEnd:integer):string;

// unit name
function RenameUnitInSource(Source:TSourceLog;const NewUnitName:string):boolean;
function FindUnitNameInSource(const Source:string;
   out UnitNameStart,UnitNameEnd: integer; NestedComments: boolean = false):string;
function FindModuleNameInSource(const Source:string;
   out ModuleType: string; out NameStart,NameEnd: integer;
   NestedComments: boolean = false):string;

// uses sections
function UnitIsUsedInSource(const Source,SrcUnitName:string):boolean;
function RenameUnitInProgramUsesSection(Source:TSourceLog;
   const OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddToProgramUsesSection(Source:TSourceLog;
   const AUnitName,InFileName:string):boolean;
function RemoveFromProgramUsesSection(Source:TSourceLog;
   const AUnitName:string):boolean;
function RenameUnitInInterfaceUsesSection(Source:TSourceLog;
   const OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddToInterfaceUsesSection(Source:TSourceLog;
   const AUnitName,InFileName:string):boolean;
function RemoveFromInterfaceUsesSection(Source:TSourceLog;
   const AUnitName:string):boolean;

// single uses section
function IsUnitUsedInUsesSection(const Source,SrcUnitName:string;
   UsesStart:integer):boolean;
function RenameUnitInUsesSection(Source:TSourceLog; UsesStart: integer;
   const OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddUnitToUsesSection(Source:TSourceLog;
   const AnUnitName,InFilename:string; UsesStart:integer):boolean;
function RemoveUnitFromUsesSection(Source:TSourceLog;
   const AnUnitName:string; UsesStart:integer):boolean;

// compiler directives
function FindIncludeDirective(const Source,Section:string; Index:integer;
   out IncludeStart,IncludeEnd:integer):boolean;
function ExtractLongParamDirective(const Source: string; CommentStartPos: integer;
   out DirectiveName, FileParam: string): boolean;
function SplitCompilerDirective(const Directive:string;
   out DirectiveName,Parameters:string):boolean;

// createform
function AddCreateFormToProgram(Source:TSourceLog;
   const AClassName,AName:string):boolean;
function RemoveCreateFormFromProgram(Source:TSourceLog;
   const AClassName,AName:string):boolean;
function CreateFormExistsInProgram(const Source,
   AClassName,AName:string):boolean;
function ListAllCreateFormsInProgram(const Source:string):TStrings;

// resource code
function FindResourceInCode(const Source, AddCode:string;
   out Position,EndPosition:integer):boolean;
function AddResourceCode(Source:TSourceLog; const AddCode:string):boolean;

// form components
function FindFormClassDefinitionInSource(const Source, FormClassName:string;
   var FormClassNameStartPos, FormBodyStartPos: integer):boolean;
function FindFormComponentInSource(const Source: string;
  FormBodyStartPos: integer;
  const ComponentName, ComponentClassName: string): integer;
function AddFormComponentToSource(Source:TSourceLog; FormBodyStartPos: integer;
  const ComponentName, ComponentClassName: string): boolean;
function RemoveFormComponentFromSource(Source:TSourceLog;
  FormBodyStartPos: integer;
  ComponentName, ComponentClassName: string): boolean;
function FindClassAncestorName(const Source, FormClassName: string): string;

// procedure specifiers
function FindFirstProcSpecifier(const ProcText: string;
   NestedComments: boolean = false): integer;
function SearchProcSpecifier(const ProcText, Specifier: string;
   out SpecifierEndPosition: integer;
   NestedComments: boolean = false;
   WithSpaceBehindSemicolon: boolean = true): integer;
function RemoveProcSpecifier(const ProcText, Specifier: string;
   NestedComments: boolean = false): string;

// code search
function SearchCodeInSource(const Source, Find: string; StartPos: integer;
   out EndFoundPosition: integer; CaseSensitive: boolean;
   NestedComments: boolean = false): integer;
function ReadNextPascalAtom(const Source: string;
   var Position: integer; out AtomStart: integer; NestedComments: boolean = false;
   SkipDirectives: boolean = false): string;
procedure ReadRawNextPascalAtom(const Source: string;
   var Position: integer; out AtomStart: integer;
   NestedComments: boolean = false; SkipDirectives: boolean = false);
procedure ReadRawNextPascalAtom(var Position: PChar; out AtomStart: PChar;
   const SrcEnd: PChar = nil; NestedComments: boolean = false;
   SkipDirectives: boolean = false);
procedure ReadPriorPascalAtom(const Source: string;
  var Position: integer; out AtomEnd: integer; NestedComments: boolean = false);
function ReadTilPascalBracketClose(const Source: string;
   var Position: integer; NestedComments: boolean = false): boolean;
function GetAtomLength(p: PChar; NestedComments: boolean): integer;
function GetAtomString(p: PChar; NestedComments: boolean): string;
function FindStartOfAtom(const Source: string; Position: integer): integer;
function FindEndOfAtom(const Source: string; Position: integer): integer;

//-----------------------------------------------------------------------------

const
  MaxLineLength: integer = 80;

//=============================================================================

implementation

function Min(i1, i2: integer): integer; inline;
begin
  if i1<=i2 then Result:=i1 else Result:=i2;
end;

function Max(i1, i2: integer): integer; inline;
begin
  if i1>=i2 then Result:=i1 else Result:=i2;
end;

{ most simple code tools - just methods }

function FindIncludeDirective(const Source,Section:string; Index:integer;
   out IncludeStart,IncludeEnd:integer):boolean;
var Atom,DirectiveName:string;
  Position,EndPos,AtomStart:integer;
  Filename:string;
begin
  Result:=false;
  // find section
  Position:=SearchCodeInSource(Source,Section,1,EndPos,false);
  if (Position<1) or (EndPos<1) then exit;
  // search for include directives
  repeat
    Atom:=ReadNextPascalAtom(Source,Position,AtomStart);
    if (copy(Atom,1,2)='{$') or (copy(Atom,1,3)='(*$') then begin
      SplitCompilerDirective(Atom,DirectiveName,Filename);
      DirectiveName:=lowercase(DirectiveName);
      if (DirectiveName='i') or (DirectiveName='include') then begin
        // include directive
        dec(Index);
        if Index=0 then begin
          IncludeStart:=AtomStart;
          IncludeEnd:=Position;
          Result:=true;
          exit;
        end;
      end;
    end;
  until Atom='';
end;

function ExtractLongParamDirective(const Source: string; CommentStartPos: integer;
  out DirectiveName, FileParam: string): boolean;
var
  p, StartPos: PChar;
begin
  Result:=false;
  FileParam:='';
  if CommentStartPos>length(Source) then exit;
  p:=@Source[CommentStartPos];
  if (p^<>'{') or (p[1]<>'$') then exit;
  inc(p,2);
  StartPos:=p;
  if not IsIdentStartChar[p^] then exit;
  while IsIdentChar[p^] do inc(p);
  DirectiveName:=copy(Source,StartPos-PChar(Source)+1,p-StartPos);
  Result:=true;
  while p^ in [' ',#9] do inc(p);
  if p^='''' then begin
    // 'param with spaces'
    inc(p);
    StartPos:=p;
    while not (p^ in [#0,#10,#13,'''']) do inc(p);
  end else begin
    // param without spaces
    StartPos:=p;
    while not (p^ in [#0,#9,#10,#13,' ','}']) do inc(p);
  end;
  FileParam:=copy(Source,StartPos-PChar(Source)+1,p-StartPos);
end;

function SplitCompilerDirective(const Directive:string;
   out DirectiveName,Parameters:string):boolean;
var EndPos,DirStart,DirEnd:integer;
begin
  if (copy(Directive,1,2)='{$') or (copy(Directive,1,3)='(*$') then begin
    if copy(Directive,1,2)='{$' then begin
      DirStart:=3;
      DirEnd:=length(Directive);
    end else begin
      DirStart:=4;
      DirEnd:=length(Directive)-1;
    end;
    EndPos:=DirStart;
    while (EndPos<DirEnd) and (IsIdentChar[Directive[EndPos]]) do
      inc(EndPos);
    DirectiveName:=lowercase(copy(Directive,DirStart,EndPos-DirStart));
    Parameters:=copy(Directive,EndPos+1,DirEnd-EndPos-1);
    Result:=true;
  end else
    Result:=false;
end;

function FindSourceType(const Source: string; var SrcNameStart,
  SrcNameEnd: integer; NestedComments: boolean): string;
var
  u: String;
  p, AtomStart: Integer;
begin
  // read first atom for type
  SrcNameStart:=0;
  SrcNameEnd:=0;
  p:=1;
  Result:=ReadNextPascalAtom(Source,p,AtomStart,NestedComments);
  u:=Uppercase(Result);
  if (u='UNIT') or (u='PROGRAM') or (u='LIBRARY') or (u='PACKAGE') then begin
    // read name
    ReadNextPascalAtom(Source,p,AtomStart,NestedComments);
    if p<=AtomStart then exit;
    if not IsIdentStartChar[Source[AtomStart]] then exit;
    SrcNameStart:=AtomStart;
    SrcNameEnd:=p;
    repeat
      ReadRawNextPascalAtom(Source,p,AtomStart,NestedComments);
      if (AtomStart=p+1) and (Source[AtomStart]='.') then begin
        ReadRawNextPascalAtom(Source,p,AtomStart,NestedComments);
        if p<=AtomStart then exit;
        if not IsIdentStartChar[Source[AtomStart]] then exit;
        SrcNameEnd:=p;
      end else
        break;
    until false;
  end else begin
    Result:='';
  end;
end;

function RenameUnitInSource(Source:TSourceLog;const NewUnitName:string):boolean;
var UnitNameStart,UnitNameEnd:integer;
begin
  UnitNameStart:=0;
  UnitNameEnd:=0;
  Result:=(FindUnitNameInSource(Source.Source,UnitNameStart,UnitNameEnd)<>'');
  if Result then
    Source.Replace(UnitNameStart,UnitNameEnd-UnitNameStart,NewUnitName);
end;

function FindUnitNameInSource(const Source: string; out UnitNameStart,
  UnitNameEnd: integer; NestedComments: boolean): string;
var
  ModuleType: string;
begin
  Result:=FindModuleNameInSource(Source,ModuleType,UnitNameStart,UnitNameEnd,NestedComments);
  if CompareText(ModuleType,'UNIT')<>0 then
    Result:='';
end;

function FindModuleNameInSource(const Source: string; out ModuleType: string;
  out NameStart, NameEnd: integer; NestedComments: boolean): string;
var
  u: String;
  p, AtomStart: Integer;
begin
  // read first atom for type
  Result:='';
  NameStart:=0;
  NameEnd:=0;
  p:=1;
  ModuleType:=ReadNextPascalAtom(Source,p,AtomStart,NestedComments);
  u:=UpperCase(ModuleType);
  if (u='UNIT') or (u='PROGRAM') or (u='LIBRARY') or (u='PACKAGE') then begin
    // read name
    ReadNextPascalAtom(Source,p,AtomStart,NestedComments);
    if p<=AtomStart then exit;
    if not IsIdentStartChar[Source[AtomStart]] then exit;
    NameStart:=AtomStart;
    NameEnd:=AtomStart;
    Result:=ReadDottedIdentifier(Source,NameEnd,NestedComments);
  end else
    ModuleType:='';
end;

function ReadDottedIdentifier(const Source: string; var Position: integer;
  NestedComments: boolean): string;
var
  p: PChar;
begin
  if (Position<1) or (Position>length(Source)) then exit('');
  p:=@Source[Position];
  Result:=ReadDottedIdentifier(p,PChar(Source)+length(Source),NestedComments);
  Position:=p-PChar(Source)+1;
end;

function ReadDottedIdentifier(var Position: PChar; SrcEnd: PChar;
  NestedComments: boolean): string;
var
  AtomStart, p: PChar;
begin
  Result:='';
  p:=Position;
  ReadRawNextPascalAtom(p,AtomStart,SrcEnd,NestedComments);
  Position:=AtomStart;
  if (AtomStart>=p) or not IsIdentStartChar[AtomStart^] then exit;
  Result:=GetIdentifier(AtomStart);
  repeat
    ReadRawNextPascalAtom(p,AtomStart,SrcEnd,NestedComments);
    if (AtomStart+1<>p) or (AtomStart^<>'.') then exit;
    ReadRawNextPascalAtom(p,AtomStart,SrcEnd,NestedComments);
    if (AtomStart>=p) or not IsIdentStartChar[AtomStart^] then exit;
    Position:=AtomStart;
    Result:=Result+'.'+GetIdentifier(AtomStart);
  until false;
end;

function RenameProgramInSource(Source: TSourceLog;
  const NewProgramName:string):boolean;
var ProgramNameStart,ProgramNameEnd:integer;
begin
  Result:=(FindProgramNameInSource(Source.Source,
                                   ProgramNameStart,ProgramNameEnd)<>'');
  if Result then
    Source.Replace(ProgramNameStart,
                   ProgramNameEnd-ProgramNameStart,NewProgramName)
end;

function FindProgramNameInSource(const Source:string;
   out ProgramNameStart,ProgramNameEnd:integer):string;
begin
  ProgramNameStart:=0;
  ProgramNameEnd:=0;
  if uppercasestr(FindSourceType(Source,ProgramNameStart,ProgramNameEnd))=
    'PROGRAM'
  then
    Result:=copy(Source,ProgramNameStart,ProgramNameEnd-ProgramNameStart)
  else
    Result:='';
end;

function UnitIsUsedInSource(const Source,SrcUnitName:string):boolean;
// search in all uses sections
var UsesStart,UsesEnd:integer;
begin
  Result:=false;
  repeat
    UsesStart:=SearchCodeInSource(Source,'uses',1,UsesEnd,false);
    if UsesEnd=0 then ;
    if UsesStart>0 then begin
      if IsUnitUsedInUsesSection(Source,SrcUnitName,UsesStart) then begin
        Result:=true;
        exit;
      end;
    end;
  until UsesStart<1;
end;

function RenameUnitInProgramUsesSection(Source:TSourceLog;
  const OldUnitName, NewUnitName, NewInFile:string): boolean;
var
  ProgramTermStart,ProgramTermEnd,
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  // search Program section
  ProgramTermStart:=SearchCodeInSource(Source.Source,'program',1,ProgramTermEnd
    ,false);
  if ProgramTermStart<1 then exit;
  // search programname
  ReadNextPascalAtom(Source.Source,ProgramTermEnd,ProgramTermStart);
  // search semicolon after programname
  if not (ReadNextPascalAtom(Source.Source,ProgramTermEnd,ProgramTermStart)=';')
  then exit;
  UsesEnd:=ProgramTermEnd;
  ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source.Source) then exit;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then begin
    // no uses section in interface -> add one
    Source.Insert(ProgramTermEnd,LineEnding+LineEnding+'uses'+LineEnding+'  ;');
    UsesEnd:=ProgramTermEnd;
    ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then exit;
  Result:=RenameUnitInUsesSection(Source,UsesStart,OldUnitName
    ,NewUnitName,NewInFile);
end;

function AddToProgramUsesSection(Source:TSourceLog;
  const AUnitName,InFileName:string):boolean;
var
  ProgramTermStart,ProgramTermEnd,
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  if (AUnitName='') or (AUnitName=';') then exit;
  // search program
  ProgramTermStart:=SearchCodeInSource(Source.Source,'program',1,ProgramTermEnd
    ,false);
  if ProgramTermStart<1 then exit;
  // search programname
  ReadNextPascalAtom(Source.Source,ProgramTermEnd,ProgramTermStart);
  // search semicolon after programname
  if not (ReadNextPascalAtom(Source.Source,ProgramTermEnd,ProgramTermStart)=';')
  then exit;
  // search uses section
  UsesEnd:=ProgramTermEnd;
  ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source.Source) then exit;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then begin
    // no uses section after program term -> add one
    Source.Insert(ProgramTermEnd,LineEnding+LineEnding+'uses'+LineEnding+'  ;');
    UsesEnd:=ProgramTermEnd;
    ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then exit;
  Result:=AddUnitToUsesSection(Source,AUnitName,InFileName,UsesStart);
end;

function RenameUnitInInterfaceUsesSection(Source:TSourceLog;
  const OldUnitName, NewUnitName, NewInFile:string): boolean;
var
  InterfaceStart,InterfaceWordEnd,
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  // search interface section
  InterfaceStart:=SearchCodeInSource(Source.Source,'interface',1
     ,InterfaceWordEnd,false);
  if InterfaceStart<1 then exit;
  UsesEnd:=InterfaceWordEnd;
  ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source.Source) then exit;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then begin
    // no uses section in interface -> add one
    Source.Insert(InterfaceWordEnd,LineEnding+LineEnding+'uses'+LineEnding+'  ;');
    UsesEnd:=InterfaceWordEnd;
    ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then exit;
  Result:=RenameUnitInUsesSection(Source,UsesStart,OldUnitName
    ,NewUnitName,NewInFile);
end;

function AddToInterfaceUsesSection(Source:TSourceLog;
  const AUnitName,InFileName:string):boolean;
var
  InterfaceStart,InterfaceWordEnd,
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  if AUnitName='' then exit;
  // search interface section
  InterfaceStart:=SearchCodeInSource(Source.Source,'interface',1
    ,InterfaceWordEnd,false);
  if InterfaceStart<1 then exit;
  UsesEnd:=InterfaceWordEnd;
  ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source.Source) then exit;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then begin
    // no uses section in interface -> add one
    Source.Insert(InterfaceWordEnd,LineEnding+LineEnding+'uses'+LineEnding+'  ;');
    UsesEnd:=InterfaceWordEnd;
    ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source.Source,UsesStart,UsesEnd-UsesStart))='uses')
  then exit;
  Result:=AddUnitToUsesSection(Source,AUnitName,InFileName,UsesStart);
end;

function RemoveFromProgramUsesSection(Source:TSourceLog;
  const AUnitName:string):boolean;
var
  ProgramTermStart,ProgramTermEnd,
  UsesStart,UsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if AUnitName='' then exit;
  // search program
  ProgramTermStart:=SearchCodeInSource(Source.Source,'program',1
     ,ProgramTermEnd,false);
  if ProgramtermStart<1 then exit;
  // search programname
  ReadNextPascalAtom(Source.Source,ProgramTermEnd,ProgramTermStart);
  // search semicolon after programname
  if not (ReadNextPascalAtom(Source.Source,ProgramTermEnd,ProgramTermStart)=';')
  then exit;
  UsesEnd:=ProgramTermEnd;
  Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source.Source) then exit;
  if not (lowercase(Atom)='uses') then exit;
  Result:=RemoveUnitFromUsesSection(Source,AUnitName,UsesStart);
end;

function RemoveFromInterfaceUsesSection(Source:TSourceLog;
  const AUnitName:string):boolean;
var
  InterfaceStart,InterfaceWordEnd,
  UsesStart,UsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if AUnitName='' then exit;
  // search interface section
  InterfaceStart:=SearchCodeInSource(Source.Source,'interface',1
    ,InterfaceWordEnd,false);
  if InterfaceStart<1 then exit;
  UsesEnd:=InterfaceWordEnd;
  Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source.Source) then exit;
  if not (lowercase(Atom)='uses') then exit;
  Result:=RemoveUnitFromUsesSection(Source,AUnitName,UsesStart);
end;

function IsUnitUsedInUsesSection(const Source,SrcUnitName:string;
   UsesStart:integer):boolean;
var UsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if SrcUnitName='' then exit;
  if UsesStart<1 then exit;
  if not (lowercase(copy(Source,UsesStart,4))='uses') then exit;
  UsesEnd:=UsesStart+4;
  // parse through all used units and see if it is there
  repeat
    Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(SrcUnitName)) then begin
      // unit found
      Result:=true;
      exit;
    end;
    // read til next comma or semicolon
    repeat
      Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
    until (Atom=',') or (Atom=';') or (Atom='');
  until Atom<>',';
  // unit not used
  Result:=true;
end;

function RenameUnitInUsesSection(Source:TSourceLog; UsesStart: integer;
  const OldUnitName, NewUnitName, NewInFile:string): boolean;
var UsesEnd:integer;
  LineStart,LineEnd,OldUsesStart:integer;
  s,Atom,NewUnitTerm:string;
begin
  Result:=false;
  if (OldUnitName='') then begin
    Result:=AddUnitToUsesSection(Source,NewUnitName,NewInFile,UsesStart);
    exit;
  end;
  if (NewUnitName='') or (NewUnitName=';')
  or (OldUnitName=';') or (UsesStart<1) then exit;
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source.Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is already there
  if NewInFile<>'' then
    NewUnitTerm:=NewUnitName+' in '''+NewInFile+''''
  else
    NewUnitTerm:=NewUnitName;
  s:=', ';
  repeat
    Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(OldUnitName)) then begin
      // unit already used
      OldUsesStart:=UsesStart;
      // find comma or semicolon
      repeat
        Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
      until (Atom=',') or (Atom=';') or (Atom='');
      Source.Replace(OldUsesStart,UsesStart-OldUsesStart,NewUnitTerm);
      Result:=true;
      exit;
    end else if (Atom=';') then begin
      s:=' ';
      break;
    end;
    // read til next comma or semicolon
    while (Atom<>',') and (Atom<>';') and (Atom<>'') do
      Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  until Atom<>',';
  // unit not used yet -> add it
  Source.Insert(UsesStart,s+NewUnitTerm);
  GetLineStartEndAtPosition(Source.Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (NewInFile<>'') then
    Source.Insert(UsesStart,LineEnding+'  ');
  Result:=true;
end;

function AddUnitToUsesSection(Source:TSourceLog;
 const AnUnitName,InFilename:string; UsesStart:integer):boolean;
var UsesEnd:integer;
  LineStart,LineEnd:integer;
  s,Atom,NewUnitTerm:string;
begin
  Result:=false;
  if (AnUnitName='') or (AnUnitName=';') or (UsesStart<1) then exit;
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source.Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is already there
  s:=', ';
  repeat
    Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(AnUnitName)) then begin
      // unit found
      Result:=true;
      exit;
    end else if (Atom=';') then begin
      s:=' ';
      break;
    end;
    // read til next comma or semicolon
    while (Atom<>',') and (Atom<>';') and (Atom<>'') do
      Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  until Atom<>',';
  // unit not used yet -> add it
  if InFilename<>'' then
    NewUnitTerm:=AnUnitName+' in '''+InFileName+''''
  else
    NewUnitTerm:=AnUnitName;
  Source.Insert(UsesStart,s+NewUnitTerm);
  GetLineStartEndAtPosition(Source.Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (InFileName<>'') then
    Source.Insert(UsesStart,LineEnding+'  ');
  Result:=true;
end;

function RemoveUnitFromUsesSection(Source:TSourceLog; const AnUnitName:string;
   UsesStart:integer):boolean;
var UsesEnd,OldUsesStart,OldUsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if (UsesStart<1) or (AnUnitName='') or (AnUnitName=',') or (AnUnitName=';') then
    exit;
  // search interface section
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source.Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is there
  OldUsesEnd:=-1;
  repeat
    Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(AnUnitName)) then begin
      // unit found
      OldUsesStart:=UsesStart;
      // find comma or semicolon
      repeat
        Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
      until (Atom=',') or (Atom=';') or (Atom='');
      if OldUsesEnd<1 then
        // first used unit
        Source.Delete(OldUsesStart,UsesStart-OldUsesStart)
      else
        // not first used unit (remove comma in front of AnUnitName too)
        Source.Delete(OldUsesEnd,UsesStart-OldUsesEnd);
      Result:=true;
      exit;
    end else
      OldUsesEnd:=UsesEnd;

    // read til next comma or semicolon
    while (Atom<>',') and (Atom<>';') and (Atom<>'') do
      Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
  until Atom<>',';
  // unit not used
end;

function AddCreateFormToProgram(Source:TSourceLog;
  const AClassName,AName:string):boolean;
// insert 'Application.CreateForm(<AClassName>,<AName>);'
// in front of 'Application.Run;'
var Position, EndPosition: integer;
begin
  Result:=false;
  Position:=SearchCodeInSource(Source.Source,'application.run',1
    ,EndPosition,false);
  if Position<1 then exit;
  if EndPosition=0 then ;
  Source.Insert(Position,
         'Application.CreateForm('+AClassName+','+AName+');'+LineEnding+'  ');
  Result:=true;
end;

function RemoveCreateFormFromProgram(Source:TSourceLog;
  const AClassName,AName:string):boolean;
// remove 'Application.CreateForm(<AClassName>,<AName>);'
var Position,EndPosition,AtomStart:integer;
begin
  Result:=false;
  Position:=SearchCodeInSource(Source.Source,
     'application.createform('+AClassName+','+AName+')',1,EndPosition,false);
  if Position<1 then exit;
  if ReadNextPascalAtom(Source.Source,EndPosition,AtomStart)=';' then
    ReadNextPascalAtom(Source.Source,EndPosition,AtomStart);
  EndPosition:=AtomStart;
  Source.Delete(Position,EndPosition-Position);
  Result:=true;
end;

function CreateFormExistsInProgram(const Source,
  AClassName,AName:string):boolean;
var Position,EndPosition:integer;
begin
  Position:=SearchCodeInSource(Source,
     'application.createform('+AClassName+','+AName+')',1,EndPosition,false);
  Result:=Position>0;
  if EndPosition=0 then ;
end;

function ListAllCreateFormsInProgram(const Source:string):TStrings;
// list format: <formname>:<formclassname>
var Position, EndPosition: integer;
  s:string;
begin
  Result:=TStringList.Create;
  Position:=1;
  repeat
    Position:=SearchCodeInSource(Source,
      'application.createform(',Position,EndPosition,false);
    if Position>0 then begin
      s:=ReadNextPascalAtom(Source,EndPosition,Position);
      ReadNextPascalAtom(Source,EndPosition,Position);
      s:=ReadNextPascalAtom(Source,EndPosition,Position)+':'+s;
      Result.Add(s);
    end;
  until Position<1;
end;

function FindResourceInCode(const Source, AddCode:string;
   out Position,EndPosition:integer):boolean;
var Find,Atom:string;
  FindPosition,FindAtomStart,SemicolonPos:integer;
begin
  Result:=false;
  if AddCode='' then begin
    Result:=true;
    exit;
  end;
  if Source='' then exit;
  // search "LazarusResources.Add('<ResourceName>',"
  FindPosition:=1;
  repeat
    Atom:=ReadNextPascalAtom(AddCode,FindPosition,FindAtomStart);
  until (Atom='') or (Atom=',');
  if Atom='' then exit;
  // search the resource start in code
  Find:=copy(AddCode,1,FindPosition-1);
  Position:=SearchCodeInSource(Source,Find,1,EndPosition,false);
  if Position<1 then exit;
  // search resource end in code
  SemicolonPos:=SearchCodeInSource(Source,');',EndPosition,EndPosition,false);
  if SemicolonPos<1 then exit;
  Result:=true;
end;

function AddResourceCode(Source:TSourceLog; const AddCode:string):boolean;
var StartPos,EndPos:integer;
begin
  if FindResourceInCode(Source.Source,AddCode,StartPos,EndPos) then begin
    // resource exists already -> replace it
    Source.Replace(StartPos,EndPos-StartPos,AddCode);
  end else begin
    // add resource
    Source.Insert(length(Source.Source)+1,LineEnding+AddCode);
  end;
  Result:=true;
end;

function FindFormClassDefinitionInSource(const Source, FormClassName:string;
  var FormClassNameStartPos, FormBodyStartPos: integer):boolean;
var AtomEnd,AtomStart: integer;
begin
  Result:=false;
  if FormClassName='' then exit;
  repeat
    FormClassNameStartPos:=SearchCodeInSource(Source,
      FormClassName+'=class(TForm)',1,FormBodyStartPos,false);
    if FormClassNameStartPos<1 then exit;
    AtomEnd:=FormBodyStartPos;
  until ReadNextPascalAtom(Source,AtomEnd,AtomStart)<>';';
  Result:=true;
end;

function FindFormComponentInSource(const Source: string;
  FormBodyStartPos: integer;
  const ComponentName, ComponentClassName: string): integer;
var AtomStart, OldPos: integer;
  Atom,LowComponentName,LowComponentClassName: string;
begin
  LowComponentName:=lowercase(ComponentName);
  LowComponentClassName:=lowercase(ComponentClassName);
  Result:=FormBodyStartPos;
  repeat
    Atom:=lowercase(ReadNextPascalAtom(Source,Result,AtomStart));
    if (Atom='public') or (Atom='private') or (Atom='end')
    or (Atom='protected') or (Atom='') then begin
      Result:=-1;
      exit;
    end;
    OldPos:=Result;
    if (lowercase(ReadNextPascalAtom(Source,Result,AtomStart))=LowComponentName)
    and (ReadNextPascalAtom(Source,Result,AtomStart)=':')
    and (lowercase(ReadNextPascalAtom(Source,Result,AtomStart))=
          LowComponentClassName)
    and (ReadNextPascalAtom(Source,Result,AtomStart)=';') then begin
      Result:=OldPos;
      exit;
    end;
  until Result>length(Source);
  Result:=-1;
end;

function AddFormComponentToSource(Source:TSourceLog; FormBodyStartPos: integer;
  const ComponentName, ComponentClassName: string): boolean;
var Position, AtomStart: integer;
  Atom: string;
  PriorSpaces, NextSpaces: string;
begin
  Result:=false;
  if FindFormComponentInSource(Source.Source,FormBodyStartPos
       ,ComponentName,ComponentClassName)>0 then begin
    Result:=true;
    exit;
  end;
  Position:=FormBodyStartPos;
  repeat
    // find a good position to insert the component
    // in front of next section and in front of procedures/functions
    Atom:=lowercase(ReadNextPascalAtom(Source.Source,Position,AtomStart));
    if (Atom='procedure') or (Atom='function') or (Atom='end') or (Atom='class')
    or (Atom='constructor') or (Atom='destructor')
    or (Atom='public') or (Atom='private') or (Atom='protected')
    or (Atom='published') or (Atom='class') or (Atom='property') then begin
      // insert component definition in source
      if (Atom='public') or (Atom='private') or (Atom='protected')
      or (Atom='published') then begin
        PriorSpaces:='  ';
        NextSpaces:='  ';
      end else begin
        PriorSpaces:='';
        NextSpaces:='    ';
      end;
      Source.Insert(AtomStart,
              PriorSpaces+ComponentName+': '+ComponentClassName+';'+LineEnding
             +NextSpaces);
      Result:=true;
      exit;
    end;
  until Position>length(Source.Source);
  Result:=false;
end;

function RemoveFormComponentFromSource(Source:TSourceLog;
  FormBodyStartPos: integer;
  ComponentName, ComponentClassName: string): boolean;
var AtomStart, Position, ComponentStart, LineStart, LineEnd: integer;
  Atom: string;
begin
  ComponentName:=lowercase(ComponentName);
  ComponentClassName:=lowercase(ComponentClassName);
  Position:=FormBodyStartPos;
  repeat
    Atom:=lowercase(ReadNextPascalAtom(Source.Source,Position,AtomStart));
    if (Atom='public') or (Atom='private') or (Atom='end')
    or (Atom='protected') or (Atom='') then begin
      Result:=false;
      exit;
    end;
    if (Atom=ComponentName) then begin
      ComponentStart:=AtomStart;
      if (ReadNextPascalAtom(Source.Source,Position,AtomStart)=':')
      and (lowercase(ReadNextPascalAtom(Source.Source,Position,AtomStart))=
         ComponentClassName)
      then begin
        GetLineStartEndAtPosition(Source.Source,ComponentStart,LineStart,LineEnd);
        if (LineEnd<=length(Source.Source))
        and (Source.Source[LineEnd] in [#10,#13]) then begin
          inc(LineEnd);
          if (LineEnd<=length(Source.Source))
          and (Source.Source[LineEnd] in [#10,#13])
          and (Source.Source[LineEnd]<>Source.Source[LineEnd-1]) then
            inc(LineEnd);
        end;
        Source.Delete(LineStart,LineEnd-LineStart);
        Result:=true;
        exit;
      end;
    end;
  until Atom='';
  Result:=true;
end;

function FindClassAncestorName(const Source, FormClassName: string): string;
var
  SrcPos, AtomStart: integer;
begin
  Result:='';
  if SearchCodeInSource(Source,FormClassName+'=class(',1,SrcPos,false)<1 then
    exit;
  Result:=ReadNextPascalAtom(Source,SrcPos,AtomStart);
  if not IsValidIdent(Result) then
    Result:='';
end;

function SearchCodeInSource(const Source, Find: string; StartPos: integer;
  out EndFoundPosition: integer; CaseSensitive: boolean;
  NestedComments: boolean):integer;
// search pascal atoms of <Find> in <Source>
// returns the start pos
// -1 on failure
var
  FindLen: Integer;
  SrcLen: Integer;
  Position: Integer;
  FirstFindPos: Integer;
  FindAtomStart: Integer;
  AtomStart: Integer;
  FindAtomLen: Integer;
  AtomLen: Integer;
  SrcPos: Integer;
  FindPos: Integer;
  SrcAtomStart: Integer;
  FirstFindAtomStart: Integer;
begin
  Result:=-1;
  if (Find='') or (StartPos>length(Source)) then exit;

  FindLen:=length(Find);
  SrcLen:=length(Source);

  Position:=StartPos;
  AtomStart:=StartPos;
  FirstFindPos:=1;
  FirstFindAtomStart:=1;

  // search first atom in find
  ReadRawNextPascalAtom(Find,FirstFindPos,FirstFindAtomStart,NestedComments);
  FindAtomLen:=FirstFindPos-FirstFindAtomStart;
  if FirstFindAtomStart>FindLen then exit;

  repeat
    // read next atom
    ReadRawNextPascalAtom(Source,Position,AtomStart,NestedComments);
    if AtomStart>SrcLen then exit;
    AtomLen:=Position-AtomStart;

    if (AtomLen=FindAtomLen)
    and (CompareText(@Find[FirstFindAtomStart],FindAtomLen,
                     @Source[AtomStart],AtomLen,CaseSensitive)=0)
    then begin
      // compare all atoms
      SrcPos:=Position;
      SrcAtomStart:=SrcPos;
      FindPos:=FirstFindPos;
      FindAtomStart:=FindPos;
      repeat
        // read the next atom from the find
        ReadRawNextPascalAtom(Find,FindPos,FindAtomStart,NestedComments);
        if FindAtomStart>FindLen then begin
          // found !
          EndFoundPosition:=SrcPos;
          Result:=AtomStart;
          exit;
        end;
        // read the next atom from the source
        ReadRawNextPascalAtom(Source,SrcPos,SrcAtomStart,NestedComments);
        // compare
        if (CompareText(@Find[FindAtomStart],FindPos-FindAtomStart,
                        @Source[SrcAtomStart],SrcPos-SrcAtomStart,
                        CaseSensitive)<>0)
        then
          break;
      until false;
    end;
  until false;
end;

function FindCommentEnd(Src: PChar; NestedComments: boolean): PChar;
// returns position after the comment end, e.g. after }
var
  CommentLvl: integer;
begin
  Result:=Src;
  if Result=nil then exit;
  case Result^ of
  '/':
    if Result[1]='/' then begin
      inc(Result,2);
      while not (Result^ in [#0,#10,#13]) do inc(Result);
    end;

  '{':
    begin
      inc(Result);
      if Result^=#3 then begin
        // codetools skip comment
        inc(Result);
        repeat
          case Result^ of
          #0: break;
          #3:
            if Result[1]='}' then begin
              inc(Result,2);
              break;
            end;
          end;
          inc(Result);
        until false;
      end else begin
        // pascal comment {}
        CommentLvl:=1;
        repeat
          case Result^ of
          #0: break;
          '{':
            if NestedComments then
              inc(CommentLvl);

          '}':
            begin
              dec(CommentLvl);
              if CommentLvl=0 then begin
                inc(Result);
                break;
              end;
            end;

          end;
          inc(Result);
        until false;
      end;
    end;

  '(':
    if Result[1]='*' then begin
      inc(Result,2);
      CommentLvl:=1;
      repeat
        if Result^=#0 then break;
        if (Result^='*') and (Result[1]=')') then begin
          inc(Result,2);
          dec(CommentLvl);
          if CommentLvl=0 then
            break;
        end else if (Result^='(') and (Result[1]='*') and NestedComments then begin
          inc(Result,2);
          inc(CommentLvl);
        end else
          inc(Result);
      until false;
    end;

  end;
end;

function IsCommentEnd(const ASource: string; EndPos: integer): boolean;
// return true if EndPos on } or on *) or in a // comment
var
  LineStart: LongInt;
begin
  Result:=false;
  if EndPos<1 then exit;
  if EndPos>length(ASource) then exit;
  if ASource[EndPos]='}' then begin
    // delphi or codetools comment end
    Result:=true;
    exit;
  end;
  if (EndPos>1) and (ASource[EndPos]=')') and (ASource[EndPos-1]='*') then begin
    // TP comment end
    Result:=true;
    exit;
  end;
  // test for Delphi comment //
  // skip line end
  LineStart:=EndPos;
  if ASource[LineStart] in [#10,#13] then begin
    dec(LineStart);
    if (LineStart>=1) and (ASource[LineStart] in [#10,#13])
    and (ASource[LineStart]<>ASource[LineStart+1]) then
      dec(LineStart);
    if LineStart<1 then exit;
  end;
  // find line start
  while (LineStart>1) and (not (ASource[LineStart-1] in [#10,#13])) do
    dec(LineStart);
  // find first non space char in line
  while (LineStart<=EndPos) and (ASource[LineStart] in [' ',#9]) do
    inc(LineStart);
  if (LineStart<EndPos)
  and (ASource[LineStart]='/') and (ASource[LineStart+1]='/') then begin
    // Delphi comment end
    Result:=true;
    exit;
  end;
end;

function FindNextComment(const ASource: string; StartPos: integer;
  MaxPos: integer): integer;
// if not found: Result=MaxPos+1
var
  NotFoundPos: Integer;
begin
  NotFoundPos:=MaxPos;
  if (MaxPos>length(ASource)) or (MaxPos<1) then
    MaxPos:=length(ASource);
  Result:=StartPos;
  while (Result<=MaxPos) do begin
    case ASource[Result] of
    '''':
      begin
        inc(Result);
        while (Result<=MaxPos) do begin
          if (ASource[Result] in ['''',#10,#13]) then
            break;
          inc(Result);
        end;
      end;

    '/':
      if (Result<MaxPos) and (ASource[Result+1]='/') then
        exit;

    '{':
      exit;

    '(':
      if (Result<MaxPos) and (ASource[Result+1]='*') then
        exit;

    end;
    inc(Result);
  end;
  if Result>MaxPos then
    if NotFoundPos>=1 then
      Result:=NotFoundPos+1
    else
      Result:=MaxPos+1;
end;

procedure FindCommentsInRange(const Src: string; StartPos, EndPos: integer;
  out FirstCommentStart, FirstAtomStart, LastCommentEnd, LastAtomEnd: integer;
  NestedComments: boolean);
var
  p: PChar;
  i: integer;
  AtomStart: integer;
  SrcLen: Integer;
begin
  FirstCommentStart:=0;
  FirstAtomStart:=0;
  LastCommentEnd:=0;
  LastAtomEnd:=0;
  SrcLen:=length(Src);
  if (StartPos<1) then StartPos:=1;
  if StartPos>SrcLen then exit;
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  i:=StartPos;
  while i<EndPos do begin
    p:=@Src[i];
    // skip space
    while IsSpaceChar[p^] do inc(p);
    i:=p-PChar(Src)+1;
    if i>=EndPos then exit;

    if (p^='{') or ((p^='(') and (p[1]='*')) or ((p^='/') and (p[1]='/')) then
    begin
      // a comment
      if FirstCommentStart=0 then
        FirstCommentStart:=i;
      i:=FindCommentEnd(Src,i,NestedComments);
      if LastCommentEnd=0 then
        LastCommentEnd:=i;
    end else begin
      // normal atom
      if FirstAtomStart=0 then
        FirstAtomStart:=i;
      ReadRawNextPascalAtom(Src,i,AtomStart);
      if LastAtomEnd=0 then
        LastAtomEnd:=i;
    end;
  end;
end;

function FindNextCompilerDirective(const ASource: string; StartPos: integer;
  NestedComments: boolean): integer;
var
  MaxPos: integer;
begin
  MaxPos:=length(ASource);
  Result:=StartPos;
  while (Result<=MaxPos) do begin
    case ASource[Result] of
    '''':
      begin
        inc(Result);
        while (Result<=MaxPos) do begin
          if (ASource[Result]<>'''') then
            inc(Result)
          else begin
            inc(Result);
            break;
          end;
        end;
      end;

    '/':
      begin
        inc(Result);
        if (Result<=MaxPos) and (ASource[Result]='/') then begin
          // skip Delphi comment
          while (Result<=MaxPos) and (not (ASource[Result] in [#10,#13])) do
            inc(Result);
        end;
      end;

    '{':
      begin
        if (Result<MaxPos) and (ASource[Result+1]='$') then
          exit;
        // skip pascal comment
        Result:=FindCommentEnd(ASource,Result,NestedComments);
      end;

    '(':
      begin
        if (Result<MaxPos) and (ASource[Result+1]='*') then begin
          if (Result+2<=MaxPos) and (ASource[Result+2]='$') then
            exit;
          // skip TP comment
          Result:=FindCommentEnd(ASource,Result,NestedComments);
        end else
          inc(Result);
      end;

    else
      inc(Result);
    end;

  end;
  if Result>MaxPos+1 then Result:=MaxPos+1;
end;

function FindNextCompilerDirectiveWithName(const ASource: string;
  StartPos: integer; const DirectiveName: string;
  NestedComments: boolean; out ParamPos: integer): integer;
var
  Offset: Integer;
  SrcLen: Integer;
begin
  Result:=StartPos;
  ParamPos:=0;
  SrcLen:=length(ASource);
  repeat
    Result:=FindNextCompilerDirective(ASource,Result,NestedComments);
    if (Result<1) or (Result>SrcLen) then break;
    if (ASource[Result]='{') then
      Offset:=2
    else if ASource[Result]='(' then
      Offset:=3
    else
      Offset:=-1;
    if Offset>0 then begin
      if (CompareIdentifiers(PChar(Pointer(DirectiveName)),// pointer type cast avoids #0 check
                             @ASource[Result+Offset])=0)
      then begin
        ParamPos:=FindNextNonSpace(ASource,Result+Offset+length(DirectiveName));
        exit;
      end;
    end;
    Result:=FindCommentEnd(ASource,Result,NestedComments);
  until false;
  Result:=-1;
end;

function FindNextNonSpace(const ASource: string; StartPos: integer
  ): integer;
var
  SrcLen: integer;
begin
  SrcLen:=length(ASource);
  Result:=StartPos;
  while (Result<=SrcLen) and (ASource[Result] in [' ',#9,#10,#13]) do
    inc(Result);
end;

function FindPrevNonSpace(const ASource: string; StartPos: integer
    ): integer;
begin
  Result:=StartPos;
  while (Result>=1) and (ASource[Result] in [' ',#9,#10,#13]) do
    dec(Result);
end;

function FindCommentEnd(const ASource: string; StartPos: integer;
  NestedComments: boolean): integer;
// returns position after the comment end, e.g. after }
// failure: returns length(ASource)+1
var
  CommentLvl: integer;
  p: PChar;
begin
  Result:=StartPos;
  if Result<1 then exit;
  if Result>length(ASource) then exit;
  p:=@ASource[Result];
  case p^ of
  '/':
    begin
      if p[1]='/' then begin
        // skip Delphi comment
        while (not (p^ in [#0,#10,#13])) do
          inc(p);
      end;
    end;

  '{':
    begin
      inc(p);
      if p^=#3 then begin
        // Codetools skip comment {#3 #3}
        inc(p);
        repeat
          case p^ of
          #0:
            if p-PChar(ASource)>=length(ASource) then break;
          #3:
            if p[1]='}' then begin
              inc(p,2);
              break;
            end;
          end;
          inc(p);
        until false;
      end else begin
        // Pascal comment {}
        CommentLvl:=1;
        repeat
          case p^ of
          #0:
            if p-PChar(ASource)>=length(ASource) then break;
          '{':
            if NestedComments then
              inc(CommentLvl);
          '}':
            begin
              dec(CommentLvl);
              if CommentLvl=0 then begin
                inc(p);
                break;
              end;
            end;
          end;
          inc(p);
        until false;
      end;
    end;

  '(':
    if (p[1]='*') then begin
      inc(p,2);
      CommentLvl:=1;
      repeat
        if (p^=#0) then begin
          if p-PChar(ASource)>=length(ASource) then break;
          inc(p);
        end else if (p^='(') and (p[1]='*') and NestedComments then begin
          inc(p,2);
          inc(CommentLvl);
        end else if (p^='*') and (p[1]=')') then begin
          inc(p,2);
          dec(CommentLvl);
          if CommentLvl=0 then break;
        end else
          inc(p);
      until false;
    end;

  end;
  Result:=p-PChar(ASource)+1;
end;

procedure GetLineStartEndAtPosition(const Source:string; Position:integer;
   out LineStart,LineEnd:integer);
begin
  if Position<1 then begin
    LineStart:=0;
    LineEnd:=0;
    exit;
  end;
  if Position>length(Source)+1 then begin
    LineStart:=length(Source)+1;
    LineEnd:=LineStart;
    exit;
  end;
  LineStart:=Position;
  while (LineStart>1) and (not (Source[LineStart-1] in [#10,#13])) do
    dec(LineStart);
  LineEnd:=Position;
  while (LineEnd<=length(Source)) and (not (Source[LineEnd] in [#10,#13])) do
    inc(LineEnd);
end;

function LineEndCount(const Txt: string; StartPos, EndPos: integer; out
  LengthOfLastLine: integer): integer;
var
  l: Integer;
  p, LineStart: PChar;
begin
  Result:=0;
  LengthOfLastLine:=0;
  l:=length(Txt);
  if l=0 then exit;
  if StartPos>l then exit;
  if EndPos>l then EndPos:=l+1;
  if StartPos>=EndPos then exit;
  p:=@Txt[StartPos];
  LineStart:=p;
  repeat
    if p^ in [#0,#10,#13] then begin
      if p-PChar(Txt)+1>=EndPos then
        break;
      if p^<>#0 then begin
        inc(Result);
        if (p[1] in [#10,#13]) and (p^<>p[1]) and (p-PChar(Txt)+1<EndPos) then
          inc(p,2)
        else
          inc(p);
        LineStart:=p;
        continue;
      end;
    end;
    inc(p);
  until false;
  LengthOfLastLine:=EndPos-(LineStart-PChar(Txt)+1);
end;

function EmptyCodeLineCount(const Source: string; StartPos, EndPos: integer;
    NestedComments: boolean): integer;
{ search forward for a line end or code
  ignore line ends in comments
  Result is Position of Start of Line End
}
var
  SrcLen: integer;
  SrcPos: Integer;
  CommentEndPos: Integer;
begin
  Result:=0;
  SrcLen:=length(Source);
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  SrcPos:=StartPos;
  while (SrcPos<EndPos) do begin
    case Source[SrcPos] of
    '{','(','/':
      begin
        CommentEndPos:=FindCommentEnd(Source,SrcPos,NestedComments);
        if CommentEndPos>SrcPos then
          SrcPos:=CommentEndPos
        else
          inc(SrcPos); // not a comment start => skip char
      end;
    #10,#13:
      begin
        // skip line end
        inc(SrcPos);
        if (SrcPos<EndPos) and (Source[SrcPos] in [#10,#13])
        and (Source[SrcPos]<>Source[SrcPos-1]) then
          inc(SrcPos);
        // count empty lines
        if (SrcPos<EndPos) and (Source[SrcPos] in [#10,#13]) then
          inc(Result);
      end;
    else
      inc(SrcPos);
    end;
  end;
end;

function PositionsInSameLine(const Source: string;
    Pos1, Pos2: integer): boolean;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  if Pos1<Pos2 then begin
    StartPos:=Pos1;
    EndPos:=Pos2;
  end else begin
    StartPos:=Pos2;
    EndPos:=Pos1;
  end;
  if EndPos>length(Source) then EndPos:=length(Source);
  while StartPos<EndPos do begin
    if Source[StartPos] in [#10,#13] then begin
      Result:=false;
      exit;
    end else
      inc(StartPos);
  end;
  Result:=true;
end;

procedure GetIdentStartEndAtPosition(const Source: string; Position: integer;
  out IdentStart, IdentEnd: integer);
// on success: IdentStart<IdentEnd
begin
  IdentStart:=Position;
  IdentEnd:=Position;
  if (Position<1) or (Position>length(Source)+1) then exit;
  while (IdentStart>1)
  and (IsIdentChar[Source[IdentStart-1]]) do
    dec(IdentStart);
  while (IdentEnd<=length(Source))
  and (IsIdentChar[Source[IdentEnd]]) do
    inc(IdentEnd);
  while (IdentStart<Position)
  and (not IsIdentStartChar[Source[IdentStart]]) do
    inc(IdentStart);
  if (IdentStart>1) and (Source[IdentStart-1]='&') then
    dec(IdentStart);
  if (IdentStart>length(Source)) or not IsIdentStartChar[Source[IdentStart]] then
    IdentEnd:=IdentStart;
end;

function GetIdentStartPosition(const Source: string; Position: integer
  ): integer;
begin
  Result:=Position;
  if (Result<1) or (Result>length(Source)+1) then exit;
  while (Result>1)
  and (IsIdentChar[Source[Result-1]]) do
    dec(Result);
  while (Result<Position)
  and (not IsIdentStartChar[Source[Result]]) do
    inc(Result);
  if (Result>1) and (Source[Result-1]='&') then
    dec(Result);
end;

function GetIdentLen(Identifier: PChar): integer;
begin
  Result:=0;
  if Identifier=nil then exit;
  if not IsIdentStartChar[Identifier^] then exit;
  while (IsIdentChar[Identifier[Result]]) do inc(Result);
end;

function FindFirstProcSpecifier(const ProcText: string; NestedComments: boolean
  ): integer;
// length(ProcText)+1 on failure
var
  AtomStart: integer;
  p: Integer;
begin
  Result:=length(ProcText)+1;
  // read till first semicolon
  p:=1;
  while p<=length(ProcText) do begin
    ReadRawNextPascalAtom(ProcText,p,AtomStart,NestedComments,true);
    if AtomStart>length(ProcText) then exit;
    if ProcText[AtomStart] in ['[','('] then begin
      if not ReadTilPascalBracketClose(ProcText,p,NestedComments) then
        exit;
    end else if ProcText[AtomStart]=';' then begin
      ReadRawNextPascalAtom(ProcText,p,AtomStart,NestedComments,true);
      Result:=AtomStart;
      exit;
    end;
  end;
end;

function SearchProcSpecifier(const ProcText, Specifier: string; out
  SpecifierEndPosition: integer; NestedComments: boolean;
  WithSpaceBehindSemicolon: boolean): integer;
// Result = -1 on failure
// Result = start of Specifier on success
// SpecifierEndPosition on semicolon or >length(ProcText)
// if WithSpaceBehindSemicolon then SpecifierEndPosition is start of next specifier
var
  AtomStart: integer;
begin
  Result:=FindFirstProcSpecifier(ProcText,NestedComments);
  repeat
    if Result>length(ProcText) then exit(-1);
    ReadRawNextPascalAtom(ProcText,Result,AtomStart,NestedComments,true);
    if AtomStart>length(ProcText) then exit(-1);
    if CompareIdentifiers(@ProcText[AtomStart],@Specifier[1])=0 then begin
      Result:=AtomStart;
      break;
    end;
    if ProcText[AtomStart] in ['[','('] then begin
      if not ReadTilPascalBracketClose(ProcText,Result,NestedComments)
      then
        exit(-1);
    end;
  until false;
  SpecifierEndPosition:=Result;
  while (SpecifierEndPosition<=length(ProcText))
  and (ProcText[SpecifierEndPosition]<>';') do begin
    ReadRawNextPascalAtom(ProcText,SpecifierEndPosition,AtomStart,NestedComments,true);
    if AtomStart>length(ProcText) then exit;
    if ProcText[AtomStart] in ['[','('] then begin
      if not ReadTilPascalBracketClose(ProcText,SpecifierEndPosition,NestedComments)
      then
        exit(-1);
    end;
  end;
  if WithSpaceBehindSemicolon and (SpecifierEndPosition<=length(ProcText)) then
  begin
    SpecifierEndPosition:=FindLineEndOrCodeAfterPosition(ProcText,
                                       SpecifierEndPosition+1,0,NestedComments);
  end;
  //DebugLn(['SearchProcSpecifier ',copy(ProcText,Result,SpecifierEndPosition-Result)]);
end;

function RemoveProcSpecifier(const ProcText, Specifier: string;
  NestedComments: boolean): string;
var
  EndPos: integer;
  StartPos: LongInt;
begin
  Result:=ProcText;
  StartPos:=SearchProcSpecifier(Result,Specifier,EndPos,NestedComments);
  if StartPos>=1 then
    Result:=copy(Result,1,StartPos-1)+copy(Result,EndPos,length(Result));
end;

function ReadNextPascalAtom(const Source: string; var Position: integer; out
  AtomStart: integer; NestedComments: boolean; SkipDirectives: boolean): string;
begin
  ReadRawNextPascalAtom(Source,Position,AtomStart,NestedComments,SkipDirectives);
  Result:=copy(Source,AtomStart,Position-AtomStart);
end;

{$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
{$R-}
procedure ReadRawNextPascalAtom(const Source: string;
  var Position: integer; out AtomStart: integer; NestedComments: boolean;
  SkipDirectives: boolean);
var
  Len:integer;
  SrcPos, SrcStart, SrcAtomStart: PChar;
begin
  Len:=length(Source);
  if Position>Len then begin
    Position:=Len+1;
    AtomStart:=Position;
    exit;
  end;
  SrcStart:=PChar(Source);
  SrcPos:=@Source[Position];
  ReadRawNextPascalAtom(SrcPos,SrcAtomStart,SrcStart+len,NestedComments,SkipDirectives);
  Position:=SrcPos-SrcStart+1;
  AtomStart:=SrcAtomStart-SrcStart+1;
end;
{$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}

procedure ReadRawNextPascalAtom(var Position: PChar; out AtomStart: PChar;
  const SrcEnd: PChar; NestedComments: boolean; SkipDirectives: boolean);
var
  c1,c2:char;
  CommentLvl: Integer;
  Src: PChar;
begin
  Src:=Position;
  // read till next atom
  while true do begin
    case Src^ of
    #0:
      if (SrcEnd=nil) or (Src>=SrcEnd) then
        break
      else
        inc(Src);
    #1..#32:  // spaces and special characters
      inc(Src);
    #$EF:
      if (Src[1]=#$BB)
      and (Src[2]=#$BF) then begin
        // skip UTF BOM
        inc(Src,3);
      end else begin
        break;
      end;
    '{':    // comment start or compiler directive
      begin
        if (Src[1]='$') and (not SkipDirectives) then
          // compiler directive
          break
        else if Src[1]=#3 then begin
          // codetools comment => skip
          inc(Src,2);
          repeat
            case Src^ of
            #0:
              if (SrcEnd=nil) or (Src>=SrcEnd) then
                break;
            #3:
              if Src[1]='}' then begin
                inc(Src,2);
                break;
              end;
            end;
            inc(Src);
          until false;
        end else begin
          // Pascal comment => skip
          CommentLvl:=1;
          while true do begin
            inc(Src);
            case Src^ of
            #0:
              if (SrcEnd=nil) or (Src>=SrcEnd) then
                break;
            '{':
              if NestedComments then
                inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then begin
                  inc(Src);
                  break;
                end;
              end;
            end;
          end;
        end;
      end;
    '/':  // comment or real division
      if (Src[1]='/') then begin
        // comment start -> read til line end
        inc(Src);
        while not (Src^ in [#0,#10,#13]) do
          inc(Src);
      end else
        break;
    '(':  // comment, bracket or compiler directive
      if (Src[1]='*') then begin
        if (Src[2]='$') and (not SkipDirectives) then
          // compiler directive
          break
        else begin
          // comment start -> read til comment end
          inc(Src,2);
          CommentLvl:=1;
          while true do begin
            case Src^ of
            #0:
              if (SrcEnd=nil) or (Src>=SrcEnd) then
                break
              else
                inc(Src);
            '(':
              if NestedComments and (Src[1]='*') then
                inc(CommentLvl);
            '*':
              if (Src[1]=')') then begin
                dec(CommentLvl);
                if CommentLvl=0 then begin
                  inc(Src,2);
                  break;
                end;
                inc(Position);
              end;
            end;
            inc(Src);
          end;
        end;
      end else
        // round bracket open
        break;
    else
      break;
    end;
  end;
  // read atom
  AtomStart:=Src;
  c1:=Src^;
  case c1 of
  #0:
    ;
  'A'..'Z','a'..'z','_':
    begin
      // identifier
      inc(Src);
      while IsIdentChar[Src^] do
        inc(Src);
    end;
  '0'..'9': // number
    begin
      inc(Src);
      // read numbers
      while (Src^ in ['0'..'9']) do
        inc(Src);
      if (Src^='.')
      and (Src[1]<>'.') then begin
        // real type number
        inc(Src);
        while (Src^ in ['0'..'9']) do
          inc(Src);
      end;
      if (Src^ in ['e','E']) then begin
        // read exponent
        inc(Src);
        if (Src^='-') then inc(Src);
        while (Src^ in ['0'..'9']) do
          inc(Src);
      end;
    end;
  '''','#':  // string constant
    begin
      while true do begin
        case Src^ of
        #0:
          if (SrcEnd=nil) or (Src>=SrcEnd) then
            break
          else
            inc(Src);
        '#':
          begin
            inc(Src);
            while Src^ in ['0'..'9'] do
              inc(Src);
          end;
        '''':
          begin
            inc(Src);
            while not (Src^ in ['''',#0]) do
              inc(Src);
            if Src^='''' then
              inc(Src);
          end;
        else
          break;
        end;
      end;
    end;
  '$':  // hex constant
    begin
      inc(Src);
      while IsHexNumberChar[Src^] do
        inc(Src);
    end;
  '&':  // octal constant or keyword as identifier (e.g. &label)
    begin
      inc(Src);
      if Src^ in ['0'..'7'] then begin
        while Src^ in ['0'..'7'] do
          inc(Src);
      end else begin
        while IsIdentChar[Src^] do
          inc(Src);
      end;
    end;
  '{':  // compiler directive (it can not be a comment, because see above)
    begin
      CommentLvl:=1;
      while true do begin
        inc(Src);
        case Src^ of
        #0:
          if (SrcEnd=nil) or (Src>=SrcEnd) then
            break;
        '{':
          if NestedComments then
            inc(CommentLvl);
        '}':
          begin
            dec(CommentLvl);
            if CommentLvl=0 then begin
              inc(Src);
              break;
            end;
          end;
        end;
      end;
    end;
  '(':  // bracket or compiler directive
    if (Src[1]='*') then begin
      // compiler directive -> read til comment end
      inc(Src,2);
      while (Src^<>#0) and ((Src^<>'*') or (Src[1]<>')')) do
        inc(Src);
      inc(Src,2);
    end else
      // round bracket open
      inc(Src);
  #192..#255:
    begin
      // read UTF8 character
      inc(Src);
      if ((ord(c1) and %11100000) = %11000000) then begin
        // could be 2 byte character
        if (ord(Src[0]) and %11000000) = %10000000 then
          inc(Src);
      end
      else if ((ord(c1) and %11110000) = %11100000) then begin
        // could be 3 byte character
        if ((ord(Src[0]) and %11000000) = %10000000)
        and ((ord(Src[1]) and %11000000) = %10000000) then
          inc(Src,2);
      end
      else if ((ord(c1) and %11111000) = %11110000) then begin
        // could be 4 byte character
        if ((ord(Src[0]) and %11000000) = %10000000)
        and ((ord(Src[1]) and %11000000) = %10000000)
        and ((ord(Src[2]) and %11000000) = %10000000) then
          inc(Src,3);
      end;
    end;
  else
    inc(Src);
    c2:=Src^;
    // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **
    if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
    or ((c1='<') and (c2='>'))
    or ((c1='.') and (c2='.'))
    or ((c1='*') and (c2='*'))
    then
      inc(Src)
    else if ((c1='@') and (c2='@')) then begin
      // @@ label
      repeat
        inc(Src);
      until (not IsIdentChar[Src^]);
    end;
  end;
  Position:=Src;
end;

procedure ReadPriorPascalAtom(const Source: string; var Position: integer; out
  AtomEnd: integer; NestedComments: boolean);
var
  CommentLvl, PrePos, OldPrePos: integer;
  IsStringConstant: boolean;

  procedure ReadStringConstantBackward;
  var PrePos: integer;
  begin
    while (Position>1) do begin
      case Source[Position-1] of
      '''':
        begin
          dec(Position);
          repeat
            dec(Position);
          until (Position<1) or (Source[Position] in [#0,#10,#13,'''']);
        end;
      '0'..'9','A'..'Z','a'..'z':
        begin
          // test if char constant
          PrePos:=Position-1;
          while (PrePos>1) and (IsHexNumberChar[Source[PrePos]]) do
            dec(PrePos);
          if (PrePos<1) then break;
          if (Source[PrePos]='$') then begin
            dec(PrePos);
            if (PrePos<1) then break;
          end;
          if (Source[PrePos]='#') then
            Position:=PrePos
          else
            break;
        end;
      else
        break;
      end;
    end;
  end;

  procedure ReadBackTilCodeLineEnd;
  begin
    dec(Position);
    if (Position>=1) and (Source[Position] in [#10,#13])
    and (Source[Position+1]<>Source[Position]) then
      dec(Position);

    // read backwards till line start
    PrePos:=Position;
    while (PrePos>=1) and (not (Source[PrePos] in [#10,#13])) do
      dec(PrePos);
    // read line forward to find out,
    // if line ends in comment or string constant
    IsStringConstant:=false;
    repeat
      inc(PrePos);
      case Source[PrePos] of

      '/':
        if Source[PrePos+1]='/' then begin
          // this was a delphi comment -> skip comment
          Position:=PrePos-1;
          break;
        end;

      '{':
        begin
          inc(PrePos);
          if (PrePos<=Position) and (Source[PrePos]=#3) then begin
            // skip codetools comment
            inc(PrePos);
            while (PrePos<=Position) do begin
              if (Source[PrePos]=#3) and (PrePos<Position)
              and (Source[PrePos+1]='}') then begin
                inc(PrePos,2);
                break;
              end;
              inc(PrePos);
            end;
          end else begin
            // skip pascal comment
            CommentLvl:=1;
            while (PrePos<=Position) do begin
              case Source[PrePos] of
              '{': if NestedComments then inc(CommentLvl);
              '}':
                begin
                  dec(CommentLvl);
                  if CommentLvl=0 then break;
                end;
              end;
              inc(PrePos);
            end;
          end;
        end;

      '(':
        if Source[PrePos+1]='*' then begin
          // skip turbo pascal comment
          inc(PrePos,2);
          while (PrePos<Position)
          and ((Source[PrePos]<>'*') or (Source[PrePos+1]<>')')) do
            inc(PrePos);
          inc(PrePos);
        end;

      '''':
        begin
          // a string constant -> skip it
          OldPrePos:=PrePos;
          while (PrePos<Position) do begin
            inc(PrePos);
            case Source[PrePos] of
            '''':
              break;

            #0,#10,#13:
              begin
                // string constant right border is the line end
                // -> last atom of line found
                IsStringConstant:=true;
                break;
              end;

            end;
          end;
          if IsStringConstant then break;
        end;

      #10,#13:
        // no comment and no string constant found
        break;

      end;
    until PrePos>=Position;
  end;

type
  TNumberType = (ntDecimal, ntHexadecimal, ntBinary, ntIdentifier,
    ntCharConstant, ntFloat, ntFloatWithExponent);
  TNumberTypes = set of TNumberType;

const
  AllNumberTypes: TNumberTypes = [ntDecimal, ntHexadecimal, ntBinary,
    ntIdentifier, ntCharConstant, ntFloat, ntFloatWithExponent];

var c1, c2: char;
  ForbiddenNumberTypes: TNumberTypes;
begin
  // Skip all spaces and comments
  CommentLvl:=0;
  dec(Position);
  IsStringConstant:=false;
  OldPrePos:=0;
  while Position>=1 do begin
    if IsCommentEndChar[Source[Position]] then begin
      case Source[Position] of

      '}':
        begin
          dec(Position);
          if (Position>=1) and (Source[Position]=#3) then begin
            // codetools skip comment {#3 #3}
            dec(Position);
            while (Position>=1) do begin
              if (Source[Position]=#3) and (Position>1)
              and (Source[Position-1]='}') then begin
                dec(Position,2);
                break;
              end;
              dec(Position);
            end;
          end else begin
            // pascal comment {}
            CommentLvl:=1;
            while (Position>=1) and (CommentLvl>0) do begin
              case Source[Position] of
              '}': if NestedComments then inc(CommentLvl);
              '{': dec(CommentLvl);
              end;
              dec(Position);
            end;
          end;
        end;

      #10,#13: // possible Delphi comment
        ReadBackTilCodeLineEnd;

      ')': // old turbo pascal comment
        if (Position>1) and (Source[Position-1]='*') then begin
          dec(Position,3);
          while (Position>=1)
          and ((Source[Position]<>'(') or (Source[Position+1]<>'*')) do
            dec(Position);
          dec(Position);
        end else
          break;

      end;
    end else if IsSpaceChar[Source[Position]] then begin
      repeat
        dec(Position);
      until (Position<1) or (Source[Position] in [#10,#13])
      or (not (IsSpaceChar[Source[Position]]));
    end else begin
      break;
    end;
  end;
  // Position now points to the last char of the prior atom
  AtomEnd:=Position+1;
  if Position<1 then begin
    Position:=1;
    AtomEnd:=1;
    exit;
  end;
  // read atom
  if IsStringConstant then begin
    Position:=OldPrePos;
    if (Position>1) and (Source[Position-1]='''') then begin
      ReadStringConstantBackward;
    end;
    exit;
  end;
  c2:=Source[Position];
  case c2 of
    '_','A'..'Z','a'..'z':
      begin
        // identifier or keyword or hexnumber
        while (Position>1) do begin
          if (IsIdentChar[Source[Position-1]]) then
            dec(Position)
          else begin
            case UpChars[Source[Position-1]] of
            '@':
              // assembler label
              if (Position>2)
              and (Source[Position-2]='@') then
                dec(Position,2);
            '$':
              // hex number
              dec(Position);
            end;
            break;
          end;
        end;
      end;
    '''':
      begin
        inc(Position);
        ReadStringConstantBackward;
      end;
    '0'..'9':
      begin
        // could be a decimal number, an identifier, a hex number,
        // a binary number, a char constant, a float, a float with exponent
        ForbiddenNumberTypes:=[];
        while true do begin
          case UpChars[Source[Position]] of
          '0'..'1':
            ;
          '2'..'9':
            ForbiddenNumberTypes:=ForbiddenNumberTypes+[ntBinary];
          'A'..'D','F':
            ForbiddenNumberTypes:=ForbiddenNumberTypes
               +[ntBinary,ntDecimal,ntCharConstant,ntFloat,ntFloatWithExponent];
          'E':
            ForbiddenNumberTypes:=ForbiddenNumberTypes
               +[ntBinary,ntDecimal,ntCharConstant,ntFloat];
          'G'..'Z','_':
            ForbiddenNumberTypes:=AllNumberTypes-[ntIdentifier];
          '.':
            begin
              // could be the point of a float
              if (ntFloat in ForbiddenNumberTypes)
              or (Position<=1) or (Source[Position-1]='.') then begin
                inc(Position);
                break;
              end;
              dec(Position);
              // this was the part of a float after the point
              //  -> read decimal in front
              ForbiddenNumberTypes:=AllNumberTypes-[ntDecimal];
            end;
          '+','-':
            begin
              // could be part of an exponent
              if (ntFloatWithExponent in ForbiddenNumberTypes)
              or (Position<=1)
              or (not (Source[Position-1] in ['e','E']))
              then begin
                inc(Position);
                break;
              end;
              dec(Position);
              // this was the exponent of a float -> read the float
              ForbiddenNumberTypes:=AllNumberTypes-[ntFloat];
            end;
          '#': // char constant found
            begin
              if (ntCharConstant in ForbiddenNumberTypes) then
                inc(Position);
              ReadStringConstantBackward;
              break;
            end;
          '$':
            begin
              // hexadecimal number found
              if (ntHexadecimal in ForbiddenNumberTypes) then
                inc(Position);
              break;
            end;
          '%':
            begin
              // binary number found
              if (ntBinary in ForbiddenNumberTypes) then
                inc(Position);
              break;
            end;
          '@':
            begin
              if (Position=1) or (Source[Position-1]<>'@')
              or (([ntIdentifier,ntDecimal]*ForbiddenNumberTypes)=[]) then
                // atom start found
                inc(Position)
              else
                // label found
                dec(Position);
              break;
            end;
          else
            begin
              inc(Position);
              break;
            end;
          end;
          if ForbiddenNumberTypes=AllNumberTypes then begin
            inc(Position);
            break;
          end;
          if Position<=1 then break;
          dec(Position);
        end;
        if IsIdentStartChar[Source[Position]] then begin
          // it is an identifier
        end;
      end;

    ';': ;
    ':': ;
    ',': ;
    '(': ;
    ')': ;
    '[': ;
    ']': ;

    else
      begin
        if Position>1 then begin
          c1:=Source[Position-1];
          // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
          if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
          or ((c1='<') and (c2='>'))
          or ((c1='>') and (c2='<'))
          or ((c1='.') and (c2='.'))
          or ((c1='*') and (c2='*'))
          or ((c1='@') and (c2='@'))
          then begin
            dec(Position);
          end;
        end;
      end;
  end;
end;

function ReadTilPascalBracketClose(const Source: string; var Position: integer;
  NestedComments: boolean): boolean;
// Input: Position points right after the opening bracket
// Output: Position points right after the closing bracket
var
  CloseBracket: Char;
  AtomStart: LongInt;
  Len: Integer;
begin
  Result:=false;
  Len:=length(Source);
  if Position>Len+1 then
    exit;  // no bracket open found
  case Source[Position-1] of
  '{': CloseBracket:='}';
  '(': CloseBracket:=')';
  '[': CloseBracket:=']';
  else
    exit; // no bracket open found
  end;
  AtomStart:=Position;
  while Position<=Len do begin
    ReadRawNextPascalAtom(Source,Position,AtomStart,NestedComments,true);
    //DebugLn(['ReadTilPascalBracketClose ',copy(Source,AtomStart,Position-AtomStart)]);
    if Position>Len then
      exit; // CloseBracket not found
    case Source[AtomStart] of
    '{','(','[':
      begin
        if not ReadTilPascalBracketClose(Source,Position) then exit;
      end;
    '}',')',']':
      if Source[AtomStart]=CloseBracket then begin
        // CloseBracket found
        Result:=true;
        exit;
      end else begin
        exit; // a bracket is closed, that was never opened
      end;
    end;
  end;
end;

function GetAtomLength(p: PChar; NestedComments: boolean): integer;
var
  c1: Char;
  CommentLvl: Integer;
  c2: Char;
  OldP: PChar;
begin
  OldP:=p;
  // read atom
  c1:=p^;
  case c1 of
   'A'..'Z','a'..'z','_':
    begin
      // identifier
      inc(p);
      while (IsIdentChar[p^]) do
        inc(p);
    end;
   '0'..'9': // number
    begin
      inc(p);
      // read numbers
      while (p^ in ['0'..'9']) do
        inc(p);
      if (p^='.')
      and (p[1]<>'.') then begin
        // real type number
        inc(p);
        while (p^ in ['0'..'9']) do
          inc(p);
      end;
      if (p^ in ['e','E']) then begin
        // read exponent
        inc(p);
        if (p^='-') then inc(p);
        while (p^ in ['0'..'9']) do
          inc(p);
      end;
    end;
   '''','#':  // string constant
    begin
      while true do begin
        case p^ of
        '#':
          begin
            inc(p);
            while (p^ in ['0'..'9']) do
              inc(p);
          end;
        '''':
          begin
            inc(p);
            while not (p^ in ['''',#10,#13]) do
              inc(p);
            inc(p);
          end;
        else
          break;
        end;
      end;
    end;
   '$':  // hex constant
    begin
      inc(p);
      while (IsHexNumberChar[p^]) do
        inc(p);
    end;
   '{':  // compiler directive
    begin
      CommentLvl:=1;
      while true do begin
        inc(p);
        case p^ of
        #0:  break;
        '{': if NestedComments then
            inc(CommentLvl);
        '}':
          begin
            dec(CommentLvl);
            if CommentLvl=0 then begin
              inc(p);
              break;
            end;
          end;
        end;
      end;
    end;
   '(':  // bracket or compiler directive
    begin
      inc(p);
      if (p^<>'*') then begin
        // round bracket open
      end else begin
        // comment
        CommentLvl:=1;
        inc(p);
        while true do begin
          case p^ of
          #0: break;
          '*':
            if p[1]=')' then begin
              inc(p,2);
              dec(CommentLvl);
              if CommentLvl=0 then
                break;
            end else
              inc(p);
          '(':
            if (p[1]='*') and NestedComments then begin
              inc(CommentLvl);
              inc(p,2);
            end else
              inc(p);
          else
            inc(p);
          end;
        end;
      end;
    end;
  else
    inc(p);
    c2:=p^;
    // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, .., ><
    if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
    or ((c1='<') and (c2='>'))
    or ((c1='>') and (c2='<'))
    or ((c1='.') and (c2='.'))
    or ((c1='*') and (c2='*'))
    then
      inc(p)
    else if ((c1='@') and (c2='@')) then begin
      // @@ label
      repeat
        inc(p);
      until (not IsIdentChar[p^]);
    end;
  end;
  Result:=P-OldP;
end;

function GetAtomString(p: PChar; NestedComments: boolean): string;
var
  l: LongInt;
begin
  if p=nil then exit('');
  l:=GetAtomLength(p,NestedComments);
  SetLength(Result,l);
  if l>0 then
    System.Move(p^,Result[1],length(Result));
end;

function FindStartOfAtom(const Source: string; Position: integer): integer;

  procedure ReadStringConstantBackward(var p: integer);
  var
    PrePos: integer;
    StartPos: LongInt;
  begin
    StartPos:=p;
    while (p>1) do begin
      case Source[p-1] of
      '''':
        begin
          PrePos:=p;
          dec(PrePos);
          repeat
            dec(PrePos);
            if (PrePos<1) or (Source[PrePos] in [#10,#13]) then begin
              // the StartPos was the start of a string constant
              p:=StartPos-1;
              exit;
            end;
          until (Source[PrePos]='''');
          p:=PrePos;
        end;
      '0'..'9','A'..'Z','a'..'z':
        begin
          // test if char constant
          PrePos:=p-1;
          while (PrePos>1) and (IsHexNumberChar[Source[PrePos]]) do
            dec(PrePos);
          if (PrePos<1) then break;
          if (Source[PrePos]='$') then begin
            dec(PrePos);
            if (PrePos<1) then break;
          end;
          if (Source[PrePos]='#') then
            p:=PrePos
          else
            break;
        end;
      else
        break;
      end;
    end;
  end;

type
  TNumberType = (ntDecimal, ntHexadecimal, ntBinary, ntIdentifier,
    ntCharConstant, ntFloat, ntFloatWithExponent);
  TNumberTypes = set of TNumberType;

const
  AllNumberTypes: TNumberTypes = [ntDecimal, ntHexadecimal, ntBinary,
    ntIdentifier, ntCharConstant, ntFloat, ntFloatWithExponent];
var
  c: Char;
  ForbiddenNumberTypes: TNumberTypes;
  c2: Char;
begin
  Result:=Position;
  if (Result<1) then exit;
  if Result>length(Source) then begin
    Result:=length(Source);
    exit;
  end;

  c:=Source[Result];
  case c of
  '_','A'..'Z','a'..'z':
    begin
      // identifier or keyword or hexnumber
      while (Result>1) do begin
        if (IsIdentChar[Source[Result-1]]) then
          dec(Result)
        else begin
          case UpChars[Source[Result-1]] of
          '@':
            // assembler label
            if (Result>2)
            and (Source[Result-2]='@') then
              dec(Result,2);
          '$':
            // hex number
            dec(Result);
          '&':
            // &keyword
            dec(Result);
          end;
          break;
        end;
      end;
    end;
  '''':
    begin
      // could be start or end
      inc(Result);
      ReadStringConstantBackward(Result);
    end;
  '0'..'9':
    begin
      // could be a decimal number, an identifier, a hex number,
      // a binary number, a char constant, a float, a float with exponent
      ForbiddenNumberTypes:=[];
      while true do begin
        case UpChars[Source[Result]] of
        '0'..'1':
          ;
        '2'..'9':
          ForbiddenNumberTypes:=ForbiddenNumberTypes+[ntBinary];
        'A'..'D','F':
          ForbiddenNumberTypes:=ForbiddenNumberTypes
             +[ntBinary,ntDecimal,ntCharConstant,ntFloat,ntFloatWithExponent];
        'E':
          ForbiddenNumberTypes:=ForbiddenNumberTypes
             +[ntBinary,ntDecimal,ntCharConstant,ntFloat];
        'G'..'Z','_':
          ForbiddenNumberTypes:=AllNumberTypes-[ntIdentifier];
        '.':
          begin
            // could be the point of a float
            if (ntFloat in ForbiddenNumberTypes)
            or (Result<=1) or (Source[Result-1]='.') then begin
              inc(Result);
              break;
            end;
            dec(Result);
            // this was the part of a float after the point
            //  -> read decimal in front
            ForbiddenNumberTypes:=AllNumberTypes-[ntDecimal];
          end;
        '+','-':
          begin
            // could be part of an exponent
            if (ntFloatWithExponent in ForbiddenNumberTypes)
            or (Result<=1)
            or (not (Source[Result-1] in ['e','E']))
            then begin
              inc(Result);
              break;
            end;
            dec(Result);
            // this was the exponent of a float -> read the float
            ForbiddenNumberTypes:=AllNumberTypes-[ntFloat];
          end;
        '#': // char constant found
          begin
            if (ntCharConstant in ForbiddenNumberTypes) then
              inc(Result);
            ReadStringConstantBackward(Result);
            break;
          end;
        '$':
          begin
            // hexadecimal number found
            if (ntHexadecimal in ForbiddenNumberTypes) then
              inc(Result);
            break;
          end;
        '%':
          begin
            // binary number found
            if (ntBinary in ForbiddenNumberTypes) then
              inc(Result);
            break;
          end;
        '@':
          begin
            if (Result=1) or (Source[Result-1]<>'@')
            or (([ntIdentifier,ntDecimal]*ForbiddenNumberTypes)=[]) then
              // atom start found
              inc(Result)
            else
              // label found
              dec(Result);
            break;
          end;
        else
          begin
            inc(Result);
            break;
          end;
        end;
        if ForbiddenNumberTypes=AllNumberTypes then begin
          inc(Result);
          break;
        end;
        if Result<=1 then break;
        dec(Result);
      end;
      if IsIdentStartChar[Source[Result]] then begin
        // it is an identifier
      end;
    end;

  else
    if Result>1 then begin
      c2:=Source[Result-1];
      // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><, @@ ..
      if ((c2='=') and  (IsEqualOperatorStartChar[c]))
      or ((c='<') and (c2='>'))
      or ((c='>') and (c2='<'))
      or ((c='.') and (c2='.'))
      or ((c='*') and (c2='*'))
      or ((c='@') and (c2='@'))
      then begin
        dec(Result);
      end;
    end;
  end;
end;

function FindEndOfAtom(const Source: string; Position: integer): integer;
// comments have length 0
var
  p: PChar;
begin
  Result:=FindStartOfAtom(Source,Position);
  if (Result>=1) and (Result<=length(Source)) then begin
    p:=@Source[Result];
    case p^ of
    #0..#31,' ': exit;
    '{': exit;
    '/': if p[1]='/' then exit;
    '(': if p[1]='*' then exit;
    end;
    inc(Result,GetAtomLength(p,false));
  end;
end;

function LineEndCount(const Txt: string;
  out LengthOfLastLine: integer): integer;
begin
  Result:=LineEndCount(Txt,1,length(Txt)+1,LengthOfLastLine);
end;

function FindFirstNonSpaceCharInLine(const Source: string;
  Position: integer): integer;
begin
  Result:=Position;
  if (Result<0) then Result:=1;
  if (Result>length(Source)) then Result:=length(Source);
  if Result=0 then exit;
  // search beginning of line
  while (Result>1) and (not (Source[Result-1] in [#10,#13])) do
    dec(Result);
  // search
  while (Result<=length(Source)) and (Source[Result] in [' ',#9]) do inc(Result);
end;

function GetLineIndent(const Source: string; Position: integer): integer;
var LineStart: integer;
begin
  Result:=0;
  LineStart:=Position;
  if LineStart=0 then exit;
  if (LineStart<0) then LineStart:=1;
  if (LineStart>length(Source)+1) then LineStart:=length(Source)+1;
  // search beginning of line
  while (LineStart>1) and not (Source[LineStart-1] in [#10,#13]) do
    dec(LineStart);
  // search code
  Result:=LineStart;
  while (Result<=length(Source)) and (Source[Result]=' ') do inc(Result);
  dec(Result,LineStart);
end;

function FindLineEndOrCodeAfterPosition(const Source: string;
   Position, MaxPosition: integer; NestedComments: boolean;
   StopAtDirectives: boolean; SkipEmptyLines: boolean;
   IncludeLineEnd: boolean): integer;
{ search forward for a line end or code
  ignore line ends in comments
  Result is Position of Start of Line End
  
  if SkipEmptyLines=true, it will skip empty lines at the end
  
  Examples: | is the Position and # is the Result
  
  1. var i: integer;|#
     var j: integer;

     If IncludeLineEnd then
            var i: integer;|
          #  var j: integer;
     
  2. var i: integer;| (*
     *) #var j: integer;
     
  3. SkipEmptyLines=false
     var i: integer;|
     #
     // comment
     var j: integer;

     if IncludeLineEnd then the # will be one line below
     
  4. SkipEmptyLines=true
     var i: integer;|

     #// comment
     var j: integer;
}
var SrcLen: integer;

  procedure DoSkipEmptyLines(var p: integer);
  var
    r: LongInt;
  begin
    r:=p;
    repeat
      while (r<=SrcLen) and (Source[r] in [' ',#9]) do inc(r);
      if (r<=SrcLen) and (Source[r] in [#10,#13]) then begin
        // an empty line => skip
        p:=r;// remember position in front of new line characters
        inc(r);
        if (r<=SrcLen) and (Source[r] in [#10,#13]) and (Source[r]<>Source[r-1])
        then
          inc(r);
      end else begin
        exit;
      end;
    until false;
  end;

begin
  SrcLen:=length(Source);
  if SrcLen>MaxPosition then SrcLen:=MaxPosition;
  Result:=Position;
  if Result=0 then exit;
  while (Result<=SrcLen) do begin
    case Source[Result] of
      '/':
        if (Result<SrcLen) and (Source[Result+1]='/') then
          Result:=FindCommentEnd(Source,Result,NestedComments)
        else
          inc(Result);
      '{':
        if (Result<SrcLen) and (Source[Result+1]='$') and StopAtDirectives then
          exit  // stop at directive {$ }
        else
          Result:=FindCommentEnd(Source,Result,NestedComments);
      '(':
        if (Result<SrcLen) and (Source[Result+1]='*') then begin
          if (Result+1<SrcLen) and (Source[Result+2]='$') and StopAtDirectives then
            exit;  // stop at directive (*$ *)
          Result:=FindCommentEnd(Source,Result,NestedComments);
        end else
          inc(Result); // normal bracket (
      #10,#13:
        begin
          if SkipEmptyLines then DoSkipEmptyLines(Result);
          if IncludeLineEnd and (Result<=SrcLen) and (Source[Result] in [#10,#13])
          then begin
            inc(Result);
            if (Result<=SrcLen) and (Source[Result] in [#10,#13])
            and (Source[Result-1]<>Source[Result]) then
              inc(Result);
          end;
          exit;
        end;
      #9,' ',';':
        inc(Result);
    else
      exit;
    end;
  end;
end;

function IsFirstNonSpaceCharInLine(const Source: string; Position: integer
  ): boolean;
begin
  while (Position>1) and (Source[Position-1] in [' ',#9]) do
    dec(Position);
  Result:=(Position=1) or (Source[Position-1] in [#10,#13]);
end;

procedure GuessIndentSize(const Source: string; var IndentSize: integer;
  TabWidth: integer; MaxLineCount: integer = 10000);
{ check all line indents and return the most common.
  Stop after MaxLineCount lines. Ignore empty lines.
}
const
  MaxIndentSize = 20;
var
  IndentCounts: PSizeInt;
  BestCount: SizeInt;

  procedure AddIndent(CurIndent: integer);
  var
    i: SizeInt;
  begin
    if (CurIndent<1) or (CurIndent>MaxIndentSize) then exit;
    i:=IndentCounts[CurIndent-1]+1;
    IndentCounts[CurIndent-1]:=i;
    if BestCount>=i then
      exit;
    IndentSize:=CurIndent;
    BestCount:=i;
  end;

var
  LineNumber: Integer;
  p: PChar;
  CurLineIndent: Integer;
  LastLineIndent: Integer;
begin
  LineNumber:=0;
  if Source='' then exit;
  if TabWidth<=0 then TabWidth:=8;
  Getmem(IndentCounts,SizeOf(SizeInt)*MaxIndentSize);
  try
    FillByte(IndentCounts[0],SizeOf(SizeInt)*MaxIndentSize,0);
    BestCount:=0;
    p:=PChar(Source);
    LastLineIndent:=0;
    repeat
      inc(LineNumber);
      // read indent
      CurLineIndent:=0;
      repeat
        case p^ of
        ' ': inc(CurLineIndent);
        #9:
          begin
            CurLineIndent+=TabWidth;
            CurLineIndent-=(CurLineIndent mod TabWidth);
          end;
        else break;
        end;
        inc(p);
      until false;
      if not (p^ in [#0,#10,#13]) then begin
        // not an empty line
        AddIndent(CurLineIndent-LastLineIndent);
        LastLineIndent:=CurLineIndent;
      end;
      // skip to next line
      repeat
        case p^ of
        #0:
          if p-PChar(Source)=length(Source) then begin
            // end of soure
            exit;
          end;
        #10,#13:
          begin
            // line break
            repeat
              inc(p)
            until not (p^ in [#10,#13]);
            break;
          end;
        end;
        inc(p);
      until false;
    until LineNumber>MaxLineCount;
  finally
    FreeMem(IndentCounts);
  end;
end;

function ReIndent(const Source: string; OldIndent, OldTabWidth,
  NewIndent, NewTabWidth: integer): string;
{ NewTabWidth = 0 means using spaces
}
var
  Src: PChar;
  SrcIndent: Integer;
  DstIndent: Integer;
  i: Integer;
  Dst: PChar;

  procedure Grow;
  var
    Old: PtrUInt;
  begin
    if (Dst^<>#0) or (Dst-PChar(Result)<>length(Result)) then exit;
    // grow
    Old:=Dst-PChar(Result);
    SetLength(Result,(length(Result)*3) div 2);
    FillByte(Result[Old+1],length(Result)-Old,ord('A'));
    Dst:=PChar(Result)+Old;
  end;

  procedure Add(c: char); inline;
  begin
    //debugln(['Add c="',DbgStr(c),'"']);
    if (Dst^=#0) then
      Grow;
    Dst^:=c;
    inc(Dst);
  end;

begin
  Result:=Source;
  if (Result='') or (OldIndent<=0) or (OldTabWidth<0)
  or (NewIndent<0) or (NewTabWidth<0) then exit;
  UniqueString(Result);
  Src:=PChar(Source);
  Dst:=PChar(Result);
  repeat
    // read indent
    SrcIndent:=0;
    repeat
      case Src^ of
      ' ': inc(SrcIndent);
      #9:
        begin
          SrcIndent:=SrcIndent+OldTabWidth;
          SrcIndent:=SrcIndent-(SrcIndent mod SrcIndent);
        end;
      else break;
      end;
      inc(Src);
    until false;
    // write indent
    DstIndent:=((SrcIndent+OldIndent-1) div OldIndent)*NewIndent;
    //debugln(['ReIndent DstIndent=',DstIndent,' Src=',dbgstr(Src^),' at ',Src-PChar(Source)]);
    if NewTabWidth>0 then begin
      for i:=1 to (DstIndent div NewTabWidth) do
        Add(#9);
      for i:=1 to (DstIndent mod NewTabWidth) do
        Add(' ');
    end else begin
      for i:=1 to DstIndent do
        Add(' ');
    end;
    // copy line
    repeat
      case Src^ of
      #0: if Src-PChar(Source)=length(Source) then break;
      #10,#13: break;
      end;
      Add(Src^);
      inc(Src);
    until false;
    // copy line break
    while Src^ in [#10,#13] do begin
      Add(Src^);
      inc(Src);
    end;
  until (Src^=#0) and (Src-PChar(Source)=length(Source));
  SetLength(Result,Dst-PChar(Result));
end;

function FindLineEndOrCodeInFrontOfPosition(const Source: string;
  Position, MinPosition: integer; NestedComments: boolean;
  StopAtDirectives: boolean; SkipSemicolonComma: boolean;
  SkipEmptyLines: boolean): integer;
{ search backward for a line end or code
  ignore line ends in comments or at the end of comment lines
   (comment lines are lines without code and at least one comment)
  comment lines directly in front are skipped too
  if SkipEmptyLines=true then empty lines are skipped too.
  Result is Position of Start of Line End

  examples: Position points at char 'a'

    1:  |
    2: a:=1;

    1:  b:=1; |
    2:  // comment for below
    3:  // comment for below
    4:  a:=1;

    1:  |
    2: (* comment belongs to the following line *)
    3:  a:=1;

    1: end; (* comment belongs to the first line
    2: *)| a:=1;

    1: b:=1; // comment |
    2: a:=1;

    1: b:=1; (*
    2: comment *)   |
    3: a:=1;
}
var SrcStart: integer;

  function IsSpace(c: char): boolean;
  begin
    if SkipSemicolonComma then
      Result:=c in [' ',#9,';',',']
    else
      Result:=c in [' ',#9];
  end;

  function ReadComment(var p: integer; SubComment: boolean): boolean;
  // true if comment was skipped
  // false if not skipped, because comment is compiler directive or simple bracket
  var OldP: integer;
    IsDirective: Boolean;
  begin
    //debugln(['ReadComment ',dbgstr(copy(Source,p-5,5))+'|'+Source[p]+dbgstr(copy(Source,p+1,5))]);
    OldP:=p+1;
    repeat
      IsDirective:=false;
      case Source[p] of
        '}':
          begin
            dec(p);
            if (p>SrcStart) and (Source[p]=#3) then begin
              // codetools skip comment
              dec(p);
              while (p>SrcStart) do begin
                if (Source[p]=#3)
                and (Source[p-1]='{') then begin
                  dec(p);
                  break;
                end;
                dec(p);
              end;
            end else begin
              // Pascal comment {}
              while (p>SrcStart) and (Source[p]<>'{') do begin
                if NestedComments and (Source[p]='}') then
                  ReadComment(p,true)
                else
                  dec(p);
              end;
              IsDirective:=(p>=SrcStart) and (Source[p+1]='$');
              dec(p);
            end;
          end;
        ')':
          begin
            dec(p);
            if (p>SrcStart) and (Source[p]='*') then begin
              // tp comment (* *)
              dec(p);
              while (p>SrcStart)
              and ((Source[p-1]<>'(') or (Source[p]<>'*')) do begin
                if NestedComments and ((Source[p]=')') and (Source[p-1]='*')) then
                  ReadComment(p,true)
                else
                  dec(p);
              end;
              IsDirective:=(p>=SrcStart) and (Source[p+1]='$');
              dec(p,2);
            end else begin
              // normal bracket
              // => position behind code
              p:=OldP;
              exit(false);
            end;
          end;
      else
        exit(true);
      end;
      if SubComment then exit(true); // always skip nested comments
      if IsDirective and StopAtDirectives then begin
        // directive can not be skipped
        p:=OldP;
        exit(false);
      end;
      // it is a normal comment
      // check if it belongs to the code in front
      while (p>=SrcStart) and IsSpace(Source[p]) do
        dec(p);
      if (p<SrcStart) or (Source[p] in [#10,#13]) then begin
        // empty line in front of comment => comment can be skipped
        exit(true);
      end;
      if not (Source[p] in [')','}']) then begin
        // code => comment belongs to code in front
        p:=OldP;
        exit(false);
      end;
      // read next comment
    until false;
  end;

var
  TestPos: integer;
  LineStartPos: LongInt;
  IsEmpty: Boolean;
  LineEndPos: Integer;
  p: LongInt;
begin
  SrcStart:=MinPosition;
  if SrcStart<1 then SrcStart:=1;
  if (Position<=SrcStart) then begin
    Result:=SrcStart;
    exit;
  end;
  // simple heuristic
  // will fail on lines: // }
  Result:=-1;
  p:=Position-1;
  if p>length(Source) then p:=length(Source);
  while (p>=SrcStart) do begin
    case Source[p] of
      #10,#13:
        begin
          // line end found (outside comments)
          if (p>SrcStart) and (Source[p-1] in [#10,#13])
          and (Source[p]<>Source[p-1]) then
            dec(p);
          LineEndPos:=p; // start of line end
          Result:=LineEndPos;
          // test if in a // comment
          LineStartPos:=p;
          IsEmpty:=true;
          while (LineStartPos>SrcStart) do begin
            case Source[LineStartPos-1] of
              #10,#13: break;
              ' ',#9: ;
              ';',',': if not SkipSemicolonComma then IsEmpty:=false;
            else IsEmpty:=false;
            end;
            dec(LineStartPos);
          end;
          if IsEmpty then begin
            // the line is empty => return start of line end
            p:=LineEndPos;
            if SkipEmptyLines then begin
              // skip all empty lines
              LineStartPos:=p;
              while (LineStartPos>SrcStart) do begin
                case Source[LineStartPos-1] of
                  #10,#13:
                    begin
                      // empty line
                      LineEndPos:=LineStartPos-1;
                      if (LineEndPos>SrcStart) and (Source[LineEndPos-1] in [#10,#13])
                      and (Source[LineEndPos]<>Source[LineEndPos-1]) then
                        dec(LineEndPos);
                      p:=LineEndPos;
                    end;
                  ' ',#9: ;
                else
                  // not empty
                  break;
                end;
                dec(LineStartPos);
              end;
            end;
            break;
          end;
          // line is not empty
          TestPos:=LineStartPos;
          while (Source[TestPos] in [' ',#9]) do inc(TestPos);
          if (Source[TestPos]='/') and (Source[TestPos+1]='/') then begin
            // the whole line is a // comment
            // this comment belongs to the code behind
            // => continue on next line
            p:=LineStartPos-1;
            continue;
          end;
          dec(p);
        end;

      '}',')':
        if not ReadComment(p,false) then break;
      ' ',#9:
        dec(p);
      ';',',':
        begin
          if not SkipSemicolonComma then begin
            // code found
            inc(p);
            break;
          end;
          dec(p);
        end;
    else
      // code found
      inc(p);
      break;
    end;
  end;
  if Result<1 then Result:=p;
  if Result<SrcStart then Result:=SrcStart;
end;

function FindFirstLineEndAfterInCode(const Source: string;
  Position, MaxPosition: integer; NestedComments: boolean): integer;
{ search forward for a line end
  ignore line ends in comments
  Result is Position of Start of Line End
}
var
  SrcLen: integer;
  CommentEndPos: Integer;
begin
  SrcLen:=length(Source);
  if SrcLen>MaxPosition then SrcLen:=MaxPosition;
  Result:=Position;
  while (Result<=SrcLen) do begin
    case Source[Result] of
      '{','(','/':
        begin
          CommentEndPos:=FindCommentEnd(Source,Result,NestedComments);
          if CommentEndPos>Result then
            Result:=CommentEndPos
          else
            inc(Result);
        end;
      #10,#13:
        exit;
    else
      inc(Result);
    end;
  end;
end;

function ChompLineEndsAtEnd(const s: string): string;
var
  EndPos: Integer;
begin
  EndPos:=length(s)+1;
  while (EndPos>1) and (s[EndPos-1] in [#10,#13]) do dec(EndPos);
  Result:=s;
  SetLength(Result,EndPos-1);
end;

function ChompOneLineEndAtEnd(const s: string): string;
var
  EndPos: Integer;
begin
  Result:=s;
  EndPos:=length(s)+1;
  if (EndPos>1) and (s[EndPos-1] in [#10,#13]) then begin
    dec(EndPos);
    if (EndPos>1) and (s[EndPos-1] in [#10,#13]) and (s[EndPos]<>s[EndPos-1])
    then
      dec(EndPos);
    SetLength(Result,EndPos-1);
  end;
end;

function TrimLineEnds(const s: string; TrimStart, TrimEnd: boolean): string;
var
  StartPos: Integer;
  EndPos: Integer;
  LineEnd: Integer;
begin
  StartPos:=1;
  if TrimStart then begin
    // trim empty lines at start
    while (StartPos<=length(s))
    and (s[StartPos] in [#10,#13]) do begin
      inc(StartPos);
      if (StartPos<=length(s))
      and (s[StartPos] in [#10,#13])
      and (s[StartPos]<>s[StartPos-1]) then
        inc(StartPos);
    end;
  end;
  EndPos:=length(s)+1;
  if TrimEnd then begin
    // trim empty lines at end
    while (EndPos>StartPos)
    and (s[EndPos-1] in [#10,#13]) do begin
      LineEnd:=EndPos-1;
      if (LineEnd>StartPos) and (s[LineEnd-1] in [#10,#13])
      and (s[LineEnd-1]<>s[LineEnd]) then begin
        dec(LineEnd);
      end;
      if (LineEnd>StartPos) and (s[LineEnd-1] in [#10,#13]) then
        EndPos:=LineEnd
      else
        break;
    end;
  end;
  if EndPos-StartPos<length(s) then
    Result:=copy(s,StartPos,EndPos-StartPos)
  else
    Result:=s;
end;

function SrcPosToLineCol(const s: string; Position: integer;
  out Line, Col: integer): boolean;
// returns false if Postion<1 or >length(s)+1
var
  p: LongInt;
  l: Integer;
begin
  if (Position<1) then begin
    Line:=1;
    Col:=1;
    Result:=false;
    exit;
  end;
  l:=length(s);
  if l>Position then l:=Position;
  Line:=1;
  Col:=1;
  p:=1;
  while (p<l) do begin
    case s[p] of
    #10,#13:
      begin
        inc(p);
        if (p<=length(s)) and (s[p] in [#10,#13]) and (s[p-1]<>s[p]) then
        begin
          if p=Position then exit(true);
          inc(p);
        end;
        // new line
        inc(Line);
        Col:=1;
      end;
    else
      inc(p);
      inc(Col);
    end;
  end;
  Result:=p=Position;
end;

function GetBracketLvl(const Src: string; StartPos, EndPos: integer;
  NestedComments: boolean): integer;
var
  SrcLen: Integer;

  procedure ReadComment;
  var
    CommentEndPos: Integer;
  begin
    CommentEndPos:=FindCommentEnd(Src,StartPos,NestedComments);
    if CommentEndPos>=EndPos then begin
      // EndPos is in a comment
      // -> count bracket lvl in comment
      Result:=0;
      case Src[StartPos] of
      '{': inc(StartPos);
      '(','/': inc(StartPos,2);
      end;
    end else
      // continue after the comment
      StartPos:=CommentEndPos;
  end;

  procedure ReadBrackets(ClosingBracket: Char);
  begin
    while StartPos<EndPos do begin
      case Src[StartPos] of
      '{':
        ReadComment;
        
      '/':
        if (StartPos<SrcLen) and (Src[StartPos]='/') then
          ReadComment
        else
          inc(StartPos);

      '(':
        if (StartPos<SrcLen) and (Src[StartPos]='*') then
          ReadComment
        else begin
          inc(Result);
          inc(StartPos);
          ReadBrackets(')');
        end;

      '[':
        begin
          inc(Result);
          inc(StartPos);
          ReadBrackets(']');
        end;

      ')',']':
        if (Result>0) then begin
          if ClosingBracket=Src[StartPos] then
            dec(Result) // for example: ()
          else
            Result:=0; // for example: [)
          exit;
        end;

      end;
      inc(StartPos);
    end;
  end;

begin
  Result:=0;
  SrcLen:=length(Src);
  if (StartPos<1) then StartPos:=1;
  if (StartPos>SrcLen) or (EndPos<StartPos) then exit;
  if (EndPos>SrcLen) then EndPos:=SrcLen;
  ReadBrackets(#0);
end;

function FindFirstLineEndInFrontOfInCode(const Source: string;
   Position, MinPosition: integer; NestedComments: boolean): integer;
{ search backward for a line end
  ignore line ends in comments
  Result will be at the Start of the Line End
}
var
  SrcStart: integer;

  procedure ReadComment(var P: integer);
  begin
    case Source[P] of
      '}':
        begin
          dec(P);
          if (P>=SrcStart) and (Source[P]=#3) then begin
            // codetools comment {#3 #3}
            dec(p);
            while (P>SrcStart) do begin
              if (Source[P]=#3) and (Source[P-1]='{') then begin
                dec(P,2);
                break;
              end;
              dec(P);
            end;
          end else begin
            // Pascal comment {}
            while (P>=SrcStart) and (Source[P]<>'{') do begin
              if NestedComments and (Source[P] in ['}',')']) then
                ReadComment(P)
              else
                dec(P);
            end;
            dec(P);
          end;
        end;
      ')':
        begin
          dec(P);
          if (P>=SrcStart) and (Source[P]='*') then begin
            dec(P);
            while (P>SrcStart)
            and ((Source[P-1]<>'(') or (Source[P]<>'*')) do begin
              if NestedComments and (Source[P] in ['}',')']) then
                ReadComment(P)
              else
                dec(P);
            end;
            dec(P,2);
          end;
        end;
    end;
  end;

var TestPos: integer;
begin
  Result:=Position;
  SrcStart:=MinPosition;
  if SrcStart<1 then SrcStart:=1;
  while (Result>=SrcStart) do begin
    case Source[Result] of
      '}',')':
        ReadComment(Result);
      #10,#13:
        begin
          // test if it is a '//' comment
          if (Result>SrcStart) and (Source[Result-1] in [#10,#13])
          and (Source[Result]<>Source[Result-1]) then dec(Result);
          TestPos:=Result-1;
          while (TestPos>SrcStart) do begin
            if (Source[TestPos]='/') and (Source[TestPos-1]='/') then begin
              // this is a comment line end -> search further
              break;
            end else if Source[TestPos] in [#10,#13] then begin
              // no comment, the line end ist really there :)
              exit;
            end else
              dec(TestPos);
          end;
          Result:=TestPos;
        end;
    else
      dec(Result);
    end;
  end;
end;

function ReplacementNeedsLineEnd(const Source: string;
    FromPos, ToPos, NewLength, MaxLineLength: integer): boolean;
// test if old text contains a line end
// or if new line is too long
var LineStart, LineEnd: integer;
begin
  GetLineStartEndAtPosition(Source,FromPos,LineStart,LineEnd);
  Result:=((LineEnd>=FromPos) and (LineEnd<ToPos))
       or ((LineEnd-LineStart-(ToPos-FromPos)+NewLength)>MaxLineLength);
end;

function CompareTextIgnoringSpace(const Txt1, Txt2: string;
  CaseSensitive: boolean): integer;
begin
  Result:=CompareTextIgnoringSpace(
               PChar(Pointer(Txt1)),length(Txt1),// pointer type cast avoids #0 check
               PChar(Pointer(Txt2)),length(Txt2),
               CaseSensitive);
end;

function CompareTextIgnoringSpace(Txt1: PChar; Len1: integer;
  Txt2: PChar; Len2: integer; CaseSensitive: boolean): integer;
{ Txt1  Txt2  Result
   A     A      0
   A     B      1
   A     AB     1
   A;    A      -1
}
var P1, P2: integer;
begin
  P1:=0;
  P2:=0;
  while (P1<Len1) and (P2<Len2) do begin
    if (CaseSensitive and (Txt1[P1]=Txt2[P2]))
    or ((not CaseSensitive) and (UpChars[Txt1[P1]]=UpChars[Txt2[P2]])) then
    begin
      inc(P1);
      inc(P2);
    end else begin
      // different chars found
      if (P1>0) and (IsIdentChar[Txt1[P1-1]])
      and (IsIdentChar[Txt1[P1]] xor IsIdentChar[Txt2[P2]]) then begin
        // one identifier is longer than the other
        if IsIdentChar[Txt1[P1]] then
          // identifier in Txt1 is longer than in Txt2
          Result:=-1
        else
          // identifier in Txt2 is longer than in Txt1
          Result:=+1;
        exit;
      end else if (ord(Txt1[P1])<=ord(' ')) then begin
        // ignore/skip spaces in Txt1
        repeat
          inc(P1);
        until (P1>=Len1) or (ord(Txt1[P1])>ord(' '));
        if (ord(Txt2[P2])<=ord(' ')) then begin
          // ignore/skip spaces in Txt2
          repeat
            inc(P2);
          until (P2>=Len2) or (ord(Txt2[P2])>ord(' '));
        end;
      end else if (ord(Txt2[P2])<=ord(' ')) then begin
        // ignore/skip spaces in Txt2
        repeat
          inc(P2);
        until (P2>=Len2) or (ord(Txt2[P2])>ord(' '));
      end else begin
        // Txt1<>Txt2
        if (CaseSensitive and (Txt1[P1]>Txt2[P2]))
        or ((not CaseSensitive) and (UpChars[Txt1[P1]]>UpChars[Txt2[P2]])) then
          Result:=-1
        else
          Result:=+1;
        exit;
      end;
    end;
  end;
  // one text was totally read -> check the rest of the other one
  // skip spaces
  while (P1<Len1) and (ord(Txt1[P1])<=ord(' ')) do
    inc(P1);
  while (P2<Len2) and (ord(Txt2[P2])<=ord(' ')) do
    inc(P2);
  if (P1>=Len1) then begin
    // rest of P1 was only space
    if (P2>=Len2) then
      // rest of P2 was only space
      Result:=0
    else
      // there is some text at the end of P2
      Result:=1;
  end else begin
    // there is some text at the end of P1
    Result:=-1
  end;
end;

function CompareAnsiStringIgnoringSpaceIgnoreCase(Txt1, Txt2: pointer): integer;
// Txt1, Txt2 are type casted AnsiString
begin
  Result:=CompareTextIgnoringSpace(Txt1,length(AnsiString(Txt1)),
                                   Txt2,length(AnsiString(Txt2)),false);
end;

function CompareSubStrings(const Find, Txt: string;
  FindStartPos, TxtStartPos, Len: integer; CaseSensitive: boolean): integer;
begin
  Result:=CompareText(@Find[FindStartPos],Min(length(Find)-FindStartPos+1,Len),
                      @Txt[TxtStartPos],Min(length(Txt)-TxtStartPos+1,Len),
                      CaseSensitive);
end;

function FindNextIncludeDirective(const ASource: string; StartPos: integer;
  NestedComments: boolean; out FilenameStartPos, FileNameEndPos,
  CommentStartPos, CommentEndPos: integer): integer;
var
  MaxPos, Offset: Integer;
begin
  Result:=StartPos;
  MaxPos:=length(ASource);
  repeat
    Result:=FindNextCompilerDirective(ASource,Result,NestedComments);
    if (Result<1) or (Result>MaxPos) then exit;
    if (ASource[Result]='{') then
      Offset:=2
    else if ASource[Result]='(' then
      Offset:=3
    else
      Offset:=-1;
    if (Offset>0) then begin
      if ((UpChars[ASource[Result+Offset]]='I')
              and (ASource[Result+Offset+1]=' '))
      or (CompareIdentifiers('include',@ASource[Result+Offset])=0) then begin
        CommentEndPos:=FindCommentEnd(ASource,Result,NestedComments);
        if ASource[Result]='{' then
          dec(CommentEndPos)
        else
          dec(CommentEndPos,2);

        // skip directive name
        FilenameStartPos:=Result+Offset;
        while (FilenameStartPos<=CommentEndPos)
        and (IsIdentChar[ASource[FilenameStartPos]]) do
          inc(FilenameStartPos);
        // skip space after name
        while (FilenameStartPos<=CommentEndPos)
        and (IsSpaceChar[ASource[FilenameStartPos]]) do
          inc(FilenameStartPos);
        // find end of filename
        if (FilenameStartPos<=CommentEndPos) and (ASource[FilenameStartPos]='''')
        then begin
          // quoted filename
          inc(FilenameStartPos);
          FilenameEndPos:=FilenameStartPos;
          while (FilenameEndPos<=CommentEndPos) do begin
            if (ASource[FilenameEndPos]<>'''') then
              inc(FilenameEndPos)
            else
              break;
          end;
          CommentStartPos:=FilenameEndPos+1;
        end else begin
          // normal filename
          FilenameEndPos:=FilenameStartPos;
          while (FilenameEndPos<=CommentEndPos)
          and (not IsSpaceChar[ASource[FilenameEndPos]])
          and (not (ASource[FilenameEndPos] in ['*','}'])) do
            inc(FilenameEndPos);
          CommentStartPos:=FilenameEndPos;
        end;
        // skip space behind filename
        while (CommentStartPos<=CommentEndPos)
        and (IsSpaceChar[ASource[CommentStartPos]]) do
          inc(CommentStartPos);
        // success
        exit;
      end;
    end;
    // try next comment
    Result:=FindCommentEnd(ASource,Result,NestedComments);
  until Result>MaxPos;
end;

function FindNextIDEDirective(const ASource: string; StartPos: integer;
  NestedComments: boolean; EndPos: integer): integer;
var
  MaxPos: integer;
begin
  MaxPos:=length(ASource);
  if (EndPos>0) and (EndPos<=MaxPos) then
    MaxPos:=EndPos-1;
  Result:=StartPos;
  while (Result<=MaxPos) do begin
    case ASource[Result] of
    '''':
      begin
        inc(Result);
        while (Result<=MaxPos) do begin
          if (ASource[Result]<>'''') then
            inc(Result)
          else begin
            inc(Result);
            break;
          end;
        end;
      end;

    '/':
      begin
        inc(Result);
        if (Result<=MaxPos) and (ASource[Result]='/') then begin
          // skip Delphi comment
          while (Result<=MaxPos) and (not (ASource[Result] in [#10,#13])) do
            inc(Result);
        end;
      end;

    '{':
      begin
        if (Result<MaxPos) and (ASource[Result+1]='%') then
          exit;
        // skip pascal comment
        Result:=FindCommentEnd(ASource,Result,NestedComments);
      end;

    '(':
      begin
        if (Result<MaxPos) and (ASource[Result+1]='*') then begin
          // skip TP comment
          Result:=FindCommentEnd(ASource,Result,NestedComments);
        end else
          inc(Result);
      end;

    else
      inc(Result);
    end;

  end;
  Result:=-1;
end;

function CleanCodeFromComments(const Src: string; NestedComments: boolean;
  KeepDirectives: boolean; KeepVerbosityDirectives: boolean): string;
// KeepVerbosityDirectives=true requires KeepDirectives=true
var
  SrcPos: Integer;
  ResultPos: Integer;
  StartPos: Integer;
  l: Integer;
  p: PChar;
begin
  SetLength(Result,length(Src));
  SrcPos:=1;
  ResultPos:=1;
  while SrcPos<=length(Src) do begin
    StartPos:=FindNextComment(Src,SrcPos);
    l:=StartPos-SrcPos;
    if (l>0) then begin
      System.Move(Src[SrcPos],Result[ResultPos],l);
      inc(ResultPos,l);
    end;
    if StartPos>length(Src) then break;
    SrcPos:=FindCommentEnd(Src,StartPos,NestedComments);
    if KeepDirectives then begin
      p:=@Src[StartPos];
      if (p^<>'{') or (p[1]<>'$') then continue;
      if not KeepVerbosityDirectives then begin
        inc(p,2);
        if (CompareIdentifiers(p,'warn')=0)
        or (CompareIdentifiers(p,'hint')=0) then continue;
      end;
      l:=SrcPos-StartPos;
      System.Move(Src[StartPos],Result[ResultPos],l);
      inc(ResultPos,l);
    end;
  end;
  SetLength(Result,ResultPos-1);
end;

function ExtractCommentContent(const ASource: string; CommentStart: integer;
  NestedComments: boolean; TrimStart: boolean; TrimEnd: boolean;
  TrimPasDoc: boolean): string;
var
  CommentEnd: LongInt;
  StartPos: LongInt;
  EndPos: LongInt;
begin
  Result:='';
  if (CommentStart<1) or (CommentStart>length(ASource)) then exit;
  CommentEnd:=FindCommentEnd(ASource,CommentStart,NestedComments);
  StartPos:=CommentStart;
  EndPos:=CommentEnd;
  if (ASource[StartPos]='/') then begin
    inc(StartPos);
    if (StartPos<=length(ASource)) and (ASource[StartPos]='/') then
      inc(StartPos);
    if (EndPos<=length(ASource)) then begin
      while (EndPos>StartPos) and (ASource[EndPos-1] in [#10,#13]) do
        dec(EndPos);
    end;
  end else if (ASource[StartPos]='{') then begin
    inc(StartPos);
    if (StartPos<=length(ASource)) and (ASource[StartPos]=#3) then begin
      // codetools skip comment {#3#3}
      inc(StartPos);
      if (EndPos<=length(ASource)) and (ASource[EndPos-1]='}') then begin
        dec(EndPos);
        if (EndPos<=length(ASource)) and (ASource[EndPos-1]=#3) then
          dec(EndPos);
      end;
    end else begin
      // Pascal comment {}
      if (EndPos<=length(ASource)) and (ASource[EndPos-1]='}') then
        dec(EndPos);
    end;
  end else if (ASource[StartPos]='(') then begin
    inc(StartPos);
    if (StartPos<=length(ASource)) and (ASource[StartPos]='*') then
      inc(StartPos);
    if (EndPos<=length(ASource)) and (ASource[EndPos-1]=')') then begin
      dec(EndPos);
      if (ASource[EndPos-1]='*') then
        dec(EndPos);
    end;
  end;
  if TrimPasDoc then begin
    if (StartPos<EndPos) and (ASource[StartPos]='<') then
      inc(StartPos);
  end;
  if TrimStart then begin
    while (StartPos<EndPos) and (ASource[StartPos] in [' ',#9,#10,#13]) do
      inc(StartPos);
  end;
  if TrimEnd then begin
    while (StartPos<EndPos) and (ASource[Endpos-1] in [' ',#9,#10,#13]) do
      dec(EndPos);
  end;
  Result:=copy(ASource,StartPos,EndPos-StartPos);
end;

function FindMainUnitHint(const ASource: string; out Filename: string
  ): boolean;
const
  IncludeByHintStart = '{%MainUnit ';
var
  MaxPos: Integer;
  StartPos: Integer;
  EndPos: LongInt;
begin
  Result:=false;
  Filename:='';
  MaxPos:=length(ASource);
  StartPos:=length(IncludeByHintStart);
  if not TextBeginsWith(PChar(Pointer(ASource)),// pointer type cast avoids #0 check
          MaxPos,IncludeByHintStart,StartPos,false)
  then
    exit;
  while (StartPos<=MaxPos) and (ASource[StartPos]=' ') do inc(StartPos);
  EndPos:=StartPos;
  while (EndPos<=MaxPos) and (ASource[EndPos]<>'}') do inc(EndPos);
  if (EndPos=StartPos) or (EndPos>MaxPos) then exit;
  Filename:=GetForcedPathDelims(copy(ASource,StartPos,EndPos-StartPos));
  Result:=true;
end;

function InEmptyLine(const ASource: string; StartPos: integer): boolean;
var
  p: LongInt;
  SrcLen: Integer;
begin
  Result:=false;
  SrcLen:=length(ASource);
  if (StartPos<1) or (StartPos>SrcLen) or (not IsSpaceChar[ASource[StartPos]])
  then exit;
  p:=StartPos;
  while (p>1) do begin
    case ASource[p-1] of
    ' ',#9: dec(p);
    #10,#13: break;
    else exit;
    end;
  end;
  p:=StartPos;
  while p<=SrcLen do begin
    case ASource[p] of
    ' ',#9: inc(p);
    #10,#13: break;
    else exit;
    end;
  end;
  Result:=true;
end;

function SkipResourceDirective(const ASource: string;
  StartPos, EndPos: integer; NestedComments: boolean): integer;
var
  MaxPos: integer;

  function IsResourceDirective(DirNamePos: integer): boolean;
  begin
    if UpChars[ASource[DirNamePos]]<>'R' then exit(false);
    if (DirNamePos < MaxPos)
    and (UpChars[ASource[DirNamePos+1]] in [' ',#9]) then exit(true);
    result:=CompareIdentifiers(@ASource[DirNamePos],'RESOURCE')=0;
  end;

var
  i: integer;
begin
  MaxPos:=length(ASource);
  if (EndPos>0) and (EndPos<=MaxPos) then
    MaxPos:=EndPos-1;
  Result:=StartPos;
  i:=StartPos;
  while (i<=MaxPos) do begin
    case ASource[i] of
    '{':
      if (i+1<MaxPos) and (ASource[i+1]='$')
      and IsResourceDirective(i+2) then begin
        Result:=FindCommentEnd(ASource,i,NestedComments);
        exit;
      end
      else exit;

    '(':
      if (i+2<MaxPos) and (ASource[i+1]='*') and (ASource[i+2]='$')
      and IsResourceDirective(i+3) then begin
        Result:=FindCommentEnd(ASource,i,NestedComments);
        exit;
      end
      else exit;

    #9,#10,#13,' ': inc(i);
    else
      exit;
    end;

  end;
end;

function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  Result:=KeywordFuncLists.CompareIdentifiers(Identifier1,Identifier2);
end;

function CompareIdentifierPtrs(Identifier1, Identifier2: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(Identifier1), PChar(Identifier2));
end;

function CompareIdentifiersCaseSensitive(Identifier1, Identifier2: PChar
  ): integer;
begin
  if (Identifier1<>nil) then begin
    if (Identifier2<>nil) then begin
      while (Identifier1[0]=Identifier2[0]) do begin
        if (IsIdentChar[Identifier1[0]]) then begin
          inc(Identifier1);
          inc(Identifier2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsIdentChar[Identifier1[0]]) then begin
        if (IsIdentChar[Identifier2[0]]) then begin
          if Identifier1[0]>Identifier2[0] then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsIdentChar[Identifier2[0]]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Identifier2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function ComparePrefixIdent(PrefixIdent, Identifier: PChar): boolean;
begin
  if PrefixIdent<>nil then begin
    if Identifier<>nil then begin
      while (UpChars[PrefixIdent^]=UpChars[Identifier^]) and (PrefixIdent^>#0) do
      begin
        inc(PrefixIdent);
        inc(Identifier);
      end;
      Result:=not IsIdentChar[PrefixIdent^];
    end else begin
      Result:=false;
    end;
  end else begin
    Result:=true;
  end;
end;

function TextBeginsWith(Txt: PChar; TxtLen: integer; StartTxt: PChar;
    StartTxtLen: integer; CaseSensitive: boolean): boolean;
begin
  if TxtLen<StartTxtLen then exit(false);
  Result:=CompareText(Txt,StartTxtLen,StartTxt,StartTxtLen,CaseSensitive)=0;
end;

function StrBeginsWith(const s, Prefix: string): boolean;
var
  p1: PChar;
  p2: PChar;
  i: Integer;
begin
  Result:=false;
  if length(s)<length(Prefix) then exit;
  if (s='') then exit(true);
  p1:=PChar(s);
  p2:=PChar(Prefix);
  for i:=1 to length(Prefix) do begin
    if p1^<>p2^ then exit;
    inc(p1);
    inc(p2);
  end;
  Result:=true;
end;

function IdentifierPos(Search, Identifier: PChar): PtrInt;
var
  i: Integer;
begin
  if Identifier=nil then exit(-1);
  if (Search=nil) or (Search^=#0) then exit(0);
  Result:=0;
  while (IsIdentChar[Identifier[Result]]) do begin
    if UpChars[Search^]=UpChars[Identifier[Result]] then begin
      i:=1;
      repeat
        if IsIdentChar[Search[i]] then begin
          if (UpChars[Search[i]]=UpChars[Identifier[Result+i]]) then
            inc(i)
          else
            break;
        end else begin
          // whole found
          exit;
        end;
      until false;
    end;
    inc(Result);
  end;
  Result:=-1;
end;

function CompareAtom(p1, p2: PChar; NestedComments: boolean): integer;
var
  Len1: LongInt;
  Len2: LongInt;
  l: LongInt;
  c1: Char;
  c2: Char;
begin
  // quick test for the common case:
  if (p1^<>p2^) then begin
    c1:=UpChars[p1^];
    c2:=UpChars[p2^];
    if c1<c2 then
      exit(1)
    else if c1>c2 then
      exit(-1);
  end;

  case p1^ of
  '''':
    // compare string constants case sensitive
    exit(CompareStringConstants(p1,p2));
  '{':
    exit(CompareComments(p1,p2,NestedComments));
  '(':
    if (p1[1]='*') and (p2[1]='*') then
      exit(CompareComments(p1,p2,NestedComments));
  '/':
    if (p1[1]='/') and (p2[1]='/') then
      exit(CompareComments(p1,p2,NestedComments));
  end;

  // full comparison
  Len1:=GetAtomLength(p1,NestedComments);
  Len2:=GetAtomLength(p2,NestedComments);
  l:=Len1;
  if l>Len2 then l:=Len2;
  while l>0 do begin
    if (p1^<>p2^) then begin
      c1:=UpChars[p1^];
      c2:=UpChars[p2^];
      if c1<c2 then
        exit(1)
      else if c1>c2 then
        exit(-1);
    end;
    inc(p1);
    inc(p2);
    dec(l);
  end;
  if Len1>Len2 then
    Result:=1
  else if Len1<Len2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareStringConstants(p1, p2: PChar): integer;
// 1: 'aa' 'ab' because bigger
// 1: 'aa' 'a'  because longer
begin
  Result := 0;
  if (p1^='''') and (p2^='''') then begin
    inc(p1);
    inc(p2);
    repeat
      if p1^<p2^ then
        exit(1)  // p1 bigger
      else if p1^>p2 then
        exit(-1); // p2 bigger
      inc(p1);
      inc(p2);
      if p1^='''' then begin
        // maybe ''
        inc(p1);
        inc(p2);
        if p1^='''' then begin
          if p2^='''' then begin
            inc(p1);
            inc(p2);
          end else begin
            // p1 is longer (e.g.: 'a''b' 'a')
            exit(1);
          end;
        end else if p2^='''' then begin
          // p2 is longer (e.g. 'a' 'a''b')
          exit(-1);
        end else begin
          // same
          exit(0);
        end;
      end;
      if p1^ in [#0,#10,#13] then begin
        // end of p1 found
        if p2^ in [#0,#10,#13] then begin
          // same
          exit(0);
        end else begin
          // p2 is longer
          exit(-1);
        end;
      end else if p2^ in [#0,#10,#13] then begin
        // p1 is longer
        exit(1);
      end;
    until false;
  end else begin
    if p1^='''' then
      // p1 longer
      exit(1)
    else if p2^='''' then
      // p2 longer
      exit(-1)
    else
      // both empty
      exit(0);
  end;
end;

function CompareComments(p1, p2: PChar; NestedComments: boolean): integer;
var
  CommentLvl: Integer;
  IsCodetoolsComment: Boolean;
begin
  if p1^<>p2^ then begin
    if p1^<p2^ then
      exit(1)
    else
      exit(-1);
  end;
  case p1^ of
  '/':
    begin
      inc(p1);
      inc(p2);
      if p1^<>p2^ then begin
        if p1^<p2^ then
          exit(1)
        else
          exit(-1);
      end;
      if p1^<>'/' then exit(0);
      repeat
        inc(p1);
        inc(p2);
        if p1^ in [#0,#10,#13] then begin
          if p2^ in [#0,#10,#13] then begin
            exit(0);
          end else begin
            // p2 is longer
            exit(-1);
          end;
        end;
        if p1^<>p2^ then begin
          if p2^ in [#0,#10,#13] then begin
            // p1 is longer
            exit(1);
          end;
          if p1^<p2^ then
            exit(1)
          else
            exit(-1);
        end;
      until false;
    end;
  '{':
    begin
      inc(p1);
      inc(p2);
      CommentLvl:=1;
      IsCodetoolsComment:=p1^=#3;
      while true do begin
        if p1^<>p2^ then begin
          if p1^<p2^ then
            exit(1)
          else
            exit(-1);
        end;
        inc(p1);
        inc(p2);
        if IsCodetoolsComment then begin
          case p1^ of
          #0:  exit(0);
          '}': if p1[-1]=#3 then exit(0);
          end;
        end else begin
          case p1^ of
          #0:  exit(0);
          '{': if NestedComments then
              inc(CommentLvl);
          '}':
            begin
              dec(CommentLvl);
              if CommentLvl=0 then
                exit(0);
            end;
          end;
        end;
      end;
    end;
  '(':  // comment
    begin
      inc(p1);
      inc(p2);
      if p1^<>p2^ then begin
        if p1^<p2^ then
          exit(1)
        else
          exit(-1);
      end;
      if p1^<>'*' then exit(0);
      CommentLvl:=1;
      repeat
        inc(p1);
        inc(p2);
        if p1^<>p2^ then begin
          if p1^<p2^ then
            exit(1)
          else
            exit(-1);
        end;
        case p1^ of
        #0: exit(0);
        '*':
          if (p1[1]=')') then begin
            inc(p1);
            inc(p2);
            if p2^=')' then begin
              dec(CommentLvl);
              if CommentLvl=0 then
                exit(0);
            end else
              // p2 longer
              exit(-1);
          end;
        '(':
          if (p1[1]='*') and NestedComments then begin
            inc(CommentLvl);
            inc(p1);
            inc(p2);
            if p1^<>p2^ then begin
              if p1^<p2^ then
                exit(1)
              else
                exit(-1);
            end;
          end;
        end;
      until false;
    end;
  end;
  Result:=0;
end;

function FindDiff(const s1, s2: string): integer;
begin
  Result:=1;
  while (Result<=length(s1)) and (Result<=length(s2)) and (s1[Result]=s2[Result]) do
    inc(Result);
end;

function dbgsDiff(Expected, Actual: string): string;
var
  d: Integer;
begin
  Expected:=dbgstr(Expected);
  Actual:=dbgstr(Actual);
  d:=FindDiff(Expected, Actual);
  Result:='Expected: '+dbgstr(Expected,1,d-1)+'|'+dbgstr(Expected,d,length(Expected))+LineEnding
         +'Actual:   '+dbgstr(Actual,1,d-1)+'|'+dbgstr(Actual,d,length(Actual));
end;

function GetIdentifier(Identifier: PChar): string;
var len: integer;
begin
  if (Identifier=nil) then begin
    Result:='';
    exit;
  end;
  if (Identifier^='&') and (IsIdentChar[Identifier[1]]) then
    inc(Identifier);
  if IsIdentStartChar[Identifier^] then begin
    len:=0;
    while (IsIdentChar[Identifier[len]]) do inc(len);
    SetLength(Result,len);
    if len>0 then
      Move(Identifier[0],Result[1],len);
  end else
    Result:='';
end;

function FindNextIdentifier(const Source: string; StartPos, MaxPos: integer
  ): integer;
begin
  Result:=StartPos;
  while (Result<=MaxPos) and (not IsIdentStartChar[Source[Result]]) do
    inc(Result);
end;

function FindNextIdentifierSkipStrings(const Source: string; StartPos,
  MaxPos: integer): integer;
var
  c: Char;
begin
  Result:=StartPos;
  while (Result<=MaxPos) do begin
    c:=Source[Result];
    if IsIdentStartChar[c] then exit;
    if c='''' then begin
      // skip string constant
      inc(Result);
      while (Result<=MaxPos) and (not (Source[Result] in ['''',#10,#13])) do
        inc(Result);
    end;
    inc(Result);
  end;
end;

function IsValidIdentPair(const NamePair: string): boolean;
var
  p: Integer;
begin
  Result:=false;
  p:=1;
  if (p>length(NamePair)) or (not IsIdentStartChar[NamePair[p]]) then exit;
  repeat
    inc(p);
    if p>length(NamePair) then exit;
    if NamePair[p]='.' then break;
    if not IsIdentChar[NamePair[p]] then exit;
  until false;
  inc(p);
  if (p>length(NamePair)) or (not IsIdentStartChar[NamePair[p]]) then exit;
  repeat
    inc(p);
    if p>length(NamePair) then exit(true);
    if not IsIdentChar[NamePair[p]] then exit;
  until false;
end;

function IsValidIdentPair(const NamePair: string; out First, Second: string
  ): boolean;
var
  p: Integer;
  StartPos: LongInt;
begin
  Result:=false;
  First:='';
  Second:='';
  p:=1;
  if (p>length(NamePair)) or (not IsIdentStartChar[NamePair[p]]) then exit;
  StartPos:=p;
  repeat
    inc(p);
    if p>length(NamePair) then exit;
    if NamePair[p]='.' then break;
    if not IsIdentChar[NamePair[p]] then exit;
  until false;
  First:=copy(NamePair,StartPos,p-StartPos);
  inc(p);
  if (p>length(NamePair)) or (not IsIdentStartChar[NamePair[p]]) then exit;
  StartPos:=p;
  repeat
    inc(p);
    if p>length(NamePair) then begin
      Second:=copy(NamePair,StartPos,p-StartPos);
      exit(true);
    end;
    if not IsIdentChar[NamePair[p]] then exit;
  until false;
end;

function ExtractPasIdentifier(const Ident: string; AllowDots: Boolean): string;
var
  p: Integer;
begin
  p:=1;
  Result:=Ident;
  while p<=length(Result) do begin
    if Result[p] in ['a'..'z','A'..'Z','_'] then begin
      inc(p);
      while p<=length(Result) do begin
        case Result[p] of
        'a'..'z','A'..'Z','_','0'..'9': inc(p);
        '.':
          if AllowDots then
            break
          else
            Delete(Result,p,1);
        else
          Delete(Result,p,1);
        end;
      end;
      if p>length(Result) then exit;
      // p is now on the '.'
      inc(p);
    end else
      Delete(Result,p,1);
  end;
  p:=length(Result);
  if (p>0) and (Result[p]='.') then
    Delete(Result,p,1);
end;

function GetLineIndentWithTabs(const Source: string; Position: integer;
  TabWidth: integer): integer;
var p: integer;
begin
  Result:=0;
  p:=Position;
  if p=0 then exit;
  if (p<0) then p:=1;
  if (p>length(Source)+1) then p:=length(Source)+1;
  // search beginning of line
  while (p>1) and not (Source[p-1] in [#10,#13]) do
    dec(p);
  // search code
  Result:=0;
  while (p<=length(Source)) do begin
    case Source[p] of
    ' ': inc(Result);
    #9:
      begin
        Result:=Result+TabWidth;
        Result:=Result-(Result mod TabWidth);
      end;
    else break;
    end;
    inc(p);
  end;
end;

function GetPosInLine(const Source: string; Position: integer): integer;
begin
  Result:=0;
  while (Position>1) and (not (Source[Position-1] in [#10,#13])) do begin
    inc(Result);
    dec(Position);
  end;
end;

function GetBlockMinIndent(const Source: string;
  StartPos, EndPos: integer): integer;
var
  SrcLen: Integer;
  p: Integer;
  CurIndent: Integer;
begin
  SrcLen:=length(Source);
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  Result:=EndPos-StartPos;
  p:=StartPos;
  while p<=EndPos do begin
    // skip line end and empty lines
    while (p<EndPos) and (Source[p] in [#10,#13]) do
      inc(p);
    if (p>=EndPos) then break;
    // count spaces at line start
    CurIndent:=0;
    while (p<EndPos) and (Source[p] in [#9,' ']) do begin
      inc(p);
      inc(CurIndent);
    end;
    if CurIndent<Result then Result:=CurIndent;
    // skip rest of line
    while (p<EndPos) and (not (Source[p] in [#10,#13])) do
      inc(p);
  end;
end;

function GetIndentStr(Indent: integer; TabWidth: integer): string;
var
  TabCnt: Integer;
  SpaceCnt: Integer;
  i: Integer;
begin
  if TabWidth<=0 then begin
    SetLength(Result,Indent);
    if Indent>0 then
      FillChar(Result[1],length(Result),' ');
  end else begin
    TabCnt:=Indent div TabWidth;
    SpaceCnt:=Indent mod TabWidth;
    SetLength(Result,TabCnt+SpaceCnt);
    for i:=1 to TabCnt do
      Result[i]:=#9;
    for i:=TabCnt+1 to TabCnt+SpaceCnt do
      Result[i]:=' ';
  end;
end;

procedure IndentText(const Source: string; Indent, TabWidth: integer;
  out NewSource: string);

  function UnindentTxt(CopyChars: boolean): integer;
  var
    Unindent: Integer;
    SrcPos: Integer;
    SrcLen: Integer;
    NewSrcPos: Integer;
    SkippedSpaces: Integer;
    c: Char;
  begin
    Unindent:=-Indent;
    SrcPos:=1;
    SrcLen:=length(Source);
    NewSrcPos:=1;
    while SrcPos<=SrcLen do begin
      // skip spaces at start of line
      SkippedSpaces:=0;
      while (SrcPos<=SrcLen) and (SkippedSpaces<Unindent) do begin
        c:=Source[SrcPos];
        if c=' ' then begin
          inc(SkippedSpaces);
          inc(SrcPos);
        end else if c=#9 then begin
          inc(SkippedSpaces,TabWidth);
          inc(SrcPos);
        end else
          break;
      end;
      // deleting a tab can unindent too much, so insert some spaces
      while SkippedSpaces>Unindent do begin
        if CopyChars then
          NewSource[NewSrcPos]:=' ';
        inc(NewSrcPos);
        dec(SkippedSpaces);
      end;
      // copy the rest of the line
      while (SrcPos<=SrcLen) do begin
        c:=Source[SrcPos];
        // copy char
        if CopyChars then
          NewSource[NewSrcPos]:=Source[SrcPos];
        inc(NewSrcPos);
        inc(SrcPos);
        if (c in [#10,#13]) then begin
          // line end
          if (SrcPos<=SrcLen) and (Source[SrcPos] in [#10,#13])
          and (Source[SrcPos]<>Source[SrcPos-1]) then begin
            if CopyChars then
              NewSource[NewSrcPos]:=Source[SrcPos];
            inc(NewSrcPos);
            inc(SrcPos);
          end;
          break;
        end;
      end;
    end;
    Result:=NewSrcPos-1;
  end;

var
  LengthOfLastLine: integer;
  LineEndCnt: Integer;
  SrcPos: Integer;
  SrcLen: Integer;
  NewSrcPos: Integer;
  c: Char;
  NewSrcLen: Integer;

  procedure AddIndent;
  var
    i: Integer;
  begin
    for i:=1 to Indent do begin
      NewSource[NewSrcPos]:=' ';
      inc(NewSrcPos);
    end;
  end;

begin
  if (Indent=0) or (Source='') then begin
    NewSource:=Source;
    exit;
  end;
  if Indent>0 then begin
    // indent text
    LineEndCnt:=LineEndCount(Source,LengthOfLastLine);
    if LengthOfLastLine>0 then inc(LineEndCnt);
    SetLength(NewSource,LineEndCnt*Indent+length(Source));
    SrcPos:=1;
    SrcLen:=length(Source);
    NewSrcPos:=1;
    AddIndent;
    while SrcPos<=SrcLen do begin
      c:=Source[SrcPos];
      // copy char
      NewSource[NewSrcPos]:=Source[SrcPos];
      inc(NewSrcPos);
      inc(SrcPos);
      if (c in [#10,#13]) then begin
        // line end
        if (SrcPos<=SrcLen) and (Source[SrcPos] in [#10,#13])
        and (Source[SrcPos]<>Source[SrcPos-1]) then begin
          NewSource[NewSrcPos]:=Source[SrcPos];
          inc(NewSrcPos);
          inc(SrcPos);
        end;
        if (SrcPos<=SrcLen) and (not (Source[SrcPos] in [#10,#13])) then begin
          // next line is not empty -> indent
          AddIndent;
        end;
      end;
    end;
    SetLength(NewSource,NewSrcPos-1);
  end else begin
    // unindent text
    NewSrcLen:=UnindentTxt(false);
    SetLength(NewSource,NewSrcLen);
    UnindentTxt(true);
  end;
end;

function GetLineStartPosition(const Source: string; Position: integer): integer;
begin
  Result:=Position;
  while (Result>1) and (not (Source[Result-1] in [#10,#13])) do
    dec(Result);
end;

function GetLineInSrc(const Source: string; Position: integer): string;
var
  LineStart, LineEnd: integer;
begin
  GetLineStartEndAtPosition(Source,Position,LineStart,LineEnd);
  //debugln(['GetLineInSrc ',Position,' ',LineStart,' ',LineEnd]);
  Result:=copy(Source,LineStart,LineEnd-LineStart);
end;

function LineEndCount(const Txt: string): integer;
var
  LengthOfLastLine: integer;
begin
  Result:=LineEndCount(Txt,LengthOfLastLine);
  if LengthOfLastLine=0 then ;
end;

function DottedIdentifierLength(Identifier: PChar): integer;
var
  p: PChar;
begin
  Result:=0;
  if Identifier=nil then exit;
  p:=Identifier;
  repeat
    if not IsIdentStartChar[p^] then exit;
    repeat
      inc(p);
    until not IsIdentChar[p^];
    if p^<>'.' then break;
    inc(p);
  until false;
  Result:=p-Identifier;
end;

function GetDottedIdentifier(Identifier: PChar): string;
var
  l: Integer;
begin
  l:=DottedIdentifierLength(Identifier);
  SetLength(Result,l);
  if l>0 then
    System.Move(Identifier^,Result[1],l);
end;

function IsDottedIdentifier(const Identifier: string): boolean;
var
  p: PChar;
begin
  Result:=false;
  if Identifier='' then exit;
  p:=PChar(Identifier);
  repeat
    if not IsIdentStartChar[p^] then exit;
    repeat
      inc(p);
    until not IsIdentChar[p^];
    if p^<>'.' then break;
    inc(p);
  until false;
  Result:=(p-PChar(Identifier))=length(Identifier);
end;

function CompareDottedIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  if (Identifier1<>nil) then begin
    if (Identifier2<>nil) then begin
      while (UpChars[Identifier1[0]]=UpChars[Identifier2[0]]) do begin
        if (IsDottedIdentChar[Identifier1[0]]) then begin
          inc(Identifier1);
          inc(Identifier2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsDottedIdentChar[Identifier1[0]]) then begin
        if (IsDottedIdentChar[Identifier2[0]]) then begin
          if UpChars[Identifier1[0]]>UpChars[Identifier2[0]] then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsDottedIdentChar[Identifier2[0]]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Identifier2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function ChompDottedIdentifier(const Identifier: string): string;
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>0) and (Identifier[p]<>'.') do dec(p);
  Result:=LeftStr(Identifier,p-1);
end;

function TrimCodeSpace(const ACode: string): string;
// turn all lineends and special chars to space
// space is combined to one char
// space which is not needed is removed.
// space is only needed between two words or between 2-char operators
var CodePos, ResultPos, CodeLen, SpaceEndPos: integer;
  c1, c2: char;
begin
  CodeLen:=length(ACode);
  SetLength(Result,CodeLen);
  CodePos:=1;
  ResultPos:=1;
  while CodePos<=CodeLen do begin
    if ACode[CodePos]>#32 then begin
      Result[ResultPos]:=ACode[CodePos];
      inc(ResultPos);
      inc(CodePos);
    end else begin
      SpaceEndPos:=CodePos;
      while (SpaceEndPos<=CodeLen) and (ACode[SpaceEndPos]<=#32) do
        inc(SpaceEndPos);
      if (CodePos>1) and (SpaceEndPos<=CodeLen) then begin
        c1:=ACode[CodePos-1];
        c2:=ACode[SpaceEndPos];
        if (IsIdentChar[c1] and IsIdentChar[c2])
        // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
        or ((c2='=') and  (c1 in [':','+','-','/','*','>','<']))
        or ((c1='<') and (c2='>'))
        or ((c1='>') and (c2='<'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        or ((c1='@') and (c2='@')) then
        begin
          // keep one space
          Result[ResultPos]:=' ';
          inc(ResultPos);
        end;
      end;
      // skip space
      CodePos:=SpaceEndPos;
    end;
  end;
  SetLength(Result,ResultPos-1);
end;

function CodeIsOnlySpace(const ACode: string; FromPos, ToPos: integer): boolean;
// from FromPos to including ToPos
var
  SrcLen: integer;
  CodePos: integer;
begin
  Result:=true;
  SrcLen:=length(ACode);
  if ToPos>SrcLen then ToPos:=SrcLen;
  CodePos:=FromPos;
  while (CodePos<=ToPos) do begin
    if ACode[CodePos] in [' ',#9,#10,#13] then
      inc(CodePos)
    else begin
      Result:=false;
      exit;
    end;
  end;
end;

function StringToPascalConst(const s: string): string;
// converts s to a Pascal string literal
// e.g. foo becomes 'foo', bytes 0..31 become #ord

  function Convert(var DestStr: string): integer;
  var
    SrcLen, SrcPos, DestPos: integer;
    c: char;
    i: integer;
    InString: Boolean;
  begin
    SrcLen:=length(s);
    DestPos:=1;
    if DestStr<>'' then DestStr[DestPos]:='''';
    InString:=true;
    for SrcPos:=1 to SrcLen do begin
      inc(DestPos);
      c:=s[SrcPos];
      if c>=' ' then begin
        // normal char
        if not InString then begin
          if DestStr<>'' then DestStr[DestPos]:='''';
          inc(DestPos);
          InString:=true;
        end;
        if DestStr<>'' then
          DestStr[DestPos]:=c;
        if c='''' then begin
          inc(DestPos);
          if DestStr<>'' then DestStr[DestPos]:='''';
        end;
      end else begin
        // special char
        if InString then begin
          if DestStr<>'' then DestStr[DestPos]:='''';
          inc(DestPos);
          InString:=false;
        end;
        if DestStr<>'' then
          DestStr[DestPos]:='#';
        inc(DestPos);
        i:=ord(c);
        if i>=100 then begin
          if DestStr<>'' then
            DestStr[DestPos]:=chr((i div 100)+ord('0'));
          inc(DestPos);
        end;
        if i>=10 then begin
          if DestStr<>'' then
            DestStr[DestPos]:=chr(((i div 10) mod 10)+ord('0'));
          inc(DestPos);
        end;
        if DestStr<>'' then
          DestStr[DestPos]:=chr((i mod 10)+ord('0'));
      end;
    end;
    if InString then begin
      inc(DestPos);
      if DestStr<>'' then DestStr[DestPos]:='''';
      InString:=false;
    end;
    Result:=DestPos;
  end;

var
  NewLen: integer;
begin
  Result:='';
  NewLen:=Convert(Result);
  if NewLen=length(s) then begin
    Result:=s;
    exit;
  end;
  SetLength(Result,NewLen);
  Convert(Result);
end;

function SplitStringConstant(const StringConstant: string;
  FirstLineLength, OtherLineLengths, Indent: integer;
  const aLineBreak: string): string;
{ Split long string constants
  If possible it tries to split on word boundaries.

  Examples:
  1.
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ',15,20,6
    becomes:  |'ABCDEFGHIJKLM'|
              |      +'NOPQRSTUVWX'|
              |      +'YZ'|
    Result:
      'ABCDEFGHIJKLM'#13#10      +'NOPQRSTUVWX'#13#10      +'YZ'

  2.
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ',5,20,6


}
const
  // string constant character types:
  stctStart      = 'S'; // ' start char
  stctEnd        = 'E'; // ' end char
  stctWordStart  = 'W'; // word char after non word char
  stctQuotation1 = '1'; // first ' of a double ''
  stctQuotation2 = '2'; // second ' of a double ''
  stctChar       = 'C'; // normal character
  stctMBC        = 'M'; // follow character of multi byte char
  stctHash       = '#'; // hash
  stctHashNumber = '0'; // hash number
  stctLineEnd10  = #10; // hash number is 10
  stctLineEnd13  = #13; // hash number is 13
  stctJunk       = 'j'; // junk

var
  SrcLen: Integer;
  Src: String;
  CurLineMax: Integer;
  ParsedSrc: string;
  ParsedLen: integer;
  SplitPos: integer;
  i: Integer;

  procedure ParseSrc;
  var
    APos: Integer;

    procedure MarkMBC;
    var
      l: LongInt;
    begin
      l:=UTF8CodepointSize(@Src[APos]);
      inc(APos);
      dec(l);
      while (l>0) and (APos<ParsedLen) do begin
        ParsedSrc[APos]:=stctMBC;
        inc(APos);
        dec(l);
      end;
    end;

  var
    NumberStart: Integer;
    Number: Integer;
  begin
    APos:=1;
    ParsedLen:=CurLineMax+1;
    if ParsedLen>SrcLen then ParsedLen:=SrcLen;
    SetLength(ParsedSrc,CurLineMax+1);
    while APos<=ParsedLen do begin
      if Src[APos]='''' then begin
        ParsedSrc[APos]:=stctStart;
        inc(APos);
        while APos<=ParsedLen do begin
          if (Src[APos]='''') then begin
            inc(APos);
            if (APos<=ParsedLen) and (Src[APos]='''') then begin
              // double ''
              ParsedSrc[APos-1]:=stctQuotation1;
              ParsedSrc[APos]:=stctQuotation2;
              inc(APos);
            end else begin
              // end of string
              ParsedSrc[APos-1]:=stctEnd;
              break;
            end;
          end else if Src[APos] in ['A'..'Z','a'..'z',#128..#255] then begin
            // normal word char
            if (APos>1) and (Src[APos-1] in ['A'..'Z','a'..'z',#128..#255]) then
              ParsedSrc[APos]:=stctChar
            else
              ParsedSrc[APos]:=stctWordStart;
            MarkMBC;
          end else begin
            // other char in string constant
            ParsedSrc[APos]:=stctWordStart;
            inc(APos);
          end;
        end;
      end else if Src[APos]='#' then begin
        ParsedSrc[APos]:=stctHash;
        inc(APos);
        NumberStart:=APos;
        if (APos<=ParsedLen) then begin
          // parse character number
          if IsNumberChar[Src[APos]] then begin
            // parse decimal number
            while (APos<=ParsedLen) and IsNumberChar[Src[APos]] do begin
              ParsedSrc[APos]:=stctHashNumber;
              inc(APos);
            end;
          end else if Src[APos]='$' then begin
            // parse hex number
            while (APos<=ParsedLen) and IsHexNumberChar[Src[APos]] do begin
              ParsedSrc[APos]:=stctHashNumber;
              inc(APos);
            end;
          end;
          Number:=StrToIntDef(copy(Src,NumberStart,APos-NumberStart),-1);
          if (Number=10) or (Number=13) then begin
            while NumberStart<APos do begin
              ParsedSrc[NumberStart]:=chr(Number);
              inc(NumberStart);
            end;
          end;
        end;
      end else begin
        // junk
        ParsedSrc[APos]:=stctJunk;
        MarkMBC;
      end;
    end;
  end;

  function SearchCharLeftToRight(c: char): integer;
  begin
    Result:=1;
    while (Result<=ParsedLen) and (ParsedSrc[Result]<>c) do
      inc(Result);
    if Result>ParsedLen then Result:=-1;
  end;

  function SearchDiffCharLeftToRight(StartPos: integer): integer;
  begin
    Result:=StartPos+1;
    while (Result<=ParsedLen) and (ParsedSrc[Result]=ParsedSrc[StartPos]) do
      inc(Result);
  end;

  procedure SplitAtNewLineCharConstant;
  var
    HashPos: Integer;
    NewSplitPos: Integer;
  begin
    if SplitPos>0 then exit;
    // check if there is a aLineBreak character constant
    HashPos:=SearchCharLeftToRight(stctLineEnd10)-1;
    if (HashPos<1) then begin
      HashPos:=SearchCharLeftToRight(stctLineEnd13)-1;
      if HashPos<1 then exit;
    end;
    NewSplitPos:=SearchDiffCharLeftToRight(HashPos+1);
    if NewSplitPos>CurLineMax then exit;
    // check if this is a double new line char const #13#10
    if (NewSplitPos<ParsedLen) and (ParsedSrc[NewSplitPos]=stctHash)
    and (ParsedSrc[NewSplitPos+1] in [stctLineEnd10,stctLineEnd13])
    and (ParsedSrc[NewSplitPos+1]<>ParsedSrc[NewSplitPos-1])
    then begin
      NewSplitPos:=SearchDiffCharLeftToRight(NewSplitPos+1);
      if NewSplitPos>CurLineMax then exit;
    end;
    SplitPos:=NewSplitPos;
  end;

  procedure SplitBetweenConstants;
  var
    APos: Integer;
  begin
    if SplitPos>0 then exit;
    APos:=CurLineMax;
    while (APos>=2) do begin
      if (ParsedSrc[APos] in [stctHash,stctStart]) then begin
        SplitPos:=APos;
        exit;
      end;
      dec(APos);
    end;
  end;

  procedure SplitAtWordBoundary;
  var
    APos: Integer;
  begin
    if SplitPos>0 then exit;
    APos:=CurLineMax-1;
    while (APos>2) and (APos>(CurLineMax shr 1)) do begin
      if (ParsedSrc[APos]=stctWordStart) then begin
        SplitPos:=APos;
        exit;
      end;
      dec(APos);
    end;
  end;

  procedure SplitDefault;
  begin
    if SplitPos>0 then exit;
    SplitPos:=CurLineMax;
    while (SplitPos>1) do begin
      if (ParsedSrc[SplitPos]
      in [stctStart,stctWordStart,stctChar,stctHash,stctJunk])
      then
        break;
      dec(SplitPos);
    end;
  end;

  procedure Split;
  var
    CurIndent: Integer;
  begin
    // move left split side from Src to Result
    //DebugLn('Split: SplitPos=',SplitPos,' ',copy(Src,SplitPos-5,6));
    Result:=Result+copy(Src,1,SplitPos-1);
    Src:=copy(Src,SplitPos,length(Src)-SplitPos+1);
    if ParsedSrc[SplitPos] in [stctWordStart,stctChar] then begin
      // line break in string constant
      // -> add ' to end of last line and to start of next
      Result:=Result+'''';
      Src:=''''+Src;
    end;
    SrcLen:=length(Src);
    // calculate indent size for next line
    CurLineMax:=OtherLineLengths;
    CurIndent:=Indent;
    if CurIndent>(CurLineMax-10) then
      CurIndent:=CurLineMax-10;
    if CurIndent<0 then CurIndent:=0;
    // add indent spaces to Result
    Result:=Result+aLineBreak+GetIndentStr(CurIndent)+'+';
    // calculate next maximum line length
    CurLineMax:=CurLineMax-CurIndent-1;
  end;

begin
  Result:='';
  if FirstLineLength<5 then FirstLineLength:=5;
  if OtherLineLengths<5 then OtherLineLengths:=5;
  Src:=StringConstant;
  SrcLen:=length(Src);
  CurLineMax:=FirstLineLength;
  //DebugLn('SplitStringConstant FirstLineLength=',FirstLineLength,
  //' OtherLineLengths=',OtherLineLengths,' Indent=',Indent,' ');
  i:=0;
  repeat
    //DebugLn(['SrcLen=',SrcLen,' CurMaxLine=',CurLineMax]);
    //DebugLn('Src="',Src,'"');
    //DebugLn('Result="',Result,'"');
    if SrcLen<=CurLineMax then begin
      // line fits
      Result:=Result+Src;
      break;
    end;
    // split line -> search nice split position
    ParseSrc;
    //debugln(['ParsedSrc=',ParsedSrc]);
    SplitPos:=0;
    SplitAtNewLineCharConstant;
    SplitBetweenConstants;
    SplitAtWordBoundary;
    SplitDefault;
    if SplitPos<=1 then begin
      // no split possible
      Result:=Result+Src;
      break;
    end;
    //debugln(['SplitStringConstant SplitPos=',SplitPos]);
    Split;
    inc(i);
    if i>10 then break;
  until false;
  //DebugLn('END Result="',Result,'"');
  //DebugLn('SplitStringConstant END---------------------------------');
end;

procedure ImproveStringConstantStart(const ACode: string; var StartPos: integer);
// if StartPos is on the first character of a string constant it will be moved
// one in front, that means on the start of the string constant.
// Example:  'A' StartPos=2 -> StartPos:=1
var
  AtomStartPos, AtomEndPos: Integer;
  Len: Integer;
  SubTokenStart: LongInt;
begin
  AtomEndPos:=1;
  repeat
    AtomStartPos:=AtomEndPos;
    ReadRawNextPascalAtom(ACode,AtomEndPos,AtomStartPos,true);
    if (AtomEndPos>StartPos) then begin
      // token found
      Len:=length(ACode);
      while (AtomStartPos<=Len) do begin
        case (ACode[AtomStartPos]) of
        '#':
          begin
            SubTokenStart:=AtomStartPos;
            inc(AtomStartPos);
            while (AtomStartPos<=Len)
            and (ACode[AtomStartPos] in ['0'..'9']) do
              inc(AtomStartPos);
            if StartPos<AtomStartPos then begin
              StartPos:=SubTokenStart;
              exit;
            end;
          end;
        '''':
          begin
            inc(AtomStartPos);
            if StartPos=AtomStartPos then begin
              StartPos:=AtomStartPos-1;
              exit;
            end;
            while (AtomStartPos<=Len) do begin
              if (ACode[AtomStartPos]<>'''') then
                inc(AtomStartPos)
              else begin
                if (AtomStartPos<Len) and (ACode[AtomStartPos+1]='''') then
                  inc(AtomStartPos)
                else
                  break;
              end;
            end;
            inc(AtomStartPos);
          end;
        else
          break;
        end;
      end;
    end;
  until AtomEndPos>StartPos;
end;

procedure ImproveStringConstantEnd(const ACode: string; var EndPos: integer);
// if EndPos is on the last character of a string constant it will be moved
// to the end, that means on the end of the string constant.
// Example:  'A' EndPos=3 -> EndPos:=4
var
  AtomStartPos, AtomEndPos: Integer;
  Len: Integer;
begin
  AtomEndPos:=1;
  repeat
    AtomStartPos:=AtomEndPos;
    ReadRawNextPascalAtom(ACode,AtomEndPos,AtomStartPos,true);
    if (AtomEndPos>=EndPos) then begin
      // token found
      Len:=length(ACode);
      while (AtomStartPos<=Len) do begin
        case (ACode[AtomStartPos]) of
        '#':
          begin
            inc(AtomStartPos);
            while (AtomStartPos<=Len)
            and (ACode[AtomStartPos] in ['0'..'9']) do
              inc(AtomStartPos);
            if EndPos<AtomStartPos then begin
              EndPos:=AtomStartPos;
              exit;
            end;
          end;
        '''':
          begin
            inc(AtomStartPos);
            while (AtomStartPos<=Len) do begin
              if (ACode[AtomStartPos]<>'''') then
                inc(AtomStartPos)
              else begin
                if (AtomStartPos<Len) and (ACode[AtomStartPos+1]='''') then
                  inc(AtomStartPos)
                else
                  break;
              end;
            end;
            inc(AtomStartPos);
            if EndPos=AtomStartPos-1 then begin
              EndPos:=AtomStartPos;
              exit;
            end;
          end;
        else
          break;
        end;
      end;
    end;
  until AtomEndPos>=EndPos;
end;

function HexStrToIntDef(p: PChar; Def: integer): integer;
var
  OldP: PChar;
  i: Integer;
begin
  Result:=0;
  OldP:=p;
  while true do begin
    case p^ of
    '0'..'9': i:=ord(p^)-ord('0');
    'A'..'Z': i:=ord(p^)-ord('A')+10;
    'a'..'z': i:=ord(p^)-ord('a')+10;
    else
      exit;
    end;
    if Result>(High(Result) shr 4) then exit(Def);
    Result:=(Result shl 4)+i;
    inc(p);
  end;
  if OldP=p then exit(Def);
end;

function SearchNextInText(Search: PChar; SearchLen: PtrInt; Src: PChar;
  SrcLen: PtrInt; StartPos: PtrInt; out MatchStart, MatchEnd: PtrInt;
  WholeWords: boolean; MultiLine: boolean): boolean;
{ search Search in Src starting at StartPos.
  MatchEnd will be the position of the first character after the found pattern.
  if WholeWords then in front of MatchStart and behind MatchEnd will be
    a non word character.
  if MultiLine then newline characters are the same #13#10 = #10 = #13. }
var
  EndSrc: PChar;
  EndSearch: PChar;
  FirstChar: Char;
  CurPos: PChar;
  CmpSearch: PChar;
  CmpSrc: PChar;
begin
  Result:=false;
  MatchStart:=0;
  MatchEnd:=0;
  if (Search=nil) or (Src=nil) then exit;

  EndSrc:=@Src[SrcLen];
  EndSearch:=@Search[SearchLen];
  FirstChar:=Search^;
  CurPos:=@Src[StartPos];
  while (CurPos<EndSrc) do begin
    if (FirstChar=CurPos^)
    and ((not WholeWords) or (CurPos=Src) or (IsNonWordChar[PChar(CurPos-1)^]))
    then begin
      CmpSearch:=Search;
      CmpSrc:=CurPos;
      while (CmpSearch<EndSearch) and (CmpSrc<EndSrc) do begin
        if CmpSearch^=CmpSrc^ then begin
          inc(CmpSearch);
          inc(CmpSrc);
        end else if MultiLine
        and (CmpSrc^ in [#13,#10]) and (CmpSearch^ in [#13,#10]) then begin
          if (CmpSrc+1<EndSrc) and (CmpSrc[1] in [#13,#10])
          and (CmpSrc^<>CmpSrc[1]) then
            inc(CmpSrc,2)
          else
            inc(CmpSrc);
          if (CmpSearch+1<EndSearch) and (CmpSearch[1] in [#13,#10])
          and (CmpSearch^<>CmpSearch[1]) then
            inc(CmpSearch,2)
          else
            inc(CmpSearch);
        end else begin
          break;
        end;
      end;
      if (CmpSearch=EndSearch)
      and ((not WholeWords) or (CmpSrc=EndSrc) or (IsNonWordChar[CmpSrc^])) then
      begin
        // pattern found
        Result:=true;
        MatchStart:=CurPos-Src;
        MatchEnd:=CmpSrc-Src;
        exit;
      end;
    end;
    inc(CurPos);
  end;
end;

procedure HasTxtWord(SearchWord, Txt: PChar; out WholeWord: boolean; out
  Count: SizeInt);
var
  StartChar: Char;
  CurSearchP: PChar;
  CurTxtP: PChar;
  TxtRun: PChar;
begin
  WholeWord:=false;
  Count:=0;
  if (SearchWord=nil) or (SearchWord^=#0) then exit;
  if (Txt=nil) or (Txt^=#0) then exit;
  TxtRun:=Txt;
  StartChar:=SearchWord^;
  while TxtRun^<>#0 do begin
    if TxtRun^=StartChar then begin
      CurSearchP:=SearchWord+1;
      CurTxtP:=TxtRun+1;
      while (CurTxtP^=CurSearchP^) and (CurTxtP^<>#0) do begin
        inc(CurTxtP);
        inc(CurSearchP);
      end;
      if CurSearchP^=#0 then begin
        // word found
        if ((TxtRun=Txt) or IsNonWordChar[TxtRun[-1]])
        and IsNonWordChar[CurTxtP^] then begin
          // word boundaries
          if not WholeWord then begin
            WholeWord:=true;
            Count:=1;
          end else
            inc(Count);
        end else
          inc(Count);
        TxtRun:=CurTxtP;
        continue;
      end;
    end;
    inc(TxtRun);
  end;
end;

function SubString(p: PChar; Count: SizeInt): string;
var
  l: SizeInt;
begin
  if (p=nil) or (Count=0) then exit('');
  l:=IndexByte(p^,Count,0);
  if l<0 then l:=Count;
  if l=0 then exit('');
  SetLength(Result,l);
  System.Move(p^,Result[1],l);
end;

function ExtractFileNamespace(const Filename: string): string;
begin
  Result:=ExtractFileNameOnly(Filename);
  if Result='' then exit;
  Result:=ChompDottedIdentifier(Result);
end;

procedure AddToTreeOfUnitFilesOrNamespaces(var TreeOfUnitFiles,
  TreeOfNameSpaces: TAVLTree; const NameSpacePath, Filename: string;
  CaseInsensitive, KeepDoubles: boolean);

  procedure FileAndNameSpaceFits(const UnitName: string;
    out FileNameFits, NameSpaceFits: Boolean);
  var
    CompareCaseInsensitive: Boolean;
  begin
    FileNameFits := False;
    NameSpaceFits := False;
    if NameSpacePath = '' then begin
      FileNameFits := true;
      NameSpaceFits := true;
      Exit;
    end;
    if Length(UnitName) < Length(NameSpacePath) then Exit;

    CompareCaseInsensitive:=CaseInsensitive;
    {$IFDEF Windows}
    CompareCaseInsensitive:=true;
    {$ENDIF}

    if CompareText(PChar(UnitName), Length(NameSpacePath), PChar(NameSpacePath), Length(NameSpacePath), not CompareCaseInsensitive) = 0 then
    begin
      FileNameFits := PosEx('.', UnitName, Length(NameSpacePath)+1) = 0;
      NameSpaceFits := not FileNameFits;
    end;
  end;

var
  FileNameFits, NameSpaceFits: Boolean;
  UnitName: string;
begin
  UnitName := ExtractFileNameOnly(Filename);
  if not IsDottedIdentifier(UnitName) then exit;
  FileAndNameSpaceFits(UnitName, FileNameFits, NameSpaceFits);
  if FileNameFits then
    AddToTreeOfUnitFiles(TreeOfUnitFiles,FileName,UnitName,KeepDoubles);
  if NameSpaceFits then
    AddToTreeOfNamespaces(TreeOfNamespaces,UnitName,NameSpacePath,KeepDoubles)
end;

function GatherUnitFiles(const BaseDir, SearchPath, Extensions,
  NameSpacePath: string; KeepDoubles, CaseInsensitive: boolean;
  var TreeOfUnitFiles, TreeOfNamespaces: TAVLTree): boolean;
{ BaseDir: base directory, used when SearchPath is relative
 SearchPath: semicolon separated list of directories
 Extensions: semicolon separated list of extensions (e.g. 'pas;.pp;ppu')
 NameSpacePath: gather files only from this namespace path, empty '' for all
 KeepDoubles: false to return only the first match of each unit
 CaseInsensitive: true to ignore case on comparing extensions
 TreeOfUnitFiles: tree of TUnitFileInfo
 TreeOfNamespaces: tree of TNameSpaceInfo }
var
  SearchedDirectories: TAVLTree; // tree of AnsiString

  function DirectoryAlreadySearched(const ADirectory: string): boolean;
  begin
    Result:=(SearchedDirectories<>nil)
        and (SearchedDirectories.Find(Pointer(ADirectory))<>nil);
  end;

  procedure MarkDirectoryAsSearched(const ADirectory: string);
  var
    s: String;
  begin
    // increase refcount
    //DebugLn('MarkDirectoryAsSearched ',ADirectory);
    s:=ADirectory;  // increase refcount
    if SearchedDirectories=nil then
      SearchedDirectories:=TAVLTree.Create(@CompareAnsiStringFilenames);
    SearchedDirectories.Add(Pointer(s));
    Pointer(s):=nil; // keep refcount
  end;

  procedure FreeSearchedDirectories;
  var
    ANode: TAVLTreeNode;
    s: String;
  begin
    if SearchedDirectories=nil then exit;
    s:='';
    ANode:=SearchedDirectories.FindLowest;
    while ANode<>nil do begin
      Pointer(s):=ANode.Data;
      //DebugLn('FreeSearchedDirectories ',s);
      s:=''; // decrease refcount
      ANode:=SearchedDirectories.FindSuccessor(ANode);
    end;
    if s='' then ;
    SearchedDirectories.Free;
  end;

  function ExtensionFits(const Filename: string): boolean;
  var
    ExtStart: Integer;
    ExtLen: Integer; // length without '.'
    CurExtStart: Integer;
    CurExtEnd: LongInt;
    CompareCaseInsensitive: Boolean;
    p: Integer;
  begin
    CompareCaseInsensitive:=CaseInsensitive;
    {$IFDEF Windows}
    CompareCaseInsensitive:=true;
    {$ENDIF}

    ExtStart:=length(Filename);
    while (ExtStart>=1) and (not (Filename[ExtStart] in [PathDelim,'.'])) do
      dec(ExtStart);
    if (ExtStart>0) and (Filename[ExtStart]='.') then begin
      // filename has an extension
      ExtLen:=length(Filename)-ExtStart;
      inc(ExtStart);
      CurExtStart:=1;
      while (CurExtStart<=length(Extensions)) do begin
        // skip '.'
        if Extensions[CurExtStart]='.' then inc(CurExtStart);
        // read till semicolon
        CurExtEnd:=CurExtStart;
        while (CurExtEnd<=length(Extensions)) and (Extensions[CurExtEnd]<>';')
        do
          inc(CurExtEnd);
        if (CurExtEnd>CurExtStart) and (CurExtEnd-CurExtStart=ExtLen) then begin
          // compare extension
          p:=ExtLen-1;
          while (p>=0) do begin
            if CompareCaseInsensitive then begin
              if UpChars[Filename[ExtStart+p]]
              <>UpChars[Extensions[CurExtStart+p]]
              then
                break;
            end else begin
              if Filename[ExtStart+p]<>Extensions[CurExtStart+p] then
                break;
            end;
            dec(p);
          end;
          if p<0 then begin
            // extension fit
            Result:=true;
            exit;
          end;
        end;
        CurExtStart:=CurExtEnd+1;
      end;
    end;
    Result:=false;
  end;

  function SearchDirectory(const ADirectory: string): boolean;
  var
    FileInfo: TSearchRec;
  begin
    Result:=true;
    //DebugLn('SearchDirectory ADirectory="',ADirectory,'"');
    if DirectoryAlreadySearched(ADirectory) then exit;
    MarkDirectoryAsSearched(ADirectory);
    //DebugLn('SearchDirectory searching ...');

    if not DirPathExists(ADirectory) then exit;
    if FindFirstUTF8(ADirectory+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then
          continue;
        if ExtensionFits(FileInfo.Name) then begin
          AddToTreeOfUnitFilesOrNamespaces(TreeOfUnitFiles, TreeOfNamespaces,
            NameSpacePath, ADirectory+FileInfo.Name, CaseInsensitive, KeepDoubles);
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;

var
  PathStartPos: Integer;
  PathEndPos: LongInt;
  CurDir: String;
begin
  Result:=false;
  SearchedDirectories:=nil;
  try
    // search all paths in SearchPath
    PathStartPos:=1;
    while PathStartPos<=length(SearchPath) do begin
      PathEndPos:=PathStartPos;
      while (PathEndPos<=length(SearchPath)) and (SearchPath[PathEndPos]<>';')
      do
        inc(PathEndPos);
      if PathEndPos>PathStartPos then begin
        CurDir:=AppendPathDelim(TrimFilename(
                        copy(SearchPath,PathStartPos,PathEndPos-PathStartPos)));
        if not FilenameIsAbsolute(CurDir) then
          CurDir:=AppendPathDelim(BaseDir)+CurDir;
        if not SearchDirectory(CurDir) then exit;
      end;
      PathStartPos:=PathEndPos;
      while (PathStartPos<=length(SearchPath))
      and (SearchPath[PathStartPos]=';') do
        inc(PathStartPos);
    end;
    Result:=true;
  finally
    FreeSearchedDirectories;
  end;
end;

procedure FreeTreeOfUnitFiles(TreeOfUnitFiles: TAVLTree);
begin
  if TreeOfUnitFiles=nil then exit;
  TreeOfUnitFiles.FreeAndClear;
  TreeOfUnitFiles.Free;
end;

procedure AddToTreeOfUnitFiles(var TreeOfUnitFiles: TAVLTree; const Filename,
  Unitname: string; KeepDoubles: boolean);
var
  NewItem: TUnitFileInfo;
begin
  if (not KeepDoubles) then begin
    if (TreeOfUnitFiles<>nil)
    and (TreeOfUnitFiles.FindKey(Pointer(UnitName),
                                 @CompareUnitNameAndUnitFileInfo)<>nil)
    then begin
      // an unit with the same name was already found and doubles are not
      // wanted
      exit;
    end;
  end;
  // add
  if TreeOfUnitFiles=nil then
    TreeOfUnitFiles:=TAVLTree.Create(@CompareUnitFileInfos);
  NewItem:=TUnitFileInfo.Create(UnitName,Filename);
  TreeOfUnitFiles.Add(NewItem);
end;

procedure AddToTreeOfNamespaces(var TreeOfNameSpaces: TAVLTree; const UnitName,
  ParentNameSpacePath: string; KeepDoubles: boolean);
var
  AnNameSpace: String;
  NewItem: TNameSpaceInfo;
  PointPos: Integer;
begin
  PointPos := PosEx('.', UnitName, Length(ParentNameSpacePath)+1);
  if PointPos = 0 then Exit;
  AnNameSpace:=Copy(UnitName, Length(ParentNameSpacePath)+1, PointPos - Length(ParentNameSpacePath) - 1);
  if AnNameSpace = '' then Exit;
  if (not KeepDoubles) then begin
    if (TreeOfNameSpaces<>nil)
    and (TreeOfNameSpaces.FindKey(Pointer(AnNameSpace),
                                 @CompareNameSpaceAndNameSpaceInfo)<>nil)
    then begin
      // a namespace with the same name was already found and doubles are not
      // wanted
      exit;
    end;
  end;
  // add
  if TreeOfNameSpaces=nil then
    TreeOfNameSpaces:=TAVLTree.Create(@CompareNameSpaceInfos);
  NewItem:=TNameSpaceInfo.Create(AnNameSpace,UnitName,Length(ParentNameSpacePath)+1);
  TreeOfNameSpaces.Add(NewItem);
end;

function CompareUnitFileInfos(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(TUnitFileInfo(Data1).FileUnitName),
                             PChar(TUnitFileInfo(Data2).FileUnitName));
end;

function CompareNameSpaceInfos(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(TNameSpaceInfo(Data1).NameSpace),
                             PChar(TNameSpaceInfo(Data2).NameSpace));
end;

function CompareUnitNameAndUnitFileInfo(UnitnamePAnsiString,
  UnitFileInfo: Pointer): integer;
begin
  //do not use CompareIdentifiers - they compare only to the first "."
  Result:=CompareText(PChar(UnitnamePAnsiString),
                      PChar(TUnitFileInfo(UnitFileInfo).FileUnitName));
end;

function CompareNameSpaceAndNameSpaceInfo(NamespacePAnsiString,
  NamespaceInfo: Pointer): integer;
begin
  //do not use CompareIdentifiers - they compare only to the first "."
  Result:=CompareText(PChar(NamespacePAnsiString),
                      PChar(TNameSpaceInfo(NamespaceInfo).NameSpace));
end;

function CountNeededLineEndsToAddForward(const Src: string;
  StartPos, MinLineEnds: integer): integer;
var c:char;
  SrcLen: integer;
begin
  Result:=MinLineEnds;
  if (StartPos<1) or (Result=0)  then exit;
  SrcLen:=length(Src);
  while (StartPos<=SrcLen) do begin
    c:=Src[StartPos];
    if c in [#10,#13] then begin
      dec(Result);
      if Result=0 then break;
      inc(StartPos);
      if (StartPos<=SrcLen)
      and (Src[StartPos] in [#10,#13])
      and (Src[StartPos]<>c) then
        inc(StartPos);
    end else if IsSpaceChar[c] then
      inc(StartPos)
    else
      break;
  end;
end;

function CountNeededLineEndsToAddBackward(
  const Src: string; StartPos, MinLineEnds: integer): integer;
var c:char;
  SrcLen: integer;
begin
  Result:=MinLineEnds;
  SrcLen:=length(Src);
  if (StartPos>SrcLen) or (Result=0) then exit;
  while (StartPos>=1) do begin
    c:=Src[StartPos];
    if c in [#10,#13] then begin
      dec(Result);
      if Result=0 then break;
      dec(StartPos);
      if (StartPos>=1)
      and (Src[StartPos] in [#10,#13])
      and (Src[StartPos]<>c) then
        dec(StartPos);
    end else if IsSpaceChar[c] then
      dec(StartPos)
    else
      break;
  end;
end;

procedure AdjustPositionAfterInsert(var p: integer; IsStart: boolean; FromPos,
  ToPos, DiffPos: integer);
begin
  if (ToPos>FromPos) then begin
    // replace
    if p>FromPos then begin
      if p>ToPos then
        inc(p,DiffPos)
      else
        p:=FromPos;
    end;
  end else begin
    // insert
    if IsStart then begin
      if p>=FromPos then inc(p,DiffPos);
    end else begin
      if p>FromPos then inc(p,DiffPos);
    end;
  end;
end;

function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
  CaseSensitive: boolean): integer;
begin
  if CaseSensitive then begin
    while (Len1>0) and (Len2>0) do begin
      if Txt1^=Txt2^ then begin
        inc(Txt1);
        dec(Len1);
        inc(Txt2);
        dec(Len2);
      end else begin
        if Txt1^<Txt2^ then
          Result:=1
        else
          Result:=-1;
        exit;
      end;
    end;
  end else begin
    while (Len1>0) and (Len2>0) do begin
      if UpChars[Txt1^]=UpChars[Txt2^] then begin
        inc(Txt1);
        dec(Len1);
        inc(Txt2);
        dec(Len2);
      end else begin
        if UpChars[Txt1^]<UpChars[Txt2^] then
          Result:=1
        else
          Result:=-1;
        exit;
      end;
    end;
  end;
  if Len1>Len2 then
    Result:=-1
  else if Len1<Len2 then
    Result:=1
  else
    Result:=0;
end;

function CompareTextCT(const Txt1, Txt2: string; CaseSensitive: boolean): integer;
begin
  Result:=CompareText(PChar(Pointer(Txt1)),length(Txt1),
                      PChar(Pointer(Txt2)),length(Txt2),CaseSensitive);
end;

function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
  CaseSensitive, IgnoreSpace: boolean): integer;
begin
  if IgnoreSpace then
    Result:=CompareTextIgnoringSpace(Txt1,Len1,Txt2,Len2,CaseSensitive)
  else
    Result:=CompareText(Txt1,Len1,Txt2,Len2,CaseSensitive);
end;

{ TNameSpaceInfo }

constructor TNameSpaceInfo.Create(const TheNamespace, TheUnitName: string;
  TheIdentifierStartInUnitName: Integer);
begin
  FNamespace:=TheNamespace;
  FUnitName:=TheUnitName;
  FIdentifierStartInUnitName:=TheIdentifierStartInUnitName;
end;

{ TUnitFileInfo }

constructor TUnitFileInfo.Create(const TheUnitName, TheFilename: string);
begin
  FUnitName:=TheUnitName;
  FFilename:=TheFilename;
end;

function TUnitFileInfo.GetFileUnitNameWithoutNamespace: string;
var
  LastPoint: Integer;
begin
  LastPoint := LastDelimiter('.', FUnitName);
  if LastPoint > 0 then
    Result := Copy(FUnitName, LastPoint+1, High(Integer))
  else
    Result := FUnitName;
end;

function TUnitFileInfo.GetIdentifierStartInUnitName: Integer;
var
  LastPoint: Integer;
begin
  LastPoint := LastDelimiter('.', FUnitName);
  if LastPoint > 0 then
    Result := LastPoint+1
  else
    Result := 1;
end;

//=============================================================================

end.

