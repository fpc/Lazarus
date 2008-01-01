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
    Basic pascal code functions. Many of the functions have counterparts in the
    code tools, which are faster, more flexible and aware of compiler settings
    and directives.
}
unit BasicCodeTools;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, AVL_Tree, SourceLog, KeywordFuncLists, FileProcs;

//----------------------------------------------------------------------------
{ These functions are used by the codetools }

// comments
function FindNextNonSpace(const ASource: string; StartPos: integer): integer;
function FindPrevNonSpace(const ASource: string; StartPos: integer): integer;
function FindCommentEnd(const ASource: string; StartPos: integer;
    NestedComments: boolean): integer;
function IsCommentEnd(const ASource: string; EndPos: integer): boolean;
function FindNextComment(const ASource: string;
    StartPos: integer; NestedComments: boolean): integer;
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
function CleanCodeFromComments(const DirtyCode: string;
    NestedComments: boolean): string;
function FindMainUnitHint(const ASource: string; var Filename: string): boolean;

// indent
procedure GetLineStartEndAtPosition(const Source:string; Position:integer;
    var LineStart,LineEnd:integer);
function GetLineIndent(const Source: string; Position: integer): integer;
function GetBlockMinIndent(const Source: string;
    StartPos, EndPos: integer): integer;
function GetIndentStr(Indent: integer): string;
procedure IndentText(const Source: string; Indent, TabWidth: integer;
    out NewSource: string);

// identifiers
procedure GetIdentStartEndAtPosition(const Source:string; Position:integer;
    var IdentStart,IdentEnd:integer);
function GetIdentStartPosition(const Source:string; Position:integer): integer;
function GetIdentLen(Identifier: PChar): integer;
function GetIdentifier(Identifier: PChar): string;
function FindNextIdentifier(const Source: string; StartPos, MaxPos: integer
    ): integer;
function FindNextIdentifierSkipStrings(const Source: string;
    StartPos, MaxPos: integer): integer;

// line/code ends
function LineEndCount(const Txt: string): integer;
function LineEndCount(const Txt: string; out LengthOfLastLine:integer): integer;
function EmptyCodeLineCount(const Source: string; StartPos, EndPos: integer;
    NestedComments: boolean): integer;
function PositionsInSameLine(const Source: string;
    Pos1, Pos2: integer): boolean;
function FindFirstNonSpaceCharInLine(const Source: string;
    Position: integer): integer;
function FindLineEndOrCodeInFrontOfPosition(const Source: string;
    Position, MinPosition: integer; NestedComments: boolean;
    StopAtDirectives: boolean = true): integer;
function FindLineEndOrCodeAfterPosition(const Source: string;
    Position, MaxPosition: integer; NestedComments: boolean;
    StopAtDirectives: boolean = true; SkipEmptyLines: boolean = false): integer;
function FindFirstLineEndInFrontOfInCode(const Source: string;
    Position, MinPosition: integer; NestedComments: boolean): integer;
function FindFirstLineEndAfterInCode(const Source: string;
    Position, MaxPosition: integer; NestedComments: boolean): integer;
function ChompLineEndsAtEnd(const s: string): string;
function ChompOneLineEndAtEnd(const s: string): string;
function TrimLineEnds(const s: string; TrimStart, TrimEnd: boolean): string;
function SrcPosToLineCol(const s: string; Position: integer;
  out Line, Col: integer): boolean;

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

// comparison
function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
    CaseSensitive: boolean): integer; overload;
function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
    CaseSensitive, IgnoreSpace: boolean): integer; overload;
function CompareTextIgnoringSpace(const Txt1, Txt2: string;
    CaseSensitive: boolean): integer;
function CompareTextIgnoringSpace(Txt1: PChar; Len1: integer;
    Txt2: PChar; Len2: integer; CaseSensitive: boolean): integer;
function CompareAnsiStringIgnoringSpaceIgnoreCase(Txt1, Txt2: pointer): integer;
function CompareSubStrings(const Find, Txt: string;
    FindStartPos, TxtStartPos, Len: integer; CaseSensitive: boolean): integer;
function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
function CompareIdentifiersCaseSensitive(Identifier1, Identifier2: PChar): integer;
function CompareIdentifierPtrs(Identifier1, Identifier2: Pointer): integer;
function ComparePrefixIdent(PrefixIdent, Identifier: PChar): boolean;
function TextBeginsWith(Txt: PChar; TxtLen: integer; StartTxt: PChar;
    StartTxtLen: integer; CaseSensitive: boolean): boolean;
function StrBeginsWith(const s, Prefix: string): boolean;
function IdentifierPos(Search, Identifier: PChar): PtrInt;

// space and special chars
function TrimCodeSpace(const ACode: string): string;
function CodeIsOnlySpace(const ACode: string; FromPos, ToPos: integer): boolean;
function StringToPascalConst(const s: string): string;

// string constants
function SplitStringConstant(const StringConstant: string;
    FirstLineLength, OtherLineLengths, Indent: integer;
    const NewLine: string): string;
procedure ImproveStringConstantStart(const ACode: string; var StartPos: integer);
procedure ImproveStringConstantEnd(const ACode: string; var EndPos: integer);

// search
function SearchNextInText(Search: PChar; SearchLen: PtrInt;
    Src: PChar; SrcLen: PtrInt;
    StartPos: PtrInt;// 0 based
    out MatchStart, MatchEnd: PtrInt;// 0 based
    WholeWords: boolean = false; MultiLine: boolean = false): boolean;


// files
type

  { TUnitFileInfo }

  TUnitFileInfo = class
  private
    FFilename: string;
    FUnitName: string;
  public
    constructor Create(const TheUnitName, TheFilename: string);
    property UnitName: string read FUnitName;
    property Filename: string read FFilename;
  end;

function GatherUnitFiles(const BaseDir, SearchPath,
    Extensions: string; KeepDoubles, CaseInsensitive: boolean;
    var TreeOfUnitFiles: TAVLTree): boolean;
procedure FreeTreeOfUnitFiles(TreeOfUnitFiles: TAVLTree);
procedure AddToTreeOfUnitFiles(var TreeOfUnitFiles: TAVLTree;
  const Filename: string;
  KeepDoubles: boolean);
function CompareUnitFileInfos(Data1, Data2: Pointer): integer;
function CompareUnitNameAndUnitFileInfo(UnitnamePAnsiString,
                                        UnitFileInfo: Pointer): integer;

// other useful stuff
procedure RaiseCatchableException(const Msg: string);


//-----------------------------------------------------------------------------
// functions / procedures

{ These functions are not context sensitive. Especially they ignore compiler
  settings and compiler directives. They exist only for basic usage.
}

// source type
function FindSourceType(const Source: string;
  var SrcNameStart, SrcNameEnd: integer): string;

// program name
function RenameProgramInSource(Source:TSourceLog;
   const NewProgramName:string):boolean;
function FindProgramNameInSource(const Source:string;
   var ProgramNameStart,ProgramNameEnd:integer):string;

// unit name
function RenameUnitInSource(Source:TSourceLog;const NewUnitName:string):boolean;
function FindUnitNameInSource(const Source:string;
   var UnitNameStart,UnitNameEnd:integer):string;

// uses sections
function UnitIsUsedInSource(const Source,UnitName:string):boolean;
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
function IsUnitUsedInUsesSection(const Source,UnitName:string;
   UsesStart:integer):boolean;
function RenameUnitInUsesSection(Source:TSourceLog; UsesStart: integer;
   const OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddUnitToUsesSection(Source:TSourceLog;
   const UnitName,InFilename:string; UsesStart:integer):boolean;
function RemoveUnitFromUsesSection(Source:TSourceLog;
   const UnitName:string; UsesStart:integer):boolean;

// compiler directives
function FindIncludeDirective(const Source,Section:string; Index:integer;
   var IncludeStart,IncludeEnd:integer):boolean;
function SplitCompilerDirective(const Directive:string;
   var DirectiveName,Parameters:string):boolean;

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
   var Position,EndPosition:integer):boolean;
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

// code search
function SearchCodeInSource(const Source, Find: string; StartPos:integer;
   out EndFoundPosition: integer; CaseSensitive: boolean;
   NestedComments: boolean = false): integer;
function ReadNextPascalAtom(const Source: string;
   var Position, AtomStart: integer; NestedComments: boolean = false): string;
procedure ReadRawNextPascalAtom(const Source: string;
   var Position: integer; out AtomStart: integer;
   NestedComments: boolean = false);
function ReadTilPascalBracketClose(const Source: string;
   var Position: integer; NestedComments: boolean = false): boolean;

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
   var IncludeStart,IncludeEnd:integer):boolean;
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

function SplitCompilerDirective(const Directive:string;
   var DirectiveName,Parameters:string):boolean;
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

function FindSourceType(const Source: string;
  var SrcNameStart, SrcNameEnd: integer): string;
begin
  // read first atom for type
  SrcNameEnd:=1;
  Result:=ReadNextPascalAtom(Source,SrcNameEnd,SrcNameStart);
  // read second atom for name
  if Result<>'' then
    ReadNextPascalAtom(Source,SrcNameEnd,SrcNameStart);
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

function FindUnitNameInSource(const Source:string;
  var UnitNameStart,UnitNameEnd:integer):string;
begin
  if uppercasestr(FindSourceType(Source,UnitNameStart,UnitNameEnd))='UNIT' then
    Result:=copy(Source,UnitNameStart,UnitNameEnd-UnitNameStart)
  else
    Result:='';
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
   var ProgramNameStart,ProgramNameEnd:integer):string;
begin
  if uppercasestr(FindSourceType(Source,ProgramNameStart,ProgramNameEnd))=
    'PROGRAM'
  then
    Result:=copy(Source,ProgramNameStart,ProgramNameEnd-ProgramNameStart)
  else
    Result:='';
end;

function UnitIsUsedInSource(const Source,UnitName:string):boolean;
// search in all uses sections
var UsesStart,UsesEnd:integer;
begin
  Result:=false;
  repeat
    UsesStart:=SearchCodeInSource(Source,'uses',1,UsesEnd,false);
    if UsesEnd=0 then ;
    if UsesStart>0 then begin
      if IsUnitUsedInUsesSection(Source,UnitName,UsesStart) then begin
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

function IsUnitUsedInUsesSection(const Source,UnitName:string;
   UsesStart:integer):boolean;
var UsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if UnitName='' then exit;
  if UsesStart<1 then exit;
  if not (lowercase(copy(Source,UsesStart,4))='uses') then exit;
  UsesEnd:=UsesStart+4;
  // parse through all used units and see if it is there
  repeat
    Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(UnitName)) then begin
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
 const UnitName,InFilename:string; UsesStart:integer):boolean;
var UsesEnd:integer;
  LineStart,LineEnd:integer;
  s,Atom,NewUnitTerm:string;
begin
  Result:=false;
  if (UnitName='') or (UnitName=';') or (UsesStart<1) then exit;
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source.Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is already there
  s:=', ';
  repeat
    Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(UnitName)) then begin
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
    NewUnitTerm:=UnitName+' in '''+InFileName+''''
  else
    NewUnitTerm:=UnitName;
  Source.Insert(UsesStart,s+NewUnitTerm);
  GetLineStartEndAtPosition(Source.Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (InFileName<>'') then
    Source.Insert(UsesStart,LineEnding+'  ');
  Result:=true;
end;

function RemoveUnitFromUsesSection(Source:TSourceLog; const UnitName:string;
   UsesStart:integer):boolean;
var UsesEnd,OldUsesStart,OldUsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if (UsesStart<1) or (UnitName='') or (UnitName=',') or (UnitName=';') then
    exit;
  // search interface section
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source.Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is there
  OldUsesEnd:=-1;
  repeat
    Atom:=ReadNextPascalAtom(Source.Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(UnitName)) then begin
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
        // not first used unit (remove comma in front of unitname too)
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
   var Position,EndPosition:integer):boolean;
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
  if (Result<>'') and (not IsValidIdent(Result)) then
    Result:='';
end;

function SearchCodeInSource(const Source, Find: string; StartPos: integer;
  out EndFoundPosition: integer; CaseSensitive: boolean;
  NestedComments: boolean):integer;
// search pascal atoms of Find in Source
// returns the start pos
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

function IsCommentEnd(const ASource: string; EndPos: integer): boolean;
// return true if EndPos on } or on *) or in a // comment
var
  l: Integer;
  LineStart: LongInt;
begin
  Result:=false;
  if EndPos<1 then exit;
  l:=length(ASource);
  if EndPos>l then exit;
  if ASource[EndPos]='}' then begin
    // delphi comment end
    Result:=true;
    exit;
  end;
  if (EndPos>1) and (ASource[EndPos]=')') and (ASource[EndPos-1]='*') then begin
    // TP comment end
    Result:=true;
    exit;
  end;
  // test for Delphi comment
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
            break;
          end;
        end;
      end;

    '/':
      if (Result<MaxPos) and (ASource[Result+1]='/') then
        exit;

    '{':
      if (Result<MaxPos) and (ASource[Result+1]='$') then
        exit;

    '(':
      if (Result<MaxPos) and (ASource[Result+1]='*') then
        exit;

    end;
    inc(Result);
  end;
  if Result>MaxPos+1 then Result:=MaxPos+1;
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
var
  MaxPos, CommentLvl: integer;
begin
  MaxPos:=length(ASource);
  Result:=StartPos;
  if Result>MaxPos then exit;
  case ASource[Result] of
  '/':
    begin
      if (Result<MaxPos) and (ASource[Result+1]='/') then begin
        // skip Delphi comment
        while (Result<=MaxPos) and (not (ASource[Result] in [#10,#13])) do
          inc(Result);
      end;
    end;

  '{':
    begin
      CommentLvl:=1;
      inc(Result);
      while Result<=MaxPos do begin
        case ASource[Result] of
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
      end;
    end;

  '(':
    if (Result<MaxPos) and (ASource[Result+1]='*') then begin
      inc(Result,2);
      while (Result<=MaxPos) do begin
        if (Result<MaxPos) and (ASource[Result]='*') and (ASource[Result+1]=')')
        then begin
          inc(Result,2);
          break;
        end;
        inc(Result);
      end;
    end;

  end;
end;

procedure GetLineStartEndAtPosition(const Source:string; Position:integer;
   var LineStart,LineEnd:integer);
begin
  LineStart:=Position;
  while (LineStart>0) and (not (Source[LineStart] in [#10,#13])) do
    dec(LineStart);
  inc(LineStart);
  LineEnd:=Position;
  while (LineEnd<=length(Source)) and (not (Source[LineEnd] in [#10,#13])) do
    inc(LineEnd);
end;

function EmptyCodeLineCount(const Source: string; StartPos, EndPos: integer;
    NestedComments: boolean): integer;
{ search forward for a line end or code
  ignore line ends in comments
  Result is Position of Start of Line End
}
var SrcLen: integer;
  SrcPos: Integer;

  procedure ReadComment(var P: integer);
  begin
    case Source[P] of
      '{':
        begin
          inc(P);
          while (P<=SrcLen) and (Source[P]<>'}') do begin
            if NestedComments and (Source[P] in ['{','(','/']) then
              ReadComment(P)
            else
              inc(P);
          end;
          inc(P);
        end;
      '(':
        begin
          inc(P);
          if (P<=SrcLen) and (Source[P]='*') then begin
            inc(P);
            while (P<=SrcLen-1)
            and ((Source[P]<>'*') or (Source[P-1]<>')')) do begin
              if NestedComments and (Source[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
            inc(P,2);
          end;
        end;
      '/':
        begin
          inc(P);
          if (P<=SrcLen) and (Source[P]='/') then begin
            inc(P);
            while (P<=SrcLen)
            and (not (Source[P] in [#10,#13])) do begin
              if NestedComments and (Source[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
          end;
        end;
    end;
  end;

begin
  Result:=0;
  SrcLen:=length(Source);
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  SrcPos:=StartPos;
  while (SrcPos<EndPos) do begin
    case Source[SrcPos] of
    '{','(','/':
      ReadComment(SrcPos);
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
  while StartPos<=EndPos do begin
    if Source[StartPos] in [#10,#13] then begin
      Result:=false;
      exit;
    end else
      inc(StartPos);
  end;
  Result:=true;
end;

procedure GetIdentStartEndAtPosition(const Source: string; Position: integer;
    var IdentStart, IdentEnd: integer);
begin
  IdentStart:=Position;
  IdentEnd:=Position;
  if (Position<1) or (Position>length(Source)) then exit;
  while (IdentStart>1)
  and (IsIdentChar[Source[IdentStart-1]]) do
    dec(IdentStart);
  while (IdentEnd<=length(Source))
  and (IsIdentChar[Source[IdentEnd]]) do
    inc(IdentEnd);
  while (IdentStart<IdentEnd)
  and (not IsIdentStartChar[Source[IdentStart]]) do
    inc(IdentStart);
end;

function GetIdentStartPosition(const Source: string; Position: integer
  ): integer;
begin
  Result:=Position;
  if (Result<1) or (Result>length(Source)) then exit;
  while (Result>1)
  and (IsIdentChar[Source[Result-1]]) do
    dec(Result);
  while (Result<Position)
  and (not IsIdentStartChar[Source[Result]]) do
    inc(Result);
end;

function GetIdentLen(Identifier: PChar): integer;
begin
  Result:=0;
  if Identifier=nil then exit;
  if not IsIdentStartChar[Identifier^] then exit;
  while (IsIdentChar[Identifier[Result]]) do inc(Result);
end;

function ReadNextPascalAtom(const Source:string;
  var Position, AtomStart: integer; NestedComments: boolean):string;
var DirectiveName:string;
  DirStart,DirEnd,EndPos:integer;
begin
  repeat
    ReadRawNextPascalAtom(Source,Position,AtomStart,NestedComments);
    Result:=copy(Source,AtomStart,Position-AtomStart);
    if (length(Result)>=2)
    and (Result[1] in ['{','(']) and (Result[2]='*') then begin
      if copy(Result,1,2)='{$' then begin
        DirStart:=3;
        DirEnd:=length(Result);
      end else begin
        DirStart:=4;
        DirEnd:=length(Result)-1;
      end;
      EndPos:=DirStart;
      while (EndPos<DirEnd) and (IsIdentChar[Result[EndPos]]) do inc(EndPos);
      DirectiveName:=lowercase(copy(Result,DirStart,EndPos-DirStart));
      if (length(DirectiveName)=1) and (Result[DirEnd] in ['+','-']) then begin
        // switch
        break;
      end else if (DirectiveName='i') or (DirectiveName='include') then begin
        // include directive
        break;
      end;
    end else
      break;
  until false;
end;

procedure ReadRawNextPascalAtom(const Source: string;
  var Position: integer; out AtomStart: integer; NestedComments: boolean);
var Len:integer;
  c1,c2:char;
  CommentLvl: Integer;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Len:=length(Source);
  // read til next atom
  while (Position<=Len) do begin
    case Source[Position] of
     #0..#32:  // spaces and special characters
      begin
        inc(Position);
      end;
     '{':    // comment start or compiler directive
      begin
        if (Position<Len) and (Source[Position+1]='$') then
          // compiler directive
          break
        else begin
          // read till comment end
          CommentLvl:=1;
          while true do begin
            inc(Position);
            case Source[Position] of
            #0:  if Position>Len then break;
            '{': if NestedComments then
              begin
                inc(CommentLvl);
              end;
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then begin
                  inc(Position);
                  break;
                end;
              end;
            end;
          end;
        end;
      end;
     '/':  // comment or real division
      if (Position<Len) and (Source[Position]='/') then begin
        // comment start -> read til line end
        inc(Position);
        while (Position<=Len) and (not (Source[Position] in [#10,#13])) do
          inc(Position);
      end else
        break;
     '(':  // comment, bracket or compiler directive
      if (Position<Len) and (Source[Position]='*') then begin
        if (Position+2<=Len) and (Source[Position]='$') then
          // compiler directive
          break
        else begin
          // comment start -> read til comment end
          inc(Position,2);
          CommentLvl:=1;
          while true do begin
            case Source[Position] of
            #0:  if Position>Len then break;
            '(': if NestedComments and (Source[Position+1]='*') then
              begin
                inc(CommentLvl);
              end;
            '*':
              if (Source[Position+1]=')') then begin
                dec(CommentLvl);
                if CommentLvl=0 then begin
                  inc(Position,2);
                  break;
                end;
                inc(Position);
              end;
            end;
            inc(Position);
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
  AtomStart:=Position;
  if Position<=Len then begin
    c1:=Source[Position];
    case c1 of
     'A'..'Z','a'..'z','_':
      begin
        // identifier
        inc(Position);
        while (Position<=Len) and (IsIdentChar[Source[Position]]) do
          inc(Position);
      end;
     '0'..'9': // number
      begin
        inc(Position);
        // read numbers
        while (Position<=Len) and (Source[Position] in ['0'..'9']) do
          inc(Position);
        if (Position<Len) and (Source[Position]='.')
        and (Source[Position+1]<>'.') then begin
          // real type number
          inc(Position);
          while (Position<=Len) and (Source[Position] in ['0'..'9']) do
            inc(Position);
          if (Position<=Len) and (Source[Position] in ['e','E']) then begin
            // read exponent
            inc(Position);
            if (Position<=Len) and (Source[Position]='-') then inc(Position);
            while (Position<=Len) and (Source[Position] in ['0'..'9']) do
              inc(Position);
          end;
        end;
      end;
     '''','#':  // string constant
      begin
        while (Position<=Len) do begin
          case (Source[Position]) of
          '#':
            begin
              inc(Position);
              while (Position<=Len)
              and (Source[Position] in ['0'..'9']) do
                inc(Position);
            end;
          '''':
            begin
              inc(Position);
              while (Position<=Len)
              and (Source[Position]<>'''') do
                inc(Position);
              inc(Position);
            end;
          else
            break;
          end;
        end;
      end;
     '$':  // hex constant
      begin
        inc(Position);
        while (Position<=Len)
        and (IsHexNumberChar[Source[Position]]) do
          inc(Position);
      end;
     '{':  // compiler directive
      begin
        CommentLvl:=1;
        while true do begin
          inc(Position);
          case Source[Position] of
          #0:  if Position>Len then break;
          '{': if NestedComments then
            begin
              inc(CommentLvl);
            end;
          '}':
            begin
              dec(CommentLvl);
              if CommentLvl=0 then begin
                inc(Position);
                break;
              end;
            end;
          end;
        end;
      end;
     '(':  // bracket or compiler directive
      if (Position<Len) and (Source[Position]='*') then begin
        // compiler directive -> read til comment end
        inc(Position,2);
        while (Position<Len)
        and ((Source[Position]<>'*') or (Source[Position]<>')')) do
          inc(Position);
        inc(Position,2);
      end else
        // round bracket open
        inc(Position);
    else
      inc(Position);
      if Position<=Len then begin
        c2:=Source[Position];
        // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **
        if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
        or ((c1='<') and (c2='>'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        then
          inc(Position)
        else if ((c1='@') and (c2='@')) then begin
          // @@ label
          repeat
            inc(Position);
          until (Position>Len) or (not IsIdentChar[Source[Position]]);
        end;
      end;
    end;
  end;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
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
    ReadRawNextPascalAtom(Source,Position,AtomStart,NestedComments);
    if Position>Len then
      exit; // CloseBracket not found
    case Source[Position] of
    '{','(','[':
      if not ReadTilPascalBracketClose(Source,Position) then exit;
    '}',')',']':
      if Source[Position]=CloseBracket then begin
        // CloseBracket found
        inc(Position);
        Result:=true;
        exit;
      end else begin
        exit; // a bracket is closed, that was never opened
      end;
    end;
  end;
end;

function LineEndCount(const Txt: string;
  out LengthOfLastLine: integer): integer;
var i, LastLineEndPos: integer;
begin
  i:=1;
  LastLineEndPos:=0;
  Result:=0;
  while i<=length(Txt) do begin
    if (Txt[i] in [#10,#13]) then begin
      inc(Result);
      inc(i);
      if (i<=length(Txt)) and (Txt[i] in [#10,#13]) and (Txt[i-1]<>Txt[i]) then
        inc(i);
      LastLineEndPos:=i-1;
    end else
      inc(i);
  end;
  LengthOfLastLine:=length(Txt)-LastLineEndPos;
end;

function FindFirstNonSpaceCharInLine(const Source: string;
  Position: integer): integer;
begin
  Result:=Position;
  if (Result<0) then Result:=1;
  if (Result>length(Source)) then Result:=length(Source);
  if Result=0 then exit;
  // search beginning of line
  while (Result>1) and (not (Source[Result] in [#10,#13])) do
    dec(Result);
  // search
  while (Result<length(Source)) and (Source[Result]<' ') do inc(Result);
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
  repeat
    dec(LineStart);
  until (LineStart<1) or (Source[LineStart] in [#10,#13]);
  inc(LineStart);
  // search code
  Result:=LineStart;
  while (Result<=length(Source)) and (Source[Result]=' ') do inc(Result);
  dec(Result,LineStart);
end;

function FindLineEndOrCodeAfterPosition(const Source: string;
   Position, MaxPosition: integer; NestedComments: boolean;
   StopAtDirectives: boolean; SkipEmptyLines: boolean): integer;
{ search forward for a line end or code
  ignore line ends in comments
  Result is Position of Start of Line End
  
  if SkipEmptyLines=true, it will skip empty lines at the end
  
  Examples: | is the Position and # is the Result
  
  1. var i: integer;|#
     var j: integer;
     
  2. var i: integer;| (*
     *) #var j: integer;
     
  3. SkipEmptyLines=false
     var i: integer;|
     #
     // comment
     var j: integer;
     
  4. SkipEmptyLines=true
     var i: integer;|

     #// comment
     var j: integer;
}
var SrcLen: integer;

  function ReadComment(var P: integer): boolean;
  // true if it is a comment
  // false if it is a directive
  begin
    case Source[P] of
      '{':
        begin
          if StopAtDirectives and (P<length(Source)) and (Source[P+1]='$') then
            exit(false);
          inc(P);
          while (P<=SrcLen) and (Source[P]<>'}') do begin
            if NestedComments and (Source[P] in ['{','(','/']) then
              ReadComment(P)
            else
              inc(P);
          end;
          inc(P);
        end;
      '(':
        begin
          inc(P);
          if (P<=SrcLen) and (Source[P]='*') then begin
            inc(P);
            while (P<=SrcLen-1)
            and ((Source[P]<>'*') or (Source[P-1]<>')')) do begin
              if NestedComments and (Source[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
            inc(P,2);
          end;
        end;
      '/':
        begin
          inc(P);
          if (P<=SrcLen) and (Source[P]='/') then begin
            inc(P);
            while (P<=SrcLen)
            and (not (Source[P] in [#10,#13])) do begin
              if NestedComments and (Source[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
          end;
        end;
    end;
    Result:=true;
  end;
  
  procedure DoSkipEmptyLines(var p: integer);
  var
    OldPos: LongInt;
  begin
    OldPos:=p;
    repeat
      while (p<=SrcLen) and (Source[p] in [' ',#9]) do inc(p);
      if (p<=SrcLen) and (Source[p] in [#10,#13]) then begin
        // an empty line => skip
        OldPos:=p;// remember position in front of new line characters
        inc(p);
        if (p<=SrcLen) and (Source[p] in [#10,#13]) and (Source[p]<>Source[p-1])
        then
          inc(p);
      end else begin
        p:=OldPos;
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
      '{','(','/':
        if not ReadComment(Result) then exit;
      #10,#13:
        begin
          if SkipEmptyLines then DoSkipEmptyLines(Result);
          exit;
        end;
      #9,' ',';':
        inc(Result);
    else
      exit;
    end;
  end;
end;

function FindLineEndOrCodeInFrontOfPosition(const Source: string;
   Position, MinPosition: integer; NestedComments: boolean;
   StopAtDirectives: boolean): integer;
{ search backward for a line end or code
  ignore line ends in comments or at the end of comment lines
   (comment lines are lines without code and at least one comment)
  Result is Position of Start of Line End

  examples: Position points at char 'a'

    1:  |
    2: a:=1;

    1:  b:=1; |
    2:  // comment
    3:  // comment
    4:  a:=1;

    1:  |
    2: /* */
    3:  a:=1;

    1: end;| /*
    2: */ a:=1;

    1: b:=1; // comment |
    2: a:=1;

    1: b:=1; /*
    2: comment */   |
    3: a:=1;
}
var SrcStart: integer;

  function ReadComment(var P: integer): boolean;
  // false if compiler directive
  var OldP: integer;
  begin
    OldP:=P;
    case Source[P] of
      '}':
        begin
          dec(P);
          while (P>=SrcStart) and (Source[P]<>'{') do begin
            if NestedComments and (Source[P] in ['}',')']) then
              ReadComment(P)
            else
              dec(P);
          end;
          Result:=not (StopAtDirectives
                       and (P>=SrcStart) and (Source[P+1]='$'));
          dec(P);
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
            Result:=not (StopAtDirectives
                         and (P>=SrcStart) and (Source[P+1]='$'));
            dec(P,2);
          end else
            Result:=true;
        end;
    else
      Result:=true;
    end;
    if not Result then P:=OldP+1;
  end;

var TestPos: integer;
  OnlySpace: boolean;
begin
  SrcStart:=MinPosition;
  if SrcStart<1 then SrcStart:=1;
  if Position<=SrcStart then begin
    Result:=SrcStart;
    exit;
  end;
  Result:=Position-1;
  if Result>length(Source) then Result:=length(Source);
  while (Result>=SrcStart) do begin
    case Source[Result] of
      '}',')':
        if not ReadComment(Result) then exit;

      #10,#13:
        begin
          // line end in code found
          if (Result>SrcStart) and (Source[Result-1] in [#10,#13])
          and (Source[Result]<>Source[Result-1]) then dec(Result);
          // test if it is a comment line (a line without code and at least one
          // comment)
          TestPos:=Result-1;
          OnlySpace:=true;
          while (TestPos>SrcStart) do begin
            if (Source[TestPos]='/') and (Source[TestPos-1]='/') then begin
              // this is a comment line end -> search further
              while (TestPos>SrcStart) and (Source[TestPos]='/') do
                dec(TestPos);
              break;
            end else if Source[TestPos] in [#10,#13] then begin
              // no comment, the line end ist really there :)
              exit;
            end else if OnlySpace
            and ((Source[TestPos]='}')
              or ((Source[TestPos]=')') and (Source[TestPos-1]='*'))) then begin
              // this is a comment line end -> search further
              break;
            end else begin
              if (Source[Result]>' ') then OnlySpace:=false;
              dec(TestPos);
            end;
          end;
          Result:=TestPos;
        end;

      ' ',';',',':
        dec(Result);

    else
      // code found
      inc(Result);
      exit;
    end;
  end;
  if Result<SrcStart then Result:=SrcStart;
end;

function FindFirstLineEndAfterInCode(const Source: string;
  Position, MaxPosition: integer; NestedComments: boolean): integer;
{ search forward for a line end
  ignore line ends in comments
  Result is Position of Start of Line End
}
var SrcLen: integer;

  procedure ReadComment(var P: integer);
  begin
    case Source[P] of
      '{':
        begin
          inc(P);
          while (P<=SrcLen) and (Source[P]<>'}') do begin
            if NestedComments and (Source[P] in ['{','(','/']) then
              ReadComment(P)
            else
              inc(P);
          end;
          inc(P);
        end;
      '(':
        begin
          inc(P);
          if (P<=SrcLen) and (Source[P]='*') then begin
            inc(P);
            while (P<=SrcLen-1)
            and ((Source[P]<>'*') or (Source[P-1]<>')')) do begin
              if NestedComments and (Source[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
            inc(P,2);
          end;
        end;
      '/':
        begin
          inc(P);
          if (P<=SrcLen) and (Source[P]='/') then begin
            inc(P);
            while (P<=SrcLen)
            and (not (Source[P] in [#10,#13])) do begin
              if NestedComments and (Source[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
          end;
        end;
    end;
  end;

begin
  SrcLen:=length(Source);
  if SrcLen>MaxPosition then SrcLen:=MaxPosition;
  Result:=Position;
  while (Result<=SrcLen) do begin
    case Source[Result] of
      '{','(','/':
        ReadComment(Result);
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
  if p=Position then Result:=true;
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
          while (P>=SrcStart) and (Source[P]<>'{') do begin
            if NestedComments and (Source[P] in ['}',')']) then
              ReadComment(P)
            else
              dec(P);
          end;
          dec(P);
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
  InIdentifier: boolean;
begin
  P1:=0;
  P2:=0;
  InIdentifier:=false;
  while (P1<Len1) and (P2<Len2) do begin
    //DebugLn('CompareTextIgnoringSpace P1=',dbgs(P1),' P2=',dbgs(P2));
    if (CaseSensitive and (Txt1[P1]=Txt2[P2]))
    or ((not CaseSensitive) and (UpChars[Txt1[P1]]=UpChars[Txt2[P2]])) then
    begin
      inc(P1);
      inc(P2);
    end else begin
      // different chars found
      if InIdentifier and (IsIdentChar[Txt1[P1]] xor IsIdentChar[Txt2[P2]]) then begin
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
    InIdentifier:=IsIdentChar[Txt1[P1]];
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

function CompareAnsiStringIgnoringSpaceIgnoreCase(Txt1, Txt2: pointer
  ): integer;
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
  MaxPos: Integer;
  Offset: Integer;
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
        FilenameEndPos:=FilenameStartPos;
        if (FilenameEndPos<=CommentEndPos) and (ASource[FilenameEndPos]='''')
        then begin
          // quoted filename
          inc(FilenameStartPos);
          while (FilenameEndPos<=CommentEndPos) do begin
            if (ASource[FilenameEndPos]<>'''') then
              inc(FilenameEndPos)
            else begin
              inc(FilenameEndPos);
              break;
            end;
          end;
        end else begin
          // normal filename
          while (FilenameEndPos<=CommentEndPos)
          and (not IsSpaceChar[ASource[FilenameEndPos]])
          and (not (ASource[FilenameEndPos] in ['*','}'])) do
            inc(FilenameEndPos);
        end;
        // skip space behind filename
        CommentStartPos:=FilenameEndPos;
        while (CommentStartPos<=CommentEndPos)
        and (IsSpaceChar[ASource[CommentStartPos]]) do inc(CommentStartPos);
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

function CleanCodeFromComments(const DirtyCode: string;
  NestedComments: boolean): string;
var DirtyPos, CleanPos, DirtyLen: integer;
  c: char;

  procedure ReadComment(var P: integer);
  begin
    case DirtyCode[P] of
      '{':
        begin
          inc(P);
          while (P<=DirtyLen) and (DirtyCode[P]<>'}') do begin
            if NestedComments and (DirtyCode[P] in ['{','(','/']) then
              ReadComment(P)
            else
              inc(P);
          end;
          inc(P);
        end;
      '(':
        begin
          inc(P);
          if (P<=DirtyLen) and (DirtyCode[P]='*') then begin
            inc(P);
            while (P<=DirtyLen-1)
            and ((DirtyCode[P]<>'*') or (DirtyCode[P-1]<>')')) do begin
              if NestedComments and (DirtyCode[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
            inc(P,2);
          end;
        end;
      '/':
        begin
          inc(P);
          if (P<=DirtyLen) and (DirtyCode[P]='/') then begin
            inc(P);
            while (P<=DirtyLen)
            and (not (DirtyCode[P] in [#10,#13])) do begin
              if NestedComments and (DirtyCode[P] in ['{','(','/']) then
                ReadComment(P)
              else
                inc(P);
            end;
          end;
        end;
    end;
  end;

begin
  DirtyLen:=length(DirtyCode);
  SetLength(Result,DirtyLen);
  DirtyPos:=1;
  CleanPos:=1;
  while (DirtyPos<=DirtyLen) do begin
    c:=DirtyCode[DirtyPos];
    if not (c in ['/','{','(']) then begin
      Result[CleanPos]:=c;
      inc(DirtyPos);
      inc(CleanPos);
    end else begin
      ReadComment(DirtyPos);
    end;
  end;
  SetLength(Result,CleanPos-1);
end;

function FindMainUnitHint(const ASource: string; var Filename: string
  ): boolean;
const
  IncludeByHintStart = '{%MainUnit ';
var
  MaxPos: Integer;
  StartPos: Integer;
  EndPos: LongInt;
begin
  Result:=false;
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
  Filename:=SetDirSeparators(copy(ASource,StartPos,EndPos-StartPos));
  Result:=true;
end;

function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  if (Identifier1<>nil) then begin
    if (Identifier2<>nil) then begin
      while (UpChars[Identifier1[0]]=UpChars[Identifier2[0]]) do begin
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
          if UpChars[Identifier1[0]]>UpChars[Identifier2[0]] then
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

function CompareIdentifierPtrs(Identifier1, Identifier2: Pointer): integer;
begin
  Result := CompareIdentifiers(PChar(Identifier1), PChar(Identifier2));
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
      while UpChars[PrefixIdent^]=UpChars[Identifier^] do begin
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
  if TxtLen<StartTxtLen then exit;
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
  if Search=nil then exit(-1);
  if Identifier=nil then exit(-1);
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

function GetIdentifier(Identifier: PChar): string;
var len: integer;
begin
  if (Identifier<>nil) and IsIdentStartChar[Identifier^] then begin
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

function GetIndentStr(Indent: integer): string;
begin
  SetLength(Result,Indent);
  if Indent>0 then
    FillChar(Result[1],length(Result),' ');
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

function LineEndCount(const Txt: string): integer;
var
  LengthOfLastLine: integer;
begin
  Result:=LineEndCount(Txt,LengthOfLastLine);
  if LengthOfLastLine=0 then ;
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
// converts s to

  function Convert(var DestStr: string): integer;
  var
    SrcLen, SrcPos, DestPos: integer;
    c: char;
    i: integer;
    InString: Boolean;
  begin
    SrcLen:=length(s);
    DestPos:=0;
    InString:=false;
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
          if DestStr<>'' then DestStr[DestPos]:='''';
          inc(DestPos);
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
  const NewLine: string): string;
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
  stctQuotation1 = 'Q'; // first ' of a double ''
  stctQuotation2 = 'M'; // second ' of a double ''
  stctChar       = 'C'; // normal character
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

  procedure ParseSrc;
  var
    APos: Integer;
    NumberStart: Integer;
    Number: Integer;
  begin
    SetLength(ParsedSrc,CurLineMax+1);
    APos:=1;
    ParsedLen:=CurLineMax+1;
    if ParsedLen>SrcLen then ParsedLen:=SrcLen;
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
          end else begin
            // normal char
            if (Src[APos] in ['A'..'Z','a'..'z'])
            and (APos>1)
            and (ParsedSrc[APos-1]=stctChar)
            and (not (Src[APos-1] in ['A'..'Z','a'..'z'])) then
              ParsedSrc[APos]:=stctWordStart
            else
              ParsedSrc[APos]:=stctChar;
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
        inc(APos);
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
    // check if there is a newline character constant
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
    Result:=Result+NewLine+GetIndentStr(CurIndent)+'+';
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
  repeat
    //DebugLn('SrcLen=',SrcLen,' CurMaxLine=',CurLineMax);
    //DebugLn('Src="',Src,'"');
    //DebugLn('Result="',Result,'"');
    if SrcLen<=CurLineMax then begin
      // line fits
      Result:=Result+Src;
      break;
    end;
    // split line -> search nice split position
    ParseSrc;
    SplitPos:=0;
    SplitAtNewLineCharConstant;
    SplitBetweenConstants;
    SplitAtWordBoundary;
    SplitDefault;
    Split;
  until false;
  //DebugLn('END Result="',Result,'"');
  //DebugLn('SplitStringConstant END---------------------------------');
end;

procedure ImproveStringConstantStart(const ACode: string; var StartPos: integer
  );
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
    ReadRawNextPascalAtom(ACode,AtomEndPos,AtomStartPos);
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
    ReadRawNextPascalAtom(ACode,AtomEndPos,AtomStartPos);
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

function GatherUnitFiles(const BaseDir, SearchPath,
  Extensions: string; KeepDoubles, CaseInsensitive: boolean;
  var TreeOfUnitFiles: TAVLTree): boolean;
// BaseDir: base directory, used when SearchPath is relative
// SearchPath: semicolon separated list of directories
// Extensions: semicolon separated list of extensions (e.g. 'pas;.pp;ppu')
// KeepDoubles: false to return only the first match of each unit
// CaseInsensitive: true to ignore case on comparing extensions
// TreeOfUnitFiles: tree of TUnitFileInfo
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
    {$IFDEF MSWindows}
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
    if SysUtils.FindFirst(ADirectory+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then
          continue;
        if ExtensionFits(FileInfo.Name) then begin
          AddToTreeOfUnitFiles(TreeOfUnitFiles,ADirectory+FileInfo.Name,
                               KeepDoubles);
        end;
      until SysUtils.FindNext(FileInfo)<>0;
    end;
    SysUtils.FindClose(FileInfo);
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

procedure AddToTreeOfUnitFiles(var TreeOfUnitFiles: TAVLTree;
  const Filename: string; KeepDoubles: boolean);
var
  AnUnitName: String;
  NewItem: TUnitFileInfo;
begin
  AnUnitName:=ExtractFileNameOnly(Filename);
  if (not KeepDoubles) then begin
    if (TreeOfUnitFiles<>nil)
    and (TreeOfUnitFiles.FindKey(Pointer(AnUnitName),
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
  NewItem:=TUnitFileInfo.Create(AnUnitName,Filename);
  TreeOfUnitFiles.Add(NewItem);
end;

function CompareUnitFileInfos(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(TUnitFileInfo(Data1).UnitName),
                             PChar(TUnitFileInfo(Data2).UnitName));
end;

function CompareUnitNameAndUnitFileInfo(UnitnamePAnsiString,
  UnitFileInfo: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(UnitnamePAnsiString),
                             PChar(TUnitFileInfo(UnitFileInfo).UnitName));
end;

procedure RaiseCatchableException(const Msg: string);
begin
  { Raises an exception.
    gdb does not catch fpc Exception objects, therefore this procedure raises
    a standard AV which is catched by gdb. }
  DebugLn('ERROR in CodeTools: ',Msg);
  // creates an exception, that gdb catches:
  DebugLn('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
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

function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
  CaseSensitive, IgnoreSpace: boolean): integer;
begin
  if IgnoreSpace then
    Result:=CompareTextIgnoringSpace(Txt1,Len1,Txt2,Len2,CaseSensitive)
  else
    Result:=CompareText(Txt1,Len1,Txt2,Len2,CaseSensitive);
end;

{ TUnitFileInfo }

constructor TUnitFileInfo.Create(const TheUnitName, TheFilename: string);
begin
  FUnitName:=TheUnitName;
  FFilename:=TheFilename;
end;


//=============================================================================

end.

