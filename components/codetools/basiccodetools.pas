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

interface

uses
  Classes, SysUtils, CodeToolsStrConsts, SourceLog, KeywordFuncLists;

//-----------------------------------------------------------------------------
// functions / procedures

{ These functions are not context sensitive. Especially they ignore compiler
  settings and compiler directives. They exist only for easy usage, they are not
  used by the CodeTools
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

// code search
function SearchCodeInSource(const Source,Find:string; StartPos:integer;
   var EndFoundPosition:integer;  CaseSensitive:boolean):integer;
function ReadNextPascalAtom(const Source:string; 
   var Position,AtomStart:integer):string;
function ReadRawNextPascalAtom(const Source:string;
   var Position,AtomStart:integer):string;

//----------------------------------------------------------------------------
{ These functions are used by the codetools
}

// comments
function FindNextNonSpace(const ASource: string; StartPos: integer
    ): integer;
function FindCommentEnd(const ASource: string; StartPos: integer;
    NestedComments: boolean): integer;
function FindNextCompilerDirective(const ASource: string; StartPos: integer;
    NestedComments: boolean): integer;
function CleanCodeFromComments(const DirtyCode: string;
    NestedComments: boolean): string;

// line ranges and indent
procedure GetLineStartEndAtPosition(const Source:string; Position:integer;
    var LineStart,LineEnd:integer);
function GetLineIndent(const Source: string; Position: integer): integer;
function GetIndentStr(Indent: integer): string;
function LineEndCount(const Txt: string; var LengthOfLastLine:integer): integer;

// identifiers
procedure GetIdentStartEndAtPosition(const Source:string; Position:integer;
    var IdentStart,IdentEnd:integer);
function GetIdentLen(Identifier: PChar): integer;
function GetIdentifier(Identifier: PChar): string;
function FindNextIdentifier(const Source: string; StartPos, MaxPos: integer
    ): integer;

// line/code ends
function FindFirstNonSpaceCharInLine(const Source: string;
    Position: integer): integer;
function FindLineEndOrCodeInFrontOfPosition(const Source: string;
    Position, MinPosition: integer; NestedComments: boolean;
    StopAtDirectives: boolean): integer;
function FindLineEndOrCodeAfterPosition(const Source: string;
    Position, MaxPosition: integer; NestedComments: boolean): integer;
function FindFirstLineEndInFrontOfInCode(const Source: string;
    Position, MinPosition: integer; NestedComments: boolean): integer;
function FindFirstLineEndAfterInCode(const Source: string;
    Position, MaxPosition: integer; NestedComments: boolean): integer;

// replacements
function ReplacementNeedsLineEnd(const Source: string;
    FromPos, ToPos, NewLength, MaxLineLength: integer): boolean;
function CountNeededLineEndsToAddForward(const Src: string;
    StartPos, MinLineEnds: integer): integer;
function CountNeededLineEndsToAddBackward(const Src: string;
    StartPos, MinLineEnds: integer): integer;

// comparison
function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
    CaseSensitive: boolean): integer;
function CompareText(Txt1: PChar; Len1: integer; Txt2: PChar; Len2: integer;
    CaseSensitive, IgnoreSpace: boolean): integer;
function CompareTextIgnoringSpace(const Txt1, Txt2: string;
    CaseSensitive: boolean): integer;
function CompareTextIgnoringSpace(Txt1: PChar; Len1: integer;
    Txt2: PChar; Len2: integer; CaseSensitive: boolean): integer;
function CompareSubStrings(const Find, Txt: string;
    FindStartPos, TxtStartPos, Len: integer; CaseSensitive: boolean): integer;
function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
function ComparePrefixIdent(PrefixIdent, Identifier: PChar): boolean;

// space and special chars
function TrimCodeSpace(const ACode: string): string;
function CodeIsOnlySpace(const ACode: string; FromPos, ToPos: integer): boolean;
function StringToPascalConst(const s: string): string;

// other useful stuff
procedure RaiseCatchableException(const Msg: string);



//-----------------------------------------------------------------------------

const
  MaxLineLength: integer = 80;

const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

//=============================================================================

implementation

var
  IsIDChar,          // ['a'..'z','A'..'Z','0'..'9','_']
  IsIDStartChar,     // ['a'..'z','A'..'Z','_']
  IsSpaceChar
     : array[char] of boolean;

function Min(i1, i2: integer): integer;
begin
  if i1<i2 then Result:=i1 else Result:=i2;
end;

function Max(i1, i2: integer): integer;
begin
  if i1>i2 then Result:=i1 else Result:=i2;
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
  if Position<1 then exit;
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
    while (EndPos<DirEnd) and (IsIDChar[Directive[EndPos]]) do
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
    Source.Insert(ProgramTermEnd,EndOfLine+EndOfLine+'uses'+EndOfLine+'  ;');
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
    Source.Insert(ProgramTermEnd,EndOfline+EndOfline+'uses'+EndOfline+'  ;');
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
    Source.Insert(InterfaceWordEnd,EndOfLine+EndOfLine+'uses'+EndOfLine+'  ;');
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
    Source.Insert(InterfaceWordEnd,EndOfLine+EndOfLine+'uses'+EndOfLine+'  ;');
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
    Source.Insert(UsesStart,EndOfLine+'  ');
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
    Source.Insert(UsesStart,EndOfLine+'  ');
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
  Source.Insert(Position,
         'Application.CreateForm('+AClassName+','+AName+');'+EndOfLine+'  ');
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
end;

function ListAllCreateFormsInProgram(const Source:string):TStrings;
// list format: <formname>:<formclassname>
var Position,EndPosition:integer;
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
    Source.Insert(length(Source.Source)+1,EndOfLine+AddCode);
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
    Atom:=lowercase(ReadNextPascalAtom(Source.SOurce,Position,AtomStart));
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
              PriorSpaces+ComponentName+': '+ComponentClassName+';'+EndOfLine
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

function ReadNextPascalAtomEx(const Source : string;var Position,EndPosition : integer;CaseSensitive : boolean; var Atom : string):boolean;
begin
     Atom := ReadNextPascalAtom(Source,Position,EndPosition);
     if not(CaseSensitive) then Atom := lowerCase(Atom);
     Result := (Position > length(Source));
end;

// search pascal atoms of Find in Source

function SearchCodeInSource(const Source,Find:string; StartPos:integer;
   var EndFoundPosition:integer;  CaseSensitive:boolean):integer;
var
	FindAtomStart     : integer;
	FindPos           : integer;
	Position          : integer;
	AtomStart         : integer;
	FirstSrcAtomStart : integer;
	CompareSrcPosition: integer;
	FindAtom          : string ;
	SrcAtom           : string;
	HasFound          : boolean;
	FirstFindAtom     : string;
	FirstFindPos      : integer;
begin
  Result:=-1;
  if (Find='') or (StartPos>length(Source)) then exit;

  Position:=StartPos;
  FirstFindPos:=1;

  {search first atom in find}

  if ReadNextPascalAtomEx(Find,FirstFindPos,FindAtomStart,CaseSensitive,FirstFindAtom) then exit;

  repeat

     if ReadNextPascalAtomEx(Source,Position,AtomStart,CaseSensitive,SrcAtom) then break;

     if SrcAtom=FirstFindAtom then begin
      {first atom found}
      FirstSrcAtomStart  := AtomStart;
      CompareSrcPosition := Position;
      FindPos := FirstFindPos;

      {read next source and find atoms and compare}

      repeat

        if ReadNextPascalAtomEx(Find,FindPos,FindAtomStart,CaseSensitive,FindAtom) then break;        
        if ReadNextPascalAtomEx(Source,CompareSrcPosition,AtomStart,CaseSensitive,SrcAtom) then break;

        HasFound := SrcAtom = FindAtom;

        if HasFound then begin
           Result := FirstSrcAtomStart;
           EndFoundPosition := CompareSrcPosition;
           exit;
        end;

      until not(HasFound);
     end;
  until false;
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

procedure GetIdentStartEndAtPosition(const Source: string; Position: integer;
    var IdentStart, IdentEnd: integer);
begin
  IdentStart:=Position;
  IdentEnd:=Position;
  if (Position<1) or (Position>length(Source)) then exit;
  while (IdentStart>1)
  and (IsIdChar[Source[IdentStart-1]]) do
    dec(IdentStart);
  while (IdentEnd<=length(Source))
  and (IsIdChar[Source[IdentEnd]]) do
    inc(IdentEnd);
end;

function GetIdentLen(Identifier: PChar): integer;
begin
  Result:=0;
  if Identifier=nil then exit;
  while (IsIDChar[Identifier[Result]]) do inc(Result);
end;

function ReadNextPascalAtom(const Source:string;
  var Position,AtomStart:integer):string;
var DirectiveName:string;
  DirStart,DirEnd,EndPos:integer;
begin
  repeat
    Result:=ReadRawNextPascalAtom(Source,Position,AtomStart);
    if (copy(Result,1,2)='{$') or (copy(Result,1,3)='(*$') then begin
      if copy(Result,1,2)='{$' then begin
        DirStart:=3;
        DirEnd:=length(Result);
      end else begin
        DirStart:=4;
        DirEnd:=length(Result)-1;
      end;        
      EndPos:=DirStart;
      while (EndPos<DirEnd) and (IsIDChar[Result[EndPos]]) do inc(EndPos);
      DirectiveName:=lowercase(copy(Result,DirStart,EndPos-DirStart));
      if (length(DirectiveName)=1) and (Result[DirEnd] in ['+','-']) then begin
        // switch

      end else if (DirectiveName='i') or (DirectiveName='include') then begin
        // include directive
        break;
      end;
      // ToDo: compiler directives
    end else
      break;
  until false;
end;

function ReadRawNextPascalAtom(const Source:string; 
  var Position,AtomStart:integer):string;
var Len:integer;
  c1,c2:char;
begin
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
          while (Position<=Len) and (Source[Position]<>'}') do inc(Position);
          inc(Position);
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
          while (Position<Len) 
          and ((Source[Position]<>'*') or (Source[Position]<>')')) do
            inc(Position);
          inc(Position,2);
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
    if IsIDStartChar[c1] then begin
      // identifier
      inc(Position);
      while (Position<=Len) and (IsIDChar[Source[Position]]) do
        inc(Position);
    end else begin
      case c1 of
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
          and (Source[Position] in ['0'..'9','A'..'F','a'..'f']) do 
            inc(Position);
        end;
       '{':  // compiler directive
        begin
          inc(Position);
          while (Position<=Len) and (Source[Position]<>'}') do 
            inc(Position);
          inc(Position);
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
          if ((c2='=') and  (c1 in [':','+','-','/','*','<','>']))
          or ((c1='<') and (c2='>'))
          or ((c1='.') and (c2='.'))
          or ((c1='*') and (c2='*'))
          then inc(Position);
        end;
      end;
    end;
  end;
  Result:=copy(Source,AtomStart,Position-AtomStart);
end;

function LineEndCount(const Txt: string;
  var LengthOfLastLine: integer): integer;
var i, LastLineEndPos: integer;
begin
  i:=1;
  LastLineEndPos:=0;
  Result:=0;
  while i<length(Txt) do begin
    if (Txt[i] in [#10,#13]) then begin
      inc(Result);
      inc(i);
      if (i<=length(Txt)) and (Txt[i] in [#10,#13]) and (Txt[i-1]<>Txt[i]) then
        inc(i);
      LastLineEndPos:=i;
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
  while (Result<length(Source)) and (Source[Result]<=' ') do inc(Result);
  dec(Result,LineStart);
end;

function FindLineEndOrCodeAfterPosition(const Source: string;
   Position, MaxPosition: integer; NestedComments: boolean): integer;
{ search forward for a line end or code
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
  if Result=0 then exit;
  while (Result<=SrcLen) do begin
    case Source[Result] of
      '{','(','/':
        ReadComment(Result);
      #10,#13:
        exit;
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
               PChar(Txt1),length(Txt1),PChar(Txt2),length(Txt2),
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
    if (CaseSensitive and (Txt1[P1]=Txt2[P2]))
    or ((not CaseSensitive) and (UpChars[Txt1[P1]]=UpChars[Txt2[P2]])) then
    begin
      inc(P1);
      inc(P2);
    end else begin
      // different chars found
      if InIdentifier and (IsIDChar[Txt1[P1]] xor IsIDChar[Txt2[P2]]) then begin
        // one identifier is longer than the other
        if IsIDChar[Txt1[P1]] then
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
    InIdentifier:=IsIDChar[Txt1[P1]];
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

function CompareSubStrings(const Find, Txt: string;
  FindStartPos, TxtStartPos, Len: integer; CaseSensitive: boolean): integer;
begin
  Result:=CompareText(@Find[FindStartPos],Min(length(Find)-FindStartPos+1,Len),
                      @Txt[TxtStartPos],Min(length(Txt)-TxtStartPos+1,Len),
                      CaseSensitive);
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

function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  if (Identifier1<>nil) then begin
    if (Identifier2<>nil) then begin
      while (UpChars[Identifier1[0]]=UpChars[Identifier2[0]]) do begin
        if (IsIDChar[Identifier1[0]]) then begin
          inc(Identifier1);
          inc(Identifier2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsIDChar[Identifier1[0]]) then begin
        if (IsIDChar[Identifier2[0]]) then begin
          if UpChars[Identifier1[0]]>UpChars[Identifier2[0]] then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsIDChar[Identifier2[0]]) then
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
      Result:=not IsIDChar[PrefixIdent^];
    end else begin
      Result:=false;
    end;
  end else begin
    Result:=true;
  end;
end;

function GetIdentifier(Identifier: PChar): string;
var len: integer;
begin
  if Identifier<>nil then begin
    len:=0;
    while (IsIdChar[Identifier[len]]) do inc(len);
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
  while (Result<=MaxPos) and (not IsIDStartChar[Source[Result]]) do
    inc(Result);
end;

function GetIndentStr(Indent: integer): string;
begin
  SetLength(Result,Indent);
  if Indent>0 then
    FillChar(Result[1],length(Result),' ');
end;

function TrimCodeSpace(const ACode: string): string;
// turn all lineends and special chars to space
// space is combined to one char
// space which is not needed is removed.
// space is only needed between two words or between 2-char operators
const
  NonSpaceSymbols = [',',';','(',')','[',']'];
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
        if (IsIdChar[c1] and IsIdChar[c2])
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

  function Convert(var DestStr: string): integer;
  var
    SrcLen, SrcPos, DestPos: integer;
    c: char;
    i: integer;
  begin
    SrcLen:=length(s);
    DestPos:=0;
    for SrcPos:=1 to SrcLen do begin
      inc(DestPos);
      c:=s[SrcPos];
      if c>=' ' then begin
        if DestStr<>'' then
          DestStr[DestPos]:=c;
      end else begin
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

procedure RaiseCatchableException(const Msg: string);
begin
  { Raises an exception.
    gdb does not catch fpc Exception objects, therefore this procedure raises
    a standard AV which is catched by gdb. }
  writeln('ERROR in CodeTools: ',Msg);
  // creates an exception, that gdb catches:
  writeln('Creating gdb catchable error:');
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


//=============================================================================

procedure BasicCodeToolInit;
var c: char;
begin
  for c:=#0 to #255 do begin
    IsIDChar[c]:=(c in ['a'..'z','A'..'Z','0'..'9','_']);
    IsIDStartChar[c]:=(c in ['a'..'z','A'..'Z','_']);
    IsSpaceChar[c]:=c in [#0..#32];
  end;
end;

initialization
  BasicCodeToolInit;


end.

