{
  Author: Mattias Gaertner

  Abstract:
    Functions for automatic code editing.
    This unit should eventually contain the frontend of the parser.

  ToDo:
    -compiler directives

}
unit CodeTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// program name
function RenameProgramInSource(var Source:string;
   NewProgramName:string):boolean;
function FindProgramNameInSource(Source:string;
   var ProgramNameStart,ProgramNameEnd:integer):string;

// unit name
function RenameUnitInSource(var Source:string; NewUnitName:string):boolean;
function FindUnitNameInSource(Source:string;
   var UnitNameStart,UnitNameEnd:integer):string;

// uses sections
function UnitIsUsedInSource(Source,UnitName:string):boolean;
function RenameUnitInProgramUsesSection(var Source:string; 
   OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddToProgramUsesSection(var Source:string; 
   AUnitName,InFileName:string):boolean;
function RemoveFromProgramUsesSection(var Source:string; 
   AUnitName:string):boolean;
function RenameUnitInInterfaceUsesSection(var Source:string; 
   OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddToInterfaceUsesSection(var Source:string; 
   AUnitName,InFileName:string):boolean;
function RemoveFromInterfaceUsesSection(var Source:string; 
   AUnitName:string):boolean;

function IsUnitUsedInUsesSection(Source,UnitName:string; 
   UsesStart:integer):boolean;
function RenameUnitInUsesSection(var Source: string; UsesStart: integer;
   OldUnitName, NewUnitName, NewInFile:string): boolean;
function AddUnitToUsesSection(var Source:string; UnitName,InFilename:string;
   UsesStart:integer):boolean;
function RemoveUnitFromUsesSection(var Source:string; UnitName:string;
   UsesStart:integer):boolean;

// compiler directives
function FindIncludeDirective(Source,Section:string; Index:integer;
   var IncludeStart,IncludeEnd:integer):boolean;
function SplitCompilerDirective(Directive:string; 
   var DirectiveName,Parameters:string):boolean;

// createform
function AddCreateFormToProgram(var Source:string;
   AClassName,AName:string):boolean;
function RemoveCreateFormFromProgram(var Source:string;
   AClassName,AName:string):boolean;
function CreateFormExistsInProgram(Source:string;
   AClassName,AName:string):boolean;
function ListAllCreateFormsInProgram(Source:string):TStrings;

// resource code
function FindResourceInCode(Source:string; AddCode:string;
   var Position,EndPosition:integer):boolean;
function AddResourceCode(var Source:string; AddCode:string):boolean;

// code search
function SearchCodeInSource(Source,Find:string; StartPos:integer;
   var EndFoundPosition:integer;  CaseSensitive:boolean):integer;
procedure GetLineStartEndAtPosition(Source:string; Position:integer; 
   var LineStart,LineEnd:integer);
function ReadNextPascalAtom(Source:string; 
   var Position,AtomStart:integer):string;
function ReadRawNextPascalAtom(Source:string;
   var Position,AtomStart:integer):string;


const MaxLineLength:integer=80;


implementation


const
  IdentifierStartChar = ['a'..'z','A'..'Z','_'];
  IdentifierChar = ['a'..'z','A'..'Z','_','0'..'9'];
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;
  

function FindIncludeDirective(Source,Section:string; Index:integer;
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

function SplitCompilerDirective(Directive:string; 
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
    while (EndPos<DirEnd) and (Directive[EndPos] in IdentifierChar) do
      inc(EndPos);
    DirectiveName:=lowercase(copy(Directive,DirStart,EndPos-DirStart));
    Parameters:=copy(Directive,EndPos+1,DirEnd-EndPos-1);
    Result:=true;
  end else
    Result:=false;
end;

function RenameUnitInSource(var Source:string; NewUnitName:string):boolean;
var UnitNameStart,UnitNameEnd:integer;
begin
  UnitNameStart:=0;
  UnitNameEnd:=0;
  Result:=(FindUnitNameInSource(Source,UnitNameStart,UnitNameEnd)<>'');
  if Result then
    Source:=copy(Source,1,UnitNameStart-1)
           +NewUnitName
           +copy(Source,UnitNameEnd,length(Source)-UnitNameEnd+1);
end;

function FindUnitNameInSource(Source:string;
  var UnitNameStart,UnitNameEnd:integer):string;
begin
  UnitNameStart:=SearchCodeInSource(Source,'unit',1,UnitNameEnd,false);
  if UnitNameStart>0 then
    Result:=ReadNextPascalAtom(Source,UnitNameEnd,UnitNameStart)
  else
    Result:='';
end;

function RenameProgramInSource(var Source:string;
   NewProgramName:string):boolean;
var ProgramNameStart,ProgramNameEnd:integer;
begin
  Result:=(FindProgramNameInSource(Source,ProgramNameStart,ProgramNameEnd)<>'');
  if Result then
    Source:=copy(Source,1,ProgramNameStart-1)
           +NewProgramName
           +copy(Source,ProgramNameEnd,length(Source)-ProgramNameEnd+1);
end;

function FindProgramNameInSource(Source:string;
   var ProgramNameStart,ProgramNameEnd:integer):string;
begin
  ProgramNameStart:=SearchCodeInSource(Source,'program',1,ProgramNameEnd,false);
  if ProgramNameStart>0 then
    Result:=ReadNextPascalAtom(Source,ProgramNameEnd,ProgramNameStart)
  else
    Result:='';
end;

function UnitIsUsedInSource(Source,UnitName:string):boolean;
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

function RenameUnitInProgramUsesSection(var Source:string; 
   OldUnitName, NewUnitName, NewInFile:string): boolean;
var
  ProgramTermStart,ProgramTermEnd, 
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  // search Program section
  ProgramTermStart:=SearchCodeInSource(Source,'program',1,ProgramTermEnd,false);
  if ProgramTermStart<1 then exit;
  // search programname
  ReadNextPascalAtom(Source,ProgramTermEnd,ProgramTermStart);
  // search semicolon after programname
  if not (ReadNextPascalAtom(Source,ProgramTermEnd,ProgramTermStart)=';') then exit;
  UsesEnd:=ProgramTermEnd;
  ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source) then exit;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then begin
    // no uses section in interface -> add one
    Source:=copy(Source,1,ProgramTermEnd-1)
           +EndOfLine+EndOfLine+'uses'+EndOfLine+'  ;'
           +copy(Source,ProgramTermEnd,length(Source)-ProgramTermEnd+1);
    UsesEnd:=ProgramTermEnd;
    ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then exit;
  Result:=RenameUnitInUsesSection(Source,UsesStart,OldUnitName
    ,NewUnitName,NewInFile);
end;

function AddToProgramUsesSection(var Source:string; 
  AUnitName,InFileName:string):boolean;
var
  ProgramTermStart,ProgramTermEnd, 
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  if (AUnitName='') or (AUnitName=';') then exit;
  // search program
  ProgramTermStart:=SearchCodeInSource(Source,'program',1,ProgramTermEnd,false);
  if ProgramTermStart<1 then exit;
  // search programname
  ReadNextPascalAtom(Source,ProgramTermEnd,ProgramTermStart);
  // search semicolon after programname
  if not (ReadNextPascalAtom(Source,ProgramTermEnd,ProgramTermStart)=';') then exit;
  // search uses section
  UsesEnd:=ProgramTermEnd;
  ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source) then exit;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then begin
    // no uses section after program term -> add one
    Source:=copy(Source,1,ProgramTermEnd-1)
           +EndOfline+EndOfline+'uses'+EndOfline+'  ;'
           +copy(Source,ProgramTermEnd,length(Source)-ProgramTermEnd+1);
    UsesEnd:=ProgramTermEnd;
    ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then exit;
  Result:=AddUnitToUsesSection(Source,AUnitName,InFileName,UsesStart);
end;

function RenameUnitInInterfaceUsesSection(var Source:string; 
   OldUnitName, NewUnitName, NewInFile:string): boolean;
var
  InterfaceStart,InterfaceWordEnd, 
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  // search interface section
  InterfaceStart:=SearchCodeInSource(Source,'interface',1,InterfaceWordEnd,false);
  if InterfaceStart<1 then exit;
  UsesEnd:=InterfaceWordEnd;
  ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source) then exit;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then begin
    // no uses section in interface -> add one
    Source:=copy(Source,1,InterfaceWordEnd-1)
           +EndOfLine+EndOfLine+'uses'+EndOfLine+'  ;'
           +copy(Source,InterfaceWordEnd,length(Source)-InterfaceWordEnd+1);
    UsesEnd:=InterfaceWordEnd;
    ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then exit;
  Result:=RenameUnitInUsesSection(Source,UsesStart,OldUnitName
    ,NewUnitName,NewInFile);
end;

function AddToInterfaceUsesSection(var Source:string; 
  AUnitName,InFileName:string):boolean;
var
  InterfaceStart,InterfaceWordEnd, 
  UsesStart,UsesEnd:integer;
begin
  Result:=false;
  if AUnitName='' then exit;
  // search interface section
  InterfaceStart:=SearchCodeInSource(Source,'interface',1,InterfaceWordEnd,false);
  if InterfaceStart<1 then exit;
  UsesEnd:=InterfaceWordEnd;
  ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source) then exit;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then begin
    // no uses section in interface -> add one
    Source:=copy(Source,1,InterfaceWordEnd-1)
           +EndOfLine+EndOfLine+'uses'+EndOfLine+'  ;'
           +copy(Source,InterfaceWordEnd,length(Source)-InterfaceWordEnd+1);
    UsesEnd:=InterfaceWordEnd;
    ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  end;
  if not (lowercase(copy(Source,UsesStart,UsesEnd-UsesStart))='uses') then exit;
  Result:=AddUnitToUsesSection(Source,AUnitName,InFileName,UsesStart);
end;

function RemoveFromProgramUsesSection(var Source:string; 
   AUnitName:string):boolean;
var
  ProgramTermStart,ProgramTermEnd, 
  UsesStart,UsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if AUnitName='' then exit;
  // search program
  ProgramTermStart:=SearchCodeInSource(Source,'program',1,ProgramTermEnd,false);
  if ProgramtermStart<1 then exit;
  // search programname
  ReadNextPascalAtom(Source,ProgramTermEnd,ProgramTermStart);
  // search semicolon after programname
  if not (ReadNextPascalAtom(Source,ProgramTermEnd,ProgramTermStart)=';') then exit;
  UsesEnd:=ProgramTermEnd;
  Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source) then exit;
  if not (lowercase(Atom)='uses') then exit;
  Result:=RemoveUnitFromUsesSection(Source,AUnitName,UsesStart);
end;

function RemoveFromInterfaceUsesSection(var Source:string; 
   AUnitName:string):boolean;
var
  InterfaceStart,InterfaceWordEnd, 
  UsesStart,UsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if AUnitName='' then exit;
  // search interface section
  InterfaceStart:=SearchCodeInSource(Source,'interface',1,InterfaceWordEnd,false);
  if InterfaceStart<1 then exit;
  UsesEnd:=InterfaceWordEnd;
  Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  if UsesEnd>length(Source) then exit;
  if not (lowercase(Atom)='uses') then exit;
  Result:=RemoveUnitFromUsesSection(Source,AUnitName,UsesStart);
end;

function IsUnitUsedInUsesSection(Source,UnitName:string; 
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

function RenameUnitInUsesSection(var Source: string; UsesStart: integer;
   OldUnitName, NewUnitName, NewInFile:string): boolean;
var UsesEnd:integer;
  LineStart,LineEnd,OldUsesStart:integer;
  s,Atom:string;
begin
  Result:=false;
  if (OldUnitName='') then begin
    Result:=AddUnitToUsesSection(Source,NewUnitName,NewInFile,UsesStart);
    exit;
  end;
  if (NewUnitName='') or (NewUnitName=';')
  or (OldUnitName=';') or (UsesStart<1) then exit;
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is already there
  if NewInFile<>'' then
    NewInFile:=' in '''+NewInFile+'''';
  s:=', ';
  repeat
    Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(OldUnitName)) then begin
      // unit already used
      OldUsesStart:=UsesStart;
      // find comma or semicolon
      repeat
        Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
      until (Atom=',') or (Atom=';') or (Atom='');
      Source:=copy(Source,1,OldUsesStart-1)
             +NewUnitName+NewInFile
             +copy(Source,UsesStart,length(Source)-UsesStart+1);
      Result:=true;
      exit;
    end else if (Atom=';') then begin
      s:=' ';
      break;
    end;
    // read til next comma or semicolon
    while (Atom<>',') and (Atom<>';') and (Atom<>'') do
      Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  until Atom<>',';
  // unit not used yet -> add it
  Source:=copy(Source,1,UsesStart-1)
         +s+NewUnitName+NewInFile
         +copy(Source,UsesStart,length(Source)-UsesStart+1);
  GetLineStartEndAtPosition(Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (NewInFile<>'') then
    Source:=copy(Source,1,UsesStart-1)
           +EndOfLine+'  '
           +copy(Source,UsesStart,length(Source)-UsesStart+1);
  Result:=true;
end;

function AddUnitToUsesSection(var Source:string; UnitName,InFilename:string;
   UsesStart:integer):boolean;
var UsesEnd:integer;
  LineStart,LineEnd:integer;
  s,Atom:string;
begin
  Result:=false;
  if (UnitName='') or (UnitName=';') or (UsesStart<1) then exit;
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is already there
  s:=', ';
  repeat
    Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
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
      Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  until Atom<>',';
  // unit not used yet -> add it
  if InFilename<>'' then 
    InFileName:=' in '''+InFileName+'''';
  Source:=copy(Source,1,UsesStart-1)
         +s+UnitName+InFileName
         +copy(Source,UsesStart,length(Source)-UsesStart+1);
  GetLineStartEndAtPosition(Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (InFileName<>'') then
    Source:=copy(Source,1,UsesStart-1)
           +EndOfLine+'  '
           +copy(Source,UsesStart,length(Source)-UsesStart+1);
  Result:=true;
end;

function RemoveUnitFromUsesSection(var Source:string; UnitName:string;
   UsesStart:integer):boolean;
var UsesEnd,OldUsesStart,OldUsesEnd:integer;
  Atom:string;
begin
  Result:=false;
  if (UsesStart<1) or (UnitName='') or (UnitName=',') or (UnitName=';') then
    exit;
  // search interface section
  UsesEnd:=UsesStart+4;
  if not (lowercase(copy(Source,UsesStart,4))='uses') then exit;
  // parse through all used units and see if it is there
  OldUsesEnd:=-1;
  repeat
    Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
    if (lowercase(Atom)=lowercase(UnitName)) then begin
      // unit found
      OldUsesStart:=UsesStart;
      // find comma or semicolon
      repeat
        Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
      until (Atom=',') or (Atom=';') or (Atom='');
      if OldUsesEnd<1 then
        // first used unit
        Source:=copy(Source,1,OldUsesStart-1)
               +copy(Source,UsesStart,length(Source)-UsesStart+1)
      else
        // not first used unit (remove comma in front of unitname too)
        Source:=copy(Source,1,OldUsesEnd-1)
               +copy(Source,UsesStart,length(Source)-UsesStart+1);
      Result:=true;
      exit;
    end else 
      OldUsesEnd:=UsesEnd;

    // read til next comma or semicolon
    while (Atom<>',') and (Atom<>';') and (Atom<>'') do
      Atom:=ReadNextPascalAtom(Source,UsesEnd,UsesStart);
  until Atom<>',';
  // unit not used
end;

function AddCreateFormToProgram(var Source:string;
  AClassName,AName:string):boolean;
// insert 'Application.CreateForm(<AClassName>,<AName>);'
// in front of 'Application.Run;'
var Position,EndPosition:integer;
begin
  Result:=false;
  Position:=SearchCodeInSource(Source,'application.run',1,EndPosition,false);
  if Position<1 then exit;
  Source:=copy(Source,1,Position-1)
         +'Application.CreateForm('+AClassName+','+AName+');'+EndOfLine+'  ';
         +copy(Source,EndPosition,length(Source)-EndPosition+1);
  Result:=true;
end;

function RemoveCreateFormFromProgram(var Source:string;
   AClassName,AName:string):boolean;
// remove 'Application.CreateForm(<AClassName>,<AName>);'
var Position,EndPosition:integer;
begin
  Result:=false;
  Position:=SearchCodeInSource(Source,
     ';application.createform('+AClassName+','+AName+')',1,EndPosition,false);
  if Position<1 then exit;
  Source:=copy(Source,1,Position-1)
         +copy(Source,EndPosition,length(Source)-EndPosition+1);
  Result:=true;
end;

function CreateFormExistsInProgram(Source:string;
   AClassName,AName:string):boolean;
var Position,EndPosition:integer;
begin
  Position:=SearchCodeInSource(Source,
     'application.createform('+AClassName+','+AName+')',1,EndPosition,false);
  Result:=Position>0;
end;

function ListAllCreateFormsInProgram(Source:string):TStrings;
// list format: <formname>:<formclassname>
var Position,EndPosition:integer;
  s:string;
begin
  Result:=TStrings.Create;
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

function FindResourceInCode(Source:string; AddCode:string;
   var Position,EndPosition:integer):boolean;
var Find,Atom:string;
  FindPosition,FindAtomStart,SemicolonPos:integer;
begin
  Result:=false;
  if AddCode='' then begin
    Result:=true;
    exit;
  end;
  // search "LazarusResources.Add('<ResourceName>',"
  repeat
    Atom:=ReadNextPascalAtom(Source,FindPosition,FindAtomStart);
  until (Atom='') or (Atom=',');
  if Atom='' then exit;
  // search the resource start in code
  Find:=copy(AddCode,1,FindPosition);
  Position:=SearchCodeInSource(Source,Find,1,EndPosition,false);
  if Position<1 then exit;
  // search resource end in code
  SemicolonPos:=SearchCodeInSource(Source,');',EndPosition,EndPosition,false);
  if SemicolonPos<1 then exit;
  Result:=true;
end;

function AddResourceCode(var Source:string; AddCode:string):boolean;
var StartPos,EndPos:integer;
begin
  if FindResourceInCode(Source,AddCode,StartPos,EndPos) then begin
    // resource exists already -> replace it
    Source:=copy(Source,1,StartPos-1)
           +AddCode
           +copy(Source,EndPos,length(Source)-EndPos+1);
  end else begin
    // add resource
    Source:=Source+EndOfLine+AddCode;
  end;
  Result:=true;
end;

function SearchCodeInSource(Source,Find:string; StartPos:integer;
   var EndFoundPosition:integer;  CaseSensitive:boolean):integer;
// search pascal atoms of Find in Source
var FindAtomStart,FindPos,Position,AtomStart
  ,FirstSrcAtomStart,FirstSrcAtomEnd:integer;
  FindAtom,SrcAtom:string;
begin
  Result:=-1;
  if (Find='') or (StartPos>length(Source)) then exit;
  Position:=StartPos;
  repeat
    // search first atom in find
    FindPos:=1;
    ReadNextPascalAtom(Find,FindPos,FindAtomStart);
    FindAtom:=copy(Find,FindAtomStart,FindPos-FindAtomStart);
    if FindAtom='' then exit;
    if not CaseSensitive then FindAtom:=lowercase(FindAtom);
    // search first atom in source
    repeat
      ReadNextPascalAtom(Source,Position,AtomStart);
      SrcAtom:=copy(Source,AtomStart,Position-AtomStart);
      if not CaseSensitive then SrcAtom:=lowercase(SrcAtom);
    until (Position>length(Source)) or (SrcAtom=FindAtom);
    if SrcAtom=FindAtom then begin
      // first atom found
      FirstSrcAtomStart:=AtomStart;
      FirstSrcAtomEnd:=Position;
      // compare the rest of Find
      repeat
        // get next atom in find
        ReadNextPascalAtom(Find,FindPos,FindAtomStart);
        FindAtom:=copy(Find,FindAtomStart,FindPos-FindAtomStart);
        if FindAtom='' then break;
        if not CaseSensitive then FindAtom:=lowercase(FindAtom);
        // compare to next atom in source
        ReadNextPascalAtom(Source,Position,AtomStart);
        SrcAtom:=copy(Source,AtomStart,Position-AtomStart);
        if not CaseSensitive then SrcAtom:=lowercase(SrcAtom);
      until (SrcAtom<>FindAtom);
      if (FindAtom='') and (FindAtomStart>length(Find)) then begin
        // code found
        Result:=FirstSrcAtomStart;
        EndFoundPosition:=Position;
        exit;
      end;
    end else begin
      // first atom not found
      exit;
    end;
    Position:=FirstSrcAtomEnd;
  until false;
end;

procedure GetLineStartEndAtPosition(Source:string; Position:integer; 
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

function ReadNextPascalAtom(Source:string; var Position,AtomStart:integer):string;
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
      while (EndPos<DirEnd) and (Result[EndPos] in IdentifierChar) do inc(EndPos);
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

function ReadRawNextPascalAtom(Source:string; 
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
          // compiler driective
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
    case c1 of
     'a'..'z','A'..'Z','_':  // identifier
      begin
        inc(Position);
        while (Position<=Len) and (Source[Position] in IdentifierChar) do
          inc(Position);
      end;
     '0'..'9': // number
      begin
        inc(Position);
        // read numbers
        while (Position<=Len) and (Source[Position] in ['0'..'9']) do 
          inc(Position);
        if (Position<Len) and (Source[Position]='.') and (Source[Position]<>'.')
        then begin
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
     '''':  // string constant
      begin
        inc(Position);
        while (Position<=Len) do begin
          if Source[Position]='''' then begin
            inc(Position);
            if (Position<=Len) and (Source[Position]<>'''') then break;
          end;
          inc(Position);
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
        // test for double char operator :=, +=, -=, /=, *=, <>, <=, >=, **, ..
        if ((c2='=') and  (c1 in [':','+','-','/','*','<','>']))
        or ((c1='<') and (c2='>'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        then inc(Position);
      end;
    end;
  end;
  Result:=copy(Source,AtomStart,Position-AtomStart);
end;


end.
