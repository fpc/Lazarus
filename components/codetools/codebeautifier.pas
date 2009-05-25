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
    Functions to beautify code.
    Goals:
      - Customizable
      - fully automatic
      - Beautification of whole sources. For example a unit, or several
        sources.
      - Beautification of parts of sources. For example selections.
      - Beautification of insertion source. For example beautifying code, that
        will be inserted in another source.
      - Working with syntax errors. The beautification will try its best to
        work, even if the source contains errors.
      - Does not ignore comments and directives
      - Contexts: statements, declarations

  Examples for beautification styles: see scanexamples/indentation.pas
}
unit CodeBeautifier;

{$mode objfpc}{$H+}

interface

{ $DEFINE ShowCodeBeautifier}
{$DEFINE ShowCodeBeautifierParser}

uses
  Classes, SysUtils, FileProcs, KeywordFuncLists, CodeCache, BasicCodeTools;
  
type
  TBeautifySplit =(
    bsNone,
    bsInsertSpace, // insert space before
    bsNewLine,     // break line, no indent
    bsEmptyLine,   // insert empty line, no indent
    bsNewLineAndIndent, // break line, indent
    bsEmptyLineAndIndent, // insert empty line, indent
    bsNewLineUnindent,
    bsEmptyLineUnindent,
    bsNoSplit   // do not break line here when line too long
    );
    
  TWordPolicy = (
    wpNone,
    wpLowerCase,
    wpUpperCase,
    wpLowerCaseFirstLetterUp
    );

  TFABBlockType = (
    bbtNone,
    // code sections
    bbtInterface,
    bbtImplementation,
    bbtInitialization,
    bbtFinalization,
    // identifier sections
    bbtUsesSection,
    bbtTypeSection,
    bbtConstSection,
    bbtVarSection,
    bbtResourceStringSection,
    bbtLabelSection,
    // type blocks
    bbtRecord,
    // statement blocks
    bbtProcedure, // procedure, constructor, destructor
    bbtFunction,
    bbtMainBegin,
    bbtCommentaryBegin, // begin without any need
    bbtRepeat,
    bbtProcedureBegin,
    bbtCase,
    bbtCaseOf,    // child of bbtCase
    bbtCaseColon, // child of bbtCase
    bbtCaseBegin, // child of bbtCaseColon
    bbtCaseElse   // child of bbtCase
    );
  TFABBlockTypes = set of TFABBlockType;

const
  bbtAllIdentifierSections = [bbtTypeSection,bbtConstSection,bbtVarSection,
       bbtResourceStringSection,bbtLabelSection];
  bbtAllCodeSections = [bbtInterface,bbtImplementation,bbtInitialization,
                        bbtFinalization];
  bbtAllStatements = [bbtMainBegin,bbtCommentaryBegin,bbtRepeat,
                      bbtCaseColon,bbtCaseBegin,bbtCaseElse];
type
  TOnGetFABExamples = procedure(Sender: TObject; Code: TCodeBuffer;
                                out CodeBuffers: TFPList) of object;

  TFABIndentation = record
    Indent: integer;
    UseTabs: boolean;
    InsertEmptyLines: integer;
  end;

  { TFABPolicies }

  TFABPolicies = class
  public
    Indentations: array[TFABBlockType] of TFABIndentation;
    IndentationsFound: array[TFABBlockType] of boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TFullyAutomaticBeautifier }

  TFullyAutomaticBeautifier = class
  private
    FOnGetExamples: TOnGetFABExamples;
    FAtomStarts: PInteger;
    FAtomCapacity: integer;
    FAtomCount: integer;
    procedure ParseSource(const Source: string; NewSrcLen: integer;
                          NewNestedComments: boolean);
    function IndexOfAtomInFront(CleanPos: integer): integer;
    function FindContext(CleanPos: integer; out AtomInFront: integer
                         ): TFABBlockType;
    procedure FindPolicies(Types: TFABBlockTypes; Policies: TFABPolicies);
  public
    Src: string;
    SrcLen: integer;
    NestedComments: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetIndent(const Source: string; CleanPos: integer;
                       NewNestedComments: boolean;
                       out Indent: TFABIndentation): boolean;
    { ToDo:
      - indent on paste  (position + new source)
      - indent auto generated code (several snippets)
      - learn from source
      - learn from nearest lines in source
       }
    property OnGetExamples: TOnGetFABExamples read FOnGetExamples write FOnGetExamples;
  end;

const
  FABBlockTypeNames: array[TFABBlockType] of string = (
    'bbtNone',
    // code sections
    'bbtInterface',
    'bbtImplementation',
    'bbtInitialization',
    'bbtFinalization',
    // identifier sections
    'bbtUsesSection',
    'bbtTypeSection',
    'bbtConstSection',
    'bbtVarSection',
    'bbtResourceStringSection',
    'bbtLabelSection',
    // type blocks
    'bbtRecord',
    // statement blocks
    'bbtProcedure',
    'bbtFunction',
    'bbtMainBegin',
    'bbtCommentaryBegin',
    'bbtRepeat',
    'bbtProcedureBegin',
    'bbtCase',
    'bbtCaseOf',
    'bbtCaseColon',
    'bbtCaseBegin',
    'bbtCaseElse'
    );

implementation

type
  TBlock = record
    Typ: TFABBlockType;
    StartPos: integer;
  end;
  PBlock = ^TBlock;

  { TBlockStack }

  TBlockStack = class
  public
    Stack: PBlock;
    Capacity: integer;
    Top: integer;
    TopType: TFABBlockType;
    constructor Create;
    destructor Destroy; override;
    procedure BeginBlock(Typ: TFABBlockType; StartPos: integer);
    procedure EndBlock;
    function TopMostIndexOf(Typ: TFABBlockType): integer;
    function EndTopMostBlock(Typ: TFABBlockType): boolean;
    {$IFDEF ShowCodeBeautifier}
    Src: string;
    function PosToStr(p: integer): string;
    {$ENDIF}
  end;

{ TBlockStack }

constructor TBlockStack.Create;
begin
  Top:=-1;
end;

destructor TBlockStack.Destroy;
begin
  ReAllocMem(Stack,0);
  Capacity:=0;
  Top:=-1;
  inherited Destroy;
end;

procedure TBlockStack.BeginBlock(Typ: TFABBlockType; StartPos: integer);
var
  Block: PBlock;
begin
  inc(Top);
  if Top>=Capacity then begin
    if Capacity=0 then
      Capacity:=16
    else
      Capacity:=Capacity*2;
    ReAllocMem(Stack,SizeOf(TBlock)*Capacity);
  end;
  {$IFDEF ShowCodeBeautifier}
  DebugLn([GetIndentStr(Top*2),'TBlockStack.BeginBlock ',FABBlockTypeNames[Typ],' ',StartPos,' at ',PosToStr(StartPos)]);
  {$ENDIF}
  Block:=@Stack[Top];
  Block^.Typ:=Typ;
  Block^.StartPos:=StartPos;
  TopType:=Typ;
end;

procedure TBlockStack.EndBlock;
begin
  {$IFDEF ShowCodeBeautifier}
  DebugLn([GetIndentStr(Top*2),'TBlockStack.EndBlock ',FABBlockTypeNames[TopType]]);
  {$ENDIF}
  dec(Top);
  if Top>=0 then
    TopType:=Stack[Top].Typ
  else
    TopType:=bbtNone;
end;

function TBlockStack.TopMostIndexOf(Typ: TFABBlockType): integer;
begin
  Result:=Top;
  while (Result>=0) and (Stack[Result].Typ<>Typ) do dec(Result);
end;

function TBlockStack.EndTopMostBlock(Typ: TFABBlockType): boolean;
// check if there is this type on the stack and if yes, end it
var
  i: LongInt;
begin
  i:=TopMostIndexOf(Typ);
  if i<0 then exit(false);
  Result:=true;
  while Top>=i do EndBlock;
end;

{$IFDEF ShowCodeBeautifier}
function TBlockStack.PosToStr(p: integer): string;
var
  X: integer;
  Y: LongInt;
begin
  Result:='';
  if Src='' then exit;
  Y:=LineEndCount(Src,1,p,X)+1;
  Result:='Line='+dbgs(Y)+' Col='+dbgs(X);
end;
{$ENDIF}

{ TFullyAutomaticBeautifier }

procedure TFullyAutomaticBeautifier.ParseSource(const Source: string;
  NewSrcLen: integer; NewNestedComments: boolean);
var
  Stack: TBlockStack;
  p: Integer;
  AtomStart: integer;

  {$IFDEF ShowCodeBeautifierParser}
  function PosToStr(p: integer): string;
  var
    X: integer;
    Y: LongInt;
  begin
    Y:=LineEndCount(Source,1,p,X)+1;
    Result:='Line='+dbgs(Y)+' Col='+dbgs(X);
  end;
  {$ENDIF}

  procedure BeginBlock(Typ: TFABBlockType);
  begin
    Stack.BeginBlock(Typ,AtomStart);
    {$IFDEF ShowCodeBeautifierParser}
    DebugLn([GetIndentStr(Stack.Top*2),'BeginBlock ',FABBlockTypeNames[Typ],' ',GetAtomString(@Src[AtomStart],NestedComments),' at ',PosToStr(p)]);
    {$ENDIF}
  end;

  procedure EndBlock;
  begin
    {$IFDEF ShowCodeBeautifierParser}
    DebugLn([GetIndentStr(Stack.Top*2),'EndBlock ',FABBlockTypeNames[Stack.TopType],' ',GetAtomString(@Src[AtomStart],NestedComments),' at ',PosToStr(p)]);
    {$ENDIF}
    Stack.EndBlock;
  end;

  procedure EndTopMostBlock(Typ: TFABBlockType);
  var
    i: LongInt;
  begin
    i:=Stack.TopMostIndexOf(Typ);
    if i<0 then exit;
    while Stack.Top>=i do EndBlock;
  end;

  procedure StartIdentifierSection(Section: TFABBlockType);
  begin
    if Stack.TopType in [bbtProcedure,bbtFunction] then begin
      if (Stack.Top=0) or (Stack.Stack[Stack.Top-1].Typ in [bbtImplementation])
      then begin
        // procedure with begin..end
      end else begin
        // procedure without begin..end
        EndBlock;
      end;
    end;
    if Stack.TopType in bbtAllIdentifierSections then
      EndBlock;
    if Stack.TopType in (bbtAllCodeSections+[bbtNone,bbtProcedure,bbtFunction]) then
      BeginBlock(Section);
  end;

  procedure StartProcedure(Typ: TFABBlockType);
  begin
    if Stack.TopType in [bbtProcedure,bbtFunction] then begin
      if (Stack.Top=0) or (Stack.Stack[Stack.Top-1].Typ in [bbtImplementation])
      then begin
        // procedure with begin..end
      end else begin
        // procedure without begin..end
        EndBlock;
      end;
    end;
    if Stack.TopType in bbtAllIdentifierSections then
      EndBlock;
    if Stack.TopType in (bbtAllCodeSections+[bbtNone,bbtProcedure,bbtFunction]) then
      BeginBlock(Typ);
  end;

var
  MinAtomCapacity: Integer;
  r: PChar;
begin
  Src:=Source;
  SrcLen:=NewSrcLen;
  NestedComments:=NewNestedComments;
  FAtomCount:=0;
  MinAtomCapacity:=SrcLen div 4;
  if MinAtomCapacity<1024 then
    MinAtomCapacity:=1024;
  if FAtomCapacity<MinAtomCapacity then begin
    FAtomCapacity:=MinAtomCapacity;
    ReAllocMem(FAtomStarts,FAtomCapacity*SizeOf(integer));
  end;
  Stack:=TBlockStack.Create;
  try
    p:=1;
    repeat
      ReadRawNextPascalAtom(Src,p,AtomStart,NestedComments);
      DebugLn(['TFullyAutomaticBeautifier.ParseSource ',copy(Src,AtomStart,p-AtomStart)]);
      if p>SrcLen then break;
      FAtomStarts[FAtomCount]:=AtomStart;
      inc(FAtomCount);
      if FAtomCount>FAtomCapacity then begin
        FAtomCapacity:=FAtomCapacity*2;
        ReAllocMem(FAtomStarts,FAtomCapacity*SizeOf(integer));
      end;
      r:=@Src[AtomStart];
      case UpChars[r^] of
      'B':
        if CompareIdentifiers('BEGIN',r)=0 then begin
          while Stack.TopType in (bbtAllIdentifierSections+bbtAllCodeSections) do
            EndBlock;
          case Stack.TopType of
          bbtNone:
            BeginBlock(bbtMainBegin);
          bbtProcedure,bbtFunction:
            BeginBlock(bbtProcedureBegin);
          bbtMainBegin:
            BeginBlock(bbtCommentaryBegin);
          bbtCaseElse,bbtCaseColon:
            BeginBlock(bbtCaseBegin);
          end;
        end;
      'C':
        case UpChars[r[1]] of
        'A': // CA
          if CompareIdentifiers('CASE',r)=0 then begin
            if Stack.TopType in bbtAllStatements then
              BeginBlock(bbtCase);
          end;
        'O': // CO
          if CompareIdentifiers('CONST',r)=0 then
            StartIdentifierSection(bbtConstSection);
        end;
      'E':
        case UpChars[r[1]] of
        'L': // EL
          if CompareIdentifiers('ELSE',r)=0 then begin
            case Stack.TopType of
            bbtCaseOf,bbtCaseColon:
              begin
                EndBlock;
                BeginBlock(bbtCaseElse);
              end;
            end;
          end;
        'N': // EN
          if CompareIdentifiers('END',r)=0 then begin
            case Stack.TopType of
            bbtMainBegin,bbtCommentaryBegin,
            bbtRecord,
            bbtCase,bbtCaseBegin:
              EndBlock;
            bbtCaseOf,bbtCaseElse,bbtCaseColon:
              begin
                EndBlock;
                if Stack.TopType=bbtCase then
                  EndBlock;
              end;
            bbtProcedureBegin:
              begin
                EndBlock;
                if Stack.TopType in [bbtProcedure,bbtFunction] then
                  EndBlock;
              end;
            end;
          end;
        end;
      'F':
        case UpChars[r[1]] of
        'I': // FI
          if CompareIdentifiers('FINALIZATION',r)=0 then begin
            while Stack.TopType in (bbtAllCodeSections+bbtAllIdentifierSections)
            do
              EndBlock;
            if Stack.TopType=bbtNone then
              BeginBlock(bbtInitialization);
          end;
        'O': // FO
          if CompareIdentifiers('FORWARD',r)=0 then begin
            if Stack.TopType in [bbtProcedure,bbtFunction] then begin
              EndBlock;
            end;
          end;
        'U': // FU
          if CompareIdentifiers('FUNCTION',r)=0 then
            StartProcedure(bbtFunction);
        end;
      'I':
        case UpChars[Src[1]] of
        'N': // IN
          case UpChars[Src[2]] of
          'I': // INI
            if CompareIdentifiers('INITIALIZATION',r)=0 then begin
              while Stack.TopType in (bbtAllCodeSections+bbtAllIdentifierSections)
              do
                EndBlock;
              if Stack.TopType=bbtNone then
                BeginBlock(bbtInitialization);
            end;
          'T': // INT
            if CompareIdentifiers('INTERFACE',r)=0 then begin
              if Stack.TopType=bbtNone then
                BeginBlock(bbtInterface);
            end;
          end;
        'M': // IM
          if CompareIdentifiers('IMPLEMENTATION',r)=0 then begin
            while Stack.TopType in (bbtAllCodeSections+bbtAllIdentifierSections)
            do
              EndBlock;
            if Stack.TopType=bbtNone then
              BeginBlock(bbtImplementation);
          end;
        end;
      'L':
        if CompareIdentifiers('LABEL',r)=0 then
          StartIdentifierSection(bbtLabelSection);
      'O':
        if CompareIdentifiers('OF',r)=0 then begin
          if Stack.TopType=bbtCase then
            BeginBlock(bbtCaseOf);
        end;
      'P':
        if CompareIdentifiers('PROCEDURE',r)=0 then
          StartProcedure(bbtProcedure);
      'R':
        case UpChars[r[1]] of
        'E': // RE
          case UpChars[r[2]] of
          'C': // REC
            if CompareIdentifiers('RECORD',r)=0 then
              BeginBlock(bbtRecord);
          'P': // REP
            if CompareIdentifiers('REPEAT',r)=0 then
              if Stack.TopType in bbtAllStatements then
                BeginBlock(bbtRepeat);
          'S': // RES
            if CompareIdentifiers('RESOURCESTRING',r)=0 then
              StartIdentifierSection(bbtResourceStringSection);
          end;
        end;
      'T':
        if CompareIdentifiers('TYPE',r)=0 then begin
          StartIdentifierSection(bbtTypeSection);
        end;
      'U':
        case UpChars[r[1]] of
        'S': // US
          if CompareIdentifiers('USES',r)=0 then begin
            if Stack.TopType in [bbtNone,bbtInterface,bbtImplementation] then
              BeginBlock(bbtUsesSection);
          end;
        'N': // UN
          if CompareIdentifiers('UNTIL',r)=0 then begin
            EndTopMostBlock(bbtRepeat);
          end;
        end;
      'V':
        if CompareIdentifiers('VAR',r)=0 then begin
          StartIdentifierSection(bbtVarSection);
        end;
      ';':
        case Stack.TopType of
        bbtUsesSection:
          EndBlock;
        bbtCaseColon:
          begin
            EndBlock;
            BeginBlock(bbtCaseOf);
          end;
        end;
      ':':
        if p-AtomStart=1 then begin
          // colon
          case Stack.TopType of
          bbtCaseOf:
            begin
              EndBlock;
              BeginBlock(bbtCaseColon);
            end;
          end;
        end;
      end;
    until false;
  finally
    Stack.Free;
  end;
end;

function TFullyAutomaticBeautifier.IndexOfAtomInFront(CleanPos: integer
  ): integer;
// returns index in FAtomStarts of atom in front
// if CleanPos is start of an atom the atom in front is returned
// default: -1
var
  l: Integer;
  r: LongInt;
  m: Integer;
  p: LongInt;
begin
  l:=0;
  r:=FAtomCount-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    p:=FAtomStarts[m];
    if p>CleanPos then
      r:=m-1
    else if p<CleanPos then begin
      if l=r then exit(m);
      l:=m+1;
    end else
      exit(m-1);
  end;
  Result:=-1;
end;

function TFullyAutomaticBeautifier.FindContext(CleanPos: integer; out
  AtomInFront: integer): TFABBlockType;
{  Examples:

   repeat
     |

   procedure DoSomething;
   ...
   begin
     |

   if expr then
     begin
       |

   procedure DoSomething(...;...;
     |

  if expr then
    begin
    end
    else
      begin
      end
  |

  TMyClass = class
    end;
  |

  case expr of
  |

  case expr of
  1:
    |

  case expr of
  else
    |
}
var
  i: LongInt;
  p: PChar;
  Stack: TBlockStack;
begin
  Result:=bbtNone;
  AtomInFront:=IndexOfAtomInFront(CleanPos);
  if AtomInFront<0 then exit;
  Stack:=TBlockStack.Create;
  try
    i:=0;
    p:=@Src[FAtomStarts[i]];
    //DebugLn(['TFullyAutomaticBeautifier.FindContext Atom=',GetAtomString(p,NestedComments)]);
    case UpChars[p^] of
    'B':
      if CompareIdentifiers('BEGIN',p)=0 then
        // procedure-begin
        // then-begin
        // do-begin
        // semicolon-begin
        ;
    'E':
      if CompareIdentifiers('ELSE',p)=0 then
        // case-else
        // if-else
        ;
    'O':
      if CompareIdentifiers('OF',p)=0 then
        // case-of, array-of, class-of
        ;
    'R':
      if CompareIdentifiers('REPEAT',p)=0 then
        Result:=bbtRepeat;
    ':':
      // case-colon
      ;
    ';':
      // statement or parameter
      ;
    end;
  finally
    Stack.Free;
  end;
end;

procedure TFullyAutomaticBeautifier.FindPolicies(Types: TFABBlockTypes;
  Policies: TFABPolicies);
begin

end;

constructor TFullyAutomaticBeautifier.Create;
begin

end;

destructor TFullyAutomaticBeautifier.Destroy;
begin
  Clear;
  ReAllocMem(FAtomStarts,0);
  FAtomCapacity:=0;
  inherited Destroy;
end;

procedure TFullyAutomaticBeautifier.Clear;
begin
  FAtomCount:=0;
end;

function TFullyAutomaticBeautifier.GetIndent(const Source: string;
  CleanPos: integer; NewNestedComments: boolean;
  out Indent: TFABIndentation): boolean;
var
  AtomInFront: LongInt;
  BlockType: TFABBlockType;
begin
  FillByte(Indent,SizeOf(Indent),0);

  // parse source
  ParseSource(Source,length(Source),NewNestedComments);
  DebugLn(['TFullyAutomaticBeautifier.GetIndent FAtomCount=',FAtomCount]);

  BlockType:=FindContext(CleanPos,AtomInFront);
  if AtomInFront<0 then begin
    // in comments/space in front of any code
    exit(false);
  end;

  DebugLn(['TFullyAutomaticBeautifier.GetIndent BlockType=',FABBlockTypeNames[BlockType]]);

  //Policies:=
end;

{ TFABPolicies }

constructor TFABPolicies.Create;
begin

end;

destructor TFABPolicies.Destroy;
begin
  inherited Destroy;
end;

end.

