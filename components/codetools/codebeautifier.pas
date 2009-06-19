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
{ $DEFINE ShowCodeBeautifierParser}

uses
  Classes, SysUtils, AVL_Tree, FileProcs, KeywordFuncLists, CodeCache,
  BasicCodeTools;
  
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
    bbtClass,
    bbtClassInterface,
    bbtClassSection, // public, private, protected, published
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
    bbtCaseElse,  // child of bbtCase
    bbtTry,
    bbtFinally,
    bbtExcept,
    bbtIf,
    bbtIfThen,    // child of bbtIf
    bbtIfElse,    // child of bbtIf
    bbtIfBegin    // child of bbtIfThen or bbtIfElse
    );
  TFABBlockTypes = set of TFABBlockType;

const
  bbtAllIdentifierSections = [bbtTypeSection,bbtConstSection,bbtVarSection,
       bbtResourceStringSection,bbtLabelSection];
  bbtAllCodeSections = [bbtInterface,bbtImplementation,bbtInitialization,
                        bbtFinalization];
  bbtAllStatements = [bbtMainBegin,bbtCommentaryBegin,bbtRepeat,bbtProcedureBegin,
                      bbtCaseColon,bbtCaseBegin,bbtCaseElse,
                      bbtTry,bbtFinally,bbtExcept,
                      bbtIfThen,bbtIfElse,bbtIfBegin];
type
  TOnGetFABExamples = procedure(Sender: TObject; Code: TCodeBuffer;
                                Step: integer; // starting at 0
                                var CodeBuffers: TFPList // stopping when CodeBuffers=nil
                                ) of object;
  TOnGetFABNestedComments = procedure(Sender: TObject; Code: TCodeBuffer;
                                      out NestedComments: boolean) of object;

  TFABIndentationPolicy = record
    Indent: integer;
    IndentValid: boolean;
  end;

  TFABFoundIndentationPolicy = record
    Indent: integer;
    Types: array[TFABBlockType] of TFABBlockTypes;
    AllTypes: TFABBlockTypes;
  end;

  { TFABPolicies }

  TFABPolicies = class
  public
    Indentations: array of TFABFoundIndentationPolicy;
    Code: TCodeBuffer;
    CodeChangeStep: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddIndent(Typ, SubType: TFABBlockType; Indent: integer);
    function GetSmallestIndent(Typ: TFABBlockType): integer;// -1 if none found
  end;

type
  TBlock = record
    Typ: TFABBlockType;
    StartPos: integer;
    InnerStartPos: integer;
    InnerIdent: integer; // valid if >=0
  end;
  PBlock = ^TBlock;

const
  CleanBlock: TBlock = (
    Typ: bbtNone;
    StartPos: -1;
    InnerStartPos: -1;
    InnerIdent: -1
  );

type

  { TFABBlockStack }

  TFABBlockStack = class
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

  { TFullyAutomaticBeautifier }

  TFullyAutomaticBeautifier = class
  private
    FOnGetExamples: TOnGetFABExamples;
    FCodePolicies: TAVLTree;// tree of TFABPolicies sorted for Code
    FOnGetNestedComments: TOnGetFABNestedComments;
    procedure ParseSource(const Src: string; StartPos, EndPos: integer;
                          NestedComments: boolean;
                          Stack: TFABBlockStack; Policies: TFABPolicies;
                          out LastAtomStart, LastAtomEnd: integer);
    procedure ParseSource(const Src: string; StartPos, EndPos: integer;
                          NestedComments: boolean;
                          Stack: TFABBlockStack; Policies: TFABPolicies);
    function FindPolicyInExamples(StartCode: TCodeBuffer;
                                  ParentTyp, Typ: TFABBlockType): TFABPolicies;
    function GetNestedCommentsForCode(Code: TCodeBuffer): boolean;
    procedure WriteDebugReport(Msg: string; Stack: TFABBlockStack);
  public
    DefaultTabWidth: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetIndent(const Source: string; CleanPos: integer;
                       NewNestedComments: boolean;
                       out Indent: TFABIndentationPolicy): boolean;
    procedure GetDefaultSrcIndent(const Source: string; CleanPos: integer;
                               NewNestedComments: boolean;
                               out Indent: TFABIndentationPolicy);
    procedure GetDefaultIndentPolicy(Typ, SubTyp: TFABBlockType;
                                  out Indent: TFABIndentationPolicy);
    { ToDo:
      - indent on paste  (position + new source)
      - indent auto generated code (several snippets)
       }
    property OnGetExamples: TOnGetFABExamples read FOnGetExamples
                                              write FOnGetExamples;
    property OnGetNestedComments: TOnGetFABNestedComments
                           read FOnGetNestedComments write FOnGetNestedComments;
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
    'bbtClass',
    'bbtClassInterface',
    'bbtClassSection',
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
    'bbtCaseElse',
    'bbtTry',
    'bbtFinally',
    'bbtExcept',
    'bbtIf',
    'bbtIfThen',
    'bbtIfElse',
    'bbtIfBegin'
    );

function CompareFABPoliciesWithCode(Data1, Data2: Pointer): integer;
function CompareCodeWithFABPolicy(Key, Data: Pointer): integer;

implementation

function CompareFABPoliciesWithCode(Data1, Data2: Pointer): integer;
var
  Policies1: TFABPolicies absolute Data1;
  Policies2: TFABPolicies absolute Data2;
begin
  Result:=ComparePointers(Policies1.Code,Policies2.Code);
end;

function CompareCodeWithFABPolicy(Key, Data: Pointer): integer;
var
  Policies: TFABPolicies absolute Data;
begin
  Result:=ComparePointers(Key,Policies.Code);
end;

{ TFABBlockStack }

constructor TFABBlockStack.Create;
begin
  Top:=-1;
end;

destructor TFABBlockStack.Destroy;
begin
  ReAllocMem(Stack,0);
  Capacity:=0;
  Top:=-1;
  inherited Destroy;
end;

procedure TFABBlockStack.BeginBlock(Typ: TFABBlockType; StartPos: integer);
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
  DebugLn([GetIndentStr(Top*2),'TFABBlockStack.BeginBlock ',FABBlockTypeNames[Typ],' ',StartPos,' at ',PosToStr(StartPos)]);
  {$ENDIF}
  Block:=@Stack[Top];
  Block^.Typ:=Typ;
  Block^.StartPos:=StartPos;
  Block^.InnerIdent:=-1;
  Block^.InnerStartPos:=-1;
  TopType:=Typ;
end;

procedure TFABBlockStack.EndBlock;
begin
  {$IFDEF ShowCodeBeautifier}
  DebugLn([GetIndentStr(Top*2),'TFABBlockStack.EndBlock ',FABBlockTypeNames[TopType]]);
  {$ENDIF}
  dec(Top);
  if Top>=0 then
    TopType:=Stack[Top].Typ
  else
    TopType:=bbtNone;
end;

function TFABBlockStack.TopMostIndexOf(Typ: TFABBlockType): integer;
begin
  Result:=Top;
  while (Result>=0) and (Stack[Result].Typ<>Typ) do dec(Result);
end;

function TFABBlockStack.EndTopMostBlock(Typ: TFABBlockType): boolean;
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
function TFABBlockStack.PosToStr(p: integer): string;
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

procedure TFullyAutomaticBeautifier.ParseSource(const Src: string;
  StartPos, EndPos: integer; NestedComments: boolean; Stack: TFABBlockStack;
  Policies: TFABPolicies; out LastAtomStart, LastAtomEnd: integer);
var
  p: Integer;
  AtomStart: integer;
  FirstAtomOnNewLine: Boolean;

  {$IFDEF ShowCodeBeautifierParser}
  function PosToStr(p: integer): string;
  var
    X: integer;
    Y: LongInt;
  begin
    Y:=LineEndCount(Src,1,p,X)+1;
    Result:='Line='+dbgs(Y)+' Col='+dbgs(X);
  end;
  {$ENDIF}

  procedure UpdateBlockInnerIndent;
  var
    Block: PBlock;
    BlockStartPos: LongInt;
  begin
    Block:=@Stack.Stack[Stack.Top];
    if Block^.InnerIdent<0 then begin
      if Block^.Typ in [bbtIfThen,bbtIfElse] then
        BlockStartPos:=Stack.Stack[Stack.Top-1].StartPos
      else
        BlockStartPos:=Block^.StartPos;
      if not PositionsInSameLine(Src,BlockStartPos,Block^.InnerStartPos) then
        Block^.InnerIdent:=
            GetLineIndentWithTabs(Src,Block^.InnerStartPos,DefaultTabWidth)
                  -GetLineIndentWithTabs(Src,BlockStartPos,DefaultTabWidth);
    end;
  end;

  procedure BeginBlock(Typ: TFABBlockType);
  var
    Block: PBlock;
  begin
    FirstAtomOnNewLine:=false;
    if (Stack.Top>=0) then begin
      Block:=@Stack.Stack[Stack.Top];
      if (Block^.InnerStartPos=AtomStart)
      and (Policies<>nil) then begin
        if Block^.InnerIdent<0 then UpdateBlockInnerIndent;
        if Block^.InnerIdent>=0 then begin
          Policies.AddIndent(Block^.Typ,Typ,Block^.InnerIdent);
          {$IFDEF ShowCodeBeautifierParser}
          DebugLn([GetIndentStr(Stack.Top*2),'nested indentation learned ',FABBlockTypeNames[Block^.Typ],' to ',FABBlockTypeNames[Typ],': ',GetAtomString(@Src[AtomStart],NestedComments),' at ',PosToStr(p),' Indent=',Block^.InnerIdent]);
          {$ENDIF}
        end;
      end;
    end;
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
    FirstAtomOnNewLine:=false;
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

  procedure EndStatements;
  begin
    while Stack.TopType in bbtAllStatements do EndBlock;
  end;

  procedure StartIdentifierSection(Section: TFABBlockType);
  begin
    EndStatements;  // fix dangling statements
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
    EndStatements; // fix dangling statements
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

  procedure StartClassSection;
  begin
    if Stack.TopType=bbtClassSection then
      EndBlock;
    if Stack.TopType=bbtClass then
      BeginBlock(bbtClassSection);
  end;

var
  r: PChar;
  Block: PBlock;
begin
  LastAtomStart:=0;
  LastAtomEnd:=0;
  p:=StartPos;
  if EndPos>length(Src) then EndPos:=length(Src)+1;
  repeat
    ReadRawNextPascalAtom(Src,p,AtomStart,NestedComments);
    //DebugLn(['TFullyAutomaticBeautifier.ParseSource Atom=',copy(Src,AtomStart,p-AtomStart)]);
    if p>EndPos then begin
      if (AtomStart<EndPos) then begin
        LastAtomStart:=AtomStart;
        LastAtomEnd:=p;
      end;
      break;
    end else if AtomStart=EndPos then
      break;
    // check if first block inner found
    FirstAtomOnNewLine:=false;
    if (Stack.Top>=0) then begin
      Block:=@Stack.Stack[Stack.Top];
      if (Policies<>nil)
      and (Block^.InnerIdent<0)
      and (not PositionsInSameLine(Src,Block^.StartPos,AtomStart)) then begin
        FirstAtomOnNewLine:=true;
        Block^.InnerStartPos:=AtomStart;
      end;
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
        bbtMainBegin,bbtProcedureBegin:
          BeginBlock(bbtCommentaryBegin);
        bbtCaseElse,bbtCaseColon:
          BeginBlock(bbtCaseBegin);
        bbtIfThen,bbtIfElse:
          BeginBlock(bbtIfBegin);
        end;
      end;
    'C':
      case UpChars[r[1]] of
      'A': // CA
        if CompareIdentifiers('CASE',r)=0 then begin
          if Stack.TopType in bbtAllStatements then
            BeginBlock(bbtCase);
        end;
      'L': // CL
        if CompareIdentifiers('CLASS',r)=0 then begin
          if Stack.TopType=bbtTypeSection then
            BeginBlock(bbtClass);
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
          bbtIfThen:
            begin
              EndBlock;
              BeginBlock(bbtIfElse);
            end;
          end;
        end;
      'N': // EN
        if CompareIdentifiers('END',r)=0 then begin
          // if statements can be closed by end without semicolon
          while Stack.TopType in [bbtIf,bbtIfThen,bbtIfElse] do EndBlock;
          if Stack.TopType=bbtClassSection then
            EndBlock;

          case Stack.TopType of
          bbtMainBegin,bbtCommentaryBegin,
          bbtRecord,bbtClass,bbtClassInterface,bbtTry,bbtFinally,bbtExcept,
          bbtCase,bbtCaseBegin,bbtIfBegin:
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
          bbtInterface,bbtImplementation,bbtInitialization,bbtFinalization:
            EndBlock;
          end;
        end;
      'X': // EX
        if CompareIdentifiers('EXCEPT',r)=0 then begin
          if Stack.TopType=bbtTry then begin
            EndBlock;
            BeginBlock(bbtExcept);
          end;
        end;
      end;
    'F':
      case UpChars[r[1]] of
      'I': // FI
        if CompareIdentifiers('FINALIZATION',r)=0 then begin
          while Stack.TopType in (bbtAllCodeSections+bbtAllIdentifierSections+bbtAllStatements)
          do
            EndBlock;
          if Stack.TopType=bbtNone then
            BeginBlock(bbtInitialization);
        end else if CompareIdentifiers('FINALLY',r)=0 then begin
          if Stack.TopType=bbtTry then begin
            EndBlock;
            BeginBlock(bbtFinally);
          end;
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
      case UpChars[r[1]] of
      'F': // IF
        if p-AtomStart=2 then begin
          // 'IF'
          if Stack.TopType in bbtAllStatements then
            BeginBlock(bbtIf);
        end;
      'N': // IN
        case UpChars[r[2]] of
        'I': // INI
          if CompareIdentifiers('INITIALIZATION',r)=0 then begin
            while Stack.TopType in (bbtAllCodeSections+bbtAllIdentifierSections+bbtAllStatements)
            do
              EndBlock;
            if Stack.TopType=bbtNone then
              BeginBlock(bbtInitialization);
          end;
        'T': // INT
          if CompareIdentifiers('INTERFACE',r)=0 then begin
            case Stack.TopType of
            bbtNone:
              BeginBlock(bbtInterface);
            bbtTypeSection:
              BeginBlock(bbtClassInterface);
            end;
          end;
        end;
      'M': // IM
        if CompareIdentifiers('IMPLEMENTATION',r)=0 then begin
          while Stack.TopType in (bbtAllCodeSections+bbtAllIdentifierSections+bbtAllStatements)
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
        case Stack.TopType of
        bbtCase:
          BeginBlock(bbtCaseOf);
        bbtClass,bbtClassInterface:
          EndBlock;
        end;
      end;
    'P':
      case UpChars[r[1]] of
      'R': // PR
        case UpChars[r[2]] of
        'I': // PRI
          if (CompareIdentifiers('PRIVATE',r)=0) then
            StartClassSection;
        'O': // PRO
          case UpChars[r[3]] of
          'T': // PROT
            if (CompareIdentifiers('PROTECTED',r)=0) then
              StartClassSection;
          'C': // PROC
            if CompareIdentifiers('PROCEDURE',r)=0 then
              StartProcedure(bbtProcedure);
          end;
        end;
      'U': // PU
        if (CompareIdentifiers('PUBLIC',r)=0)
        or (CompareIdentifiers('PUBLISHED',r)=0) then
          StartClassSection;
      end;
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
      case UpChars[r[1]] of
      'H': // TH
        if CompareIdentifiers('THEN',r)=0 then begin
          if Stack.TopType=bbtIf then
            BeginBlock(bbtIfThen);
        end;
      'R': // TR
        if CompareIdentifiers('TRY',r)=0 then begin
          if Stack.TopType in bbtAllStatements then
            BeginBlock(bbtTry);
        end;
      'Y': // TY
        if CompareIdentifiers('TYPE',r)=0 then begin
          StartIdentifierSection(bbtTypeSection);
        end;
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
      bbtIfThen,bbtIfElse:
        while Stack.TopType in [bbtIf,bbtIfThen,bbtIfElse] do
          EndBlock;
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
        bbtIf:
          EndBlock;
        bbtIfThen,bbtIfElse:
          begin
            EndBlock;
            if Stack.TopType=bbtIf then
              EndBlock;
          end;
        end;
      end;
    end;

    if FirstAtomOnNewLine then begin
      UpdateBlockInnerIndent;
      if Block^.InnerIdent>=0 then begin
        Policies.AddIndent(Block^.Typ,bbtNone,Block^.InnerIdent);
        {$IFDEF ShowCodeBeautifierParser}
        DebugLn([GetIndentStr(Stack.Top*2),'Indentation learned for statements: ',FABBlockTypeNames[Block^.Typ],' Indent=',Block^.InnerIdent]);
        {$ENDIF}
      end;
    end;
  until false;
end;

procedure TFullyAutomaticBeautifier.ParseSource(const Src: string; StartPos,
  EndPos: integer; NestedComments: boolean; Stack: TFABBlockStack;
  Policies: TFABPolicies);
var
  LastAtomStart, LastAtomEnd: integer;
begin
  ParseSource(Src,StartPos,EndPos,NestedComments,Stack,Policies,
              LastAtomStart,LastAtomEnd);
end;

function TFullyAutomaticBeautifier.FindPolicyInExamples(StartCode: TCodeBuffer;
  ParentTyp, Typ: TFABBlockType): TFABPolicies;
var
  CodeBuffers: TFPList;
  i: Integer;
  Code: TCodeBuffer;
  AVLNode: TAVLTreeNode;
  Policies: TFABPolicies;
  Stack: TFABBlockStack;
  Step: Integer;
begin
  Result:=nil;
  if not Assigned(OnGetExamples) then exit;
  Step:=0;
  repeat
    // get examples for current step
    CodeBuffers:=nil;
    try
      OnGetExamples(Self,StartCode,Step,CodeBuffers);
      if CodeBuffers=nil then exit;
      // search policy in every example
      for i:=0 to CodeBuffers.Count-1 do begin
        Code:=TCodeBuffer(CodeBuffers[i]);
        if Code=nil then continue;
        DebugLn(['TFullyAutomaticBeautifier.FindPolicyInExamples ',Code.Filename]);
        // search Policies for code
        AVLNode:=FCodePolicies.FindKey(Code,@CompareCodeWithFABPolicy);
        if AVLNode=nil then begin
          Policies:=TFABPolicies.Create;
          Policies.Code:=Code;
          FCodePolicies.Add(Policies);
        end else
          Policies:=TFABPolicies(AVLNode.Data);
        if Policies.CodeChangeStep<>Code.ChangeStep then begin
          // parse code
          Policies.Clear;
          Policies.CodeChangeStep:=Code.ChangeStep;
          Stack:=TFABBlockStack.Create;
          try
            ParseSource(Code.Source,1,length(Code.Source)+1,
               GetNestedCommentsForCode(Code),Stack,Policies);
          finally
            Stack.Free;
          end;
        end;
        // search policy
        if Policies.GetSmallestIndent(Typ)>=0 then begin
          Result:=Policies;
          exit;
        end;
      end;
    finally
      CodeBuffers.Free;
    end;
    // next step
    inc(Step);
  until false;
end;

function TFullyAutomaticBeautifier.GetNestedCommentsForCode(Code: TCodeBuffer
  ): boolean;
begin
  Result:=true;
  if Assigned(OnGetNestedComments) then
    OnGetNestedComments(Self,Code,Result);
end;

procedure TFullyAutomaticBeautifier.WriteDebugReport(Msg: string;
  Stack: TFABBlockStack);
var
  i: Integer;
  Block: PBlock;
begin
  DebugLn(['TFullyAutomaticBeautifier.WriteDebugReport ',Msg]);
  if Stack<>nil then begin
    for i:=0 to Stack.Top do begin
      Block:=@Stack.Stack[i];
      DebugLn([GetIndentStr(i*2),' : Typ=',FABBlockTypeNames[Block^.Typ],' StartPos=',Block^.StartPos,' InnerIdent=',Block^.InnerIdent,' InnerStartPos=',Block^.InnerStartPos]);
    end;
  end;
end;

constructor TFullyAutomaticBeautifier.Create;
begin
  FCodePolicies:=TAVLTree.Create(@CompareFABPoliciesWithCode);
  DefaultTabWidth:=4;
end;

destructor TFullyAutomaticBeautifier.Destroy;
begin
  Clear;
  FreeAndNil(FCodePolicies);
  inherited Destroy;
end;

procedure TFullyAutomaticBeautifier.Clear;
begin
  FCodePolicies.FreeAndClear;
end;

function TFullyAutomaticBeautifier.GetIndent(const Source: string;
  CleanPos: integer; NewNestedComments: boolean;
  out Indent: TFABIndentationPolicy): boolean;
var
  Block: TBlock;

  function CheckPolicies(Policies: TFABPolicies; var Found: boolean): boolean;
  // returns true to stop searching
  var
    BlockIndent: LongInt;
  begin
    Result:=false;
    Found:=false;
    if (Policies=nil) then exit;
    BlockIndent:=Policies.GetSmallestIndent(Block.Typ);
    if (BlockIndent<0) then exit;
    // policy found
    DebugLn(['TFullyAutomaticBeautifier.GetIndent policy found: Block.Typ=',FABBlockTypeNames[Block.Typ],' BlockIndent=',BlockIndent]);
    Indent.Indent:=GetLineIndentWithTabs(Source,Block.StartPos,DefaultTabWidth)
                   +BlockIndent;
    Indent.IndentValid:=true;
    Result:=true;
    Found:=true;
  end;

var
  Stack: TFABBlockStack;
  Policies: TFABPolicies;
  LastAtomStart, LastAtomEnd: integer;
  ParentBlock: TBlock;
begin
  Result:=false;
  FillByte(Indent,SizeOf(Indent),0);

  CleanPos:=FindStartOfAtom(Source,CleanPos);
  if CleanPos<1 then exit;

  Block:=CleanBlock;
  ParentBlock:=CleanBlock;
  Policies:=TFABPolicies.Create;
  Stack:=TFABBlockStack.Create;
  try
    // parse source in front
    //DebugLn(['TFullyAutomaticBeautifier.GetIndent "',copy(Source,1,CleanPos-1),'"']);
    ParseSource(Source,1,CleanPos,NewNestedComments,Stack,Policies,
                LastAtomStart,LastAtomEnd);
    WriteDebugReport('After parsing code in front:',Stack);
    if (Stack.Top<0) then begin
      // no context
      DebugLn(['TFullyAutomaticBeautifier.GetIndent parsed code in front: no context']);
      GetDefaultSrcIndent(Source,CleanPos,NewNestedComments,Indent);
      exit;
    end;
    if (LastAtomStart>0) and (CleanPos>LastAtomStart) then begin
      // in comment or atom
      DebugLn(['TFullyAutomaticBeautifier.GetIndent parsed code in front: position in middle of atom, e.g. comment']);
      GetDefaultSrcIndent(Source,CleanPos,NewNestedComments,Indent);
      exit;
    end;

    Block:=Stack.Stack[Stack.Top];

    if Stack.Top>0 then
      ParentBlock:=Stack.Stack[Stack.Top-1];
    DebugLn(['TFullyAutomaticBeautifier.GetIndent parsed code in front: context=',FABBlockTypeNames[ParentBlock.Typ],'/',FABBlockTypeNames[Block.Typ],' blockindent=',GetLineIndentWithTabs(Source,Block.StartPos,DefaultTabWidth)]);
    if CheckPolicies(Policies,Result) then exit;

    // parse source behind
    ParseSource(Source,CleanPos,length(Source)+1,NewNestedComments,Stack,Policies);
    DebugLn(['TFullyAutomaticBeautifier.GetIndent parsed source behind']);
    if CheckPolicies(Policies,Result) then exit;

  finally
    Stack.Free;
    Policies.Free;
  end;

  // parse examples
  Policies:=FindPolicyInExamples(nil,ParentBlock.Typ,Block.Typ);
  DebugLn(['TFullyAutomaticBeautifier.GetIndent parsed examples']);
  if CheckPolicies(Policies,Result) then exit;

  //GetDefaultIndentPolicy(ParentBlock.Typ,Block.Typ);
end;

procedure TFullyAutomaticBeautifier.GetDefaultSrcIndent(const Source: string;
  CleanPos: integer; NewNestedComments: boolean; out
  Indent: TFABIndentationPolicy);
// return indent of last non empty line
begin
  Indent.Indent:=0;
  Indent.IndentValid:=false;
  // go to start of line
  while (CleanPos>1) and (not (Source[CleanPos-1] in [#10,#13])) do
    dec(CleanPos);
  while CleanPos>1 do begin
    // skip line end
    dec(CleanPos);
    if (CleanPos>1) and (Source[CleanPos-1] in [#10,#13])
    and (Source[CleanPos]<>Source[CleanPos-1]) then
      dec(CleanPos);
    // read line
    while (CleanPos>1) do begin
      case Source[CleanPos-1] of
      ' ',#9: dec(CleanPos);
      #10,#13:
        begin
          // empty line
          break;
        end;
      else
        Indent.Indent:=GetLineIndentWithTabs(Source,CleanPos,DefaultTabWidth);
        Indent.IndentValid:=true;
        exit;
      end;
      dec(CleanPos);
    end;
  end;
  // only empty lines in front
end;

procedure TFullyAutomaticBeautifier.GetDefaultIndentPolicy(Typ,
  SubTyp: TFABBlockType; out Indent: TFABIndentationPolicy);
begin
  Indent.IndentValid:=false;
  Indent.Indent:=0;
  case Typ of
  bbtInterface,
  bbtImplementation,
  bbtInitialization,
  bbtFinalization,
  bbtClass,
  bbtClassInterface,
  bbtProcedure,
  bbtFunction,
  bbtCase,
  bbtCaseOf,
  bbtIf:
    begin
      Indent.Indent:=0;
      Indent.IndentValid:=true;
    end;
  bbtUsesSection,
  bbtTypeSection,
  bbtConstSection,
  bbtVarSection,
  bbtResourceStringSection,
  bbtLabelSection,
  bbtRecord,
  bbtClassSection,
  bbtMainBegin,
  bbtCommentaryBegin,
  bbtRepeat,
  bbtProcedureBegin,
  bbtCaseColon,
  bbtCaseBegin,
  bbtCaseElse,
  bbtTry,
  bbtFinally,
  bbtExcept,
  bbtIfBegin:
    begin
      Indent.Indent:=2;
      Indent.IndentValid:=true;
    end;
  bbtIfThen,
  bbtIfElse:
    if SubTyp=bbtIfBegin then begin
      Indent.Indent:=0;
      Indent.IndentValid:=true;
    end else begin
      Indent.Indent:=2;
      Indent.IndentValid:=true;
    end;
  end;
end;

{ TFABPolicies }

constructor TFABPolicies.Create;
begin

end;

destructor TFABPolicies.Destroy;
begin
  inherited Destroy;
end;

procedure TFABPolicies.Clear;
begin
  SetLength(Indentations,0);
end;

procedure TFABPolicies.AddIndent(Typ, SubType: TFABBlockType; Indent: integer);
var
  i: Integer;
  OldLength: Integer;
begin
  //DebugLn(['TFABPolicies.AddIndent Typ=',FABBlockTypeNames[Typ],' SubType=',FABBlockTypeNames[SubType]]);
  OldLength:=length(Indentations);
  i:=OldLength-1;
  while (i>=0) and (Indentations[i].Indent<>Indent) do dec(i);
  if i<0 then begin
    i:=OldLength-1;
    while (i>=0) and (Indentations[i].Indent>Indent) do dec(i);
    inc(i);
    SetLength(Indentations,OldLength+1);
    if i<OldLength then begin
      System.Move(Indentations[i],Indentations[i+1],
        SizeOf(TFABFoundIndentationPolicy)*(OldLength-i));
    end;
    FillByte(Indentations[i],SizeOf(TFABFoundIndentationPolicy),0);
    Indentations[i].Indent:=Indent;
  end;
  Include(Indentations[i].Types[Typ],SubType);
  Include(Indentations[i].AllTypes,Typ);
  //for i:=0 to length(Indentations)-1 do DebugLn(['TFABPolicies.AddIndent i=',i,' indent=',Indentations[i].Indent]);
end;

function TFABPolicies.GetSmallestIndent(Typ: TFABBlockType): integer;
var
  l: Integer;
  i: Integer;
begin
  l:=length(Indentations);
  i:=0;
  while (i<l) do begin
    if Typ in Indentations[i].AllTypes then exit(Indentations[i].Indent);
    inc(i);
  end;
  Result:=-1;
end;

end.

