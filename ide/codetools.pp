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
  Classes, SysUtils, ExprEval;

type
//-----------------------------------------------------------------------------
// source log

  TSourceLogEntryOperation = (sleoInsert, sleoDelete, sleoMove);

  TSourceLogEntry = class
  private
  public
    Position: integer;
    Len: integer;
    MoveTo: integer;
    LineEnds: integer;
    LengthOfLastLine: integer;
    Operation: TSourceLogEntryOperation;
    constructor Create(APos, ALength, AMoveTo: integer; const Txt: string;
      AnOperation: TSourceLogEntryOperation);
  end;

  TSourceLogMarker = class
  private
  public
    Position: integer;
    NewPosition: integer;
    Deleted: boolean;
    Data: Pointer;
  end;

  TLineRange = record
    StartPos, EndPos: integer;
  end;

  TSourceLog = class;

  TOnSourceLogInsert = procedure(Sender: TSourceLog; Pos: integer;
                        const Txt: string) of object;
  TOnSourceLogDelete = procedure(Sender: TSourceLog; Pos, Len: integer)
                        of object;
  TOnSourceLogMove = procedure(Sender: TSourceLog; Pos, Len, MoveTo: integer)
                      of object;

  TSourceLog = class
  private
    FLog: TList; // list of TSourceLogEntry
    FMarkers: TList; // list of TSourceLogMarker;
    FModified: boolean;
    FSource: string;
    FLineCount: integer;
    FLineRanges: ^TLineRange; // array of TLineRange
    FOnInsert: TOnSourceLogInsert;
    FOnDelete: TOnSourceLogDelete;
    FOnMove: TOnSourceLogMove;
    procedure SetSource(const NewSrc: string);
    function GetItems(Index: integer): TSourceLogEntry;
    procedure SetItems(Index: integer; AnItem: TSourceLogEntry);
    function GetMarkers(Index: integer): TSourceLogMarker;
    procedure BuildLineRanges;
  public
    property Items[Index: integer]: TSourceLogEntry
        read GetItems write SetItems; default;
    function Count: integer;
    property Markers[Index: integer]: TSourceLogMarker read GetMarkers;
    function MarkerCount: integer;
    procedure AddMarker(Position: integer; Data: Pointer);
    procedure AddMarker(Line, Column: integer; Data: Pointer);
    property Source: string read FSource write SetSource;
    property Modified: boolean read FModified write FModified;
    procedure LineColToPosition(Line, Column: integer; var Position: integer);
    procedure AbsoluteToLineCol(Position: integer; var Line, Column: integer);
    procedure Insert(Pos: integer; const Txt: string);
    procedure Delete(Pos, Len: integer);
    procedure Replace(Pos, Len: integer; const Txt: string);
    procedure Move(Pos, Len, MoveTo: integer);
    property OnInsert: TOnSourceLogInsert read FOnInsert write FOnInsert;
    property OnDelete: TOnSourceLogDelete read FOnDelete write FOnDelete;
    property OnMove: TOnSourceLogMove read FOnMove write FOnMove;
    procedure Clear;
    constructor Create(const ASource: string);
    destructor Destroy; override;
  end;


//----------------------------------------------------------------------------
// compiler switches
const
  CompilerSwitchesNames:array['A'..'Z'] of string=(
         'ALIGN'          // A
        ,'BOOLEVAL'       // B
        ,'ASSERTIONS'     // C
        ,'DEBUGINFO'      // D
        ,''               // E
        ,''               // F
        ,''               // G
        ,'LONGSTRINGS'    // H
        ,'IOCHECKS'       // I
        ,''               // J
        ,''               // K
        ,'LOCALSYMBOLS'   // L
        ,'TYPEINFO'       // M
        ,''               // N
        ,''               // O
        ,'OPENSTRINGS'    // P
        ,'OVERFLOWCHECKS' // Q
        ,'RANGECHECKS'    // R
        ,''               // S
        ,'TYPEADDRESS'    // T
        ,''               // U
        ,'VARSTRINGCHECKS'// V
        ,'STACKFRAMES'    // W
        ,'EXTENDEDSYNTAX' // X
        ,'REFERENCEINFO'  // Y
        ,''               // Z
     );

//-----------------------------------------------------------------------------
type
  TKeyWordFunction = function: boolean of object;

  TKeyWordFunctionListItem = class
  private
    IsLast: boolean;
    KeyWord: shortstring;
    DoIt: TKeyWordFunction;
  end;

  TKeyWordFunctionList = class
  private
    FItems: TList; // list of TKeyWordFunctionListItem;
    FSorted: boolean;
    FHashIndex: ^integer;
    FMaxHashIndex: integer;
    function KeyWordToHashIndex(const AKeyWord: shortstring): integer;
  public
    DefaultKeyWordFunction: TKeyWordFunction;
    function DoIt(const AKeyWord: shortstring): boolean;
    procedure Clear;
    procedure Add(const AKeyWord: shortstring; AFunction: TKeyWordFunction);
    procedure Sort;
    property Sorted: boolean read FSorted;
    constructor Create;
    destructor Destroy;  override;
  end;


//-----------------------------------------------------------------------------
// a TCodeTree is the product of a code tool. Every TCodeToolNode describes a
// logical block in the code (e.g. a class or a procedure).

type
  TCodeTreeNodeDesc = (
      ctnNone:=0,
      ctnClass:=1,
      ctnClassPublished:=2, ctnClassPrivate:=3, ctnClassProtected:=4, ctnPublic:=5,
      ctnProcedureHead:=6,
      ctnProcedureName:=7, ctnParameterList:=8,
      ctnFunctionType:=9, ctnProcedureModifier:=10,
      ctnBeginBlock:=11, ctnAsmBlock:=12,
      ctnInterface:=20, ctnImplementation:=21, ctnInitialization:=22,
      ctnFinalization:=23
    );
  TCodeTreeNodeDescs = set of TCodeTreeNodeDesc;

const
  AllClassSections: TCodeTreeNodeDescs =
     [ctnClassPublished, ctnClassPrivate, ctnClassProtected, ctnPublic];
  AllCodeSections: TCodeTreeNodeDescs =
     [ctnInterface, ctnImplementation, ctnInitialization, ctnFinalization];
  AllBlocks: TCodeTreeNodeDescs = [ctnBeginBlock, ctnAsmBlock];

  // CodeTreeNodeSubDescs
  ctnsNone               = 0;
  ctnsForwardDeclaration = 1;

type
  TCodePosition = packed record
    P: integer;
    CodeID: word;
  end;

  TCodeTreeNode = class
  public
    Desc: TCodeTreeNodeDesc;
    SubDesc: Word;
    Parent, NextBrother, PriorBrother, FirstChild, LastChild: TCodeTreeNode;
    StartPos, EndPos: TCodePosition;
    function Next: TCodeTreeNode;
    function Prior: TCodeTreeNode;
    procedure Clear;
    constructor Create;
  end;

  TCodeTree = class
  private
    FSources: TList; // list of TSourceLog
    function GetSources(CodeID: integer): TSourceLog;
    procedure SetSources(CodeID: integer; ASource: TSourceLog);
  public
    Root: TCodeTreeNode;
    property Sources[CodeID: integer]: TSourceLog read GetSources write SetSources;
    function SourcesCount: integer;
    function AddSource(NewSource: TSourceLog): integer;
    procedure DeleteNode(ANode: TCodeTreeNode);
    procedure AddNodeAsLastChild(ParentNode, ANode: TCodeTreeNode);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------------
// TCustomCodeTool is the ancestor class for code tools which parses code
// beginning with the Main Source code. It can parse atoms, the smallest code
// elements in source code, create new code tree nodes and provides several
// useful functions for parsing and changing code.
type
  TSyntaxType = (SyntaxType_Delphi, SyntaxType_FreePascal);

  TCustomCodeTool = class(TObject)
  private
    FIgnoreIncludeFiles: boolean;
    FIgnoreCompilerDirectives: boolean;
    FInitValues: TExpressionEvaluator;
  protected
    function DefaultKeyWordFunc: boolean;
    KeyWordFuncList: TKeyWordFunctionList;
    function GetMainSource: TSourceLog;
    procedure SetMainSource(ASource: TSourceLog);
    procedure SetDefaultKeyWordFunctions; virtual;
  public
    Tree: TCodeTree;
    SyntaxType: TSyntaxType;

    // current Values, Position, Node ...
    Values: TExpressionEvaluator;
    Pos: TCodePosition;
    AtomStart: integer;
    Atom, UpAtom: string;
    Node: TCodeTreeNode;

    procedure BeginParsing; virtual;
    procedure CreateChildNode; virtual;
    procedure EndChildNode; virtual;
    property MainSource: TSourceLog read GetMainSource write SetMainSource;
    function DoIt(const AnAtom: shortstring): boolean; virtual;
    procedure ReadNextPascalAtom; virtual;
    function ReadTilSection(SectionType: TCodeTreeNodeDesc): boolean;
    function ReadTilBracketClose: boolean;
    property IgnoreIncludeFiles: boolean
        read FIgnoreIncludeFiles write FIgnoreIncludeFiles;
    property IgnoreCompilerDirectives: boolean
        read FIgnoreCompilerDirectives write FIgnoreCompilerDirectives;
    property InitCompilerValues: TExpressionEvaluator
        read FInitValues write FInitValues;
    procedure Clear; virtual;

    constructor Create;
    destructor Destroy; override;
  end;

  TMultiKeyWordListCodeTool = class(TCustomCodeTool)
  private
    FKeyWordLists: TList; // list of TKeyWordFunctionList
    FCurKeyWordListID: integer;
  protected
    procedure SetKeyWordListID(NewID: integer);
  public
    property KeyWordListID: integer read FCurKeyWordListID write SetKeyWordListID;
    property CurKeyWordFuncList: TKeyWordFunctionList read KeyWordFuncList;
    function AddKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList): integer;
    procedure ClearKeyWordFuncLists;

    constructor Create;
    destructor Destroy; override;
  end;

  TClassAndProcCodeTool = class(TMultiKeyWordListCodeTool)
  private
  protected
    function KeyWordFuncSection: boolean;
    function KeyWordFuncEnd: boolean;
    function KeyWordFuncClass: boolean;
    function KeyWordFuncMethod: boolean;
    function KeyWordFuncForward: boolean;
    function KeyWordFuncBeginEnd: boolean;
  public
    LastPos: TCodePosition;
    LastAtomStart: integer;
    LastAtom: string;
    CurSection: TCodeTreeNodeDesc;

    function DoIt(const AnAtom: shortstring): boolean; override;
    procedure BuildTree; virtual;
    procedure SetDefaultKeyWordFunctions; override;
    constructor Create;
    destructor Destroy; override;
  end;

  ECodeToolError = class(Exception);

//-----------------------------------------------------------------------------


// functions / procedures

// source type
function FindSourceType(const Source: string;
  var SrcNameStart, SrcNameEnd: integer): string;

// program name
function RenameProgramInSource(Source:TSourceLog;
   const NewProgramName:string):boolean;
function FindProgramNameInSource(const Source:string;
   var ProgramNameStart,ProgramNameEnd:integer):string;

// unit name
function RenameUnitInSource(Source:TSourceLog;
   const NewUnitName:string):boolean;
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
function RemoveUnitFromUsesSection(Source:TSourceLog; const UnitName:string;
   UsesStart:integer):boolean;

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
function CreateFormExistsInProgram(const Source, AClassName,
   AName:string):boolean;
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
  ComponentName, ComponentClassName: string): integer;
function AddFormComponentToSource(Source:TSourceLog; FormBodyStartPos: integer;
  const ComponentName, ComponentClassName: string): boolean;
function RemoveFormComponentFromSource(Source:TSourceLog; FormBodyStartPos: integer;
  ComponentName, ComponentClassName: string): boolean;

// code search
function SearchCodeInSource(const Source,Find:string; StartPos:integer;
   var EndFoundPosition:integer;  CaseSensitive:boolean):integer;
procedure GetLineStartEndAtPosition(const Source:string; Position:integer;
   var LineStart,LineEnd:integer);
function ReadNextPascalAtom(const Source:string;
   var Position,AtomStart:integer):string;
function ReadRawNextPascalAtom(const Source:string;
   var Position,AtomStart:integer):string;

// utilities
function LineEndCount(const Txt: string;
   var LengthOfLastLine: integer): integer;

const
  MaxLineLength:integer=80;

implementation


const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

var
  IsIDChar,          // ['a'..'z','A'..'Z','0'..'9','_']
  IsIDStartChar      // ['a'..'z','A'..'Z','_']
     :array[#0..#255] of boolean;

  CharToHash: array[#0..#255] of integer;
  

type
  TCodeTreeNodeMemManager = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    function CreateNode: TCodeTreeNode;
    procedure DisposeNode(ANode: TCodeTreeNode);
  end;

var
  NodeMemManager: TCodeTreeNodeMemManager; 

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

function RenameUnitInSource(Source:TSourceLog;
  const NewUnitName:string):boolean;
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
  if uppercase(FindSourceType(Source,UnitNameStart,UnitNameEnd))='UNIT' then
    Result:=copy(Source,UnitNameStart,UnitNameEnd-UnitNameStart)
  else
    Result:='';
end;

function RenameProgramInSource(Source: TSourceLog;
  const NewProgramName:string):boolean;
var ProgramNameStart,ProgramNameEnd:integer;
begin
  Result:=(FindProgramNameInSource(Source.Source,ProgramNameStart,ProgramNameEnd)<>'');
  if Result then
    Source.Replace(ProgramNameStart,ProgramNameEnd-ProgramNameStart,NewProgramName)
end;

function FindProgramNameInSource(const Source:string;
   var ProgramNameStart,ProgramNameEnd:integer):string;
begin
  if uppercase(FindSourceType(Source,ProgramNameStart,ProgramNameEnd))='PROGRAM'
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
  s,Atom,InFilePhrase:string;
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
    InFilePhrase:=' in '''+NewInFile+''''
  else
    InFilePhrase:='';
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
      Source.Replace(OldUsesStart,UsesStart-OldUsesStart,
             NewUnitName+NewInFile);
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
  Source.Insert(UsesStart,s+NewUnitName+InFilePhrase);
  GetLineStartEndAtPosition(Source.Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (InFilePhrase<>'') then
    Source.Insert(UsesStart,EndOfLine+'  ');
  Result:=true;
end;

function AddUnitToUsesSection(Source:TSourceLog;
  const UnitName,InFilename:string; UsesStart:integer):boolean;
var UsesEnd:integer;
  LineStart,LineEnd:integer;
  s,Atom,InFilenamePhrase:string;
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
    InFileNamePhrase:=' in '''+InFileName+''''
  else
    InFilenamePhrase:='';
  Source.Insert(UsesStart,s+UnitName+InFileNamePhrase);
  GetLineStartEndAtPosition(Source.Source,UsesStart,LineStart,LineEnd);
  if (LineEnd-LineStart>MaxLineLength) or (InFileNamePhrase<>'') then
    Source.Insert(UsesStart,EndOfLine+'  ');
  Result:=true;
end;

function RemoveUnitFromUsesSection(Source:TSourceLog; const  UnitName:string;
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
var Position,EndPosition:integer;
begin
  Result:=false;
  Position:=SearchCodeInSource(Source.Source,'application.run',1
    ,EndPosition,false);
  if Position<1 then exit;
  Source.Insert(Position,
         +'Application.CreateForm('+AClassName+','+AName+');'+EndOfLine+'  ');
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
  if AddCode='' then exit;
  if Source='' then exit;
  // search the "LazarusResources.Add('<ResourceName>'," in AddCode
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
  ComponentName, ComponentClassName: string): integer;
var AtomStart, OldPos: integer;
  Atom: string;
begin
  ComponentName:=lowercase(ComponentName);
  ComponentClassName:=lowercase(ComponentClassName);
  Result:=FormBodyStartPos;
  repeat
    Atom:=lowercase(ReadNextPascalAtom(Source,Result,AtomStart));
    if (Atom='public') or (Atom='private') or (Atom='end')
    or (Atom='protected') or (Atom='') then begin
      Result:=-1;
      exit;
    end;
    OldPos:=Result;
    if (lowercase(ReadNextPascalAtom(Source,Result,AtomStart))=ComponentName)
    and (ReadNextPascalAtom(Source,Result,AtomStart)=':')
    and (lowercase(ReadNextPascalAtom(Source,Result,AtomStart))=ComponentClassName)
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
             +PriorSpaces+ComponentName+': '+ComponentClassName+';'+EndOfLine
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

function SearchCodeInSource(const Source,Find:string; StartPos:integer;
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


{ TSourceLogEntry }

constructor TSourceLogEntry.Create(APos, ALength, AMoveTo: integer;
  const Txt: string;
  AnOperation: TSourceLogEntryOperation);
begin
  Position:=APos;
  Len:=ALength;
  MoveTo:=AMoveTo;
  Operation:=AnOperation;
  LineEnds:=LineEndCount(Txt, LengthOfLastLine);
end;

{ TSourceLogMarker }

{ TSourceLog }

constructor TSourceLog.Create(const ASource: string);
begin
  inherited Create;
  FModified:=false;
  FSource:=ASource;
  FLog:=TList.Create;
  FMarkers:=TList.Create;
  FLineRanges:=nil;
  FLineCount:=-1;
end;

destructor TSourceLog.Destroy;
begin
  Clear;
  FMarkers.Free;
  FLog.Free;
  inherited Destroy;
end;

procedure TSourceLog.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  FLog.Clear;
  for i:=0 to MarkerCount-1 do Markers[i].Free;
  FMarkers.Clear;
  FSource:='';
  FModified:=false;
  if FLineRanges<>nil then begin
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  FLineCount:=-1;
end;

function TSourceLog.GetItems(Index: integer): TSourceLogEntry;
begin
  Result:=TSourceLogEntry(FLog[Index]);
end;

procedure TSourceLog.SetItems(Index: integer; AnItem: TSourceLogEntry);
begin
  FLog[Index]:=AnItem;
end;

function TSourceLog.Count: integer;
begin
  Result:=fLog.Count;
end;

function TSourceLog.GetMarkers(Index: integer): TSourceLogMarker;
begin
  Result:=TSourceLogMarker(FMarkers[Index]);
end;

function TSourceLog.MarkerCount: integer;
begin
  Result:=fMarkers.Count;
end;

procedure TSourceLog.SetSource(const NewSrc: string);
begin
  Clear;
  FSource:=NewSrc;
end;

procedure TSourceLog.Insert(Pos: integer; const Txt: string);
var i: integer;
begin
  if Assigned(FOnInsert) then FOnInsert(Self,Pos,Txt);
  FSource:=copy(FSource,1,Pos-1)
          +Txt
          +copy(FSource,Pos,length(FSource)-Pos+1);
  FLog.Add(TSourceLogEntry.Create(Pos,length(Txt),-1,Txt,sleoInsert));
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) and (Markers[i].NewPosition>Pos) then begin
      Markers[i].NewPosition:=Markers[i].NewPosition+length(Txt);
    end;
  end;
  FLineCount:=-1;
  FModified:=true;
end;

procedure TSourceLog.Delete(Pos, Len: integer);
var i: integer;
begin
  if Assigned(FOnDelete) then FOnDelete(Self,Pos,Len);
  System.Delete(FSource,Pos,Len);
  FLog.Add(TSourceLogEntry.Create(Pos,Len,-1,'',sleoDelete));
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) and (Markers[i].NewPosition>Pos) then begin
      if Markers[i].Position<Pos+Len then
        Markers[i].Deleted:=true
      else 
        Markers[i].NewPosition:=Markers[i].NewPosition-Len;
    end;
  end;
  FLineCount:=-1;
  FModified:=true;
end;

procedure TSourceLog.Replace(Pos, Len: integer; const Txt: string);
var i: integer;
begin
  if Assigned(FOnDelete) then FOnDelete(Self,Pos,Len);
  if Assigned(FOnInsert) then FOnInsert(Self,Pos,Txt);
  FSource:=copy(FSource,1,Pos-1)
          +Txt
          +copy(FSource,Pos+Len,length(FSource)-Pos-Len+1);
  FLog.Add(TSourceLogEntry.Create(Pos,Len,-1,'',sleoDelete));
  FLog.Add(TSourceLogEntry.Create(Pos,length(Txt),-1,Txt,sleoInsert));
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) and (Markers[i].NewPosition>Pos) then begin
      if Markers[i].Position<Pos+Len then
        Markers[i].Deleted:=true
      else 
        Markers[i].NewPosition:=Markers[i].NewPosition-Len+length(Txt);
    end;
  end;
  FLineCount:=-1;
  FModified:=true;
end;

procedure TSourceLog.Move(Pos, Len, MoveTo: integer);
var i: integer;
begin
  if Assigned(FOnMove) then FOnMove(Self,Pos,Len,MoveTo);
  if (MoveTo>=Pos) and (MoveTo<Pos+Len) then exit;
  if MoveTo<Pos then begin
    FSource:=copy(FSource,1,MoveTo-1)
            +copy(FSource,Pos,Len)
            +copy(FSource,MoveTo,Pos-MoveTo)
            +copy(FSource,Pos+Len,length(FSource)-Pos-Len+1);
  end else begin
    FSource:=copy(FSource,1,Pos-1)
            +copy(FSource,Pos+Len,MoveTo-Pos-Len)
            +copy(FSource,Pos,Len)
            +copy(FSource,MoveTo,length(FSource)-MoveTo+1);
  end;
  FLog.Add(TSourceLogEntry.Create(Pos,Len,MoveTo,'',sleoMove));
  for i:=0 to FMarkers.Count-1 do begin
    if (Markers[i].Deleted=false) and (Markers[i].NewPosition>Pos)
    and (Markers[i].Position<Pos+Len) then
      Markers[i].NewPosition:=Markers[i].NewPosition+MoveTo-Pos;
  end;
  FLineCount:=-1;
  FModified:=true;
end;

procedure TSourceLog.AddMarker(Position: integer; Data: Pointer);
var NewMarker: TSourceLogMarker;
begin
  NewMarker:=TSourceLogMarker.Create;
  NewMarker.Position:=Position;
  NewMarker.Data:=Data;
  NewMarker.Deleted:=false;
  FMarkers.Add(NewMarker);
end;

procedure TSourceLog.AddMarker(Line, Column: integer; Data: Pointer);
var NewMarker: TSourceLogMarker;
begin
  NewMarker:=TSourceLogMarker.Create;
  LineColToPosition(Line,Column,NewMarker.Position);
  NewMarker.Data:=Data;
  NewMarker.Deleted:=false;
  FMarkers.Add(NewMarker);
end;

procedure TSourceLog.BuildLineRanges;
var len,p,line:integer;
  c:char;
begin
  if FLineCount>=0 then exit;
  if FLineRanges<>nil then begin
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  // count line ends
  FLineCount:=0;
  Len:=length(FSource);
  p:=1;
  while (p<=Len) do begin
    if (not (FSource[p] in [#10,#13])) then begin
      inc(p);
    end else begin
      // new line
      inc(FLineCount);
      c:=FSource[p];
      inc(p);
      if (p<=Len) and (c in [#13,#10]) and (FSource[p]<>FSource[p-1]) then
        inc(p);
    end;
  end;
  if (FSource<>'') and (not (FSource[Len] in [#10,#13])) then inc(FLineCount);
  // build line range list
  if FLineCount>0 then begin
    GetMem(FLineRanges,FLineCount*SizeOf(TLineRange));
    p:=1;
    line:=0;
    FLineRanges[line].StartPos:=1;
    FLineRanges[FLineCount-1].EndPos:=Len;
    while (p<=Len) do begin
      if (not (FSource[p] in [#10,#13])) then begin
        inc(p);
      end else begin
        // new line
        FLineRanges[line].EndPos:=p;
        inc(line);
        c:=FSource[p];
        inc(p);
        if (p<=Len) and (c in [#10,#13]) and (FSource[p]<>FSource[p-1]) then
          inc(p);
        if line<FLineCount then
          FLineRanges[line].StartPos:=p;
      end;
    end;
  end;
end;

procedure TSourceLog.LineColToPosition(Line, Column: integer;
  var Position: integer);
begin
  BuildLineRanges;
  if (Line>=0) and (Line<FLineCount) then begin
    if (Line<FLineCount-1) then begin
      if (Column<FLineRanges[Line+1].StartPos-FLineRanges[Line].EndPos) then begin
        Position:=FLineRanges[Line].StartPos+Column-1;
      end else begin
        Position:=-1;  exit;
      end;
    end else begin
      if (Column<=length(Source)-FLineRanges[Line].StartPos) then begin
        Position:=FLineRanges[Line].StartPos+Column-1;
      end else begin
        Position:=-1;  exit;
      end;
    end;
  end else begin
    Position:=-1;  exit;
  end;
end;

procedure TSourceLog.AbsoluteToLineCol(Position: integer;
  var Line, Column: integer);
var l,r,m:integer;
begin
  BuildLineRanges;
  if (FLineCount=0) or (Position<1) or (Position>=length(FSource)) then begin
    Line:=-1;
    Column:=-1;
    exit;
  end;
  if (Position>=FLineRanges[FLineCount-1].StartPos) then begin
    Line:=FLineCount-1;
    Column:=Position-FLineRanges[Line].StartPos+1;
    exit;
  end;
  // binary search for the line
  l:=0;
  r:=FLineCount;
  repeat
    m:=(l+r) shr 1;
    if FLineRanges[m].StartPos>Position then begin
      // too high, search lower
      r:=m-1;
    end else if FLineRanges[m+1].StartPos<=Position then begin
      // too low, search higher
      l:=m+1;
    end else begin
      // line found
      Line:=m;
      Column:=Position-FLineRanges[Line].StartPos+1;
      exit;
    end;
  until false;
end;

{ TKeyWordFunctionList }

constructor TKeyWordFunctionList.Create;
begin
  inherited Create;
  FItems:=TList.Create; // list of TKeyWordFunctionListItem;
  FSorted:=true;
  FHashIndex:=nil;
  FMaxHashIndex:=-1;
end;

destructor TKeyWordFunctionList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TKeyWordFunctionList.Clear;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do TKeyWordFunctionListItem(FItems[i]).Free;
  FItems.Clear;
  if FHashIndex<>nil then begin
    FreeMem(FHashIndex);
    FHashIndex:=nil;
  end;
  FMaxHashIndex:=-1;  
  FSorted:=true;
end;

function TKeyWordFunctionList.KeyWordToHashIndex(
  const AKeyWord: shortstring): integer;
var KeyWordLen, i: integer;
begin
  KeyWordLen:=length(AKeyWord);
  if KeyWordLen>20 then KeyWordLen:=20;
  Result:=0;
  for i:=1 to KeyWordLen do
    inc(Result,CharToHash[AKeyWord[i]]);
  if Result>FMaxHashIndex then Result:=-1;
end;

function TKeyWordFunctionList.DoIt(const AKeyWord: shortstring): boolean;
var i: integer;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(AKeyWord);
  if i>=0 then begin
    i:=FHashIndex[i];
    if i>=0 then begin
      repeat
        if (TKeyWordFunctionListItem(FItems[i]).KeyWord=AKeyWord) then begin
          if TKeyWordFunctionListItem(FItems[i]).DoIt<>nil then
            Result:=TKeyWordFunctionListItem(FItems[i]).DoIt()
          else
            Result:=DefaultKeyWordFunction();
          exit;  
        end;
        if (TKeyWordFunctionListItem(FItems[i])).IsLast then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

procedure TKeyWordFunctionList.Add(const AKeyWord: shortstring;
  AFunction: TKeyWordFunction);
var NewKeyWordFunction: TKeyWordFunctionListItem;
begin
  FSorted:=false;
  NewKeyWordFunction:=TKeyWordFunctionListItem.Create;
  with NewKeyWordFunction do begin
    KeyWord:=AKeyWord;
    DoIt:=AFunction;
  end;
  FItems.Add(NewKeyWordFunction);
end;

procedure TKeyWordFunctionList.Sort;
// bucketsort
var i, h, NewMaxHashIndex: integer;
  UnsortedItems: ^pointer;
begin
  if FSorted then exit;
  if FHashIndex<>nil then begin
    FreeMem(FHashIndex);
    FHashIndex:=nil;
  end;
  // find maximum hash index
  FMaxHashIndex:=99999;
  NewMaxHashIndex:=0;
  for i:=0 to FItems.Count-1 do begin
    h:=KeyWordToHashIndex(TKeyWordFunctionListItem(FItems[i]).KeyWord);
    if h>NewMaxHashIndex then NewMaxHashIndex:=h;
  end;
  FMaxHashIndex:=NewMaxHashIndex;
  // create hash index
  GetMem(FHashIndex,FMaxHashIndex * SizeOf(integer));
  // compute every hash value count
  for i:=0 to FMaxHashIndex-1 do FHashIndex[i]:=0;
  for i:=0 to FItems.Count-1 do begin
    h:=KeyWordToHashIndex(TKeyWordFunctionListItem(FItems[i]).KeyWord);
    if h>=0 then inc(FHashIndex[h]);
  end;
  // change hash-count-index to hash-last-index
  for i:=1 to FMaxHashIndex-1 do 
    inc(FHashIndex[i],FHashIndex[i-1]);
  // copy all items
  GetMem(UnsortedItems,sizeof(pointer)*FItems.Count);
  for i:=0 to FItems.Count-1 do
    UnsortedItems[i]:=FItems[i];
  // copy unsorted items to Items back and do the bucket sort
  for i:=FItems.Count-1 downto 0 do begin
    h:=KeyWordToHashIndex(TKeyWordFunctionListItem(FItems[i]).KeyWord);
    if h>=0 then begin
      dec(FHashIndex[h]);
      FItems[FHashIndex[h]]:=UnsortedItems[i];
    end;
  end;
  // tidy up
  FreeMem(UnsortedItems);
  FSorted:=true;
end;

{ TCodeTreeNode }

constructor TCodeTreeNode.Create;
begin
  Clear;
end;

procedure TCodeTreeNode.Clear;
begin
  Desc:=ctnNone;
  SubDesc:=ctnsNone;
  Parent:=nil;
  NextBrother:=nil;
  PriorBrother:=nil;
  FirstChild:=nil;
  LastChild:=nil;
  StartPos.P:=-1;
  EndPos.P:=-1;
end;

function TCodeTreeNode.Next: TCodeTreeNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.NextBrother=nil) do
    Result:=Result.Parent;
  if Result<>nil then Result:=Result.NextBrother;
end;

function TCodeTreeNode.Prior: TCodeTreeNode;
begin
  if PriorBrother<>nil then
    Result:=PriorBrother
  else
    Result:=Parent;
end;

{ TCodeTree }

constructor TCodeTree.Create;
begin
  Root:=nil;
  FSources:=TList.Create;
end;

destructor TCodeTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCodeTree.GetSources(CodeID: integer): TSourceLog;
begin
  Result:=TSourceLog(FSources[CodeID]);
end;

procedure TCodeTree.SetSources(CodeID: integer; ASource: TSourceLog);
begin
  FSources[CodeID]:=ASource;
end;

function TCodeTree.SourcesCount: integer;
begin
  Result:=FSources.Count;
end;

function TCodeTree.AddSource(NewSource: TSourceLog): integer;
begin
  Result:=FSources.Add(NewSource);
end;

procedure TCodeTree.Clear;
begin
  DeleteNode(Root);
  Root:=nil;
  FSources.Clear;
end;

procedure TCodeTree.DeleteNode(ANode: TCodeTreeNode);
begin
  if ANode=nil then exit;
  while (ANode.FirstChild<>nil) do DeleteNode(ANode.FirstChild);
  with ANode do begin
    if (Parent<>nil) then begin
      if (Parent.FirstChild=ANode) then
        Parent.FirstChild:=NextBrother;
      if (Parent.LastChild=ANode) then
        Parent.LastChild:=PriorBrother;
    end;
    if NextBrother<>nil then NextBrother.PriorBrother:=PriorBrother;
    if PriorBrother<>nil then PriorBrother.NextBrother:=NextBrother;
    NextBrother:=nil;
    PriorBrother:=nil;
  end;
  if ANode=Root then Root:=nil;
  NodeMemManager.DisposeNode(ANode);
end;

procedure TCodeTree.AddNodeAsLastChild(ParentNode, ANode: TCodeTreeNode);
begin
  if Root=nil then Root:=ANode;
  if ParentNode<>nil then begin
    if ParentNode.FirstChild=nil then begin
      ParentNode.FirstChild:=ANode;
      ParentNode.LastChild:=ANode;
    end else begin
      ANode.PriorBrother:=ParentNode.LastChild;
      ParentNode.LastChild:=ANode;
      if ANode.PriorBrother<>nil then ANode.PriorBrother.NextBrother:=ANode;
    end;
  end;
end;

{ TCodeTreeNodeMemManager }

constructor TCodeTreeNodeMemManager.Create;
begin
  inherited Create;
end;

destructor TCodeTreeNodeMemManager.Destroy;
begin
  inherited Destroy;
end;

function TCodeTreeNodeMemManager.CreateNode: TCodeTreeNode;
begin
  Result:=TCodeTreeNode.Create;
end;

procedure TCodeTreeNodeMemManager.DisposeNode(ANode: TCodeTreeNode);
begin
  ANode.Free;
end;

{ TCustomCodeTool }

constructor TCustomCodeTool.Create;
begin
  inherited Create;
  FIgnoreIncludeFiles:=true;
  FIgnoreCompilerDirectives:=true;
  FInitValues:=nil;
  Tree:=TCodeTree.Create;
  Values:=nil;
  SyntaxType:=SyntaxType_FreePascal;
  KeyWordFuncList:=TKeyWordFunctionList.Create;
  SetDefaultKeyWordFunctions;
  Clear;
end;

destructor TCustomCodeTool.Destroy;
begin
  Clear;
  KeyWordFuncList.Free;
  inherited Destroy;
end;

procedure TCustomCodeTool.Clear;
begin
  Tree.Clear;
  if Values<>nil then begin
    Values.Free;  Values:=nil;
  end;
  Pos.P:=1;
  Pos.CodeID:=0;
  AtomStart:=-1;
  Atom:='';
end;

function TCustomCodeTool.GetMainSource: TSourceLog;
begin
  if Tree.SourcesCount=0 then Result:=nil
  else Result:=Tree.Sources[0];
end;

procedure TCustomCodeTool.SetMainSource(ASource: TSourceLog);
begin
  Clear;
  Tree.AddSource(ASource);
end;

procedure TCustomCodeTool.ReadNextPascalAtom;
var DirectiveName:string;
  DirStart,DirEnd,EndPos:integer;
begin
  repeat
    Atom:=ReadRawNextPascalAtom(Tree.Sources[Pos.CodeID].Source,Pos.P,AtomStart);
    if (copy(Atom,1,2)='{$') or (copy(Atom,1,3)='(*$') then begin
      if copy(Atom,1,2)='{$' then begin
        DirStart:=3;
        DirEnd:=length(Atom);
      end else begin
        DirStart:=4;
        DirEnd:=length(Atom)-1;
      end;        
      EndPos:=DirStart;
      while (EndPos<DirEnd) and (IsIDChar[Atom[EndPos]]) do inc(EndPos);
      DirectiveName:=lowercase(copy(Atom,DirStart,EndPos-DirStart));
      if (length(DirectiveName)=1) and (Atom[DirEnd] in ['+','-']) then begin
        // compiler short switch

      end else if (DirectiveName='i') or (DirectiveName='include') then begin
        // include directive

      end;
    end else
      break;
  until false;
  UpAtom:=uppercase(Atom);
end;

function TCustomCodeTool.ReadTilSection(
  SectionType: TCodeTreeNodeDesc): boolean;
var LastAtom: string;
  SectionName: string;
begin
  Result:=false;
  if not (SectionType in AllCodeSections) then exit;
  case SectionType of
    ctnInterface: SectionName:='interface';
    ctnImplementation: SectionName:='implementation';
    ctnInitialization: SectionName:='initialization';
   else SectionName:='finalization';
  end;
  repeat
    LastAtom:=Atom;
    ReadNextPascalAtom;
  until (Atom='') or ((LastAtom<>'=') and (lowercase(Atom)=SectionName));
  Result:=(Atom<>'');
end;

function TCustomCodeTool.ReadTilBracketClose: boolean;
var CloseBracket: char;
begin
  Result:=false;
  if Atom='(' then CloseBracket:=')'
  else if Atom='[' then CloseBracket:=']'
  else exit;
  repeat
    ReadNextPascalAtom;
    if (Atom='(') or (Atom='[') then begin
      if not ReadTilBracketClose then exit;
    end;
  until (Atom='') or (Atom=CloseBracket);
  Result:=true;
end;

procedure TCustomCodeTool.BeginParsing;
begin
  Pos.P:=1;
  Pos.CodeID:=0;
  if not IgnoreCompilerDirectives then begin
    if Values=nil then Values:=TExpressionEvaluator.Create;
    Values.Assign(FInitValues);
  end;
  Node:=nil;
  Tree.DeleteNode(Tree.Root);
end;

procedure TCustomCodeTool.CreateChildNode;
var NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.CreateNode;
  Tree.AddNodeAsLastChild(Node,NewNode);
  Node:=NewNode;
end;

procedure TCustomCodeTool.EndChildNode;
begin
  Node:=Node.Parent;
end;

procedure TCustomCodeTool.SetDefaultKeyWordFunctions;
begin
  KeyWordFuncList.Clear;
  KeyWordFuncList.DefaultKeyWordFunction:=@DefaultKeyWordFunc;
end;

function TCustomCodeTool.DoIt(const AnAtom: shortstring): boolean;
begin
  if AnAtom='' then exit(false)
  else if IsIDStartChar[AnAtom[1]] then 
    Result:=KeyWordFuncList.DoIt(AnAtom)
  else
    Result:=true;
end;

function TCustomCodeTool.DefaultKeyWordFunc: boolean;
begin
  Result:=true;
end;

{ TMultiKeyWordListCodeTool }

constructor TMultiKeyWordListCodeTool.Create;
begin
  inherited Create;
  FKeyWordLists:=TList.Create; // list of TKeyWordFunctionList
  AddKeyWordFuncList(KeyWordFuncList);
  FCurKeyWordListID:=0;
end;

destructor TMultiKeyWordListCodeTool.Destroy;
begin
  ClearKeyWordFuncLists;
  FKeyWordLists.Free;
  inherited Destroy;
end;

procedure TMultiKeyWordListCodeTool.SetKeyWordListID(NewID: integer);
begin
  if FCurKeyWordListID=NewID then exit;
  FCurKeyWordListID:=NewID;
  KeyWordFuncList:=TKeyWordFunctionList(FKeyWordLists[NewID]);
end;

function TMultiKeyWordListCodeTool.AddKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList): integer;
begin
  Result:=FKeyWordLists.Add(AKeyWordFuncList);
end;

procedure TMultiKeyWordListCodeTool.ClearKeyWordFuncLists;
var i: integer;
begin
  KeyWordListID:=0;
  for i:=FKeyWordLists.Count-1 downto 1 do begin
    TKeyWordFunctionList(FKeyWordLists[i]).Free;
    FKeyWordLists.Delete(i);
  end;
  KeyWordFuncList.Clear;
end;


{ TClassAndProcCodeTool }

constructor TClassAndProcCodeTool.Create;
begin
  inherited Create;
end;

destructor TClassAndProcCodeTool.Destroy;
begin
  inherited Destroy;
end;

procedure TClassAndProcCodeTool.SetDefaultKeyWordFunctions;
begin
  inherited SetDefaultKeyWordFunctions;
  with KeyWordFuncList do begin
    Add('INTERFACE',@KeyWordFuncSection);
    Add('IMPLEMENTATION',@KeyWordFuncSection);
    Add('INITIALIZATION',@KeyWordFuncSection);
    Add('FINALIZATION',@KeyWordFuncSection);

    Add('END',@KeyWordFuncEnd);

    Add('CLASS',@KeyWordFuncClass);
    Add('OBJECT',@KeyWordFuncClass);

    Add('PROCEDURE',@KeyWordFuncMethod);
    Add('FUNCTION',@KeyWordFuncMethod);
    Add('CONSTRUCTOR',@KeyWordFuncMethod);
    Add('DESTRUCTOR',@KeyWordFuncMethod);

    Add('FORWARD',@KeyWordFuncForward);

    Add('BEGIN',@KeyWordFuncBeginEnd);
    Add('ASM',@KeyWordFuncBeginEnd);
    Add('CASE',@KeyWordFuncBeginEnd);
    Add('TRY',@KeyWordFuncBeginEnd);
  end;
end;

procedure TClassAndProcCodeTool.BuildTree;
begin
  BeginParsing;
  // parse interface and implementation section
  // store all class definitions and method heads along with their method bodies
  if not ReadTilSection(ctnInterface) then 
    raise ECodeToolError.Create('interface section not found');
  CreateChildNode;
  Node.Desc:=ctnInterface;
  Node.StartPos:=Pos;
  CurSection:=ctnInterface;
  repeat
    LastPos:=Pos;
    LastAtomStart:=AtomStart;
    LastAtom:=Atom;
    ReadNextPascalAtom;
    if not DoIt(UpAtom) then break;
  until (Atom='');
end;

function TClassAndProcCodeTool.DoIt(const AnAtom: shortstring): boolean;
begin
  if AnAtom='' then exit(false)
  else if IsIDStartChar[AnAtom[1]] then
    Result:=CurKeyWordFuncList.DoIt(AnAtom)
  else begin

    Result:=true;
  end;
end;

function TClassAndProcCodeTool.KeyWordFuncSection: boolean;
begin
  case CurSection of
   ctnInterface:
    begin
      // close interface section node
      Node.EndPos:=Pos;
      EndChildNode;
      if UpAtom='IMPLEMENTATION' then begin
        // start implementation section node
        CreateChildNode;
        Node.Desc:=ctnProcedureHead;
        Node.StartPos:=Pos;
        CurSection:=ctnImplementation;
        Result:=true;
      end else begin
        CurSection:=ctnNone;
        Result:=true;
      end;
    end;
   ctnImplementation:
    begin
      // close implementation section node
      Node.EndPos:=Pos;
      EndChildNode;
      CurSection:=ctnNone;
      Result:=true;
    end;
  else
    Result:=false;
  end;
end;

function TClassAndProcCodeTool.KeyWordFuncEnd: boolean;
begin
  Result:=false;
end;

function TClassAndProcCodeTool.KeyWordFuncClass: boolean;
begin
  if LastAtom<>'=' then exit(true);
  // class start found
  CreateChildNode;
  Node.Desc:=ctnClass;
  Node.StartPos.P:=LastAtomStart;
  Node.StartPos.CodeID:=LastPos.CodeID;
  // find end of class
  ReadNextPascalAtom;
  if Atom='(' then
    // read inheritage brackets
    if not ReadTilBracketClose then
      raise ECodeToolError.Create(
        'syntax error: close bracket not found');
  if Atom=';' then begin
    // forward class definition found
    Node.SubDesc:=ctnsForwardDeclaration;
  end else begin
    while (lowercase(Atom)<>'end') and (Atom<>'') do
      ReadNextPascalAtom;
    if Atom='' then
      raise ECodeToolError.Create(
        'syntax error: "end" for class/object not found');
  end;
  Node.EndPos:=Pos;
  EndChildNode;
  Result:=true;
end;

function TClassAndProcCodeTool.KeyWordFuncMethod: boolean;
begin
  Result:=false;
end;

function TClassAndProcCodeTool.KeyWordFuncForward: boolean;
begin
  Result:=false;
end;

function TClassAndProcCodeTool.KeyWordFuncBeginEnd: boolean;
begin
  Result:=false;
end;


//=============================================================================

procedure CodeToolInit;
var c: char;
begin
  for c:=#0 to #255 do begin
    IsIDChar[c]:=(c in ['a'..'z','A'..'Z','0'..'9','_']);
    IsIDStartChar[c]:=(c in ['a'..'z','A'..'Z','_']);
    case c of
    'a'..'z':CharToHash[c]:=ord(c)-ord('a')+1;
    'A'..'Z':CharToHash[c]:=ord(c)-ord('A')+1;
    else CharToHash[c]:=0;
    end;
  end;
  NodeMemManager:=TCodeTreeNodeMemManager.Create;
end;

procedure CodeToolFinal;
begin
  NodeMemManager.Free;
end;


initialization
  CodeToolInit;

finalization
  CodeToolFinal;

end.

