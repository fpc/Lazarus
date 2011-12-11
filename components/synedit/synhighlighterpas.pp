{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Pascal/Delphi syntax highlighter for SynEdit)
@author(Martin Waldenburg)
@created(1998, converted to SynEdit 2000-04-07)
@lastmod(2000-06-23)
The SynHighlighterPas unit provides SynEdit with a Object Pascal syntax highlighter.
An extra boolean property "D4Syntax" is included to enable the recognition of the
advanced features found in Object Pascal in Delphi 4.
}
unit SynHighlighterPas;

{$I synedit.inc}

interface

uses
  SysUtils,
  {$IFDEF SYN_LAZARUS}
  LCLProc,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Registry, Graphics, SynEditHighlighterFoldBase, SynEditMiscProcs,
  SynEditTypes, SynEditHighlighter, SynEditTextBase, SynEditTextBuffer,
  SynEditStrConst;

type
  TSynPasStringMode = (spsmDefault, spsmStringOnly, spsmNone);

  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkDirective, tkIDEDirective,
    tkUnknown);

  TRangeState = (
    // rsAnsi, rsBor, rsDirective are exclusive to each other
    rsAnsi,         // *) comment
    rsBor,          // { comment
    rsSlash,        // //
    rsIDEDirective, // {%
    rsDirective,    // {$
    rsAsm,          // assembler block
    rsProperty,
    rsAtPropertyOrReadWrite, // very first word after property (name of the property) or after read write in property
    rsInterface,
    rsImplementation,   // Program or Implementation

    // we need to detect:    type TFoo = procedure; // must not fold
    //                       var  foo: procedure;   // must not fold
    rsAfterEqualOrColon,   // very first word after "=" or ":"

    // Detect if class/object is   type TFoo = class; // forward declaration
    //                                  TBar = class of TFoo;
    // or full class declaration  TFoo = class ... end;
    // Also included after class modifiers "sealed" and "abstract"
    rsAtClass,
    rsAfterClass,

    rsAtClosingBracket,   // ')'
    rsAtCaseLabel,
    rsInProcHeader,       // Declaration or implementation header of a Procedure, function, constructor...
    rsAfterClassMembers,  // Encountered a procedure, function, property, constructor or destructor in a class
    rsAfterClassField,    // after ";" of a field (static needs highlight)
    rsVarTypeInSpecification // between ":"/"=" and ";" in a var or type section (or class members)
                             // var a: Integer; type b = Int64;
  );
  TRangeStates = set of TRangeState;

type
  {$IFDEF SYN_LAZARUS}
  TPascalCodeFoldBlockType = ( // Do *not* change the order
    cfbtBeginEnd,      // Nested
    cfbtTopBeginEnd,   // Begin of Procedure
    cfbtNestedComment,
    cfbtProcedure,
    cfbtUses,
    cfbtVarType,
    cfbtLocalVarType,
    cfbtClass,
    cfbtClassSection,
    cfbtUnitSection,
    cfbtProgram,
    cfbtUnit,
    cfbtRecord,
    cfbtTry,
    cfbtExcept,
    cfbtRepeat,
    cfbtAsm,
    cfbtCase,
    cfbtIfDef,        // {$IfDef} directive, this is not counted in the Range-Node
    cfbtRegion,       // {%Region} user folds, not counted in the Range-Node
    cfbtAnsiComment,  // (* ... *)
    cfbtBorCommand,   // { ... }
    cfbtSlashComment, // //
    // Internal type / not configurable
    cfbtNone
    );
  TPascalCodeFoldBlockTypes = set of TPascalCodeFoldBlockType;

const
  CountPascalCodeFoldBlockOffset: Pointer =
    Pointer(PtrInt(Integer(high(TPascalCodeFoldBlockType))+1));

  cfbtAll: TPascalCodeFoldBlockTypes =
    [low(TPascalCodeFoldBlockType)..high(TPascalCodeFoldBlockType)];
  PascalWordTripletRanges: TPascalCodeFoldBlockTypes =
    [cfbtBeginEnd, cfbtTopBeginEnd, cfbtProcedure, cfbtClass, cfbtProgram, cfbtRecord,
     cfbtTry, cfbtExcept, cfbtRepeat, cfbtAsm, cfbtCase
    ];

  // restrict cdecl etc to places where they can be.
  // this needs a better parser
  ProcModifierAllowed: TPascalCodeFoldBlockTypes =
    [cfbtNone, cfbtProcedure, cfbtProgram, cfbtClass, cfbtClassSection, cfbtRecord,
     cfbtUnitSection, // unitsection, actually interface only
     cfbtVarType, cfbtLocalVarType];

  PascalFoldTypeCompatibility: Array [TPascalCodeFoldBlockType] of TPascalCodeFoldBlockType =
    ( cfbtBeginEnd,      // Nested
      cfbtBeginEnd,  // cfbtTopBeginEnd,   // Begin of Procedure
      cfbtNestedComment,
      cfbtProcedure,
      cfbtUses,
      cfbtVarType,
      cfbtVarType, // cfbtLocalVarType,
      cfbtClass,
      cfbtClassSection,
      cfbtUnitSection,
      cfbtProgram,
      cfbtUnit,
      cfbtRecord,
      cfbtTry,
      cfbtExcept,
      cfbtRepeat,
      cfbtAsm,
      cfbtCase,
      cfbtIfDef,        // {$IfDef} directive, this is not counted in the Range-Node
      cfbtRegion,       // {%Region} user folds, not counted in the Range-Node
      cfbtNestedComment, //cfbtAnsiComment,  // (* ... *)
      cfbtNestedComment, //cfbtBorCommand,   // { ... }
      cfbtSlashComment, // //
      // Internal type / not configurable
      cfbtNone
    );

type

  TPascalCompilerMode = (
    pcmObjFPC,
    pcmDelphi,
    pcmFPC,
    pcmTP,
    pcmGPC,
    pcmMacPas
    );

  TSynPasDividerDrawLocation = (
      pddlUnitSection,
      pddlUses,
      pddlVarGlobal,     // Var, Type, Const in global scope
      pddlVarLocal,      // Var, Type, Const in local (procedure) scope
      pddlStructGlobal,  // Class, Object, Record in global type block
      pddlStructLocal,   // Class, Object, Record in local (procedure) type block
      pddlProcedure,
      pddlBeginEnd,      // Includes Repeat
      pddlTry
    );

const

  PasDividerDrawLocationDefaults: Array [TSynPasDividerDrawLocation] of
    Integer = (1, 0, // unit, uses
               1, 0, // var
               1, 0, // struct
               2, 0, // proc, begin
               0);

type

 TSynPasRangeInfo = record
    EndLevelIfDef: Smallint;
    MinLevelIfDef: Smallint;
    EndLevelRegion: Smallint;
    MinLevelRegion: Smallint;
  end;

  { TSynHighlighterPasRangeList }

  TSynHighlighterPasRangeList = class(TSynHighlighterRangeList)
  private
    FItemOffset: integer;
    function GetTSynPasRangeInfo(Index: Integer): TSynPasRangeInfo;
    procedure SetTSynPasRangeInfo(Index: Integer; const AValue: TSynPasRangeInfo);
  public
    constructor Create;
    property PasRangeInfo[Index: Integer]: TSynPasRangeInfo
      read GetTSynPasRangeInfo write SetTSynPasRangeInfo;
  end;

  { TSynPasSynRange }

  TSynPasSynRange = class(TSynCustomHighlighterRange)
  private
    FMode: TPascalCompilerMode;
    FBracketNestLevel : Integer;
    FLastLineCodeFoldLevelFix: integer;
    FPasFoldEndLevel: Smallint;
    FPasFoldFixLevel: Smallint;
    FPasFoldMinLevel: Smallint;
  public
    procedure Clear; override;
    function Compare(Range: TSynCustomHighlighterRange): integer; override;
    procedure Assign(Src: TSynCustomHighlighterRange); override;
    function Add(ABlockType: Pointer = nil; IncreaseLevel: Boolean = True):
        TSynCustomCodeFoldBlock; override;
    procedure Pop(DecreaseLevel: Boolean = True); override;
    function MaxFoldLevel: Integer; override;
    procedure IncBracketNestLevel;
    procedure DecBracketNestLevel;
    procedure DecLastLineCodeFoldLevelFix;
    procedure DecLastLinePasFoldFix;
    property Mode: TPascalCompilerMode read FMode write FMode;
    property BracketNestLevel: integer read FBracketNestLevel write FBracketNestLevel;
    property LastLineCodeFoldLevelFix: integer
      read FLastLineCodeFoldLevelFix write FLastLineCodeFoldLevelFix;
    property PasFoldEndLevel: Smallint read FPasFoldEndLevel write FPasFoldEndLevel;
    property PasFoldFixLevel: Smallint read FPasFoldFixLevel write FPasFoldFixLevel;
    property PasFoldMinLevel: Smallint read FPasFoldMinLevel write FPasFoldMinLevel;
  end;
  {$ENDIF}

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  { TSynPasSyn }

  TSynPasSyn = class(TSynCustomFoldHighlighter)
  private
    fAsmStart: Boolean;
    FExtendedKeywordsMode: Boolean;
    FNestedComments: boolean;
    FStartCodeFoldBlockLevel: integer;
    FPasStartLevel: Smallint;
    fRange: TRangeStates;
    FStringKeywordMode: TSynPasStringMode;
    FSynPasRangeInfo: TSynPasRangeInfo;
    FAtLineStart: Boolean; // Line had only spaces or comments sofar
    {$IFDEF SYN_LAZARUS}
    fLineStr: string;
    fLine: PChar;
    fLineLen: integer;
    {$ELSE}
    fLine: PChar;
    {$ENDIF}
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;// current parser postion in fLine
    fStringLen: Integer;// current length of hash
    {$IFDEF SYN_LAZARUS}
    fToIdent: integer;// start of current identifier in fLine
    {$ELSE}
    fToIdent: PChar;
    {$ENDIF}
    fIdentFuncTable: array[0..191] of TIdentFuncTableFunc;
    fTokenPos: Integer;// start of current token in fLine
    FTokenID: TtkTokenKind;
    FTokenIsCaseLabel: Boolean;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    FIDEDirectiveAttri: TSynHighlighterAttributes;
    FCurIDEDirectiveAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    FCaseLabelAttri: TSynHighlighterAttributes;
    FCurCaseLabelAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    FCompilerMode: TPascalCompilerMode;
    fD4syntax: boolean;
    {$IFDEF SYN_LAZARUS}
    FCatchNodeInfo: Boolean;
    FNodeInfoLine, FNodeInfoCount: Integer;
    FNodeInfoList: Array of TSynFoldNodeInfo;
    FDividerDrawConfig: Array [TSynPasDividerDrawLocation] of TSynDividerDrawConfig;
    procedure GrowNodeInfoList;
    function GetPasCodeFoldRange: TSynPasSynRange;
    procedure SetCompilerMode(const AValue: TPascalCompilerMode);
    procedure SetExtendedKeywordsMode(const AValue: Boolean);
    procedure SetStringKeywordMode(const AValue: TSynPasStringMode);
    function TextComp(aText: PChar): Boolean;
    function KeyHash: Integer;
    {$ELSE}
    function KeyHash(ToHash: PChar): Integer;
    {$ENDIF}
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;  // "on"
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind; // "alias", "final"
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind; // "sealed"
    function Func47: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func108: TtkTokenKind;  // "operator"
    function Func112: TtkTokenKind;  // "requires"
    function Func117: TtkTokenKind;
    function Func122: TtkTokenKind; // "otherwise"
    function Func124: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func142: TtkTokenKind; // "experimental"
    function Func143: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func151: TtkTokenKind; // "unimplemented"
    function Func158: TtkTokenKind; // "unicodestring"
    function Func166: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func181: TtkTokenKind;
    function Func191: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    {$IFDEF SYN_LAZARUS}
    function IdentKind(p: integer): TtkTokenKind;
    {$ELSE}
    function IdentKind(MayBe: PChar): TtkTokenKind;
    {$ENDIF}
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure GreaterProc;
    procedure CRProc;
    {$IFDEF SYN_LAZARUS}
    procedure DirectiveProc;
    {$ENDIF}
    procedure IdentProc;
    procedure HexProc;
    procedure BinaryProc;
    procedure OctalProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure RoundCloseProc;
    procedure SquareOpenProc;
    procedure SquareCloseProc;
    procedure EqualSignProc;
    procedure SemicolonProc;                                                    //mh 2000-10-08
    procedure SlashProc;
    procedure SlashContinueProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure SetD4syntax(const Value: boolean);
    procedure InitNode(var Node: TSynFoldNodeInfo; EndOffs: Integer;
                       ABlockType: TPascalCodeFoldBlockType; aActions: TSynFoldActions);
    procedure CreateDividerDrawConfig;
    procedure DestroyDividerDrawConfig;
  protected
    function KeyComp(const aKey: string): Boolean;
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: boolean; override;                                 //mh 2000-10-08
  protected
    function GetRangeClass: TSynCustomHighlighterRangeClass; override;
    procedure CreateRootCodeFoldBlock; override;
    function CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList; override;
    function UpdateRangeInfoAtLine(Index: Integer): Boolean; override; // Returns true if range changed

    function StartPascalCodeFoldBlock
             (ABlockType: TPascalCodeFoldBlockType): TSynCustomCodeFoldBlock;
    procedure EndPascalCodeFoldBlock(NoMarkup: Boolean = False);
    procedure CloseBeginEndBlocksBeforeProc;
    procedure SmartCloseBeginEndBlocks(SearchFor: TPascalCodeFoldBlockType);
    procedure EndPascalCodeFoldBlockLastLine;
    procedure StartCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
    procedure EndCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);

    function GetFoldNodeInfo(Line, Index: Integer; Filter: TSynFoldActions): TSynFoldNodeInfo; override;
    function GetFoldNodeInfoCount(Line: Integer; Filter: TSynFoldActions): Integer; override; // Line: 0-based

    property PasCodeFoldRange: TSynPasSynRange read GetPasCodeFoldRange;
    function TopPascalCodeFoldBlockType
             (DownIndex: Integer = 0): TPascalCodeFoldBlockType;

  public
    function MinimumPasFoldLevel(Index: Integer; AType: Integer = 1): integer;
    function EndPasFoldLevel(Index: Integer; AType: Integer = 1): integer;
  protected
    function LastLinePasFoldLevelFix(Index: Integer; AType: Integer = 1): integer;

    function LastLineFoldLevelFix(Index: Integer): integer;
    function GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting; override;
    function GetDividerDrawConfig(Index: Integer): TSynDividerDrawConfig; override;
    function GetDividerDrawConfigCount: Integer; override;

    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; override;
    procedure DoFoldConfigChanged(Sender: TObject); override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetCapabilities: TSynHighlighterCapabilities; override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;

    procedure ResetRange; override;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
      LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
    procedure StartAtLineIndex(LineNumber:Integer); override; // 0 based

    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;

    // fold-nodes that can be collapsed
    function FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldTypeCount: integer; override;
    function FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
             UseCloseNodes: boolean = false): integer; override;
    function FoldLineLength(ALineIndex, FoldIndex: Integer): integer; override;
    function FoldEndLine(ALineIndex, FoldIndex: Integer): integer; override;

    // Pascal code only // TODO: make private
    function MinimumFoldLevel(Index: Integer): integer; override;
    function EndFoldLevel(Index: Integer): integer; override;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IDEDirectiveAttri: TSynHighlighterAttributes read FIDEDirectiveAttri
      write FIDEDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property CaseLabelAttri: TSynHighlighterAttributes read FCaseLabelAttri
      write FCaseLabelAttri;
    {$IFDEF SYN_LAZARUS}
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
    property CompilerMode: TPascalCompilerMode read FCompilerMode write SetCompilerMode;
    property NestedComments: boolean read FNestedComments write FNestedComments;
    {$ENDIF}
    property D4syntax: boolean read FD4syntax write SetD4syntax default true;
    property ExtendedKeywordsMode: Boolean
             read FExtendedKeywordsMode write SetExtendedKeywordsMode default False;
    property StringKeywordMode: TSynPasStringMode
             read FStringKeywordMode write SetStringKeywordMode default spsmDefault;
  end;

  { TSynFreePascalSyn }

  TSynFreePascalSyn = class(TSynPasSyn)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  end;


implementation

const
  RESERVED_WORDS_TP: array [1..54] of String = (
    'absolute', 'and', 'array', 'asm',
    'begin',
    'case', 'const', 'constructor',
    'destructor', 'div', 'do', 'downto',
    'else', 'end',
    'file', 'for', 'function',
    'goto',
    'if', 'implementation', 'in', 'inherited', 'inline', 'interface',
    'label',
    'mod',
    'nil', 'not',
    'object', 'of', 'on', 'operator', 'or',
    'packed', 'procedure', 'program',
    'record', 'reintroduce', 'repeat',
    'self', 'set', 'shl', 'shr', 'string',
    'then', 'to', 'type',
    'unit', 'until', 'uses',
    'var',
    'while', 'with',
    'xor'
  );

  RESERVED_WORDS_DELPHI: array [1..15] of String = (
    'as',
    'class',
    'except', 'exports',
    'finalization', 'finally',
    'initialization', 'is',
    'library',
    'on', 'out',
    'property',
    'raise',
    'threadvar',
    'try'
  );

  RESERVED_WORDS_FPC: array [1..5] of String = (
    'dispose', 'exit', 'false', 'new', 'true'
  );

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;
  KeywordsList: TStringList;
  IsIntegerChar: array[char] of Boolean;
  IsNumberChar: array[char] of Boolean;
  IsSpaceChar: array[char] of Boolean;
  IsUnderScoreOrNumberChar: array[char] of Boolean;
  IsLetterChar: array[char] of Boolean;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I of
      'a'..'z', 'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
      '0'..'9': mHashTable[I] := Ord(J) - 48;
    else
      mHashTable[Char(I)] := 0;
    end;
    IsIntegerChar[I]:=(I in ['0'..'9', 'A'..'F', 'a'..'f']);
    IsNumberChar[I]:=(I in ['0'..'9', '.', 'e', 'E']);
    IsSpaceChar[I]:=(I in [#1..#9, #11, #12, #14..#32]);
    IsUnderScoreOrNumberChar[I]:=(I in ['_','0'..'9']);
    IsLetterChar[I]:=(I in ['a'..'z','A'..'Z']);
  end;
end;

procedure TSynPasSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    {$IFDEF FPC}
    pF^ := @AltFunc;
    {$ELSE}
    pF^ := AltFunc;
    {$ENDIF}
    Inc(pF);
  end;
  {$IFDEF FPC}
  fIdentFuncTable[15] := @Func15;
  fIdentFuncTable[19] := @Func19;
  fIdentFuncTable[20] := @Func20;
  fIdentFuncTable[21] := @Func21;
  fIdentFuncTable[23] := @Func23;
  fIdentFuncTable[25] := @Func25;
  fIdentFuncTable[27] := @Func27;
  fIdentFuncTable[28] := @Func28;
  fIdentFuncTable[29] := @Func29; // "on"
  fIdentFuncTable[32] := @Func32;
  fIdentFuncTable[33] := @Func33;
  fIdentFuncTable[35] := @Func35;
  fIdentFuncTable[37] := @Func37;
  fIdentFuncTable[38] := @Func38;
  fIdentFuncTable[39] := @Func39;
  fIdentFuncTable[40] := @Func40;
  fIdentFuncTable[41] := @Func41;
  fIdentFuncTable[42] := @Func42;
  fIdentFuncTable[44] := @Func44;
  fIdentFuncTable[45] := @Func45;
  fIdentFuncTable[46] := @Func46;
  fIdentFuncTable[47] := @Func47;
  fIdentFuncTable[49] := @Func49;
  fIdentFuncTable[52] := @Func52;
  fIdentFuncTable[54] := @Func54;
  fIdentFuncTable[55] := @Func55;
  fIdentFuncTable[56] := @Func56;
  fIdentFuncTable[57] := @Func57;
  fIdentFuncTable[58] := @Func58;
  fIdentFuncTable[59] := @Func59;
  fIdentFuncTable[60] := @Func60;
  fIdentFuncTable[61] := @Func61;
  fIdentFuncTable[63] := @Func63;
  fIdentFuncTable[64] := @Func64;
  fIdentFuncTable[65] := @Func65;
  fIdentFuncTable[66] := @Func66;
  fIdentFuncTable[69] := @Func69;
  fIdentFuncTable[71] := @Func71;
  fIdentFuncTable[72] := @Func72;
  fIdentFuncTable[73] := @Func73;
  fIdentFuncTable[75] := @Func75;
  fIdentFuncTable[76] := @Func76;
  fIdentFuncTable[79] := @Func79;
  fIdentFuncTable[81] := @Func81;
  fIdentFuncTable[84] := @Func84;
  fIdentFuncTable[85] := @Func85;
  fIdentFuncTable[86] := @Func86;
  fIdentFuncTable[87] := @Func87;
  fIdentFuncTable[88] := @Func88;
  fIdentFuncTable[89] := @Func89;
  fIdentFuncTable[91] := @Func91;
  fIdentFuncTable[92] := @Func92;
  fIdentFuncTable[94] := @Func94;
  fIdentFuncTable[95] := @Func95;
  fIdentFuncTable[96] := @Func96;
  fIdentFuncTable[97] := @Func97;
  fIdentFuncTable[98] := @Func98;
  fIdentFuncTable[99] := @Func99;
  fIdentFuncTable[100] := @Func100;
  fIdentFuncTable[101] := @Func101;
  fIdentFuncTable[102] := @Func102;
  fIdentFuncTable[103] := @Func103;
  fIdentFuncTable[105] := @Func105;
  fIdentFuncTable[106] := @Func106;
  fIdentFuncTable[108] := @Func108; // "operator"
  fIdentFuncTable[112] := @Func112; // "requires"
  fIdentFuncTable[117] := @Func117;
  fIdentFuncTable[122] := @Func122;
  fIdentFuncTable[124] := @Func124;
  fIdentFuncTable[126] := @Func126;
  fIdentFuncTable[128] := @Func128;
  fIdentFuncTable[129] := @Func129;
  fIdentFuncTable[130] := @Func130;
  fIdentFuncTable[132] := @Func132;
  fIdentFuncTable[133] := @Func133;
  fIdentFuncTable[136] := @Func136;
  fIdentFuncTable[139] := @Func139;
  fIdentFuncTable[141] := @Func141;
  fIdentFuncTable[142] := @Func142;
  fIdentFuncTable[143] := @Func143;
  fIdentFuncTable[144] := @Func144;
  fIdentFuncTable[151] := @Func151;
  fIdentFuncTable[158] := @Func158;
  fIdentFuncTable[166] := @Func166;
  fIdentFuncTable[167] := @Func167;
  fIdentFuncTable[168] := @Func168;
  fIdentFuncTable[181] := @Func181;
  fIdentFuncTable[191] := @Func191;
  {$ELSE}
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[132] := Func132;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[158] := Func158;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[168] := Func168;
  fIdentFuncTable[181] := Func181;
  fIdentFuncTable[191] := Func191;
  {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
function TSynPasSyn.KeyHash: Integer;
var
  Start, ToHash: PChar;
begin
  Result := 0;
  if (fToIdent<fLineLen) then begin
    Start := fLine + fToIdent;
    ToHash := Start;
    if IsLetterChar[ToHash^] then
    begin
      inc(Result, mHashTable[ToHash^]);
      inc(ToHash);
      while (IsLetterChar[ToHash^] or IsUnderScoreOrNumberChar[ToHash^]) do
      begin
        inc(Result, mHashTable[ToHash^]);
        inc(ToHash);
      end;
    end;
    if IsUnderScoreOrNumberChar[ToHash^] then
      inc(ToHash);
    fStringLen := PtrUInt(ToHash) - PtrUInt(Start);
    //if CompareText(copy(fLineStr,fToIdent+1,fStringLen),'varargs')=0 then debugln('TSynPasSyn.KeyHash '+copy(fLineStr,fToIdent+1,fStringLen)+'='+dbgs(Result));
  end else begin
    fStringLen := 0;
  end;
end; { KeyHash }
{$ELSE}
function TSynPasSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then inc(ToHash);
  fStringLen := ToHash - fToIdent;
end; { KeyHash }
{$ENDIF}

function TSynPasSyn.KeyComp(const aKey: string): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  if Length(aKey) = fStringLen then
  begin
    {$IFDEF SYN_LAZARUS}
    Temp := fLine + fToIdent;
    {$ELSE}
    Temp := fToIdent;
    {$ENDIF}
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

{$IFDEF SYN_LAZARUS}
function TSynPasSyn.TextComp(aText: PChar): Boolean;
var
  CurPos: PChar;
begin
  CurPos:=@fLine[Run];
  while (aText^<>#0) do begin
    if mHashTable[aText^]<>mHashTable[CurPos^] then exit(false);
    inc(aText);
    inc(CurPos);
  end;
  Result:=true;
end;

procedure TSynPasSyn.SetCompilerMode(const AValue: TPascalCompilerMode);
begin
  if FCompilerMode=AValue then exit;
  FCompilerMode:=AValue;
  FNestedComments:=FCompilerMode in [pcmFPC,pcmObjFPC];
  PasCodeFoldRange.Mode:=FCompilerMode;
  //DebugLn(['TSynPasSyn.SetCompilerMode FCompilerMode=',ord(FCompilerMode),' FNestedComments=',FNestedComments]);
end;

procedure TSynPasSyn.SetExtendedKeywordsMode(const AValue: Boolean);
begin
  if FExtendedKeywordsMode = AValue then exit;
  FExtendedKeywordsMode := AValue;
  FAttributeChangeNeedScan := True;
  DefHighlightChange(self);
end;

procedure TSynPasSyn.SetStringKeywordMode(const AValue: TSynPasStringMode);
begin
  if FStringKeywordMode = AValue then exit;
  FStringKeywordMode := AValue;
  FAttributeChangeNeedScan := True;
  DefHighlightChange(self);
end;

procedure TSynPasSyn.GrowNodeInfoList;
begin
  if FNodeInfoCount < length(FNodeInfoList) then
    exit;
  SetLength(FNodeInfoList, FNodeInfoCount + Max(10, FNodeInfoCount div 4));
end;

function TSynPasSyn.GetPasCodeFoldRange: TSynPasSynRange;
begin
  Result := TSynPasSynRange(CodeFoldRange);
end;

{$ENDIF}

function TSynPasSyn.Func15: TtkTokenKind;
begin
  if KeyComp('If') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func19: TtkTokenKind;
begin
  if KeyComp('Do') then Result := tkKey else
    if KeyComp('And') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func20: TtkTokenKind;
begin
  if KeyComp('As') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func21: TtkTokenKind;
begin
  if KeyComp('Of') then begin
    Result := tkKey;
    if (rsAfterClass in fRange) and (TopPascalCodeFoldBlockType = cfbtClass)
    then begin
      // Accidental start of block // End at next semicolon (usually same line)
      CodeFoldRange.Pop(false); // avoid minlevel
      CodeFoldRange.Add(Pointer(PtrInt(cfbtUses)), false);
    end
    else
    if (TopPascalCodeFoldBlockType = cfbtCase) then
      fRange := fRange + [rsAtCaseLabel];
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func23: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('End') then begin
    if ((fToIdent<2) or (fLine[fToIdent-1]<>'@'))
    then begin
      Result := tkKey;
      fRange := fRange - [rsAsm, rsAfterClassMembers];
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      {$IFDEF SYN_LAZARUS}
      // there may be more than on block ending here
      tfb := TopPascalCodeFoldBlockType;
      if tfb = cfbtRecord then begin
        EndPascalCodeFoldBlock;
      end else if tfb = cfbtUnit then begin
        EndPascalCodeFoldBlock;
      end else if tfb = cfbtExcept then begin
        EndPascalCodeFoldBlock;
        if TopPascalCodeFoldBlockType = cfbtTry then
          EndPascalCodeFoldBlock;
      end else if tfb = cfbtTry then begin
          EndPascalCodeFoldBlock;
      end else if tfb in [cfbtTopBeginEnd, cfbtAsm] then begin
        EndPascalCodeFoldBlock;
        if TopPascalCodeFoldBlockType in [cfbtProcedure] then
          EndPascalCodeFoldBlock;
      end else if tfb in [cfbtCase] then begin
        EndPascalCodeFoldBlock;
        fRange := fRange - [rsAtCaseLabel];
      end else if tfb in [cfbtBeginEnd] then begin
        EndPascalCodeFoldBlock;
        if TopPascalCodeFoldBlockType = cfbtProgram then
          EndPascalCodeFoldBlock;
      end else if tfb = cfbtUnitSection then begin
        EndPascalCodeFoldBlockLastLine;
        if TopPascalCodeFoldBlockType = cfbtUnit then // "Unit".."end."
          EndPascalCodeFoldBlock;
      end else begin
        if tfb = cfbtClassSection then
          EndPascalCodeFoldBlockLastLine;
        // after class-section either a class OR a record can close with the same "end"
        if TopPascalCodeFoldBlockType = cfbtClass then
          EndPascalCodeFoldBlock
        else
        if TopPascalCodeFoldBlockType = cfbtRecord then
          EndPascalCodeFoldBlock;
      // After type declaration, allow "deprecated"?
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        fRange := fRange + [rsVarTypeInSpecification];
      end;
      {$ENDIF}
    end else begin
      Result := tkKey; // @@end or @end label
    end;
  end
  else
    if KeyComp('In') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func25: TtkTokenKind;
begin
  if KeyComp('Far') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func27: TtkTokenKind;
begin
  if KeyComp('Cdecl') and (TopPascalCodeFoldBlockType in ProcModifierAllowed) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func28: TtkTokenKind;
begin
  if KeyComp('Is') then
    Result := tkKey
  else
  if (fRange * [rsProperty, rsAtPropertyOrReadWrite, rsAfterEqualOrColon] =  [rsProperty]) and
      (PasCodeFoldRange.BracketNestLevel = 0) and
     KeyComp('Read')
  then begin
    Result := tkKey;
    fRange := fRange + [rsAtPropertyOrReadWrite];
  end
  else if KeyComp('Case') then begin
    if TopPascalCodeFoldBlockType in
       [cfbtBeginEnd, cfbtTopBeginEnd, cfbtCase, cfbtTry, cfbtExcept, cfbtRepeat] then
      StartPascalCodeFoldBlock(cfbtCase);
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

{$IFDEF SYN_LAZARUS}
function TSynPasSyn.Func29: TtkTokenKind;
begin
  if KeyComp('On') then Result := tkKey else Result := tkIdentifier;
end;
{$ENDIF}

function TSynPasSyn.Func32: TtkTokenKind;
begin
  if KeyComp('Label') then Result := tkKey else
    if KeyComp('Mod') then Result := tkKey else
      if KeyComp('File') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func33: TtkTokenKind;
begin
  if KeyComp('Or') then Result := tkKey
  else
  if KeyComp('Asm') then
  begin
    Result := tkKey;
    fRange := fRange + [rsAsm];
    fAsmStart := True;
    if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtAsm);
    //debugln('TSynPasSyn.Func37 BEGIN ',dbgs(ord(TopPascalCodeFoldBlockType)),' LineNumber=',dbgs(fLineNumber),' ',dbgs(MinimumCodeFoldBlockLevel),' ',dbgs(CurrentCodeFoldBlockLevel));
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func35: TtkTokenKind;
begin
  if KeyComp('Nil') then Result := tkKey else
    if KeyComp('To') then Result := tkKey else
      if KeyComp('Div') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func37: TtkTokenKind;
begin
  if KeyComp('Begin') then begin
    // if we are in an include file, we may not know the state
    if (fRange * [rsImplementation, rsInterface] = []) then
      Include(fRange, rsImplementation);
    PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
    if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
      EndPascalCodeFoldBlockLastLine;
    Result := tkKey;
    if TopPascalCodeFoldBlockType in [cfbtProcedure]
    then StartPascalCodeFoldBlock(cfbtTopBeginEnd)
    else StartPascalCodeFoldBlock(cfbtBeginEnd);
    //debugln('TSynPasSyn.Func37 BEGIN ',dbgs(ord(TopPascalCodeFoldBlockType)),' LineNumber=',dbgs(fLineNumber),' ',dbgs(MinimumCodeFoldBlockLevel),' ',dbgs(CurrentCodeFoldBlockLevel));
  end else
  if FExtendedKeywordsMode and KeyComp('Break') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func38: TtkTokenKind;
begin
  if KeyComp('Near') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func39: TtkTokenKind;
begin
  if KeyComp('For') then Result := tkKey else
    if KeyComp('Shl') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func40: TtkTokenKind;
begin
  if KeyComp('Packed') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func41: TtkTokenKind;
begin
  if KeyComp('Else') then
    Result := tkKey
  else if KeyComp('Var') then begin
    if (PasCodeFoldRange.BracketNestLevel = 0) and
        (TopPascalCodeFoldBlockType in
        [cfbtVarType, cfbtLocalVarType, cfbtNone, cfbtProcedure, cfbtProgram,
         cfbtUnit, cfbtUnitSection]) then begin
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;
      if TopPascalCodeFoldBlockType in [cfbtProcedure]
      then StartPascalCodeFoldBlock(cfbtLocalVarType)
      else StartPascalCodeFoldBlock(cfbtVarType);
    end;
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func42: TtkTokenKind;
begin
  if KeyComp('Alias') then
    Result := tkKey
  else
  if KeyComp('Final') and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     (fRange * [rsAfterClassMembers, rsInProcHeader, rsProperty] = [rsAfterClassMembers]) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func44: TtkTokenKind;
begin
  if KeyComp('Set') then
    Result := tkKey
  else if KeyComp('Package') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func45: TtkTokenKind;
begin
  if KeyComp('Shr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func46: TtkTokenKind;
begin
  if (rsAfterClass in fRange) and KeyComp('Sealed') and
     (TopPascalCodeFoldBlockType in [cfbtClass])
  then begin
    Result := tkKey;
    fRange := fRange + [rsAtClass]; // forward, in case of further class modifiers
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func47: TtkTokenKind;
begin
  if KeyComp('Then') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func49: TtkTokenKind;
begin
  if KeyComp('Not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func52: TtkTokenKind;
begin
  if KeyComp('Pascal') and (TopPascalCodeFoldBlockType in ProcModifierAllowed) then
    Result := tkKey
  else
  if KeyComp('Raise') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func54: TtkTokenKind;
begin
  if KeyComp('Class') then begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      fRange := fRange + [rsAtClass] - [rsVarTypeInSpecification];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func55: TtkTokenKind;
begin
  if KeyComp('Object') then begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func56: TtkTokenKind;
begin
  if KeyComp('Index') then
  begin
    if (fRange * [rsProperty, rsAtPropertyOrReadWrite, rsAfterEqualOrColon] =  [rsProperty]) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then
      Result := tkKey else Result := tkIdentifier;
  end
  else
    if KeyComp('Out') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func57: TtkTokenKind;
begin
  if KeyComp('Goto') then Result := tkKey else
    if KeyComp('While') then Result := tkKey else
      if KeyComp('Xor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func58: TtkTokenKind;
begin
  if FExtendedKeywordsMode and KeyComp('Exit') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func59: TtkTokenKind;
begin
  if KeyComp('Safecall') and (TopPascalCodeFoldBlockType in ProcModifierAllowed) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func60: TtkTokenKind;
begin
  if KeyComp('With') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func61: TtkTokenKind;
begin
  if KeyComp('Dispid') {$IFDEF SYN_LAZARUS}or KeyComp('Generic'){$ENDIF}then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func63: TtkTokenKind;
begin
  if KeyComp('Public') then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification];
    if (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) then begin
      if (TopPascalCodeFoldBlockType=cfbtClassSection) then
        EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtClassSection);
    end;
  end
  else if KeyComp('Record') then begin
    StartPascalCodeFoldBlock(cfbtRecord);
    Result := tkKey;
  end
  else if KeyComp('Array') then Result := tkKey
  else if KeyComp('Try') then
  begin
    if TopPascalCodeFoldBlockType in
       [cfbtBeginEnd, cfbtTopBeginEnd, cfbtCase, cfbtTry, cfbtExcept, cfbtRepeat] then
      StartPascalCodeFoldBlock(cfbtTry);
    Result := tkKey;
  end
  else if KeyComp('Inline') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func64: TtkTokenKind;
begin
  if KeyComp('Unit') then begin
    if TopPascalCodeFoldBlockType=cfbtNone then StartPascalCodeFoldBlock(cfbtUnit);
    Result := tkKey;
  end
  else if KeyComp('Uses') then begin
    if (TopPascalCodeFoldBlockType in
        [cfbtNone, cfbtProgram, cfbtUnit, cfbtUnitSection]) then begin
      StartPascalCodeFoldBlock(cfbtUses);
    end;
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func65: TtkTokenKind;
begin
  if KeyComp('Repeat') then begin
    Result := tkKey;
    SmartCloseBeginEndBlocks(cfbtRepeat);
    StartPascalCodeFoldBlock(cfbtRepeat);
   end
   else Result := tkIdentifier;
end;

function TSynPasSyn.Func66: TtkTokenKind;
begin
  if KeyComp('Type') then begin
    if (PasCodeFoldRange.BracketNestLevel = 0)
       and (TopPascalCodeFoldBlockType in
        [cfbtVarType, cfbtLocalVarType, cfbtNone, cfbtProcedure, cfbtProgram,
         cfbtUnit, cfbtUnitSection]) and not(rsAfterEqualOrColon in fRange)
    then begin
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;
      if TopPascalCodeFoldBlockType in [cfbtProcedure]
      then StartPascalCodeFoldBlock(cfbtLocalVarType)
      else StartPascalCodeFoldBlock(cfbtVarType);
    end;
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func69: TtkTokenKind;
begin
  if KeyComp('Default') then begin
    if (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) then
      Result := tkKey
    else
      Result := tkIdentifier;
  end else
  if KeyComp('Dynamic') then
    Result := tkKey
  else
  if KeyComp('Message') and
     (fRange * [rsAfterClassMembers, rsInProcHeader, rsProperty] = [rsAfterClassMembers]) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func71: TtkTokenKind;
begin
  if KeyComp('Stdcall') and (TopPascalCodeFoldBlockType in ProcModifierAllowed) then
    Result := tkKey
  else if KeyComp('Const') then begin
    if (PasCodeFoldRange.BracketNestLevel = 0) and
        (TopPascalCodeFoldBlockType in
        [cfbtVarType, cfbtLocalVarType, cfbtNone, cfbtProcedure, cfbtProgram,
         cfbtUnit, cfbtUnitSection]) then begin
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;
      if TopPascalCodeFoldBlockType in [cfbtProcedure]
      then StartPascalCodeFoldBlock(cfbtLocalVarType)
      else StartPascalCodeFoldBlock(cfbtVarType);
    end;
    Result := tkKey;
  end
  {$IFDEF SYN_LAZARUS}
  else if KeyComp('Bitpacked') then
    Result := tkKey
  {$ENDIF}
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func72: TtkTokenKind;
begin
  if KeyComp('Static') and (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     (fRange * [rsAfterEqualOrColon, rsInProcHeader, rsProperty] = []) and
     (fRange * [rsAfterClassMembers, rsAfterClassField] <> []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func73: TtkTokenKind;
begin
  if KeyComp('Except') then begin
    Result := tkKey;
    SmartCloseBeginEndBlocks(cfbtTry);
    if TopPascalCodeFoldBlockType = cfbtTry then
      StartPascalCodeFoldBlock(cfbtExcept);
   end
   else Result := tkIdentifier;
end;

function TSynPasSyn.Func75: TtkTokenKind;
begin
  if (fRange * [rsProperty, rsAtPropertyOrReadWrite, rsAfterEqualOrColon] =  [rsProperty]) and
      (PasCodeFoldRange.BracketNestLevel = 0) and
      KeyComp('Write') then
  begin
    Result := tkKey;
    fRange := fRange + [rsAtPropertyOrReadWrite];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func76: TtkTokenKind;
begin
  if KeyComp('Until') then begin
    Result := tkKey;
    if TopPascalCodeFoldBlockType = cfbtRepeat then EndPascalCodeFoldBlock;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func79: TtkTokenKind;
begin
  if KeyComp('Finally') then begin
    Result := tkKey;
    SmartCloseBeginEndBlocks(cfbtTry);
    if TopPascalCodeFoldBlockType = cfbtTry then
      StartPascalCodeFoldBlock(cfbtExcept);
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func81: TtkTokenKind;
var
  tbf: TPascalCodeFoldBlockType;
begin
  if KeyComp('Stored') then
  begin
    if rsProperty in fRange then Result := tkKey else Result := tkIdentifier;
  end
  else if KeyComp('Interface') then begin
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end
    else
    if not(rsAfterEqualOrColon in fRange) and
       (fRange * [rsInterface, rsImplementation] = []) then
    begin
      CloseBeginEndBlocksBeforeProc;
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;
      if TopPascalCodeFoldBlockType=cfbtUnitSection then EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtUnitSection);
      fRange := fRange + [rsInterface];
      // Interface has no ";", implicit end of statement
    end;
    Result := tkKey
  end
  else if KeyComp('Deprecated') then begin
    tbf := TopPascalCodeFoldBlockType;
    if ( ( (tbf in [cfbtVarType, cfbtLocalVarType]) and (rsVarTypeInSpecification in fRange) ) or
         ( (tbf in [cfbtClass, cfbtClassSection, cfbtRecord]) and
           (fRange * [rsAfterClassMembers, rsVarTypeInSpecification] <> []) ) or
         ( tbf in [cfbtUnitSection, cfbtProgram, cfbtProcedure] )
       ) and
       ( fRange *[rsAfterEqualOrColon, rsInProcHeader, rsProperty] = [] ) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func84: TtkTokenKind;
begin
  if KeyComp('Abstract') and (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection])
  then begin
    Result := tkKey;
    if (rsAfterClass in fRange) then
      fRange := fRange + [rsAtClass] // forward, in case of further class modifiers  end
    else
    if not (rsAfterClassMembers in fRange) then
      Result := tkIdentifier;
  end
  else if KeyComp('ObjcClass') then begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func85: TtkTokenKind;
begin
  if KeyComp('Forward') then begin
    Result := tkKey;
    if TopPascalCodeFoldBlockType = cfbtProcedure then begin
      EndPascalCodeFoldBlock(True);
    end;
  end else
    if KeyComp('Library') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func86: TtkTokenKind;
begin
  if KeyComp('VarArgs') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func87: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault, spsmStringOnly]) and KeyComp('String') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func88: TtkTokenKind;
begin
  if KeyComp('Program') then begin
    fRange := fRange - [rsInterface] + [rsImplementation];
    if TopPascalCodeFoldBlockType=cfbtNone then StartPascalCodeFoldBlock(cfbtProgram);
    Result := tkKey;
  end
  else if KeyComp('Mwpascal') and (TopPascalCodeFoldBlockType in ProcModifierAllowed) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func89: TtkTokenKind;
  function ScanForClassSection: Boolean;
  var
    Txt: String;
    NestBrace1, NestBrace2: Integer;
    i, l, Idx: Integer;
  begin
    Result := False;
    Txt := copy(fLine, Run + 7, length(fLine));
    Idx := LineIndex;
    NestBrace1 := 0;
    NestBrace2 := 0;
    while true do begin
      i := 1;
      l := length(Txt);
      while i <= l do begin
        case Txt[i] of
          '{' : if (NestBrace2 = 0) and (NestedComments or (NestBrace1 = 0)) then
                  inc(NestBrace1);
          '}' : if (NestBrace2 = 0) then
                  if NestBrace1 > 0
                  then dec(NestBrace1)
                  else exit;
          '(' : if (NestBrace1 = 0) then
                  if (i+1 > l) or (Txt[i+1] <> '*')
                    then exit
                    else
                    if NestedComments or (NestBrace2 = 0) then begin
                      inc(NestBrace2);
                      inc(i);
                    end;
          '*' : if (NestBrace1 = 0) then
                  if  (i+1 <= l) and (Txt[i+1] = ')') and (NestBrace2 > 0)
                    then begin
                      dec(NestBrace2);
                      inc(i);
                    end
                    else
                    if NestBrace2 = 0 then
                      exit;
          '/' : If (NestBrace1 = 0) and (NestBrace2 = 0) then begin
                  if  (i+1 <= l) and (Txt[i+1] = '/')
                    then i := l
                    else exit;
                end;
          #1..#32: {do nothing};
          'p', 'P' : If (NestBrace1 = 0) and (NestBrace2 = 0) then begin
                       if ( (i+6 <= l) and
                            ((i+6 = l) or (Txt[i+7] in [#1..#32])) and
                            (AnsiStrComp(PChar(copy(Txt, i+1, 6)), PChar('rivate')) = 0) )
                       or ( (i+8 <= l) and
                            ((i+8 = l) or (Txt[i+9] in [#1..#32])) and
                            (AnsiStrComp(PChar(copy(Txt, i+1, 8)), PChar('rotected')) = 0) )
                        then
                          exit(True)
                        else
                          exit;
                     end;
          else
             If (NestBrace1 = 0) and (NestBrace2 = 0) then
              exit;
        end;
        inc(i);
      end;
      inc(Idx);
      if Idx < CurrentLines.Count then
        Txt := CurrentLines[Idx]
      else
        break;
    end;
  end;

begin
  if KeyComp('CppClass') then
  begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
  // Structural Scan / Quick
  if FIsInNextToEOL and not FCatchNodeInfo then
    exit;
  // Scanning for display / Look ahead
  if KeyComp('strict') then
  begin
    if (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) then
      if ScanForClassSection then
        Result := tkKey;
  end;
end;

function TSynPasSyn.Func91: TtkTokenKind;
begin
  if KeyComp('Downto') then
    Result := tkKey
  else if KeyComp('Private') then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification];
    if (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) then begin
      if (TopPascalCodeFoldBlockType=cfbtClassSection) then
        EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtClassSection);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func92: TtkTokenKind;
begin
  if D4syntax and KeyComp('overload') then Result := tkKey else
    if KeyComp('Inherited') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func94: TtkTokenKind;
begin
  if KeyComp('Assembler') then Result := tkKey else
    if KeyComp('Readonly') then
    begin
      if rsProperty in fRange then Result := tkKey else Result := tkIdentifier;
    end else Result := tkIdentifier;
end;

function TSynPasSyn.Func95: TtkTokenKind;
begin
  if KeyComp('Absolute') then
    Result := tkKey
  else if KeyComp('Contains') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func96: TtkTokenKind;
begin
  if KeyComp('Published') then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification];
    if (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) then begin
      if (TopPascalCodeFoldBlockType=cfbtClassSection) then
        EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtClassSection);
    end;
  end
  else if KeyComp('Override') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func97: TtkTokenKind;
begin
  if KeyComp('Threadvar') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func98: TtkTokenKind;
begin
  if KeyComp('Export') then Result := tkKey else
    if KeyComp('Nodefault') then
    begin
      if rsProperty in fRange then Result := tkKey else Result := tkIdentifier;
    end else Result := tkIdentifier;
end;

function TSynPasSyn.Func99: TtkTokenKind;
begin
  if KeyComp('External') then begin
    Result := tkKey;
    if TopPascalCodeFoldBlockType = cfbtProcedure then begin
      EndPascalCodeFoldBlock(True);
    end;
  end else Result := tkIdentifier;
end;

function TSynPasSyn.Func100: TtkTokenKind;
begin
  if KeyComp('Automated') then
    Result := tkKey
  else
  if (rsInProcHeader in fRange) and KeyComp('constref') and
     (PasCodeFoldRange.BracketNestLevel = 1)
  then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynPasSyn.Func101: TtkTokenKind;
var
  tbf: TPascalCodeFoldBlockType;
begin
  if KeyComp('Register') and (TopPascalCodeFoldBlockType in ProcModifierAllowed) then
    Result := tkKey
  else
  if KeyComp('Platform') then begin
    tbf := TopPascalCodeFoldBlockType;
      if ( ( (tbf in [cfbtVarType, cfbtLocalVarType]) and (rsVarTypeInSpecification in fRange) ) or
           ( (tbf in [cfbtClass, cfbtClassSection, cfbtRecord]) and
             (fRange * [rsAfterClassMembers, rsVarTypeInSpecification] <> []) ) or
           ( tbf in [cfbtUnitSection, cfbtProgram, cfbtProcedure] )
         ) and
         ( fRange *[rsAfterEqualOrColon, rsInProcHeader, rsProperty] = [] ) and
         (PasCodeFoldRange.BracketNestLevel = 0)
    then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else if FExtendedKeywordsMode and KeyComp('Continue') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func102: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Function') then begin
    if not(rsAfterEqualOrColon in fRange) then begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;

      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
    end;
    fRange := fRange + [rsInProcHeader];
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func103: TtkTokenKind;
begin
  if KeyComp('Virtual') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func105: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Procedure') then begin
    if not(rsAfterEqualOrColon in fRange) then begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;

      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
    end;
    fRange := fRange + [rsInProcHeader];
    Result := tkKey;
  end
  {$IFDEF SYN_LAZARUS}
  else if KeyComp('specialize') then
    Result := tkKey
  {$ENDIF}
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func106: TtkTokenKind;
begin
  if KeyComp('Protected') then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification];
    if (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) then begin
      if (TopPascalCodeFoldBlockType=cfbtClassSection) then
        EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtClassSection);
    end;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func108: TtkTokenKind;
begin
  if KeyComp('Operator') then
  begin
    if not(rsAfterEqualOrColon in fRange) then
    begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;
      if ((rsImplementation in fRange) and
        not(TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord])) then
        StartPascalCodeFoldBlock(cfbtProcedure);
    end;
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func112: TtkTokenKind;
begin
  if KeyComp('Requires') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func117: TtkTokenKind;
begin
  if KeyComp('Exports') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func122: TtkTokenKind;
begin
  if KeyComp('Otherwise') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func124: TtkTokenKind;
begin
  if KeyComp('ObjcCategory') then
  begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func126: TtkTokenKind;
begin
  if D4syntax and KeyComp('Implements') then
  begin
    if rsProperty in fRange then Result := tkKey else Result := tkIdentifier;
  end else if KeyComp('NoStackFrame') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func128: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('Widestring') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func129: TtkTokenKind;
begin
  if KeyComp('Dispinterface') then
  begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func130: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('Ansistring') then
    Result := tkKey
  else
  if KeyComp('Enumerator') and (TopPascalCodeFoldBlockType in [cfbtClassSection]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func132: TtkTokenKind;
begin
  if D4syntax and KeyComp('Reintroduce') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func133: TtkTokenKind;
begin
  if KeyComp('Property') then begin
    Result := tkKey;
    fRange := fRange + [rsProperty, rsAtPropertyOrReadWrite];
    if TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord] then
      fRange := fRange + [rsAfterClassMembers];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func136: TtkTokenKind;
begin
  if KeyComp('Finalization') then begin
    PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
    CloseBeginEndBlocksBeforeProc;
    if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
      EndPascalCodeFoldBlockLastLine;
    if TopPascalCodeFoldBlockType=cfbtUnitSection then EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtUnitSection);
    fRange := fRange - [rsInterface] + [rsImplementation];
    Result := tkKey
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func139: TtkTokenKind;
begin
  if KeyComp('WeakExternal') then
  begin
    Result := tkKey;
    if TopPascalCodeFoldBlockType = cfbtProcedure then
      EndPascalCodeFoldBlock(True);
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func141: TtkTokenKind;
begin
  if KeyComp('Writeonly') then
  begin
    if rsProperty in fRange then Result := tkKey else Result := tkIdentifier;
  end else Result := tkIdentifier;
end;

function TSynPasSyn.Func142: TtkTokenKind;
var
  tbf: TPascalCodeFoldBlockType;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('UTF8String') then
    Result := tkKey
  else
  if KeyComp('Experimental') then begin
    tbf := TopPascalCodeFoldBlockType;
      if ( ( (tbf in [cfbtVarType, cfbtLocalVarType]) and (rsVarTypeInSpecification in fRange) ) or
           ( (tbf in [cfbtClass, cfbtClassSection, cfbtRecord]) and
             (fRange * [rsAfterClassMembers, rsVarTypeInSpecification] <> []) ) or
           ( tbf in [cfbtUnitSection, cfbtProgram, cfbtProcedure] )
         ) and
         ( fRange *[rsAfterEqualOrColon, rsInProcHeader, rsProperty] = [] ) and
         (PasCodeFoldRange.BracketNestLevel = 0)
    then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func143: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Destructor') then
  begin
    if not(rsAfterEqualOrColon in fRange) then
    begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;

      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
      fRange := fRange + [rsInProcHeader];
    end;
    Result := tkKey;
  end else
  if KeyComp('compilerproc') then // fpc modifier
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func144: TtkTokenKind;
begin
  if KeyComp('ObjcProtocol') then
  begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func151: TtkTokenKind;
var
  tbf: TPascalCodeFoldBlockType;
begin
  if KeyComp('Unimplemented') then begin
    tbf := TopPascalCodeFoldBlockType;
      if ( ( (tbf in [cfbtVarType, cfbtLocalVarType]) and (rsVarTypeInSpecification in fRange) ) or
           ( (tbf in [cfbtClass, cfbtClassSection, cfbtRecord]) and
             (fRange * [rsAfterClassMembers, rsVarTypeInSpecification] <> []) ) or
           ( tbf in [cfbtUnitSection, cfbtProgram, cfbtProcedure] )
         ) and
         ( fRange *[rsAfterEqualOrColon, rsInProcHeader, rsProperty] = [] ) and
         (PasCodeFoldRange.BracketNestLevel = 0)
    then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func158: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('UnicodeString') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func166: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Constructor') then begin
    if not(rsAfterEqualOrColon in fRange) then begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;

      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
      fRange := fRange + [rsInProcHeader];
    end;
    Result := tkKey;
  end else
    if KeyComp('Implementation') then begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;
      if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
        EndPascalCodeFoldBlockLastLine;
      if TopPascalCodeFoldBlockType=cfbtUnitSection then EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtUnitSection);
      fRange := fRange - [rsInterface] + [rsImplementation];
      // implicit end of statement
      Result := tkKey;
    end else
      Result := tkIdentifier;
end;

function TSynPasSyn.Func167: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('Shortstring') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func168: TtkTokenKind;
begin
  if KeyComp('Initialization') then begin
    PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
    CloseBeginEndBlocksBeforeProc;
    if TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType] then
      EndPascalCodeFoldBlockLastLine;
    if TopPascalCodeFoldBlockType=cfbtUnitSection then EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtUnitSection);
    fRange := fRange - [rsInterface] + [rsImplementation];
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func181: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('RawByteString') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func191: TtkTokenKind;
begin
  if KeyComp('Resourcestring') then begin
    Result := tkKey;
	if TopPascalCodeFoldBlockType = cfbtVarType then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtVarType);
  end
  else if KeyComp('Stringresource') then
    Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

{$IFDEF SYN_LAZARUS}
function TSynPasSyn.IdentKind(p: integer): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := p;
  HashKey := KeyHash;
  if HashKey < 192 then
    Result := fIdentFuncTable[HashKey]{$IFDEF FPC}(){$ENDIF}
  else
    Result := tkIdentifier;
end;
{$ELSE}
function TSynPasSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 192 then
    Result := fIdentFuncTable[HashKey]{$IFDEF FPC}(){$ENDIF}
  else
    Result := tkIdentifier;
end;
{$ENDIF}

procedure TSynPasSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    {$IFDEF FPC}
    case I of
      #0: fProcTable[I] := @NullProc;
      #10: fProcTable[I] := @LFProc;
      #13: fProcTable[I] := @CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      '#': fProcTable[I] := @AsciiCharProc;
      '$': fProcTable[I] := @HexProc;
      '%': fProcTable[I] := @BinaryProc;
      '&': fProcTable[I] := @OctalProc;
      #39: fProcTable[I] := @StringProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := @IdentProc;
      '{': fProcTable[I] := @BraceOpenProc;
      '}', '!', '"', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := @RoundOpenProc;
            ')': fProcTable[I] := @RoundCloseProc;
            '[': fProcTable[I] := @SquareOpenProc;
            ']': fProcTable[I] := @SquareCloseProc;
            '=': fProcTable[I] := @EqualSignProc;
            '.': fProcTable[I] := @PointProc;
            ';': fProcTable[I] := @SemicolonProc;                                //mh 2000-10-08
            '/': fProcTable[I] := @SlashProc;
            ':': fProcTable[I] := @ColonProc;
            '>': fProcTable[I] := @GreaterProc;
            '<': fProcTable[I] := @LowerProc;
            '@': fProcTable[I] := @AddressOpProc;
          else
            fProcTable[I] := @SymbolProc;
          end;
        end;
    else
      fProcTable[I] := @UnknownProc;
    end;
    {$ELSE}
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '#': fProcTable[I] := AsciiCharProc;
      '$': fProcTable[I] := HexProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := BraceOpenProc;
      '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := RoundOpenProc;
            ')': fProcTable[I] := RoundCloseProc;
            '[': fProcTable[I] := SquareOpenProc;
            ']': fProcTable[I] := SquareCloseProc;
            '=': fProcTable[I] := EqualSignProc;
            '.': fProcTable[I] := PointProc;
            ';': fProcTable[I] := SemicolonProc;                                //mh 2000-10-08
            '/': fProcTable[I] := SlashProc;
            ':': fProcTable[I] := ColonProc;
            '>': fProcTable[I] := GreaterProc;
            '<': fProcTable[I] := LowerProc;
            '@': fProcTable[I] := AddressOpProc;
          else
            fProcTable[I] := SymbolProc;
          end;
        end;
    else
      fProcTable[I] := UnknownProc;
    end;
    {$ENDIF}
end;

constructor TSynPasSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStringKeywordMode := spsmDefault;
  FExtendedKeywordsMode := False;
  CreateDividerDrawConfig;
  fD4syntax := true;
  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_XML_AttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  FIDEDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrIDEDirective, SYNS_XML_AttrIDEDirective);
  FIDEDirectiveAttri.Features := FIDEDirectiveAttri.Features + [hafStyleMask];
  AddAttribute(FIDEDirectiveAttri);
  FCurIDEDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrIDEDirective, SYNS_XML_AttrIDEDirective);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  FCaseLabelAttri := TSynHighlighterAttributes.Create(SYNS_AttrCaseLabel, SYNS_XML_AttrCaseLabel);
  FCaseLabelAttri.Features := FCaseLabelAttri.Features + [hafStyleMask];
  AddAttribute(FCaseLabelAttri);
  FCurCaseLabelAttri := TSynHighlighterAttributes.Create(SYNS_AttrCaseLabel, SYNS_XML_AttrCaseLabel);
  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_XML_AttrDirective);
  fDirectiveAttri.Style:= [fsItalic];
  AddAttribute(fDirectiveAttri);
  CompilerMode:=pcmDelphi;
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fRange := [];
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
  FNodeInfoLine := -1;
end; { Create }

destructor TSynPasSyn.Destroy;
begin
  DestroyDividerDrawConfig;
  FreeAndNil(FCurCaseLabelAttri);
  FreeAndNil(FCurIDEDirectiveAttri);
  inherited Destroy;
end;

procedure TSynPasSyn.SetLine(const NewValue: string; LineNumber:Integer);
begin
  //DebugLn(['TSynPasSyn.SetLine START LineNumber=',LineNumber,' Line="',NewValue,'"']);
  fLineStr := NewValue;
  fLineLen:=length(fLineStr);
  fLine:=PChar(Pointer(fLineStr));
  Run := 0;
  Inherited SetLine(NewValue,LineNumber);
  FStartCodeFoldBlockLevel := MinimumCodeFoldBlockLevel;
  PasCodeFoldRange.LastLineCodeFoldLevelFix := 0;
  PasCodeFoldRange.PasFoldFixLevel := 0;
  PasCodeFoldRange.PasFoldMinLevel :=
    PasCodeFoldRange.PasFoldEndLevel;
  FPasStartLevel := PasCodeFoldRange.PasFoldMinLevel;
  FSynPasRangeInfo.MinLevelIfDef := FSynPasRangeInfo.EndLevelIfDef;
  FSynPasRangeInfo.MinLevelRegion := FSynPasRangeInfo.EndLevelRegion;
  FNodeInfoLine := -1;
  fLineNumber := LineNumber;
  FAtLineStart := True;
  if not FCatchNodeInfo then
    Next;
end; { SetLine }

procedure TSynPasSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynPasSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  case FLine[Run] of
    '%':
      begin
        inc(Run);
        if (FLine[Run] in ['0'..'1']) then
          while (FLine[Run] in ['0'..'1']) do inc(Run)
        else
          fTokenID := tkSymbol;
      end;
    '&':
      begin
        inc(Run);
        if (FLine[Run] in ['0'..'7']) then
          while (FLine[Run] in ['0'..'7']) do inc(Run)
        else
          fTokenID := tkSymbol;
      end;
    '$':
      begin
        inc(Run);
        if (IsIntegerChar[FLine[Run]]) then
          while (IsIntegerChar[FLine[Run]]) do inc(Run)
        else
          fTokenID := tkSymbol;
      end;
    '0'..'9': while (FLine[Run] in ['0'..'9']) do inc(Run);
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynPasSyn.BorProc;
var
  p: LongInt;
begin
  p:=Run;
  fTokenID := tkComment;
  if rsIDEDirective in fRange then
    fTokenID := tkIDEDirective;
  repeat
    case fLine[p] of
    #0,#10,#13: break;
    '}':
      if TopPascalCodeFoldBlockType=cfbtNestedComment then
      begin
        Run:=p;
        EndPascalCodeFoldBlock;
        p:=Run;
      end else begin
        fRange := fRange - [rsBor, rsIDEDirective];
        Inc(p);
        if TopPascalCodeFoldBlockType=cfbtBorCommand then
          EndPascalCodeFoldBlock;
        break;
      end;
    '{':
      if NestedComments then begin
        fStringLen := 1;
        Run:=p;
        StartPascalCodeFoldBlock(cfbtNestedComment);
        p:=Run;
      end;
    end;
    Inc(p);
  until (p>=fLineLen);
  Run:=p;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynPasSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  if TextComp('mode') then begin
    // $mode directive
    inc(Run,4);
    // skip space
    while (fLine[Run] in [' ',#9,#10,#13]) do inc(Run);
    if TextComp('objfpc') then
      CompilerMode:=pcmObjFPC
    else if TextComp('delphi') then
      CompilerMode:=pcmDelphi
    else if TextComp('fpc') then
      CompilerMode:=pcmFPC
    else if TextComp('gpc') then
      CompilerMode:=pcmGPC
    else if TextComp('tp') then
      CompilerMode:=pcmTP
    else if TextComp('macpas') then
      CompilerMode:=pcmMacPas;
  end;
  repeat
    case fLine[Run] of
    #0,#10,#13: break;
    '}':
      if TopPascalCodeFoldBlockType=cfbtNestedComment then
        EndPascalCodeFoldBlock
      else begin
        fRange := fRange - [rsDirective];
        Inc(Run);
        break;
      end;
    '{':
      if NestedComments then begin
        fStringLen := 1;
        StartPascalCodeFoldBlock(cfbtNestedComment);
      end;
    end;
    Inc(Run);
  until (Run>=fLineLen);
  //DebugLn(['TSynPasSyn.DirectiveProc Run=',Run,' fTokenPos=',fTokenPos,' fLineStr=',fLineStr,' Token=',GetToken]);
end;
{$ENDIF}

procedure TSynPasSyn.BraceOpenProc;
  function ScanRegion: Boolean;
  var
    Txt: String;
    Idx, NestBrace, i, l: Integer;
    InString: Boolean;
  begin
    Result := False;
    Txt := copy(fLine, Run, length(fLine));
    Idx := LineIndex;
    InString := False;
    NestBrace := 0;
    while true do begin
      i := 1;
      l := length(Txt);
      while i <= l do begin
        case Txt[i] of
          '{' : inc(NestBrace);
          '}' : if NestBrace = 0
                then exit
                else dec(NestBrace);
          '''' : if (i+1 <= l) and (Txt[i+1] = '''')
                 then inc(i)
                 else InString := not InString;
          '-', '/' : If (not InString) and (i+4 <= l) and
                        ((i=1) or (Txt[i-1] in [' ', #9, #10, #13])) and
                        (AnsiStrComp(PChar(copy(Txt, i+1, 4)), PChar('fold')) = 0)
                        and ((i+4 = l) or (Txt[i+5] in [' ', #9, #10, #13, '}']))
                     then
                       exit(True);
        end;
        inc(i);
      end;
      inc(Idx);
      if Idx < CurrentLines.Count then
        Txt := CurrentLines[Idx]
      else
        break;
    end;
  end;

begin
  if (Run < fLineLen-1) and (fLine[Run+1] = '$') then begin
    // compiler directive
    fRange := fRange + [rsDirective];
    inc(Run,2);
    fToIdent := Run;
    KeyHash;
    if KeyComp('ifdef') or KeyComp('ifndef') or KeyComp('if') then
      StartCustomCodeFoldBlock(cfbtIfDef)
    else if KeyComp('endif') then
      EndCustomCodeFoldBlock(cfbtIfDef)
    else if KeyComp('else') then begin
      EndCustomCodeFoldBlock(cfbtIfDef);
      StartCustomCodeFoldBlock(cfbtIfDef);
    end
    else if KeyComp('region') then begin
      StartCustomCodeFoldBlock(cfbtRegion);
      if FCatchNodeInfo then
        // Scan ahead
        if ScanRegion and (FNodeInfoCount > 0) then
          FNodeInfoList[FNodeInfoCount-1].FoldAction :=
            FNodeInfoList[FNodeInfoCount-1].FoldAction + [sfaDefaultCollapsed];
    end
    else if KeyComp('endregion') then
      EndCustomCodeFoldBlock(cfbtRegion);
    DirectiveProc;
  end else begin
    // curly bracket open -> borland comment
    fStringLen := 1; // length of "{"
    inc(Run);
    if (Run < fLineLen) and (fLine[Run] = '%') then begin
      fRange := fRange + [rsIDEDirective];
    // IDE directive {%xxx } rsIDEDirective
      inc(Run);
      fToIdent := Run;
      KeyHash;
      if KeyComp('region') then begin
        StartCustomCodeFoldBlock(cfbtRegion);
        if FCatchNodeInfo then
          // Scan ahead
          if ScanRegion and (FNodeInfoCount > 0) then
            FNodeInfoList[FNodeInfoCount-1].FoldAction :=
              FNodeInfoList[FNodeInfoCount-1].FoldAction + [sfaDefaultCollapsed];
      end
      else if KeyComp('endregion') then
        EndCustomCodeFoldBlock(cfbtRegion)
      else begin
        dec(Run, 2);
        StartPascalCodeFoldBlock(cfbtBorCommand);
        inc(Run);
      end;
    end
    else begin
      fRange := fRange + [rsBor];
      dec(Run);
      StartPascalCodeFoldBlock(cfbtBorCommand);
      inc(Run);
    end;
    BorProc;
  end;
end;

procedure TSynPasSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then
    inc(Run) // ":="
  else begin
    fRange := fRange + [rsAfterEqualOrColon] - [rsAtCaseLabel];
    if (TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType, cfbtClass, cfbtClassSection, cfbtRecord]) and
       not(rsAfterClassMembers in fRange)
    then
      fRange := fRange + [rsVarTypeInSpecification];
  end;
end;

procedure TSynPasSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then
    inc(Run)
end;

procedure TSynPasSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynPasSyn.IdentProc;
begin
  fTokenID := IdentKind(Run);
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynPasSyn.HexProc;
begin
  inc(Run);
  if (IsIntegerChar[FLine[Run]]) then begin
    fTokenID := tkNumber;
    while (IsIntegerChar[FLine[Run]]) do inc(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynPasSyn.BinaryProc;
begin
  inc(Run);
  if FLine[Run] in ['0'..'1'] then begin
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'1'] do inc(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynPasSyn.OctalProc;
begin
  inc(Run);
  if FLine[Run] in ['0'..'7'] then begin
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'7'] do inc(Run);
  end
  else
    fTokenID := tkSymbol;
end;


procedure TSynPasSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPasSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '>'] then inc(Run);
end;

procedure TSynPasSyn.NullProc;
begin
  if (Run = 0) and (rsSlash in fRange) then begin
    fRange := fRange - [rsSlash];
    if TopPascalCodeFoldBlockType = cfbtSlashComment then
      EndPascalCodeFoldBlockLastLine;
  end;
  fTokenID := tkNull;
  {$IFDEF SYN_LAZARUS}
  if Run<fLineLen then inc(Run);
  {$ENDIF}
end;

procedure TSynPasSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  {$IFDEF SYN_LAZARUS}
  if Run<fLineLen then begin
    while (IsNumberChar[FLine[Run]]) do begin
      if (FLine[Run]='.') and (fLine[Run+1]='.')  then
        break;
      inc(Run);
    end;
  end;
  {$ELSE}
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
  {$ENDIF}
end;

procedure TSynPasSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['.', ')'] then inc(Run);
end;

procedure TSynPasSyn.AnsiProc;
begin
  {$IFDEF SYN_LAZARUS}
  fTokenID := tkComment;
  repeat
    if fLine[Run]=#0 then
      break
    else if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then
    begin
      Inc(Run, 2);
      if TopPascalCodeFoldBlockType=cfbtNestedComment then begin
        EndPascalCodeFoldBlock;
      end else begin
        fRange := fRange - [rsAnsi];
        if TopPascalCodeFoldBlockType=cfbtAnsiComment then
          EndPascalCodeFoldBlock;
        break;
      end;
    end
    else if NestedComments
    and (fLine[Run] = '(') and (fLine[Run + 1] = '*') then
    begin
      fStringLen := 2;
      StartPascalCodeFoldBlock(cfbtNestedComment);
      Inc(Run,2);
    end else
      Inc(Run);
  until (Run>=fLineLen) or (fLine[Run] in [#0, #10, #13]);
  {$ELSE}
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        fRange := fRange - [rsAnsi];
        break;
      end;
      Inc(Run);
    until (Run>fLineLen) or (fLine[Run] in [#0, #10, #13]);
  end;
  {$ENDIF}
end;

procedure TSynPasSyn.RoundOpenProc;
begin
  Inc(Run);
  {$IFDEF SYN_LAZARUS}
  if Run>=fLineLen then begin
    fTokenID:=tkSymbol;
    PasCodeFoldRange.IncBracketNestLevel;
    exit;
  end;
  {$ENDIF}
  case fLine[Run] of
    '*':
      begin
        // We would not be here, if we were in a comment or directive already
        fRange := fRange + [rsAnsi];
        fTokenID := tkComment;
        fStringLen := 2; // length of "(*"
        Dec(Run);
        StartPascalCodeFoldBlock(cfbtAnsiComment);
        Inc(Run, 2);
        if not (fLine[Run] in [#0, #10, #13]) then begin
          AnsiProc;
        end;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
        PasCodeFoldRange.IncBracketNestLevel;
      end;
    else
      begin
        fTokenID := tkSymbol;
        PasCodeFoldRange.IncBracketNestLevel;
      end;
  end;
end;

procedure TSynPasSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  PasCodeFoldRange.DecBracketNestLevel;
  fRange := fRange + [rsAtClosingBracket];
  if (PasCodeFoldRange.BracketNestLevel = 0) then
    Exclude(fRange, rsInProcHeader);
end;

procedure TSynPasSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  PasCodeFoldRange.IncBracketNestLevel;
end;

procedure TSynPasSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  PasCodeFoldRange.DecBracketNestLevel;
end;

procedure TSynPasSyn.EqualSignProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  fRange := fRange + [rsAfterEqualOrColon];
  if (TopPascalCodeFoldBlockType in [cfbtVarType, cfbtLocalVarType, cfbtClass, cfbtClassSection, cfbtRecord]) and
     not(rsAfterClassMembers in fRange)
  then
    fRange := fRange + [rsVarTypeInSpecification];
end;

procedure TSynPasSyn.SemicolonProc;
var
  tfb: TPascalCodeFoldBlockType;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  tfb := TopPascalCodeFoldBlockType;

  if tfb = cfbtUses then
    EndPascalCodeFoldBlock;

  if (tfb = cfbtClass) and (rsAfterClass in fRange) then
    EndPascalCodeFoldBlock(True);

  if (tfb = cfbtCase) then
    fRange := fRange + [rsAtCaseLabel];

  if (tfb in [cfbtClass, cfbtClassSection]) and
     (fRange * [rsVarTypeInSpecification, rsAfterClassMembers] = [rsVarTypeInSpecification])
  then
    fRange := fRange + [rsAfterClassField];

  if (fRange * [rsProperty, rsInProcHeader] <> []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    fRange := fRange - [rsProperty, rsInProcHeader];
  fRange := fRange - [rsVarTypeInSpecification];
end;

procedure TSynPasSyn.SlashProc;
begin
  if fLine[Run+1] = '/' then begin
    fTokenID := tkComment;
    if FAtLineStart then begin
      fRange := fRange + [rsSlash];
      fStringLen := 2; // length of "//"
      if not(TopPascalCodeFoldBlockType = cfbtSlashComment) then
        StartPascalCodeFoldBlock(cfbtSlashComment);
    end;
    inc(Run, 2);
    while not(fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end else begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynPasSyn.SlashContinueProc;
begin
  if (fLine[Run] = '/') and (fLine[Run + 1] = '/') then begin
    // Continue fold block
    fTokenID := tkComment;
    while not(fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
    exit;
  end;

  fTokenID := tkUnknown;
  if IsSpaceChar[FLine[Run]] then begin
    fTokenID := tkSpace;
    inc(Run);
    while IsSpaceChar[FLine[Run]] do inc(Run);
  end;

  if not((fLine[Run] = '/') and (fLine[Run + 1] = '/')) then begin
    fRange := fRange - [rsSlash];
    if TopPascalCodeFoldBlockType = cfbtSlashComment then
      EndPascalCodeFoldBlockLastLine;
  end;

  if FTokenID = tkUnknown then
    Next;
end;

procedure TSynPasSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  {$IFDEF SYN_LAZARUS}
  while IsSpaceChar[FLine[Run]] do inc(Run);
  {$ELSE}
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
  {$ENDIF}
end;

procedure TSynPasSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (not (fLine[Run] in [#0, #10, #13])) do begin
    if fLine[Run] = '''' then begin
      Inc(Run);
      if (fLine[Run] <> '''') then
        break;
    end;
    Inc(Run);
  end;
  {$ELSE}
  while not (fLine[Run] in [#0, #10, #13]) do begin
    if fLine[Run] = #39 then begin
      Inc(Run);
      if fLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
  {$ENDIF}
end;

procedure TSynPasSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPasSyn.UnknownProc;
begin
  inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  {$ENDIF}
  fTokenID := tkUnknown;
end;

procedure TSynPasSyn.Next;
var
  IsAtCaseLabel: Boolean;
  OldRange: TRangeStates;
begin
  fAsmStart := False;
  fTokenPos := Run;
    FTokenIsCaseLabel := False;
  if Run>=fLineLen then begin
    NullProc;
    exit;
  end;
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else
      if rsAnsi in fRange then
        AnsiProc
      else if fRange * [rsBor, rsIDEDirective] <> [] then
        BorProc
      else if rsDirective in fRange then
        DirectiveProc
      else if rsSlash in fRange then
        SlashContinueProc
      else begin
        OldRange := fRange;
        //if rsAtEqual in fRange then
        //  fRange := fRange + [rsAfterEqualOrColon] - [rsAtEqual]
        //else
        if rsAtClass in fRange then
          fRange := fRange + [rsAfterClass] - [rsAtClass];
        IsAtCaseLabel := rsAtCaseLabel in fRange;

        fProcTable[fLine[Run]];

        if (IsAtCaseLabel) and (rsAtCaseLabel in fRange) then begin
          FTokenIsCaseLabel := True;
          if (FTokenID = tkKey) then
            fRange := fRange - [rsAtCaseLabel];
        end;
        if not (FTokenID in [tkSpace, tkComment, tkIDEDirective, tkDirective]) then begin
          if (PasCodeFoldRange.BracketNestLevel = 0) and
             not(rsAtClosingBracket in fRange)
          then
            fRange := fRange - [rsAfterClass];
          if rsAfterEqualOrColon in OldRange then
            fRange := fRange - [rsAfterEqualOrColon];
          if rsAtPropertyOrReadWrite in OldRange then
            fRange := fRange - [rsAtPropertyOrReadWrite];
          fRange := fRange - [rsAtClosingBracket];
          if rsAfterClassField in OldRange then
            fRange := fRange - [rsAfterClassField];
        end
        else
          fRange := fRange - [rsAtClosingBracket];
      end
  end;
  if FAtLineStart and not(FTokenID in [tkSpace, tkComment, tkIDEDirective]) then
    FAtLineStart := False;
  //DebugLn(['TSynPasSyn.Next Run=',Run,' fTokenPos=',fTokenPos,' fLineStr=',fLineStr,' Token=',GetToken]);
end;

function TSynPasSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynPasSyn.GetEol: Boolean;
begin
  Result := (fTokenID = tkNull)
            {$IFDEF SYN_LAZARUS}and (Run >= fLineLen){$ENDIF};
end;

function TSynPasSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  {$IFDEF SYN_LAZARUS}
  SetLength(Result,Len);
  if Len>0 then
    System.Move(fLine[fTokenPos],Result[1],Len);
  {$ELSE}
  SetString(Result, (FLine + fTokenPos), Len);
  {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
procedure TSynPasSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  if TokenLength>0 then begin
    TokenStart:=@fLine[fTokenPos];
  end else begin
    TokenStart:=nil;
  end;
end;
{$ENDIF}

function TSynPasSyn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange * [rsAnsi, rsBor, rsIDEDirective, rsDirective, rsAsm] = [rsAsm])
    and not (fTokenId in [tkNull, tkComment, tkIDEDirective, tkSpace, tkDirective])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynPasSyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  sMask: TFontStyles;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkIDEDirective: begin
      Result := FCurIDEDirectiveAttri;
      Result.Assign(FCommentAttri);
      if FIDEDirectiveAttri.Background <> clNone then Result.Background := FIDEDirectiveAttri.Background;
      if FIDEDirectiveAttri.Foreground <> clNone then Result.Foreground := FIDEDirectiveAttri.Foreground;
      if FIDEDirectiveAttri.FrameColor <> clNone then Result.FrameColor := FIDEDirectiveAttri.FrameColor;
      sMask := FIDEDirectiveAttri.StyleMask + (fsNot(FIDEDirectiveAttri.StyleMask) * FIDEDirectiveAttri.Style); // Styles to be taken from FIDEDirectiveAttri
      Result.Style:= (Result.Style * fsNot(sMask)) + (FIDEDirectiveAttri.Style * sMask);
      Result.StyleMask:= (Result.StyleMask * fsNot(sMask)) + (FIDEDirectiveAttri.StyleMask * sMask);
    end;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkDirective: Result := fDirectiveAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
  if FTokenIsCaseLabel and
     (GetTokenID in [tkIdentifier, tkKey, tkNumber, tkString])
  then begin
    FCurCaseLabelAttri.Assign(Result);
    Result := FCurCaseLabelAttri;
    if FCaseLabelAttri.Background <> clNone then Result.Background := FCaseLabelAttri.Background;
    if FCaseLabelAttri.Foreground <> clNone then Result.Foreground := FCaseLabelAttri.Foreground;
    if FCaseLabelAttri.FrameColor <> clNone then Result.FrameColor := FCaseLabelAttri.FrameColor;
    sMask := FCaseLabelAttri.StyleMask + (fsNot(FCaseLabelAttri.StyleMask) * FCaseLabelAttri.Style); // Styles to be taken from FCaseLabelAttri
    Result.Style:= (Result.Style * fsNot(sMask)) + (FCaseLabelAttri.Style * sMask);
    Result.StyleMask:= (Result.StyleMask * fsNot(sMask)) + (FCaseLabelAttri.StyleMask * sMask);
  end;
end;

function TSynPasSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynPasSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynPasSyn.GetRange: Pointer;
begin
  // For speed reasons, we work with fRange instead of CodeFoldRange.RangeType
  // -> update now
  CodeFoldRange.RangeType:=Pointer(PtrUInt(Integer(fRange)));
  // return a fixed copy of the current CodeFoldRange instance
  Result := inherited GetRange;
end;

procedure TSynPasSyn.SetRange(Value: Pointer);
begin
  //DebugLn(['TSynPasSyn.SetRange START']);
  inherited SetRange(Value);
  CompilerMode := PasCodeFoldRange.Mode;
  fRange := TRangeStates(Integer(PtrUInt(CodeFoldRange.RangeType)));
  FNodeInfoLine := -1;
  FSynPasRangeInfo := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[LineIndex-1];
end;

procedure TSynPasSyn.StartAtLineIndex(LineNumber: Integer);
begin
  inherited StartAtLineIndex(LineNumber);
end;

procedure TSynPasSyn.ResetRange;
begin
  fRange := [];
  FStartCodeFoldBlockLevel:=0;
  FPasStartLevel := 0;
  with FSynPasRangeInfo do begin
    EndLevelIfDef := 0;
    MinLevelIfDef := 0;
    EndLevelRegion := 0;
    MinLevelRegion := 0;
  end;
  Inherited ResetRange;
  CompilerMode:=pcmDelphi;
end;

procedure TSynPasSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      {$IFNDEF SYN_LAZARUS}
      // ToDo Registry
      if OpenKeyReadOnly('\SOFTWARE\Borland\Delphi') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
      {$ENDIF}
    finally
      Free;
    end;
  end;
end;

function TSynPasSyn.TopPascalCodeFoldBlockType(DownIndex: Integer = 0): TPascalCodeFoldBlockType;
var
  p: Pointer;
begin
  p := TopCodeFoldBlockType(DownIndex);
  if p >= CountPascalCodeFoldBlockOffset then
    p := p - PtrUInt(CountPascalCodeFoldBlockOffset);
  Result := TPascalCodeFoldBlockType(PtrUInt(p));
end;

function TSynPasSyn.FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer;
var
  inf: TSynPasRangeInfo;
begin
  Result := 0;
  if (AType <> 1) then
    inf := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[ALineIndex];
  if (AType = 0) or (AType = 1) then
    Result := EndPasFoldLevel(ALineIndex) - MinimumPasFoldLevel(ALineIndex);
  if (AType = 0) or (AType = 2) then
    Result := Result + inf.EndLevelRegion - inf.MinLevelRegion;
  if (AType = 0) or (AType = 3) then
    Result := Result + inf.EndLevelIfDef - inf.MinLevelIfDef;
  if Result < 0 then
    Result := 0;
end;

function TSynPasSyn.FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer;
var
  inf, inf2: TSynPasRangeInfo;
begin
  Result := 0;
  if (AType <> 1) then begin
    inf := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[ALineIndex];
    inf2 := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[ALineIndex - 1];
  end;
  if (AType = 0) or (AType = 1) then
    Result := EndPasFoldLevel(ALineIndex - 1)
            - min(MinimumPasFoldLevel(ALineIndex), EndPasFoldLevel(ALineIndex));
  if (AType = 0) or (AType = 2) then
    Result := Result + inf2.EndLevelRegion - inf.MinLevelRegion;
  if (AType = 0) or (AType = 3) then
    Result := Result + inf2.EndLevelIfDef - inf.MinLevelIfDef;
end;

function TSynPasSyn.FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer;
var
  inf: TSynPasRangeInfo;
begin
  Result := 0;
  if (AType <> 1) then
    inf := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[ALineIndex];
  if (AType = 0) or (AType = 1) then
    Result := EndPasFoldLevel(ALineIndex);
  if (AType = 0) or (AType = 2) then
    Result := Result + inf.EndLevelRegion;
  if (AType = 0) or (AType = 3) then
    Result := Result + inf.EndLevelIfDef;
end;

function TSynPasSyn.FoldTypeCount: integer;
begin
  Result := 3;
end;

function TSynPasSyn.FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
  UseCloseNodes: boolean): integer;
var
  act: TSynFoldActions;
begin
  act := [sfaOpen, sfaFold];
  if UseCloseNodes then act := [sfaClose, sfaFold];
  case TPascalCodeFoldBlockType(PtrUInt(GetFoldNodeInfo(ALineIndex, FoldIndex, act).FoldType)) of
    cfbtRegion:
      Result := 2;
    cfbtIfDef:
      Result := 3;
    else
      Result := 1;
  end;
end;

function TSynPasSyn.FoldLineLength(ALineIndex, FoldIndex: Integer): integer;
var
  atype : Integer;
  node: TSynFoldNodeInfo;
begin
  node := GetFoldNodeInfo(ALineIndex, FoldIndex, [sfaOpen, sfaFold]);
  if sfaInvalid in node.FoldAction then exit(-1);
  if sfaOneLineOpen in node.FoldAction then exit(0);
  case TPascalCodeFoldBlockType(PtrUInt(node.FoldType)) of
    cfbtRegion:
      atype := 2;
    cfbtIfDef:
      atype := 3;
    else
      atype := 1;
  end;

  Result := FoldEndLine(ALineIndex, FoldIndex);
  // check if fold last line of block (not mixed "end begin")
  if (EndPasFoldLevel(Result, atype) > MinimumPasFoldLevel(Result, atype)) then
    dec(Result);
  // Amount of lines, that will become invisible (excludes the cfCollapsed line)
  Result := Result - ALineIndex;
end;

function TSynPasSyn.FoldEndLine(ALineIndex, FoldIndex: Integer): integer;
var
  lvl, cnt, atype : Integer;
  node: TSynFoldNodeInfo;
begin
  node := GetFoldNodeInfo(ALineIndex, FoldIndex, [sfaOpen, sfaFold]);
  if sfaInvalid in node.FoldAction then exit(-1);
  if sfaOneLineOpen in node.FoldAction then exit(ALineIndex);
  case TPascalCodeFoldBlockType(PtrUInt(node.FoldType)) of
    cfbtRegion:
      atype := 2;
    cfbtIfDef:
      atype := 3;
    else
      atype := 1;
  end;


  cnt := CurrentLines.Count;
  lvl := node.FoldLvlEnd;

  Result := ALineIndex + 1;
  while (Result < cnt) and (MinimumPasFoldLevel(Result, atype) >= lvl) do inc(Result);
  // check if fold last line of block (not mixed "end begin")
  // and not lastlinefix
  if (Result = cnt) then
    dec(Result);
end;

function TSynPasSyn.MinimumPasFoldLevel(Index: Integer; AType: Integer = 1): integer;
var
  r: TSynPasSynRange;
begin
  case AType of
    2:
      Result := TSynHighlighterPasRangeList(CurrentRanges).
                  PasRangeInfo[Index].MinLevelRegion;
    3:
      Result := TSynHighlighterPasRangeList(CurrentRanges).
                  PasRangeInfo[Index].MinLevelIfDef;
    else
      begin
        if (Index < 0) or (Index >= CurrentLines.Count) then
          exit(0);
        r := TSynPasSynRange(CurrentRanges[Index]);
        if (r <> nil) and (Pointer(r) <> NullRange) then begin
          Result := Min(r.PasFoldEndLevel + LastLinePasFoldLevelFix(Index + 1),
                        r.PasFoldMinLevel)
        end else
          Result := 0;
      end;
  end;
end;

function TSynPasSyn.EndPasFoldLevel(Index: Integer; AType: Integer = 1): integer;
var
  r: TSynPasSynRange;
begin
  if (Index < 0) or (Index >= CurrentLines.Count - 1) then
    exit(0);
  case AType of
    2:
      Result := TSynHighlighterPasRangeList(CurrentRanges).
                  PasRangeInfo[Index].EndLevelRegion;
    3:
      Result := TSynHighlighterPasRangeList(CurrentRanges).
                  PasRangeInfo[Index].EndLevelIfDef;
    else
      begin
        r := TSynPasSynRange(CurrentRanges[Index]);
        if (r <> nil) and (Pointer(r) <> NullRange) then
          Result := r.PasFoldEndLevel + LastLinePasFoldLevelFix(Index + 1)
        else
          Result := 0;
      end;
  end;
end;

function TSynPasSyn.LastLinePasFoldLevelFix(Index: Integer; AType: Integer = 1): integer;
var
  r: TSynPasSynRange;
begin
  case AType of
    2: Result := 0;
    3: Result := 0;
    else
      begin
        if (Index < 0) or (Index >= CurrentLines.Count) then
          exit(0);
        r := TSynPasSynRange(CurrentRanges[Index]);
        if (r <> nil) and (Pointer(r) <> NullRange) then
          Result := r.PasFoldFixLevel
        else
          Result := 0;
      end;
  end;
end;


function TSynPasSyn.MinimumFoldLevel(Index: Integer): integer;
var
  r: TSynPasSynRange;
begin
  if (Index < 0) or (Index >= CurrentLines.Count) then
    exit(0);
  r := TSynPasSynRange(CurrentRanges[Index]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := Min(r.CodeFoldStackSize + LastLineFoldLevelFix(Index + 1),
                  r.MinimumCodeFoldBlockLevel)
  else
    Result := 0;
end;

function TSynPasSyn.EndFoldLevel(Index: Integer): integer;
var
  r: TSynPasSynRange;
begin
  if (Index < 0) or (Index >= CurrentLines.Count) then
    exit(0);
  r := TSynPasSynRange(CurrentRanges[Index]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := r.CodeFoldStackSize + LastLineFoldLevelFix(Index + 1)
  else
    Result := 0;
end;

function TSynPasSyn.LastLineFoldLevelFix(Index: Integer): integer;
var
  r: TSynPasSynRange;
begin
  if (Index < 0) or (Index >= CurrentLines.Count) then
    exit(0);
  r := TSynPasSynRange(CurrentRanges[Index]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := r.LastLineCodeFoldLevelFix
  else
    Result := 0;
end;


procedure TSynPasSyn.InitNode(var Node: TSynFoldNodeInfo; EndOffs: Integer;
  ABlockType: TPascalCodeFoldBlockType; aActions: TSynFoldActions);
var
  OneLine: Boolean;
  i: Integer;
begin
  Node.LineIndex := LineIndex;
  Node.LogXStart := Run;
  Node.LogXEnd := Run + fStringLen;
  Node.FoldType := Pointer(PtrInt(ABlockType));
  Node.FoldTypeCompatible := Pointer(PtrInt(PascalFoldTypeCompatibility[ABlockType]));
  Node.FoldAction := aActions;
  case ABlockType of
    cfbtRegion:
      begin
        node.FoldGroup := 2;
        Node.FoldLvlStart := FSynPasRangeInfo.EndLevelRegion;
        Node.NestLvlStart := FSynPasRangeInfo.EndLevelRegion;
        OneLine := (EndOffs < 0) and (Node.FoldLvlStart > FSynPasRangeInfo.MinLevelRegion);
      end;
    cfbtIfDef:
      begin
        node.FoldGroup := 3;
        Node.FoldLvlStart := FSynPasRangeInfo.EndLevelIfDef;
        Node.NestLvlStart := FSynPasRangeInfo.EndLevelIfDef;
        OneLine := (EndOffs < 0) and (Node.FoldLvlStart > FSynPasRangeInfo.MinLevelIfDef);
      end;
    else
      begin
        node.FoldGroup := 1;
        Node.FoldLvlStart := PasCodeFoldRange.PasFoldEndLevel;
        Node.NestLvlStart := CurrentCodeFoldBlockLevel;
        OneLine := (EndOffs < 0) and (Node.FoldLvlStart > PasCodeFoldRange.PasFoldMinLevel); // MinimumCodeFoldBlockLevel);
      end;
  end;
  Node.NestLvlEnd := Node.NestLvlStart + EndOffs;
  if not (sfaFold in aActions) then
    EndOffs := 0;
  Node.FoldLvlEnd := Node.FoldLvlStart + EndOffs;
  if OneLine then begin // find opening node
    i := FNodeInfoCount - 1;
    while (i >= 0) and
    ( (FNodeInfoList[i].FoldType <> node.FoldType) or
      (FNodeInfoList[i].FoldGroup <> node.FoldGroup) or
      (not (sfaOpen in FNodeInfoList[i].FoldAction)) or
      (FNodeInfoList[i].FoldLvlEnd <> Node.FoldLvlStart)
    ) do
      dec(i);
    if i >= 0 then begin
      FNodeInfoList[i].FoldAction := FNodeInfoList[i].FoldAction + [sfaOneLineOpen];
      Node.FoldAction             := Node.FoldAction + [sfaOneLineClose];
      if (sfaFoldHide in FNodeInfoList[i].FoldAction) then begin
        // one liner: hide-able / not fold-able
        FNodeInfoList[i].FoldAction := FNodeInfoList[i].FoldAction - [sfaFoldFold];
        Node.FoldAction             := Node.FoldAction - [sfaFoldFold];
      end else begin
        // one liner: nether hide-able nore fold-able
        FNodeInfoList[i].FoldAction := FNodeInfoList[i].FoldAction - [sfaOpen, sfaFold];
        Node.FoldAction             := Node.FoldAction - [sfaClose, sfaFold];
      end;
    end;
  end;
  if ABlockType in PascalWordTripletRanges then
    Include(Node.FoldAction, sfaMarkup);
end;

procedure TSynPasSyn.StartCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
var
  act: TSynFoldActions;
begin
  if not FFoldConfig[ord(ABlockType)].Enabled then exit;
  if FCatchNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    GrowNodeInfoList;
    act := FFoldConfig[ord(ABlockType)].FoldActions;
    if not FAtLineStart then
      act := act - [sfaFoldHide];
    InitNode(FNodeInfoList[FNodeInfoCount], +1, ABlockType, [sfaOpen] + act);
    inc(FNodeInfoCount);
  end;
  case ABlockType of
    cfbtIfDef:
      inc(FSynPasRangeInfo.EndLevelIfDef);
    cfbtRegion:
      inc(FSynPasRangeInfo.EndLevelRegion);
  end;
end;

procedure TSynPasSyn.EndCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
begin
  if not FFoldConfig[ord(ABlockType)].Enabled then exit;
  if FCatchNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    GrowNodeInfoList;
    InitNode(FNodeInfoList[FNodeInfoCount], -1, ABlockType,
             [sfaClose, sfaFold]); // + FFoldConfig[ord(ABlockType)].FoldActions);
    inc(FNodeInfoCount);
  end;
  case ABlockType of
    cfbtIfDef:
      begin
        if FSynPasRangeInfo.EndLevelIfDef > 0 then
          dec(FSynPasRangeInfo.EndLevelIfDef);
        if FSynPasRangeInfo.EndLevelIfDef < FSynPasRangeInfo.MinLevelIfDef then
          FSynPasRangeInfo.MinLevelIfDef := FSynPasRangeInfo.EndLevelIfDef;
      end;
    cfbtRegion:
      begin
        if FSynPasRangeInfo.EndLevelRegion > 0 then
          dec(FSynPasRangeInfo.EndLevelRegion);
        if FSynPasRangeInfo.EndLevelRegion < FSynPasRangeInfo.MinLevelRegion then
          FSynPasRangeInfo.MinLevelRegion := FSynPasRangeInfo.EndLevelRegion;
      end;
  end;
end;


function TSynPasSyn.StartPascalCodeFoldBlock(
  ABlockType: TPascalCodeFoldBlockType): TSynCustomCodeFoldBlock;
var
  p: PtrInt;
  FoldBlock: Boolean;
  act: TSynFoldActions;
begin
  FoldBlock := FFoldConfig[ord(ABlockType)].Enabled;
  p := 0;
  if FCatchNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    GrowNodeInfoList;
    act := [sfaOpen];
    if FoldBlock then
      act := act + FFoldConfig[ord(ABlockType)].FoldActions;
    if not FAtLineStart then
      act := act - [sfaFoldHide];
    InitNode(FNodeInfoList[FNodeInfoCount], +1, ABlockType, act);
    inc(FNodeInfoCount);
  end;
  if not FoldBlock then
    p := PtrInt(CountPascalCodeFoldBlockOffset);
  Result:=TSynCustomCodeFoldBlock(StartCodeFoldBlock(p+Pointer(PtrInt(ABlockType)), FoldBlock));
end;

procedure TSynPasSyn.EndPascalCodeFoldBlock(NoMarkup: Boolean = False);
var
  DecreaseLevel: Boolean;
  act: TSynFoldActions;
  BlockType: Pointer;
begin
  BlockType := TopCodeFoldBlockType;
  DecreaseLevel := BlockType < CountPascalCodeFoldBlockOffset;
  if FCatchNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    GrowNodeInfoList;
    act := [sfaClose];
    if DecreaseLevel then
      act := act + [sfaFold]; //FFoldConfig[PtrUInt(BlockType)].FoldActions;
    InitNode(FNodeInfoList[FNodeInfoCount], -1, TopPascalCodeFoldBlockType, act);
    if NoMarkup then
      exclude(FNodeInfoList[FNodeInfoCount].FoldAction, sfaMarkup);
    inc(FNodeInfoCount);
  end;
  EndCodeFoldBlock(DecreaseLevel);
end;

procedure TSynPasSyn.CloseBeginEndBlocksBeforeProc;
begin
  if not(TopPascalCodeFoldBlockType in
         [cfbtBeginEnd, cfbtTopBeginEnd, cfbtCase, cfbtAsm, cfbtExcept, cfbtTry,
          cfbtRepeat]) then
    exit;
  while TopPascalCodeFoldBlockType in
        [cfbtBeginEnd, cfbtTopBeginEnd, cfbtCase, cfbtAsm, cfbtExcept, cfbtTry,
         cfbtRepeat] do
    EndPascalCodeFoldBlockLastLine;
  if TopPascalCodeFoldBlockType = cfbtProcedure then
    EndPascalCodeFoldBlockLastLine; // This procedure did have a begin/end block, so it must end too
end;

procedure TSynPasSyn.SmartCloseBeginEndBlocks(SearchFor: TPascalCodeFoldBlockType);
var
  i, nc: Integer;
  t: TPascalCodeFoldBlockType;
begin
  // Close unfinished blocks, IF the expected type is found
  // Only check a limited deep. Otherwhise assume, that the "SearchFor"-End node may be misplaced
  i := 0;
  while (i <= 2) do begin
    t := TopPascalCodeFoldBlockType(i);
    if not (t in [cfbtBeginEnd, cfbtTopBeginEnd, cfbtCase, cfbtAsm, cfbtExcept,
                  cfbtTry, cfbtRepeat, SearchFor]) then
      exit;
    if (t = SearchFor) then break;
    inc(i);
  end;
  if i > 2 then
    exit;

  while i > 0 do begin
    EndPascalCodeFoldBlockLastLine;
    nc := FNodeInfoCount;
    if FCatchNodeInfo and (FNodeInfoCount > nc) then
      exclude(FNodeInfoList[FNodeInfoCount-1].FoldAction, sfaMarkup);
    dec(i);
  end;
end;

procedure TSynPasSyn.EndPascalCodeFoldBlockLastLine;
var
  i: Integer;
begin
  i := FNodeInfoCount;
  EndPascalCodeFoldBlock;
  if FAtLineStart then begin
    // If we are not at linestart, new folds could have been opened => handle as normal close
    if (CurrentCodeFoldBlockLevel < FStartCodeFoldBlockLevel) and
      (FStartCodeFoldBlockLevel > 0)
    then begin
      PasCodeFoldRange.DecLastLineCodeFoldLevelFix;
      dec(FStartCodeFoldBlockLevel);
      if FCatchNodeInfo then dec(FNodeInfoCount);
    end;
    if (PasCodeFoldRange.PasFoldEndLevel < FPasStartLevel) and
      (FPasStartLevel > 0)
    then begin
      PasCodeFoldRange.DecLastLinePasFoldFix;
      dec(FPasStartLevel);
    end;
  end
  else if FNodeInfoCount > i then begin
    exclude(FNodeInfoList[FNodeInfoCount - 1].FoldAction, sfaMarkup); // not markup able
    FNodeInfoList[FNodeInfoCount - 1].LogXEnd := 0;
  end;
end;

function TSynPasSyn.GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting;
  function CheckFoldNestLevel(MaxDepth, StartLvl: Integer;
    CountTypes, SkipTypes: TPascalCodeFoldBlockTypes;
    out ResultLvl: Integer): Boolean;
  var
    i, j, m: Integer;
    t: TPascalCodeFoldBlockType;
  begin
    i := 0;
    j := StartLvl;
    m := CurrentCodeFoldBlockLevel;
    t := TopPascalCodeFoldBlockType(j);
    while (i <= MaxDepth) and (j < m) and
          ((t in CountTypes) or (t in SkipTypes)) do begin
      inc(j);
      if t in CountTypes then
        inc(i);
      t := TopPascalCodeFoldBlockType(j)
    end;
    ResultLvl := i;
    Result := i <= MaxDepth;
  end;

var
  cur: TPascalCodeFoldBlockType;
  CloseCnt, ClosedByNextLine, ClosedInLastLine: Integer;
  i, top, c: Integer;
  t: TSynPasDividerDrawLocation;
begin
  Result := inherited;
  if (index = 0) then exit;
  CloseCnt :=  EndFoldLevel(Index - 1) - MinimumFoldLevel(Index);
  if (CloseCnt = 0) or (MinimumFoldLevel(Index) <> EndFoldLevel(Index)) then // not a mixed line
    exit;

  // SetRange[Index] has the folds at the start of this line
  // ClosedByNextLine: Folds closed by the next lines LastLineFix
  //                   must be taken from SetRange[Index+1] (end of this line)
  ClosedByNextLine := -LastLineFoldLevelFix(Index + 1);
  // ClosedInLastLine: Folds Closed by this lines LastLineFix
  //                   must be ignored. (They are part of SetRange[Index] / this line)
  ClosedInLastLine := -LastLineFoldLevelFix(Index);

  // Get the highest close-offset
  i := ClosedByNextLine - 1;
  if i >= 0 then begin
    SetRange(CurrentRanges[Index]);     // Checking ClosedByNextLine
    top := 0;
  end else begin
    SetRange(CurrentRanges[Index - 1]); // Checking Closed in this Line
    i := CloseCnt - ClosedByNextLine + ClosedInLastLine - 1;
    top := ClosedInLastLine;
  end;

  cur := TopPascalCodeFoldBlockType(i + 1);
  while (i >= top) do begin
    //nxt := cur; // The "next higher" close node compared to current
    cur := TopPascalCodeFoldBlockType(i);
    Result := FDividerDrawConfig[pddlUses].TopSetting; //// xxxx
    case cur of
      cfbtUnitSection:
        if FDividerDrawConfig[pddlUnitSection].MaxDrawDepth > 0 then
          exit(FDividerDrawConfig[pddlUnitSection].TopSetting);
      cfbtUses:
        if FDividerDrawConfig[pddlUses].MaxDrawDepth > 0 then
          exit(FDividerDrawConfig[pddlUses].TopSetting);
      cfbtLocalVarType:
        if CheckFoldNestLevel(FDividerDrawConfig[pddlVarLocal].MaxDrawDepth - 1,
                              i + 2, [cfbtProcedure], cfbtAll, c) then begin
          if c = 0
          then exit(FDividerDrawConfig[pddlVarLocal].TopSetting)
          else exit(FDividerDrawConfig[pddlVarLocal].NestSetting);
        end;
      cfbtVarType:
        if FDividerDrawConfig[pddlVarGlobal].MaxDrawDepth > 0 then
          exit(FDividerDrawConfig[pddlVarGlobal].TopSetting);
      cfbtClass, cfbtRecord:
        begin
          if CheckFoldNestLevel(0, i + 1, [cfbtProcedure],
                                cfbtAll - [cfbtVarType, cfbtLocalVarType], c)
          then t := pddlStructGlobal
          else t := pddlStructLocal;
          if CheckFoldNestLevel(FDividerDrawConfig[t].MaxDrawDepth - 1,
                                i + 1, [cfbtClass, cfbtRecord],
                                cfbtAll - [cfbtVarType, cfbtLocalVarType], c) then begin
            if c = 0
            then exit(FDividerDrawConfig[t].TopSetting)
            else exit(FDividerDrawConfig[t].NestSetting);
          end;
        end;
      cfbtProcedure:
        if CheckFoldNestLevel(FDividerDrawConfig[pddlProcedure].MaxDrawDepth - 1,
                              i + 1, [cfbtProcedure], cfbtAll, c) then begin
          if c = 0
          then exit(FDividerDrawConfig[pddlProcedure].TopSetting)
          else exit(FDividerDrawConfig[pddlProcedure].NestSetting);
        end;
      cfbtTopBeginEnd:
        if FDividerDrawConfig[pddlBeginEnd].MaxDrawDepth > 0 then
          exit(FDividerDrawConfig[pddlBeginEnd].TopSetting);
      cfbtBeginEnd, cfbtRepeat, cfbtCase, cfbtAsm:
        if CheckFoldNestLevel(FDividerDrawConfig[pddlBeginEnd].MaxDrawDepth - 2,
                              i + 1, [cfbtBeginEnd, cfbtRepeat, cfbtCase, cfbtAsm],
                              cfbtAll - [cfbtProcedure, cfbtTopBeginEnd], c) then
          exit(FDividerDrawConfig[pddlBeginEnd].NestSetting);
      cfbtTry:
        if CheckFoldNestLevel(FDividerDrawConfig[pddlTry].MaxDrawDepth - 1,
                              i + 1, [cfbtTry], cfbtAll - [cfbtProcedure], c)
        then begin
          if c = 0
          then exit(FDividerDrawConfig[pddlTry].TopSetting)
          else exit(FDividerDrawConfig[pddlTry].NestSetting);
        end;
    end;
    dec(i);
    if (i < top) and (ClosedByNextLine > 0) then begin
      // Switch to blocks closed in this line
      SetRange(CurrentRanges[Index - 1]);
      i := CloseCnt - ClosedByNextLine + ClosedInLastLine - 1;
      ClosedByNextLine := 0;
      top := ClosedInLastLine;
      cur := TopPascalCodeFoldBlockType(i + 1);
    end;
  end;
  Result := inherited;
end;

procedure TSynPasSyn.CreateDividerDrawConfig;
var
  i: TSynPasDividerDrawLocation;
begin
  for i := low(TSynPasDividerDrawLocation) to high(TSynPasDividerDrawLocation) do
  begin
    FDividerDrawConfig[i] := TSynDividerDrawConfig.Create;
    FDividerDrawConfig[i].OnChange := {$IFDEF FPC}@{$ENDIF}DefHighlightChange;
    FDividerDrawConfig[i].MaxDrawDepth := PasDividerDrawLocationDefaults[i];
  end;
end;

procedure TSynPasSyn.DestroyDividerDrawConfig;
var
  i: TSynPasDividerDrawLocation;
begin
  for i := low(TSynPasDividerDrawLocation) to high(TSynPasDividerDrawLocation) do
    FreeAndNil(FDividerDrawConfig[i]);
end;

function TSynPasSyn.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := inherited GetFoldConfigInstance(Index);
  Result.Enabled := TPascalCodeFoldBlockType(Index) in
    [cfbtBeginEnd, cfbtTopBeginEnd, cfbtNestedComment,
     cfbtProcedure, cfbtUses, cfbtLocalVarType, cfbtClass,
     cfbtClassSection, cfbtRecord, cfbtRepeat, cfbtCase,
     cfbtAsm, cfbtRegion];
  if TPascalCodeFoldBlockType(Index) in
     [cfbtRegion, cfbtNestedComment, cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment]
  then
    Result.SupportedModes := [fmFold, fmHide];
  if TPascalCodeFoldBlockType(Index) in [cfbtSlashComment] then
    Result.Modes := [fmFold, fmHide];
end;

function TSynPasSyn.CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList;
begin
  Result := TSynHighlighterPasRangeList.Create;
end;

function TSynPasSyn.UpdateRangeInfoAtLine(Index: Integer): Boolean;
var
  r: TSynPasRangeInfo;
begin
  Result := inherited;
  r := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[Index];
  Result := Result
        or (FSynPasRangeInfo.EndLevelIfDef <> r.EndLevelIfDef)
        or (FSynPasRangeInfo.MinLevelIfDef <> r.MinLevelIfDef)
        or (FSynPasRangeInfo.EndLevelRegion <> r.EndLevelRegion)
        or (FSynPasRangeInfo.MinLevelRegion <> r.MinLevelRegion);
  TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[Index] := FSynPasRangeInfo;
end;

function TSynPasSyn.GetFoldConfigCount: Integer;
begin
  // excluded cfbtNone;
  Result := ord(high(TPascalCodeFoldBlockType)) -
            ord(low(TPascalCodeFoldBlockType));
end;

function TSynPasSyn.GetFoldConfigInternalCount: Integer;
begin
  // include cfbtNone;
  Result := ord(high(TPascalCodeFoldBlockType)) -
            ord(low(TPascalCodeFoldBlockType))
            + 1;
end;

procedure TSynPasSyn.DoFoldConfigChanged(Sender: TObject);
begin
  FNodeInfoLine := -1;
  inherited DoFoldConfigChanged(Sender);
end;

function TSynPasSyn.GetDividerDrawConfig(Index: Integer): TSynDividerDrawConfig;
begin
  Result := FDividerDrawConfig[TSynPasDividerDrawLocation(Index)];
end;

function TSynPasSyn.GetDividerDrawConfigCount: Integer;
begin
  Result := ord(high(TSynPasDividerDrawLocation))
          - ord(low(TSynPasDividerDrawLocation)) + 1;
end;

function TSynPasSyn.GetFoldNodeInfo(Line, Index: Integer;
  Filter: TSynFoldActions): TSynFoldNodeInfo;
var
  i, j: LongInt;
begin
  if FNodeInfoLine <> Line then begin
    FCatchNodeInfo := True;
    FNodeInfoCount := 0;
    StartAtLineIndex(Line);
    fStringLen := 0;
    NextToEol;
    fStringLen := 0;
    i := LastLineFoldLevelFix(Line+1);
    while i < 0 do begin
      EndPascalCodeFoldBlock;
      inc(i);
    end;
    if Line = CurrentLines.Count - 1 then begin
      // last line, close all folds
      // Run (for LogXStart) is at line-end
      i := FNodeInfoCount;
      while TopPascalCodeFoldBlockType <> cfbtNone do
        EndPascalCodeFoldBlock(True);
      while FSynPasRangeInfo.EndLevelIfDef > 0 do
        EndCustomCodeFoldBlock(cfbtIfDef);
      while FSynPasRangeInfo.EndLevelRegion > 0 do
        EndCustomCodeFoldBlock(cfbtRegion);
      while i < FNodeInfoCount do begin
        FNodeInfoList[i].FoldAction := FNodeInfoList[i].FoldAction + [sfaLastLineClose];
        inc(i);
      end;
    end;
    FCatchNodeInfo := False;
    FNodeInfoLine := Line;
  end;

  if (index < 0) or (index >= FNodeInfoCount) then
    Result := inherited GetFoldNodeInfo(Line, Index, [])
  else
  if Filter = [] then
    Result := FNodeInfoList[Index]
  else
  begin
    Result.FoldAction := [sfaInvalid];
    j := Index;
    for i := 0 to FNodeInfoCount - 1 do
      if FNodeInfoList[i].FoldAction * Filter = Filter then begin
        Result := FNodeInfoList[i];
        if j = 0 then break;
        dec(j);
      end;
  end;
  Result.NodeIndex := Index; // only set copy on result
end;

function TSynPasSyn.GetFoldNodeInfoCount(Line: Integer;
  Filter: TSynFoldActions): Integer;
var
  i: Integer;
begin
  // TODO: assert line > 0
  if FNodeInfoLine <> Line then
    GetFoldNodeInfo(Line, 0, []);
  Result := FNodeInfoCount;
  if Filter <> [] then
    for i := 0 to FNodeInfoCount - 1 do
      if FNodeInfoList[i].FoldAction * Filter <> Filter then
        dec(Result);
end;

function TSynPasSyn.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  Result:=TSynPasSynRange;
end;

function TSynPasSyn.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

  function ReadDelphiSettings(settingIndex: integer): boolean;

    function ReadDelphiSetting(settingTag: string;
      attri: TSynHighlighterAttributes; key: string): boolean;

      function ReadDelphi2Or3(settingTag: string;
        attri: TSynHighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                '\Software\Borland\Delphi\'+settingTag+'\Highlight',name,true);
      end; { ReadDelphi2Or3 }

      function ReadDelphi4OrMore(settingTag: string;
        attri: TSynHighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\Delphi\'+settingTag+'\Editor\Highlight',
               key,false);
      end; { ReadDelphi4OrMore }

    begin { ReadDelphiSetting }
      try
        if (settingTag[1] = '2') or (settingTag[1] = '3')
          then Result := ReadDelphi2Or3(settingTag,attri,key)
          else Result := ReadDelphi4OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadDelphiSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpAsmAttri       : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    {$IFDEF SYN_LAZARUS}
    tmpDirectiveAttri : TSynHighlighterAttributes;
    {$ENDIF}
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    s                 : TStringList;

  begin { ReadDelphiSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if (settingIndex < 0) or (settingIndex >= s.Count) then Result := false
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('');
        tmpAsmAttri       := TSynHighlighterAttributes.Create('');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('');
        {$IFDEF SYN_LAZARUS}
        tmpDirectiveAttri := TSynHighlighterAttributes.Create('');
        {$ENDIF}
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('');
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpAsmAttri       .Assign(fAsmAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        {$IFDEF SYN_LAZARUS}
        tmpDirectiveAttri .Assign(fDirectiveAttri);
        {$ENDIF}
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        Result := ReadDelphiSetting(s[settingIndex],fAsmAttri,'Assembler')
              and ReadDelphiSetting(s[settingIndex],fCommentAttri,'Comment')
              {$IFDEF SYN_LAZARUS}
              and ReadDelphiSetting(s[settingIndex],fDirectiveAttri,'Directive')
              {$ENDIF}
              and ReadDelphiSetting(s[settingIndex],fIdentifierAttri,'Identifier')
              and ReadDelphiSetting(s[settingIndex],fKeyAttri,'Reserved word')
              and ReadDelphiSetting(s[settingIndex],fNumberAttri,'Number')
              and ReadDelphiSetting(s[settingIndex],fSpaceAttri,'Whitespace')
              and ReadDelphiSetting(s[settingIndex],fStringAttri,'string')
              and ReadDelphiSetting(s[settingIndex],fSymbolAttri,'Symbol');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fAsmAttri       .Assign(tmpAsmAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          FIDEDirectiveAttri.Assign(tmpCommentAttri);
          {$IFDEF SYN_LAZARUS}
          fDirectiveAttri .Assign(tmpDirectiveAttri);
          {$ENDIF}
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpAsmAttri       .Free;
        tmpCommentAttri   .Free;
        {$IFDEF SYN_LAZARUS}
        tmpDirectiveAttri .Free;
        {$ENDIF}
        tmpIdentifierAttri.Free;
        tmpSpaceAttri     .Free;
      end;
    finally s.Free; end;
  end; { ReadDelphiSettings }

begin
  Result := ReadDelphiSettings(settingIndex);
end; { TSynPasSyn.UseUserSettings }

function TSynPasSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynPasSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynPasSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

{begin}                                                                         //mh 2000-10-08
function TSynPasSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynPasSyn.CreateRootCodeFoldBlock;
begin
  inherited;
  RootCodeFoldBlock.InitRootBlockType(Pointer(PtrInt(cfbtNone)));
end;

function TSynPasSyn.IsKeyword(const AKeyword: string): boolean;
// returns true for some common keywords
// Note: this words are not always keywords (e.g. end), and some keywords are
// not listed here at all (e.g. static)
var
  i: integer;
  m: TPascalCompilerMode;
begin
  if KeywordsList = nil then begin
    KeywordsList := TStringList.Create;
    for i := 1 to High(RESERVED_WORDS_TP) do
      KeywordsList.AddObject(RESERVED_WORDS_TP[i], TObject(pcmTP));
    for i := 1 to High(RESERVED_WORDS_DELPHI) do
      KeywordsList.AddObject(RESERVED_WORDS_DELPHI[i], TObject(pcmDelphi));
    for i := 1 to High(RESERVED_WORDS_FPC) do
      KeywordsList.AddObject(RESERVED_WORDS_FPC[i], TObject(pcmFPC));
    KeywordsList.Sorted := true;
  end;
  Result := KeywordsList.Find(LowerCase(AKeyword), i);
  if not Result then exit;
  m := TPascalCompilerMode(PtrUInt(KeywordsList.Objects[i]));
  case FCompilerMode of
    pcmFPC, pcmObjFPC: ;
    pcmDelphi: Result := m in [pcmTP, pcmDelphi];
    else Result := m = pcmTP;
  end;
end;

{end}                                                                           //mh 2000-10-08

procedure TSynPasSyn.SetD4syntax(const Value: boolean);
begin
  FD4syntax := Value;
end;

{ TSynFreePascalSyn }

constructor TSynFreePascalSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CompilerMode:=pcmObjFPC;
end;

procedure TSynFreePascalSyn.ResetRange;
begin
  inherited ResetRange;
  CompilerMode:=pcmObjFPC;
end;

{ TSynPasSynRange }

procedure TSynPasSynRange.Clear;
begin
  inherited Clear;
  FBracketNestLevel := 0;
  FLastLineCodeFoldLevelFix := 0;
  FPasFoldEndLevel := 0;
  FPasFoldFixLevel := 0;
  FPasFoldMinLevel := 0;
end;

function TSynPasSynRange.Compare(Range: TSynCustomHighlighterRange): integer;
begin
  Result:=inherited Compare(Range);
  if Result<>0 then exit;

  Result:=ord(FMode)-ord(TSynPasSynRange(Range).FMode);
  if Result<>0 then exit;
  Result := FBracketNestLevel - TSynPasSynRange(Range).FBracketNestLevel;
  if Result<>0 then exit;
  Result := FLastLineCodeFoldLevelFix - TSynPasSynRange(Range).FLastLineCodeFoldLevelFix;
  if Result<>0 then exit;
  Result := FPasFoldEndLevel - TSynPasSynRange(Range).FPasFoldEndLevel;
  if Result<>0 then exit;
  Result := FPasFoldMinLevel - TSynPasSynRange(Range).FPasFoldMinLevel;
  if Result<>0 then exit;
  Result := FPasFoldFixLevel - TSynPasSynRange(Range).FPasFoldFixLevel;
end;

procedure TSynPasSynRange.Assign(Src: TSynCustomHighlighterRange);
begin
  if (Src<>nil) and (Src<>TSynCustomHighlighterRange(NullRange)) then begin
    inherited Assign(Src);
    FMode:=TSynPasSynRange(Src).FMode;
    FBracketNestLevel:=TSynPasSynRange(Src).FBracketNestLevel;
    FLastLineCodeFoldLevelFix := TSynPasSynRange(Src).FLastLineCodeFoldLevelFix;
    FPasFoldEndLevel := TSynPasSynRange(Src).FPasFoldEndLevel;
    FPasFoldMinLevel := TSynPasSynRange(Src).FPasFoldMinLevel;
    FPasFoldFixLevel := TSynPasSynRange(Src).FPasFoldFixLevel;
  end;
end;

function TSynPasSynRange.Add(ABlockType: Pointer; IncreaseLevel: Boolean): TSynCustomCodeFoldBlock;
begin
  Result := inherited Add(ABlockType, True);
  if IncreaseLevel and assigned(result) then
    inc(FPasFoldEndLevel);
end;

procedure TSynPasSynRange.Pop(DecreaseLevel: Boolean);
begin
  if assigned(Top.Parent) then begin
    if DecreaseLevel then
      dec(FPasFoldEndLevel);
    if FPasFoldMinLevel > FPasFoldEndLevel then
      FPasFoldMinLevel := FPasFoldEndLevel;
  end;
  inherited Pop(True);
end;

function TSynPasSynRange.MaxFoldLevel: Integer;
begin
  // Protect from overly mem consumption, by too many nested folds
  Result := 100;
end;

procedure TSynPasSynRange.IncBracketNestLevel;
begin
  inc(FBracketNestLevel);
end;

procedure TSynPasSynRange.DecBracketNestLevel;
begin
  dec(FBracketNestLevel);
end;

procedure TSynPasSynRange.DecLastLineCodeFoldLevelFix;
begin
  dec(FLastLineCodeFoldLevelFix)
end;

procedure TSynPasSynRange.DecLastLinePasFoldFix;
begin
  dec(FPasFoldFixLevel);
end;

{ TSynHighlighterPasRangeList }

function TSynHighlighterPasRangeList.GetTSynPasRangeInfo(Index: Integer): TSynPasRangeInfo;
begin
  if (Index < 0) or (Index >= Count) then begin
    Result.MinLevelRegion := 0;
    Result.EndLevelRegion := 0;
    Result.MinLevelIfDef := 0;
    Result.EndLevelIfDef := 0;
    exit;
  end;
  Result := TSynPasRangeInfo((ItemPointer[Index] + FItemOffset)^);
end;

procedure TSynHighlighterPasRangeList.SetTSynPasRangeInfo(Index: Integer;
  const AValue: TSynPasRangeInfo);
begin
  TSynPasRangeInfo((ItemPointer[Index] + FItemOffset)^) := AValue;
end;

constructor TSynHighlighterPasRangeList.Create;
begin
  inherited;
  FItemOffset := ItemSize;
  ItemSize := FItemOffset + SizeOf(TSynPasRangeInfo);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynPasSyn);
{$ENDIF}

finalization
  FreeAndNil(KeywordsList);

end.


