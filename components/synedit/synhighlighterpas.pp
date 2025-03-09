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
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Classes, fgl, Registry, Graphics, SynEditHighlighterFoldBase,
  SynEditMiscProcs, SynEditTypes, SynEditHighlighter, SynEditTextBase,
  SynEditStrConst, SynEditMiscClasses, LazLoggerBase, LazEditMiscProcs;

type
  TSynPasStringMode = (spsmDefault, spsmStringOnly, spsmNone);
  TSynPasMultilineStringMode  = (spmsmDoubleQuote);
  TSynPasMultilineStringModes = set of TSynPasMultilineStringMode;

  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkModifier, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkDirective, tkIDEDirective,
    tkUnknown);
  TtkTokenKinds= set of TtkTokenKind;

  TRangeState = (
    rsAnsiMultiDQ,  // Multi line double quoted string

    // rsAnsi, rsBor, rsDirective are exclusive to each other
    rsAnsi,         // *) comment
    rsBor,          // { comment
    rsSlash,        // //
    rsIDEDirective, // {%
    rsDirective,    // {$
    rsAsm,          // assembler block
    rsProperty,
    rsAtPropertyOrReadWrite, // very first word after property (name of the property) or after read write in property (or after any operator within a property "default 0 + 1" (next token can **not** be read/write/index
    rsInterface,
    rsImplementation,   // Program or Implementation
    rsCompilerModeSet,  // there was an explicit {$mode ...

    // we need to detect:    type TFoo = procedure; // must not fold
    //                       var  foo: procedure;   // must not fold
    rsAfterSemiColon,   // very first word after ";"
    rsAfterEqualOrColon,   // very first word after "=" or ":"
    rsAfterEqual,          // between "=" and ";" (or block end) // a ^ means ctrl-char, not pointer to type

    // Detect if class/object is   type TFoo = class; // forward declaration
    //                                  TBar = class of TFoo;
    // or full class declaration  TFoo = class ... end;
    // Also included after class modifiers "sealed" and "abstract"
    rsAtClass,
    rsInObjcProtocol,
    rsAfterClass,
    rsInTypeHelper,
    rsAfterIdentifierOrValue, // anywhere where a ^ deref can happen "foo^", "foo^^", "foo()^", "foo[]^"

    rsAtCaseLabel,
    rsInProcHeader,       // Declaration or implementation header of a Procedure, function, constructor...
    rsWasInProcHeader,    // after the semicolon that ended a "ProcHeader / proc-modifiers are possible
    rsAfterClassMembers,  // Encountered a procedure, function, property, constructor or destructor in a class
    rsAfterClassField,    // after ";" of a field (static needs highlight)
    rsInRaise,            // between raise and either ";" or "at"
    rsVarTypeInSpecification, // between ":"/"=" and ";" in a var or type section (or class members)
                              // var a: Integer; type b = Int64;
    rsInTypedConst,
    rsSkipAllPasBlocks        // used for: class of ... ;
  );
  TRangeStates = set of TRangeState;

  // Just for the current token, unless renewed during parsing for the next token
  // Except, will be kept for: tkSpace, tkComment, tkIDEDirective, tkDirective, tkNull  // maybe in future line break
  TTokenState = (
    tsNone,
    tsAtProcName,         // procedure ___
                          // unit ____    // used for "deprecated" detection / check in tsAfterProcName
                          //    >>> after a procedure/function/... keyword, when the name is expected (not for types)
                          //    >>> renewed after dot "."
    tsAfterProcName,      // procedure NAME
                          // unit NAME    // used for "deprecated" detection
    tsAfterEqualThenType, // TFoo = type
                          //    >>> ONLY if type-helper enabled
    tsAfterAbsolute,      // var x absolute y;
    tsAfterExternal,      // procedure Foo; public name 'bar';
                          // procedure Foo; external 'x' name 'bar';
                          // var Foo; public name 'bar';
                          //    after public, export or external: "name" may follow
                          //    >>> KEPT until ONE AFTER the ";" => to prevent next token from being mistaken
                          //    >>> Also SET BY "var"/"type"/"const" => to prevent next token from being mistaken
    tsAfterCvar,          // cvar;
                          //    >>> KEPT until ONE AFTER the ";" => to prevent next token from being mistaken
    tsAfterTypedConst,    // const foo: ___=___; public;
                          //    >>> typed const can have modifiers
                          //        Set AFTER ";"
    tsAfterRaise          // After the raise keyword (or "." or operator inside rsInRaise)
  );

type
  TPascalCodeFoldBlockType = ( // Do *not* change the order
    cfbtBeginEnd,      // Nested
    cfbtTopBeginEnd,   // Begin of Procedure
    cfbtNestedComment,
    cfbtProcedure,
    cfbtUses,
    cfbtVarBlock,        // Var, ResourceString, Label // Config shared with Const/Type-Block
    cfbtLocalVarBlock,
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
    cfbtIfThen,
    cfbtForDo,
    cfbtWhileDo,
    cfbtWithDo,
    cfbtIfElse,
    cfbtRecordCase,
    cfbtRecordCaseSection,
    cfbtAnonymousProcedure,
    // Internal type / not configurable
    cfbtCaseElse,     // "else" in case can have multiply statements
    cfbtPackage,
    //cfbtIfThen,
    cfbtConstBlock,
    cfbtLocalConstBlock,
    cfbtClassConstBlock, // in class and record section
    cfbtTypeBlock,
    cfbtLocalTypeBlock,
    cfbtClassTypeBlock, // in class and record section
    cfbtNone
    );
  TPascalCodeFoldBlockTypes = set of TPascalCodeFoldBlockType;


const
  cfbtLastPublic = cfbtAnonymousProcedure;
  cfbtFirstPrivate = cfbtCaseElse;

  cfbtVarType      = cfbtVarBlock      deprecated 'use cfbtVarBlock / To be removed in 5.99';
  cfbtLocalVarType = cfbtLocalVarBlock deprecated 'use cfbtLocalVarBlock / To be removed in 5.99';

  CountPascalCodeFoldBlockOffset =
    Pointer(PtrInt(Integer(high(TPascalCodeFoldBlockType))+1));

  cfbtAll = TPascalCodeFoldBlockTypes(
    [low(TPascalCodeFoldBlockType)..high(TPascalCodeFoldBlockType)]);

  PascalWordTripletRanges = TPascalCodeFoldBlockTypes(
    [cfbtBeginEnd, cfbtTopBeginEnd, cfbtProcedure, cfbtAnonymousProcedure, cfbtClass, cfbtProgram,
     cfbtRecord, cfbtRecordCase, // TODO recordcase needs fmMarkup
     cfbtTry, cfbtExcept, cfbtRepeat, cfbtAsm, cfbtCase, cfbtCaseElse,
     cfbtIfDef, cfbtRegion,
     cfbtIfThen, cfbtForDo,cfbtWhileDo,cfbtWithDo
    ]);
  PascalNoOutlineRanges = TPascalCodeFoldBlockTypes(
    [cfbtProgram,cfbtUnit,cfbtUnitSection, cfbtRegion, //cfbtProcedure,//=need by nested proc?
      cfbtVarBlock, cfbtConstBlock, cfbtClassConstBlock, cfbtTypeBlock, cfbtClassTypeBlock,
      cfbtCaseElse,
      cfbtIfDef, cfbtAnsiComment,cfbtBorCommand,cfbtSlashComment, cfbtNestedComment]);

  // restrict cdecl etc to places where they can be.
  // this needs a better parser
  ProcModifierAllowed = TPascalCodeFoldBlockTypes(
    [cfbtNone, cfbtProcedure, cfbtProgram, cfbtClass, cfbtClassSection, cfbtRecord,
     cfbtUnitSection, // unitsection, actually interface only
     cfbtVarBlock, cfbtLocalVarBlock,
     cfbtConstBlock, cfbtLocalConstBlock, cfbtClassConstBlock,
     cfbtTypeBlock, cfbtLocalTypeBlock, cfbtClassTypeBlock
    ]);
  ProcModifierAllowedNoVar = TPascalCodeFoldBlockTypes(
    [cfbtNone, cfbtProcedure, cfbtProgram, cfbtClass, cfbtClassSection, cfbtRecord,
     cfbtUnitSection // unitsection, actually interface only
    ]);

  PascalStatementBlocks = TPascalCodeFoldBlockTypes(
    [cfbtBeginEnd, cfbtTopBeginEnd, cfbtCase, cfbtTry, cfbtExcept, cfbtRepeat,
     cfbtCaseElse, cfbtIfThen, cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfElse ]);

  cfbtEssential = TPascalCodeFoldBlockTypes([
    cfbtClass, cfbtClassSection, cfbtRecord,
    cfbtUnitSection, cfbtProcedure, cfbtProgram, cfbtPackage,
    cfbtVarBlock, cfbtLocalVarBlock,
    cfbtConstBlock, cfbtLocalConstBlock, cfbtClassConstBlock,
    cfbtTypeBlock, cfbtLocalTypeBlock, cfbtClassTypeBlock,
    cfbtAsm,
    cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment, cfbtNestedComment,
    cfbtCase, // for case-else
    cfbtTry // only if cfbtExcept
    ]
    + ProcModifierAllowed + PascalStatementBlocks
    // the following statementblocks can only be nested in another statement.
    - [cfbtBeginEnd, cfbtCase, {cfbtTry,} cfbtExcept, cfbtRepeat,
       cfbtCaseElse, cfbtIfThen, cfbtForDo,cfbtWhileDo,cfbtWithDo ]);

  cfbtVarConstType = TPascalCodeFoldBlockTypes([
    cfbtVarBlock, cfbtLocalVarBlock,
    cfbtConstBlock, cfbtLocalConstBlock,
    cfbtTypeBlock, cfbtLocalTypeBlock]);

  cfbtVarConstTypeExt = TPascalCodeFoldBlockTypes([
    cfbtVarBlock, cfbtLocalVarBlock,
    cfbtConstBlock, cfbtLocalConstBlock, cfbtClassConstBlock,
    cfbtTypeBlock, cfbtLocalTypeBlock, cfbtClassTypeBlock]);

  //cfbtClassOrRecord

  PascalFoldTypeConfigMap: Array [TPascalCodeFoldBlockType] of TPascalCodeFoldBlockType =
    ( cfbtBeginEnd,      // Nested
      cfbtTopBeginEnd,   // Begin of Procedure
      cfbtNestedComment,
      cfbtProcedure,
      cfbtUses,
      cfbtVarBlock,        // Var, ResourceString, Label // Config shared with Const/Type-Block
      cfbtLocalVarBlock,
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
      cfbtIfThen,
      cfbtForDo,
      cfbtWhileDo,
      cfbtWithDo,
      cfbtIfElse,
      cfbtRecordCase,
      cfbtRecordCaseSection,
      cfbtAnonymousProcedure,
      // Internal type / not configurable
      cfbtCaseElse,     // "else" in case can have multiply statements
      cfbtPackage,
      //cfbtIfThen,
      cfbtVarBlock,      // cfbtConstBlock,
      cfbtLocalVarBlock, // cfbtLocalConstBlock,
      cfbtClassConstBlock,
      cfbtVarBlock,      // cfbtTypeBlock,
      cfbtLocalVarBlock, // cfbtLocalTypeBlock,
      cfbtClassTypeBlock,
      //cfbtConstBlock,
      //cfbtLocalConstBlock,
      //cfbtClassConstBlock, // in class and record section
      //cfbtTypeBlock,
      //cfbtLocalTypeBlock,
      //cfbtClassTypeBlock, // in class and record section
      cfbtNone
    );

  PascalFoldTypeCompatibility: Array [TPascalCodeFoldBlockType] of TPascalCodeFoldBlockType =
    ( cfbtBeginEnd,      // Nested
      cfbtBeginEnd,  // cfbtTopBeginEnd,   // Begin of Procedure
      cfbtNestedComment,
      cfbtProcedure,
      cfbtUses,
      cfbtVarBlock,
      cfbtVarBlock, // cfbtLocalVarBlock,
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
      cfbtIfThen,
      cfbtForDo,
      cfbtWhileDo,
      cfbtWithDo,
      cfbtIfElse,
      cfbtRecordCase,
      cfbtRecordCaseSection,
      cfbtProcedure,
      // Internal type / not configurable
      cfbtCaseElse,
      cfbtPackage,
      cfbtConstBlock,
      cfbtConstBlock, // cfbtLocalConstBlock,
      cfbtConstBlock, // cfbtClassConstBlock,
      cfbtTypeBlock,
      cfbtTypeBlock, // cfbtLocalTypeBlock,
      cfbtTypeBlock, //cfbtClassTypeBlock,
      cfbtNone
    );

  FOLDGROUP_PASCAL = 1;
  FOLDGROUP_REGION = 2;
  FOLDGROUP_IFDEF  = 3;


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

  { TSynPasSynCustomToken }

  TSynPasSynCustomToken = class
  private
    FOnChange: TNotifyEvent;
    FOnMarkupChange: TNotifyEvent;
    procedure DoMarkupChaged(Sender: TObject);
    procedure DoTokensChanged(Sender: TObject);
  private
    FMarkup: TSynHighlighterAttributesModifier;
    FMatchTokenKinds: TtkTokenKinds;
    FTokens: TStrings;

    procedure SetMatchTokenKinds(AValue: TtkTokenKinds);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMarkupChange: TNotifyEvent read FOnMarkupChange write FOnMarkupChange;
  public
    constructor Create;
    destructor Destroy; override;
    property MatchTokenKinds: TtkTokenKinds read FMatchTokenKinds write SetMatchTokenKinds;
    property Tokens: TStrings read FTokens;
    property Markup: TSynHighlighterAttributesModifier read FMarkup;
  end;

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
    FNestedComments: Boolean;
    FBracketNestLevel : Integer;
    FLastLineCodeFoldLevelFix: integer;
    FPasFoldFixLevel: Smallint;
    FTypeHelpers: Boolean;
    FTokenState: TTokenState;
  public
    procedure Clear; override;
    function Compare(Range: TSynCustomHighlighterRange): integer; override;
    procedure Assign(Src: TSynCustomHighlighterRange); override;
    function MaxFoldLevel: Integer; override;
    procedure IncBracketNestLevel;
    procedure DecBracketNestLevel;
    procedure DecLastLineCodeFoldLevelFix;
    procedure DecLastLinePasFoldFix;
    property Mode: TPascalCompilerMode read FMode write FMode;
    property NestedComments: Boolean read FNestedComments write FNestedComments;
    property TypeHelpers: Boolean read FTypeHelpers write FTypeHelpers;
    property BracketNestLevel: integer read FBracketNestLevel write FBracketNestLevel;
    property LastLineCodeFoldLevelFix: integer
      read FLastLineCodeFoldLevelFix write FLastLineCodeFoldLevelFix;
    property PasFoldFixLevel: Smallint read FPasFoldFixLevel write FPasFoldFixLevel;
    property TokenState: TTokenState read FTokenState write FTokenState;
  end;

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  { TSynPasSyn }

  TSynPasSyn = class(TSynCustomFoldHighlighter)
  private type

    { TSynPasSynCustomTokenInfo }

    TSynPasSynCustomTokenInfo = record
      MatchTokenKinds: TtkTokenKinds;
      Word: String;
      Token: TSynPasSynCustomToken;
      class operator = (a, b: TSynPasSynCustomTokenInfo): boolean;
    end;
    PSynPasSynCustomTokenInfo = ^TSynPasSynCustomTokenInfo;
    PPSynPasSynCustomTokenInfo = ^PSynPasSynCustomTokenInfo;
    TSynPasSynCustomTokenInfoList = specialize TFPGList<TSynPasSynCustomTokenInfo>;
  private
    FSynCustomTokens: array of TSynPasSynCustomToken;
    FNeedCustomTokenBuild: boolean;
    FCustomTokenInfo: array [byte] of record
      MatchTokenKinds: TtkTokenKinds;
      List: TSynPasSynCustomTokenInfoList;
    end;
    FCustomTokenMarkup: TSynHighlighterAttributesModifier;
    FCustomTokenMergedMarkup: TSynSelectedColorMergeResult;

    fAsmStart: Boolean;
    FExtendedKeywordsMode: Boolean;
    FNestedComments: boolean;
    FUsePasDoc, FIsPasDocKey, FIsPasUnknown, FIsPasDocSym, FIsInSlash: Boolean;
    FPasDocWordList: TStringList;
    fPasDocKeyWordAttri: TSynHighlighterAttributesModifier;
    fPasDocSymbolAttri: TSynHighlighterAttributesModifier;
    fPasDocUnknownAttr: TSynHighlighterAttributesModifier;
    FProcedureHeaderNameAttr: TSynHighlighterAttributesModifier;
    FCurProcedureHeaderNameAttr: TSynSelectedColorMergeResult;
    FStartCodeFoldBlockLevel: integer; // TODO: rename FStartNestedFoldBlockLevel
    FPasStartLevel: Smallint;
    fRange: TRangeStates;
    FOldRange: TRangeStates;
    FTokenState, FNextTokenState: TTokenState;
    FStringKeywordMode: TSynPasStringMode;
    FStringMultilineMode: TSynPasMultilineStringModes;
    FSynPasRangeInfo: TSynPasRangeInfo;
    FAtLineStart: Boolean; // Line had only spaces or comments sofar
    fLineStr: string;
    fLine: PChar;
    fLineLen: integer;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    FTypeHelpers: boolean;
    Run: LongInt;// current parser postion in fLine
    fStringLen: Integer;// current length of hash
    fToIdent: integer;// start of current identifier in fLine
    fIdentFuncTable: array[0..220] of TIdentFuncTableFunc;
    fTokenPos: Integer;// start of current token in fLine
    FTokenID: TtkTokenKind;
    FTokenHashKey: Integer;
    FTokenFlags: set of (tfProcName);
    FTokenIsCaseLabel: Boolean;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fModifierAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    FIDEDirectiveAttri: TSynHighlighterAttributesModifier;
    FCurIDEDirectiveAttri: TSynSelectedColorMergeResult;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    FCaseLabelAttri: TSynHighlighterAttributesModifier;
    FCurCaseLabelAttri: TSynSelectedColorMergeResult;
    FCurPasDocAttri: TSynSelectedColorMergeResult;
    fDirectiveAttri: TSynHighlighterAttributes;
    FCompilerMode: TPascalCompilerMode;
    fD4syntax: boolean;
    // Divider
    FDividerDrawConfig: Array [TSynPasDividerDrawLocation] of TSynDividerDrawConfig;

    procedure DoCustomTokenChanged(Sender: TObject);
    procedure RebuildCustomTokenInfo;
    function  GetCustomTokenCount: integer;
    procedure SetCustomTokenCount(AValue: integer);
    function  GetCustomTokens(AnIndex: integer): TSynPasSynCustomToken;
    function GetPasCodeFoldRange: TSynPasSynRange;
    procedure PasDocAttrChanged(Sender: TObject);
    procedure SetCompilerMode(const AValue: TPascalCompilerMode);
    procedure SetExtendedKeywordsMode(const AValue: Boolean);
    procedure SetNestedComments(const ANestedComments: boolean);
    procedure SetStringKeywordMode(const AValue: TSynPasStringMode);
    procedure SetStringMultilineMode(const AValue: TSynPasMultilineStringModes);
    function TextComp(aText: PChar): Boolean;
    function KeyHash: Integer;
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
    function Func108: TtkTokenKind; // "operator"
    function Func111: TtkTokenKind; // "vectorcall"
    function Func112: TtkTokenKind; // "requires"
    function Func117: TtkTokenKind;
    function Func122: TtkTokenKind; // "otherwise"
    function Func124: TtkTokenKind;
    function Func125: TtkTokenKind;
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
    function Func170: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func181: TtkTokenKind;
    function Func191: TtkTokenKind;
    function Func220: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(p: integer): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    function  CheckPasDoc(APeekOnly: Boolean = False): boolean;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure GreaterProc;
    procedure CRProc;
    procedure DirectiveProc;
    procedure IdentProc;
    procedure HexProc;
    procedure BinaryProc;
    procedure OctalProc;
    procedure LFProc;
    procedure LowerProc;
    procedure CaretProc;
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
    procedure DoubleQuoteProc;
    procedure StringProc_MultiLineDQ;
    procedure SymbolProc;
    function TypeHelpersIsStored: Boolean;
    procedure UnknownProc;
    procedure SetD4syntax(const Value: boolean);
    // Divider
    procedure CreateDividerDrawConfig;
    procedure DestroyDividerDrawConfig;
  protected
    function KeyComp(const aKey: string): Boolean;
    function KeyCompEx(AText1, AText2: pchar; ALen: Integer): Boolean;
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: boolean; override;                                 //mh 2000-10-08
  protected
    procedure DoAfterOperator; inline;
    procedure EndStatement(ACurTfb: TPascalCodeFoldBlockType;
                           ACloseFolds: TPascalCodeFoldBlockTypes); inline;
    procedure EndStatementLastLine(ACurTfb: TPascalCodeFoldBlockType;
                           ACloseFolds: TPascalCodeFoldBlockTypes); inline;
    // "Range"
    function GetRangeClass: TSynCustomHighlighterRangeClass; override;
    procedure CreateRootCodeFoldBlock; override;
    function CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList; override;
    function UpdateRangeInfoAtLine(Index: Integer): Boolean; override; // Returns true if range changed

    property PasCodeFoldRange: TSynPasSynRange read GetPasCodeFoldRange;
    function TopPascalCodeFoldBlockType
             (DownIndex: Integer = 0): TPascalCodeFoldBlockType;

    // Open/Close Folds
    procedure GetTokenBounds(out LogX1,LogX2: Integer); override;
    function ScanAheadForNextToken(RunOffs: Integer;
                                   out AFndLine: String; out ATokStart, ATokLen: integer;
                                   MaxLineCnt: Integer = 1000): Boolean; //inline;
    function IsAnonymousFunc(RunOffs: Integer; AnIsFunction: boolean): Boolean;

    function StartPascalCodeFoldBlock
             (ABlockType: TPascalCodeFoldBlockType; ForceDisabled: Boolean = False
              ): TSynCustomCodeFoldBlock;
    procedure EndPascalCodeFoldBlock(NoMarkup: Boolean = False; UndoInvalidOpen: Boolean = False);
    procedure CloseBeginEndBlocksBeforeProc;
    procedure SmartCloseBeginEndBlocks(SearchFor: TPascalCodeFoldBlockType);
    procedure EndPascalCodeFoldBlockLastLine;
    procedure StartCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
    procedure EndCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
    function CloseOneFold(ACurTfb: TPascalCodeFoldBlockType; ACloseFold: TPascalCodeFoldBlockType): TPascalCodeFoldBlockType; inline;
    function CloseOneFold(ACurTfb: TPascalCodeFoldBlockType; ACloseFolds: TPascalCodeFoldBlockTypes): TPascalCodeFoldBlockType; inline;
    function CloseFolds(ACurTfb: TPascalCodeFoldBlockType; ACloseFolds: TPascalCodeFoldBlockTypes): TPascalCodeFoldBlockType; inline;
    procedure CollectNodeInfo(FinishingABlock: Boolean; ABlockType: Pointer;
      LevelChanged: Boolean); override;

    // Info about Folds
    function CreateFoldNodeInfoList: TLazSynFoldNodeInfoList; override;
    procedure ScanFoldNodeInfo(); override;
    procedure DoInitNode(var Node: TSynFoldNodeInfo;
                       //EndOffs: Integer;
                       FinishingABlock: Boolean;
                       ABlockType: Pointer; aActions: TSynFoldActions;
                       AIsFold: Boolean); override;

  protected
    function LastLinePasFoldLevelFix(Index: Integer; AType: Integer = 1;
                                     AIncludeDisabled: Boolean = False): integer; // TODO deprecated; // foldable nodes

    // Divider
    function GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting; override;
    function GetDividerDrawConfig(Index: Integer): TSynDividerDrawConfig; override;
    function GetDividerDrawConfigCount: Integer; override;

    // Fold Config
    function CreateFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
      override;
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; override;
    procedure DoFoldConfigChanged(Sender: TObject); override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Loading; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function GetTokenLen: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;

    procedure ResetRange; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
    procedure StartAtLineIndex(LineNumber:Integer); override; // 0 based
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;

    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;

    function IsLineStartingInDirective(ALineIndex: TLineIdx): Boolean;
    // Info about Folds
    //function FoldBlockOpeningCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    //function FoldBlockClosingCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockEndLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer; out
      AType: Pointer; const AFilter: TSynFoldBlockFilter): boolean; override; overload;

    function FoldTypeCount: integer; override;
    function FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;                // accesses FoldNodeInfo
             UseCloseNodes: boolean = false): integer; override;
    function FoldLineLength(ALineIndex, FoldIndex: Integer): integer; override; // accesses FoldNodeInfo
    function FoldEndLine(ALineIndex, FoldIndex: Integer): integer; override;    // accesses FoldNodeInfo

    property CustomTokenCount: integer read GetCustomTokenCount write SetCustomTokenCount;
    property CustomTokens[AnIndex: integer]: TSynPasSynCustomToken read GetCustomTokens;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IDEDirectiveAttri: TSynHighlighterAttributesModifier read FIDEDirectiveAttri
      write FIDEDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property ModifierAttri: TSynHighlighterAttributes read fModifierAttri write fModifierAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property ProcedureHeaderName: TSynHighlighterAttributesModifier read FProcedureHeaderNameAttr
      write FProcedureHeaderNameAttr;
    property CaseLabelAttri: TSynHighlighterAttributesModifier read FCaseLabelAttri
      write FCaseLabelAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
    property CompilerMode: TPascalCompilerMode read FCompilerMode write SetCompilerMode;
    property NestedComments: boolean read FNestedComments write SetNestedComments;
    property TypeHelpers: boolean read FTypeHelpers write FTypeHelpers stored TypeHelpersIsStored;
    property D4syntax: boolean read FD4syntax write SetD4syntax default true;
    property ExtendedKeywordsMode: Boolean
             read FExtendedKeywordsMode write SetExtendedKeywordsMode default False;
    property StringKeywordMode: TSynPasStringMode
             read FStringKeywordMode write SetStringKeywordMode default spsmDefault;
    property StringMultilineMode: TSynPasMultilineStringModes
             read FStringMultilineMode write SetStringMultilineMode;

    property PasDocKeyWord: TSynHighlighterAttributesModifier read fPasDocKeyWordAttri write fPasDocKeyWordAttri;
    property PasDocSymbol: TSynHighlighterAttributesModifier read fPasDocSymbolAttri write fPasDocSymbolAttri;
    property PasDocUnknown: TSynHighlighterAttributesModifier read fPasDocUnknownAttr write fPasDocUnknownAttr;
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

  RESERVED_PASDOC: array [1..51] of String = ( // all lowercase
    'abstract',
    'anchor',
    'author',
    'bold',
    'br',
    'cell',
    'classname',
    'code',
    'created',
    'cvs',
    'definitionlist',
    'deprecated',
    'exclude',
    'false',
    'html',
    'image',
    'include',
    'includecode',
    'inherited',
    'inheritedclass',
    'italic',
    'item',
    'itemlabel',
    'itemsetnumber',
    'itemspacing',
    'lastmod',
    'latex',
    'link',
    'longcode',
    'member',
    'name',
    'nil',
    'noautolink',
    'noautolinkhere',
    'orderedlist',
    'param',
    'preformatted',
    'raises',
    'return',
    'returns',
    'row',
    'rowhead',
    'section',
    'seealso',
    'shorttitle',
    'table',
    'tableofcontents',
    'title',
    'true',
    'unorderedlist',
    'value'
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

function dbgs(FoldType: TPascalCodeFoldBlockType): String; overload;
begin
  WriteStr(Result, FoldType);
end;

function dbgs(RsState: TRangeState): String; overload;
begin
  WriteStr(Result, RsState);
end;
function dbgs(Range: TRangeStates): String; overload;
var
  i: TRangeState;
begin
  Result := '[';
  for i := low(TRangeState) to high(TRangeState) do
    if i in Range then begin
      if Result <> '[' then Result := Result + ', ';
      Result := Result + dbgs(i);
    end;
  Result := Result + ']';
end;

function dbgs(TkState: TTokenState): String; overload;
begin
  WriteStr(Result, TkState);
end;

function dbgs(TkKind: TtkTokenKind): String; overload;
begin
  WriteStr(Result, TkKind);
end;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z', #128..#255: Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I of
      'a'..'z', 'A'..'Z': mHashTable[I] := Ord(J) - 64;
      '_': mHashTable[I] := 27; // after Z
      '0'..'9': mHashTable[I] := Ord(J) - 48 + 28; // after "_"
    else
      mHashTable[Char(I)] := 0;
    end;
    IsIntegerChar[I]:=(I in ['0'..'9', 'A'..'F', 'a'..'f']);
    IsNumberChar[I]:=(I in ['0'..'9']);
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
    pF^ := @AltFunc;
    Inc(pF);
  end;
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
  fIdentFuncTable[111] := @Func111; // "vectorcall"
  fIdentFuncTable[112] := @Func112; // "requires"
  fIdentFuncTable[117] := @Func117;
  fIdentFuncTable[122] := @Func122;
  fIdentFuncTable[124] := @Func124;
  fIdentFuncTable[125] := @Func125;
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
  fIdentFuncTable[170] := @Func170;
  fIdentFuncTable[178] := @Func178;
  fIdentFuncTable[181] := @Func181;
  fIdentFuncTable[191] := @Func191;
  fIdentFuncTable[220] := @Func220;

  FPasDocWordList.Clear;
  for i := low(RESERVED_PASDOC) to high(RESERVED_PASDOC) do
    FPasDocWordList.Add(RESERVED_PASDOC[i]);
  FPasDocWordList.Sorted := True;
end;

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

function TSynPasSyn.KeyComp(const aKey: string): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  if Length(aKey) = fStringLen then
  begin
    Temp := fLine + fToIdent;
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

function TSynPasSyn.KeyCompEx(AText1, AText2: pchar; ALen: Integer): Boolean;
begin
  Result := False;
  while ALen > 0 do begin
    if mHashTable[AText1^] <> mHashTable[AText2^] then
      exit;
    dec(ALen);
    inc(AText1);
    inc(AText2);
  end;
  Result := True;
end;

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
  if not(csLoading in ComponentState) then begin
    NestedComments:=AValue in [pcmFPC,pcmObjFPC]; // NestedComments has to be reset even if CompilerMode doesn't change
    TypeHelpers := AValue in [pcmDelphi];  // keep in sync with TypeHelpersIsStored / Loaded
  end;
  if FCompilerMode=AValue then exit;
  FCompilerMode:=AValue;
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

procedure TSynPasSyn.SetStringMultilineMode(const AValue: TSynPasMultilineStringModes);
begin
  if FStringMultilineMode=AValue then Exit;
  FStringMultilineMode:=AValue;
  FAttributeChangeNeedScan := True;
  DefHighlightChange(self);
end;

function TSynPasSyn.GetPasCodeFoldRange: TSynPasSynRange;
begin
  Result := TSynPasSynRange(CodeFoldRange);
end;

function TSynPasSyn.GetCustomTokenCount: integer;
begin
  Result := Length(FSynCustomTokens);
end;

procedure TSynPasSyn.DoCustomTokenChanged(Sender: TObject);
begin
  FNeedCustomTokenBuild := True;
  FCustomTokenMarkup := nil;
end;

procedure TSynPasSyn.RebuildCustomTokenInfo;
var
  i, j, h: Integer;
  ti: TSynPasSynCustomTokenInfo;
  t: String;
begin
  FNeedCustomTokenBuild := False;
  FCustomTokenMarkup := nil;
  for i := 0 to 255 do begin
    FreeAndNil(FCustomTokenInfo[i].List);
    FCustomTokenInfo[i].MatchTokenKinds := [];
  end;
  for i := 0 to Length(FSynCustomTokens) - 1 do begin
    for j := 0 to FSynCustomTokens[i].FTokens.Count - 1 do begin
      if FSynCustomTokens[i].MatchTokenKinds = [] then
        continue;
      t := FSynCustomTokens[i].FTokens[j];
      if t = '' then
        continue;
      fLine    := PChar(t);
      fLineLen := Length(t);
      fToIdent := 0;
      h := KeyHash and 255;

      if FCustomTokenInfo[h].List = nil then
        FCustomTokenInfo[h].List := TSynPasSynCustomTokenInfoList.Create;

      ti.MatchTokenKinds := FSynCustomTokens[i].MatchTokenKinds;
      ti.Word := UpperCase(t);
      ti.Token := FSynCustomTokens[i];
      FCustomTokenInfo[h].MatchTokenKinds := FCustomTokenInfo[h].MatchTokenKinds + FSynCustomTokens[i].MatchTokenKinds;
      FCustomTokenInfo[h].List.Add(ti);
    end;
  end;
end;

procedure TSynPasSyn.SetCustomTokenCount(AValue: integer);
var
  l: SizeInt;
  i: Integer;
begin
  l := Length(FSynCustomTokens);
  if AValue = l then
    exit;

  for i := AValue to l - 1 do
    FSynCustomTokens[i].Free;
  SetLength(FSynCustomTokens, AValue);
  for i := l to AValue - 1 do begin
    FSynCustomTokens[i] := TSynPasSynCustomToken.Create;
    FSynCustomTokens[i].OnMarkupChange := @DefHighlightChange;
    FSynCustomTokens[i].OnChange := @DoCustomTokenChanged;
  end;
  FNeedCustomTokenBuild := True;
  FCustomTokenMarkup := nil;
end;

function TSynPasSyn.GetCustomTokens(AnIndex: integer): TSynPasSynCustomToken;
begin
  Result := FSynCustomTokens[AnIndex];
end;

procedure TSynPasSyn.PasDocAttrChanged(Sender: TObject);
begin
  FUsePasDoc := fPasDocKeyWordAttri.IsEnabled or
                fPasDocSymbolAttri.IsEnabled or
                fPasDocUnknownAttr.IsEnabled;
  DefHighlightChange(Sender);
end;

function TSynPasSyn.Func15: TtkTokenKind;
begin
  if KeyComp('If') then begin
// Anything that may be nested in a "case", and does not have an end (like "end", "until",...)
    StartPascalCodeFoldBlock(cfbtIfThen,
      TopPascalCodeFoldBlockType in [cfbtCase, cfbtIfThen, cfbtIfElse, cfbtForDo, cfbtWhileDo, cfbtWithDo]);
    Result := tkKey
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func19: TtkTokenKind;
var pas : TPascalCodeFoldBlockType;
begin
  if KeyComp('Do') then begin
    Result := tkKey;
    pas := TopPascalCodeFoldBlockType;
    if pas in [cfbtForDo, cfbtWhileDo, cfbtWithDo] then
    begin
      EndPascalCodeFoldBlock();
      StartPascalCodeFoldBlock(pas);
    end
  end
  else
    if KeyComp('And') then begin
      Result := tkKey;
      DoAfterOperator;
    end
    else
      Result := tkIdentifier;
end;

function TSynPasSyn.Func20: TtkTokenKind;
begin
  if KeyComp('As') then begin
    Result := tkKey;
    DoAfterOperator;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func21: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  tfb := TopPascalCodeFoldBlockType;
  if KeyComp('Of') then begin
    Result := tkKey;
    if not (rsInProcHeader in fRange) then
      fRange := fRange + [rsAfterEqualOrColon]; // Identifier for type expected
    if (tfb = cfbtClass) and
       (fRange * [rsAfterClass, rsAfterIdentifierOrValue] = [rsAfterClass]) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      // Accidental start of block // End at next semicolon (usually same line)
      fRange := fRange + [rsSkipAllPasBlocks, rsVarTypeInSpecification];
      //CodeFoldRange.Pop(false); // avoid minlevel // does not work, still minlevel for disabled
      //CodeFoldRange.Add(Pointer(PtrInt(cfbtUses)), false);
    end
    else
    if (tfb in [cfbtCase, cfbtRecordCase]) then begin
      EndPascalCodeFoldBlock();
      StartPascalCodeFoldBlock(tfb, True);
      fRange := fRange + [rsAtCaseLabel];
    end;
  end
  else
  if (FTokenState <> tsAfterRaise) and (PasCodeFoldRange.BracketNestLevel = 0) and
     (rsInRaise in fRange) and
     KeyComp('at')
  then begin
    Exclude(fRange, rsInRaise);
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func23: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
  sl : integer;
begin
  if KeyComp('End') then begin
    if ((fToIdent<2) or (fLine[fToIdent-1]<>'@'))
    then begin
      Result := tkKey;
      fRange := fRange - [rsAsm, rsAfterClassMembers];
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      sl := fStringLen;
      // there may be more than on block ending here
      tfb := TopPascalCodeFoldBlockType;
      fStringLen:=0;
      EndStatement(tfb, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfThen,cfbtIfElse]);

      tfb := CloseFolds(TopPascalCodeFoldBlockType, [cfbtRecordCaseSection { missing ")"? },
                    cfbtClassConstBlock, cfbtClassTypeBlock]);
      fStringLen := sl;
      if tfb = cfbtRecordCase then begin
        tfb := CloseFolds(TopPascalCodeFoldBlockType, [cfbtRecordCase, cfbtRecordCaseSection, cfbtClassConstBlock, cfbtClassTypeBlock]);
        fRange := fRange - [rsAtCaseLabel];
        if TopPascalCodeFoldBlockType = cfbtRecord then begin
          EndPascalCodeFoldBlock;
        end;
        // After type declaration, allow "deprecated"?
        if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt +
           [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection]
        then
          fRange := fRange + [rsVarTypeInSpecification];
      end
      else
      if tfb = cfbtRecord then begin
        EndPascalCodeFoldBlock;
        // After type declaration, allow "deprecated"?
        if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt + [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection]
        then
          fRange := fRange + [rsVarTypeInSpecification];
      end else if tfb = cfbtUnit then begin
        EndPascalCodeFoldBlock;
      end else if tfb = cfbtPackage then begin
        EndPascalCodeFoldBlock;
      end else if tfb = cfbtExcept then begin
        EndPascalCodeFoldBlock;
        if TopPascalCodeFoldBlockType = cfbtTry then
          EndPascalCodeFoldBlock;
      end else if tfb = cfbtTry then begin
          EndPascalCodeFoldBlock;
      end else if tfb in [cfbtTopBeginEnd, cfbtAsm] then begin
        EndPascalCodeFoldBlock;
        if TopPascalCodeFoldBlockType in [cfbtProcedure, cfbtAnonymousProcedure] then
          EndPascalCodeFoldBlock;
      end else if tfb in [cfbtCaseElse] then begin
        EndPascalCodeFoldBlock;
        EndPascalCodeFoldBlock; // must be cfbtCase
        fRange := fRange - [rsAtCaseLabel];
      end else if tfb in [cfbtCase] then begin
        EndPascalCodeFoldBlock;
        fRange := fRange - [rsAtCaseLabel];
      end else if tfb in [cfbtBeginEnd] then begin
        EndPascalCodeFoldBlock;
        if TopPascalCodeFoldBlockType = cfbtProgram then
          EndPascalCodeFoldBlock;
        fStringLen:=0;
        EndStatement(TopPascalCodeFoldBlockType, [cfbtForDo,cfbtWhileDo,cfbtWithDo,{cfbtIfThen,}cfbtIfElse]);
        //while (TopPascalCodeFoldBlockType in [cfbtForDo,cfbtWhileDo,cfbtWithDo,{cfbtIfThen,}cfbtIfElse]) do begin // no semicolon after end
        //  EndPascalCodeFoldBlock(True);
        //end;
        fStringLen := sl;
      end else if tfb = cfbtProcedure then begin //cfbtAnonymousProcedure ?
//        EndPascalCodeFoldBlock; // wrong source: procedure end, without begin
      end else if tfb = cfbtUnitSection then begin
        EndPascalCodeFoldBlockLastLine;
        if TopPascalCodeFoldBlockType = cfbtUnit then // "Unit".."end."
          EndPascalCodeFoldBlock;
      end else begin
        if tfb = cfbtClassSection then begin
          EndPascalCodeFoldBlockLastLine;
          tfb := TopPascalCodeFoldBlockType;
        end;
        // after class-section either a class OR a record can close with the same "end"
        if tfb = cfbtClass then begin
          EndPascalCodeFoldBlock;
          fRange := fRange - [rsInObjcProtocol];
        end
        else
        begin
          if tfb = cfbtRecordCase then
            tfb := CloseFolds(TopPascalCodeFoldBlockType, [cfbtRecordCase, cfbtRecordCaseSection, cfbtClassConstBlock, cfbtClassTypeBlock]);
          if tfb = cfbtRecord then
            EndPascalCodeFoldBlock;
        end;
        // After type declaration, allow "deprecated"?
        if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt + [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection]
        then
          fRange := fRange + [rsVarTypeInSpecification];
      end;
    end else begin
      Result := tkKey; // @@end or @end label
    end;
  end
  else
    if KeyComp('In') then begin
      Result := tkKey;
      DoAfterOperator;
    end
    else
      Result := tkIdentifier;
end;

function TSynPasSyn.Func25: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed) and
     KeyComp('Far')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func27: TtkTokenKind;
begin
  if KeyComp('Cdecl') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func28: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Is') then begin
    Result := tkKey;
    DoAfterOperator;
  end
  else
  if (fRange * [rsProperty, rsAtPropertyOrReadWrite, rsAfterEqualOrColon] =  [rsProperty]) and
      (PasCodeFoldRange.BracketNestLevel = 0) and
     KeyComp('Read')
  then begin
    Result := tkKey;
    fRange := fRange + [rsAtPropertyOrReadWrite] - [rsVarTypeInSpecification];
  end
  else if KeyComp('Case') then begin
    if TopPascalCodeFoldBlockType in PascalStatementBlocks + [cfbtUnitSection] then begin
      StartPascalCodeFoldBlock(cfbtCase, True);
    end
    else begin
      tfb := CloseFolds(TopPascalCodeFoldBlockType(), [cfbtClassConstBlock, cfbtClassTypeBlock]);
      if tfb in [cfbtRecord, cfbtRecordCaseSection] then
        StartPascalCodeFoldBlock(cfbtRecordCase, True); // TODO: only force, if there is case-label highlight // also word-triplet?
    end;
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func29: TtkTokenKind;
begin
  if KeyComp('On') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func32: TtkTokenKind;
begin
  if KeyComp('Label') then begin
    if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
      EndPascalCodeFoldBlockLastLine;
    if (TopPascalCodeFoldBlockType in cfbtVarConstType + [cfbtNone,
        cfbtProcedure, cfbtAnonymousProcedure, cfbtProgram, cfbtUnit, cfbtUnitSection])
    then begin
      if TopPascalCodeFoldBlockType in [cfbtProcedure, cfbtAnonymousProcedure]
      then StartPascalCodeFoldBlock(cfbtLocalVarBlock)
      else StartPascalCodeFoldBlock(cfbtVarBlock);
    end;
    Result := tkKey;
  end
  else
  if KeyComp('Mod') then begin
    Result := tkKey;
    DoAfterOperator;
  end
  else
  if KeyComp('File') then Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func33: TtkTokenKind;
begin
  if KeyComp('Or') then begin
    Result := tkKey;
    DoAfterOperator;
  end
  else
  if KeyComp('Asm') then
  begin
    Result := tkKey;
    fRange := fRange + [rsAsm];
    fAsmStart := True;
    if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtAsm);
  end
  else
  if (fRange * [rsAfterClass, rsInObjcProtocol, rsInProcHeader] = [rsAfterClass, rsInObjcProtocol]) and
     ((CompilerMode = pcmMacPas) or not (rsCompilerModeSet in fRange)) and
     KeyComp('name') and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (TopPascalCodeFoldBlockType in [cfbtClass])
  then
  begin
    Result := tkModifier;
    fRange := fRange + [rsAtClass];
  end
  else
  if (FTokenState = tsAfterExternal) and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsAfterSemiColon, rsAfterEqualOrColon, rsAfterEqual] = []) and
     KeyComp('name') // procedure foo; public name 'abc';
  then
  begin
    Result := tkModifier;
    FNextTokenState := tsAfterExternal; // external 'foo' name 'bar'
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func35: TtkTokenKind;
begin
  if KeyComp('Nil') then begin
    Result := tkKey;
    fRange := fRange + [rsAfterIdentifierOrValue];
    FOldRange := FOldRange - [rsAfterIdentifierOrValue];
  end
  else
  if KeyComp('To') then
    Result := tkKey
  else
  if KeyComp('Div') then begin
    Result := tkKey;
    DoAfterOperator;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func37: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Begin') then begin
    // if we are in an include file, we may not know the state
    if (fRange * [rsImplementation, rsInterface] = []) then
      Include(fRange, rsImplementation);
    PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
    if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
      EndPascalCodeFoldBlockLastLine;
    Result := tkKey;
    tfb := TopPascalCodeFoldBlockType;
    if tfb in [cfbtProcedure, cfbtAnonymousProcedure]
    then StartPascalCodeFoldBlock(cfbtTopBeginEnd, True)
    else StartPascalCodeFoldBlock(cfbtBeginEnd, tfb in [
      cfbtProgram, cfbtUnit, cfbtUnitSection, cfbtPackage,
      cfbtTopBeginEnd, cfbtBeginEnd,
      cfbtTry, cfbtExcept, cfbtAsm,
      cfbtCase, cfbtCaseElse,
      // TODO: cfbtIfThen..cfbtWithDo => only if they are nested in one of the above
      cfbtIfThen, cfbtIfElse, cfbtForDo, cfbtWhileDo, cfbtWithDo
    ]);
    //debugln('TSynPasSyn.Func37 BEGIN ',dbgs(ord(TopPascalCodeFoldBlockType)),' LineNumber=',dbgs(fLineNumber),' ',dbgs(MinimumNestFoldBlockLevel),' ',dbgs(CurrentCodeFoldBlockLevel));
  end else
  if FExtendedKeywordsMode and KeyComp('Break') and
     (TopPascalCodeFoldBlockType() in PascalStatementBlocks) and (fRange * [rsAfterEqualOrColon] = []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func38: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed) and
     KeyComp('Near')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func39: TtkTokenKind;
begin
  if KeyComp('For') then begin
    Result := tkKey;
    if TopPascalCodeFoldBlockType in PascalStatementBlocks then
      StartPascalCodeFoldBlock(cfbtForDo)
    else
    if rsInTypeHelper in FOldRange then begin
      fRange := fRange + [rsInTypeHelper];
      FOldRange := FOldRange - [rsInTypeHelper];
    end;
  end
  else
  if KeyComp('Shl') then begin
    Result := tkKey;
    DoAfterOperator;
   end
   else
     Result := tkIdentifier;
end;

function TSynPasSyn.Func40: TtkTokenKind;
begin
  if KeyComp('Packed') then begin
    Result := tkKey;
    if (fRange * [rsProperty, rsAfterEqualOrColon] =  [rsAfterEqualOrColon]) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then
      FOldRange := FOldRange - [rsAfterEqualOrColon]; // Keep flag in FRange
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func41: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Else') then begin
    Result := tkKey;
    // close all parent "else" and "do" // there can only be one else
    EndStatementLastLine(TopPascalCodeFoldBlockType, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfElse]);
    tfb := TopPascalCodeFoldBlockType;
    if (tfb in [cfbtIfThen]) then begin
      EndPascalCodeFoldBlock;
      StartPascalCodeFoldBlock(cfbtIfElse);
    end else
    if tfb = cfbtCase then begin
      FTokenIsCaseLabel := True;
      StartPascalCodeFoldBlock(cfbtCaseElse, True);
    end
  end
  else if KeyComp('Var') then begin
    if (PasCodeFoldRange.BracketNestLevel = 0) then begin
      tfb := TopPascalCodeFoldBlockType;
      if tfb in cfbtVarConstTypeExt then begin
        EndPascalCodeFoldBlockLastLine;
        tfb := TopPascalCodeFoldBlockType;
      end;
      if tfb in [cfbtProcedure, cfbtAnonymousProcedure] then
        StartPascalCodeFoldBlock(cfbtLocalVarBlock)
      else
      if (tfb in cfbtVarConstType + [cfbtNone, cfbtProgram, cfbtUnit, cfbtUnitSection]) then
        StartPascalCodeFoldBlock(cfbtVarBlock);
      fRange := fRange + [rsAfterSemiColon];
      FOldRange := FOldRange - [rsAfterSemiColon];
      FNextTokenState := tsAfterExternal;  // prevent a variable of name public/export/external to be highlighted
    end;
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func42: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  tfb := TopPascalCodeFoldBlockType;
  if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (tfb in ProcModifierAllowedNoVar) and
     ( (rsAfterClassMembers in fRange) or not(tfb in [cfbtClass, cfbtClassSection, cfbtClassConstBlock, cfbtClassTypeBlock]) ) and
     KeyComp('Alias')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('Final') and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
     (tfb in [cfbtClass, cfbtClassSection]) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func44: TtkTokenKind;
begin
  if KeyComp('Set') then
    Result := tkKey
  else
  if KeyComp('Package') and (TopPascalCodeFoldBlockType=cfbtNone) then begin
    Result := tkKey;
    StartPascalCodeFoldBlock(cfbtPackage);
  end
  else
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     KeyComp('CVAR') and
     ( ( (FTokenState = tsAfterTypedConst) and
         (TopPascalCodeFoldBlockType() in [cfbtConstBlock, cfbtLocalConstBlock])
       )
       or
       ( not(FTokenState in [tsAfterExternal, tsAfterCvar]) and
         (fRange * [rsAfterSemiColon, rsInProcHeader, rsWasInProcHeader] = [rsAfterSemiColon]) and
         (TopPascalCodeFoldBlockType() in [cfbtVarBlock, cfbtLocalVarBlock])
       )
     )
  then begin
    Result := tkModifier;
    FNextTokenState := tsAfterCvar;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func45: TtkTokenKind;
begin
  if KeyComp('Shr') then begin
    Result := tkKey;
    DoAfterOperator;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func46: TtkTokenKind;
begin
  if (rsAfterClass in fRange) and KeyComp('Sealed') and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (TopPascalCodeFoldBlockType in [cfbtClass])
  then begin
    Result := tkModifier;
    fRange := fRange + [rsAtClass, rsAfterIdentifierOrValue]; // forward, in case of further class modifiers
    FOldRange := FOldRange - [rsAfterIdentifierOrValue];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func47: TtkTokenKind;
begin
  if KeyComp('Then') then begin
    Result := tkKey;
    // in a "case", we need to distinguish a possible follwing "else"
    if (TopPascalCodeFoldBlockType = cfbtIfThen) then
      EndPascalCodeFoldBlock;
    StartPascalCodeFoldBlock(cfbtIfThen,
      TopPascalCodeFoldBlockType in [cfbtCase, cfbtIfThen, cfbtIfElse, cfbtForDo, cfbtWhileDo, cfbtWithDo]);
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func49: TtkTokenKind;
begin
  if KeyComp('Not') then begin
    Result := tkKey;
    DoAfterOperator;
    FOldRange := FOldRange - [rsAtPropertyOrReadWrite];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func52: TtkTokenKind;
begin
  if KeyComp('Pascal') and
     (PasCodeFoldRange.BracketNestLevel in [0,1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('Raise') then begin
    Result := tkKey;
    fRange := fRange + [rsInRaise];
    FNextTokenState := tsAfterRaise;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func54: TtkTokenKind;
begin
  if KeyComp('Class') then begin
    Result := tkKey;
    if (rsAfterEqual in fRange) and (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      fRange := fRange + [rsAtClass] - [rsVarTypeInSpecification, rsAfterEqual];
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
    if (fRange * [rsAfterEqualOrColon] <> []) and
       not(rsInProcHeader in fRange) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      fRange := fRange + [rsAtClass] - [rsVarTypeInSpecification, rsAfterEqual];
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
    then begin
      Result := tkKey;
      fRange := fRange + [rsAtPropertyOrReadWrite] - [rsVarTypeInSpecification];
    end
    else
      Result := tkIdentifier;
  end
  else
    if KeyComp('Out') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func57: TtkTokenKind;
begin
  if KeyComp('Goto') then Result := tkKey else
    if KeyComp('While') then begin
      Result := tkKey;
      StartPascalCodeFoldBlock(cfbtWhileDo);
    end
    else
    if KeyComp('Xor') then begin
      Result := tkKey;
      DoAfterOperator;
    end
    else
      Result := tkIdentifier;
end;

function TSynPasSyn.Func58: TtkTokenKind;
begin
  if FExtendedKeywordsMode and KeyComp('Exit') and
     (TopPascalCodeFoldBlockType() in PascalStatementBlocks) and (fRange * [rsAfterEqualOrColon] = []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func59: TtkTokenKind;
begin
  if (KeyComp('Safecall') or KeyComp('cppdecl')) and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func60: TtkTokenKind;
begin
  if KeyComp('With') then begin
    Result := tkKey;
    StartPascalCodeFoldBlock(cfbtWithDo);
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func61: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsAtPropertyOrReadWrite, rsInProcHeader, rsAfterEqualOrColon] = []) and
     ( (rsProperty in fRange) or
       ( (fRange * [rsProperty, rsWasInProcHeader] = [rsWasInProcHeader]) and
         (TopPascalCodeFoldBlockType in ProcModifierAllowed)
       )
     ) and
     KeyComp('Dispid')
  then begin
    Result := tkKey;
   if rsWasInProcHeader in fRange then
     FRange := FRange + [rsInProcHeader];
   if rsProperty in fRange then
     fRange := fRange + [rsAtPropertyOrReadWrite] - [rsVarTypeInSpecification];
  end
  else
  if KeyComp('Generic') then begin
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func63: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Public') then begin
    tfb := CloseFolds(TopPascalCodeFoldBlockType, [cfbtClassConstBlock, cfbtClassTypeBlock]);
    if (tfb in [cfbtClass, cfbtClassSection, cfbtRecord]) and
       (fRange * [rsInProcHeader, rsAfterEqual, rsAfterEqualOrColon, rsVarTypeInSpecification] = []) and
       (fRange * [rsAfterSemiColon, rsAfterClass] <> [])
    then begin
      Result := tkKey;
      fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification] + [rsAfterSemiColon];
      FOldRange := FOldRange - [rsAfterSemiColon];
      if (tfb=cfbtClassSection) then
        EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtClassSection);
    end
    else
    // outside class: procedure foo; public name 'abc';
    if (PasCodeFoldRange.BracketNestLevel = 0) and
       ( (FTokenState in [tsAfterTypedConst, tsAfterCvar])
         or
         ( (FTokenState <> tsAfterExternal) and
           ( ( (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
               (tfb in ProcModifierAllowed - [cfbtClass, cfbtClassSection, cfbtRecord, cfbtClassConstBlock, cfbtClassTypeBlock])
             ) or
             ( (fRange * [rsAfterSemiColon, rsInProcHeader, rsWasInProcHeader] = [rsAfterSemiColon]) and
               (tfb in [cfbtVarBlock, cfbtLocalVarBlock])
         ) ) )
       )
    then begin
      Result := tkModifier;
      FNextTokenState := tsAfterExternal;
    end
    else
      Result := tkIdentifier;
  end
  else if KeyComp('Record') then begin
    StartPascalCodeFoldBlock(cfbtRecord);
    fRange := fRange - [rsVarTypeInSpecification, rsAfterEqual, rsAfterEqualOrColon] + [rsAfterSemiColon];
    FOldRange := FOldRange - [rsAfterSemiColon];
    if (CompilerMode = pcmDelphi) or (TypeHelpers {and adv_record}) then
      fRange := fRange + [rsAtClass]; // highlight helper
    Result := tkKey;
  end
  else if KeyComp('Array') then Result := tkKey
  else if KeyComp('Try') then
  begin
    if TopPascalCodeFoldBlockType in PascalStatementBlocks + [cfbtUnitSection] then
      StartPascalCodeFoldBlock(cfbtTry);
    Result := tkKey;
  end
  else
  if (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowedNoVar) and
     ( (rsAfterClassMembers in fRange) or not(TopPascalCodeFoldBlockType() in [cfbtClass, cfbtClassSection, cfbtClassConstBlock, cfbtClassTypeBlock]) ) and
     KeyComp('Inline')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func64: TtkTokenKind;
begin
  if KeyComp('Unit') then begin
    if TopPascalCodeFoldBlockType=cfbtNone then StartPascalCodeFoldBlock(cfbtUnit);
    Result := tkKey;
    FNextTokenState := tsAtProcName;
  end
  else if KeyComp('Uses') then begin
    if (TopPascalCodeFoldBlockType in
        [cfbtNone, cfbtProgram, cfbtUnit, cfbtUnitSection]) then begin
      StartPascalCodeFoldBlock(cfbtUses);
    end;
    Result := tkKey;
  end
  // TODO: "class helper" fold at "class", but "type helper" fold at "helper"
  else if KeyComp('helper') then begin
    if (rsAtClass in fRange) and (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      Result := tkKey; // tkModifier
      fRange := fRange - [rsVarTypeInSpecification, rsAfterEqual] + [rsInTypeHelper];
    end
    else
    if (FTokenState = tsAfterEqualThenType) and TypeHelpers then begin
      Result := tkKey;
      fRange := fRange - [rsVarTypeInSpecification, rsAfterEqual] + [rsInTypeHelper];
      StartPascalCodeFoldBlock(cfbtClass); // type helper
    end
    else
      Result := tkIdentifier;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func65: TtkTokenKind;
begin
  if KeyComp('Repeat') then begin
    Result := tkKey;
    StartPascalCodeFoldBlock(cfbtRepeat);
   end
   else Result := tkIdentifier;
end;

function TSynPasSyn.Func66: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Type') then begin
    tfb := TopPascalCodeFoldBlockType;
    if (PasCodeFoldRange.BracketNestLevel = 0)
       and (tfb in
        cfbtVarConstTypeExt + [cfbtNone, cfbtProcedure, cfbtAnonymousProcedure, cfbtProgram,
         cfbtUnit, cfbtUnitSection,
         cfbtClass, cfbtClassSection, cfbtRecord // if inside a type section in class/record
        ])
    then begin
      if (rsAfterEqualOrColon in fRange) then begin
        FOldRange := FOldRange - [rsAfterEqualOrColon];
        if TypeHelpers then
          FNextTokenState := tsAfterEqualThenType;
      end
      else begin
        // If already in cfbtClassTypeBlock, then keep block going / save the close, open
        if tfb in cfbtVarConstTypeExt - [cfbtClassTypeBlock] then begin
          EndPascalCodeFoldBlockLastLine;
          tfb := TopPascalCodeFoldBlockType;
        end;
        if tfb in [cfbtClass, cfbtClassSection, cfbtRecord]
        then StartPascalCodeFoldBlock(cfbtClassTypeBlock)
        else
        if tfb in [cfbtProcedure, cfbtAnonymousProcedure]
        then StartPascalCodeFoldBlock(cfbtLocalTypeBlock)
        else StartPascalCodeFoldBlock(cfbtTypeBlock);
        fRange := fRange + [rsAfterSemiColon];
        FOldRange := FOldRange - [rsAfterSemiColon];
        FNextTokenState := tsAfterExternal;  // prevent a type of name public/export/external to be highlighted
      end;
    end;
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func69: TtkTokenKind;
begin
  if KeyComp('Default') then begin
    if (PasCodeFoldRange.BracketNestLevel = 0) and
       (fRange * [rsAtPropertyOrReadWrite, rsAfterEqualOrColon, rsInProcHeader] = []) and
       ( ( (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) and
           (rsAfterClassMembers in fRange)
         ) or
         (rsProperty in fRange)
       )
    then begin
      if rsProperty in fRange then begin
        fRange := fRange + [rsAtPropertyOrReadWrite] - [rsVarTypeInSpecification];
        Result := tkKey
      end
      else
        Result := tkModifier;
    end
    else
      Result := tkIdentifier;
  end else
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     KeyComp('Dynamic')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('Message') and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func71: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Stdcall') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else if KeyComp('Const') then begin
    if (PasCodeFoldRange.BracketNestLevel = 0) then begin
      tfb := TopPascalCodeFoldBlockType;
      // If already in cfbtClassTypeBlock, then keep block going / save the close, open
      if tfb in cfbtVarConstTypeExt - [cfbtClassConstBlock] then begin
        EndPascalCodeFoldBlockLastLine;
        tfb := TopPascalCodeFoldBlockType;
      end;
      if tfb in [cfbtClass, cfbtClassSection, cfbtRecord] then
        StartPascalCodeFoldBlock(cfbtClassConstBlock)
      else
      if tfb in [cfbtProcedure, cfbtAnonymousProcedure] then
        StartPascalCodeFoldBlock(cfbtLocalConstBlock)
      else
      if (tfb in cfbtVarConstType + [cfbtNone, cfbtProgram, cfbtUnit, cfbtUnitSection]) then
        StartPascalCodeFoldBlock(cfbtConstBlock);

      fRange := fRange + [rsAfterSemiColon];
      FOldRange := FOldRange - [rsAfterSemiColon];
      FNextTokenState := tsAfterExternal;  // prevent a variable of name public/export/external to be highlighted
    end;
    Result := tkKey;
  end
  else if KeyComp('Bitpacked') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func72: TtkTokenKind;
begin
  if KeyComp('Static') and (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtClassConstBlock, cfbtClassTypeBlock]) and
     (fRange * [rsAfterEqualOrColon, rsInProcHeader, rsProperty] = []) and
     (fRange * [rsAfterClassMembers, rsAfterClassField] <> []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkModifier
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func73: TtkTokenKind;
begin
  if KeyComp('Except') then begin
    Result := tkKey;
    // no semicolon before except
    EndStatement(TopPascalCodeFoldBlockType, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfThen,cfbtIfElse]);
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
    fRange := fRange + [rsAtPropertyOrReadWrite] - [rsVarTypeInSpecification];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func76: TtkTokenKind;
begin
  if KeyComp('Until') then begin
    Result := tkKey;
    // no semicolon before until;
    EndStatement(TopPascalCodeFoldBlockType, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfThen,cfbtIfElse]);
    SmartCloseBeginEndBlocks(cfbtRepeat);
    if TopPascalCodeFoldBlockType = cfbtRepeat then EndPascalCodeFoldBlock;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func79: TtkTokenKind;
begin
  if KeyComp('Finally') then begin
    Result := tkKey;
     // no semicolon before finally
    EndStatement(TopPascalCodeFoldBlockType, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfThen,cfbtIfElse]);
    SmartCloseBeginEndBlocks(cfbtTry);
    if TopPascalCodeFoldBlockType = cfbtTry then
      StartPascalCodeFoldBlock(cfbtExcept);
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func81: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if (fRange * [rsProperty, rsAtPropertyOrReadWrite, rsAfterEqualOrColon] =  [rsProperty]) and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     KeyComp('Stored')
  then begin
    Result := tkKey;
    fRange := fRange + [rsAtPropertyOrReadWrite] - [rsVarTypeInSpecification];
  end
  else if KeyComp('Interface') then begin
    if (rsAfterEqual in fRange) and (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      // type IFoo = INTERFACE
      fRange := fRange + [rsAtClass] - [rsVarTypeInSpecification, rsAfterEqual];
      StartPascalCodeFoldBlock(cfbtClass);
    end
    else
    if not(rsAfterEqualOrColon in fRange) and
       (fRange * [rsInterface, rsImplementation] = []) then
    begin
      // unit section INTERFACE
      CloseBeginEndBlocksBeforeProc;
      if TopPascalCodeFoldBlockType in cfbtVarConstType then
        EndPascalCodeFoldBlockLastLine;
      if TopPascalCodeFoldBlockType=cfbtUnitSection then EndPascalCodeFoldBlockLastLine;
      StartPascalCodeFoldBlock(cfbtUnitSection);
      fRange := fRange + [rsInterface];
      // Interface has no ";", implicit end of statement
    end;
    Result := tkKey
  end
  else if KeyComp('Deprecated') then begin
    tfb := TopPascalCodeFoldBlockType;
    if ( ( (tfb in cfbtVarConstType) and
           (FTokenState <> tsAfterAbsolute) and
           (fRange * [rsVarTypeInSpecification, rsAfterEqualOrColon] = [rsVarTypeInSpecification]) ) or
         ( (tfb in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection, cfbtClassConstBlock, cfbtClassTypeBlock]) and
           ( (fRange * [rsAfterClassMembers, rsInProcHeader] = [rsAfterClassMembers]) or
             (fRange * [rsAfterClassMembers, rsAfterEqualOrColon, rsVarTypeInSpecification] = [rsVarTypeInSpecification])
         ) ) or
         ( (tfb in [cfbtUnitSection, cfbtProgram, cfbtProcedure]) and
           (fRange * [rsInProcHeader] = []) ) or
         ( (tfb in [cfbtUnit, cfbtNone]) and
           (fRange * [rsInProcHeader] = []) and (FTokenState = tsAfterProcName) )
       ) and
       ( fRange *[rsAfterEqualOrColon, rsProperty] = [] ) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      Result := tkModifier;
      if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
         (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
         (CompilerMode = pcmDelphi)
      then
        FRange := FRange + [rsInProcHeader]; // virtual reintroduce overload can be after virtual
    end
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func84: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     KeyComp('Abstract')
  then begin
    Result := tkModifier;
    // type foo = class abstract
    if (rsAfterClass in fRange) and (TopPascalCodeFoldBlockType = cfbtClass) then begin
      fRange := fRange + [rsAtClass, rsAfterIdentifierOrValue]; // forward, in case of further class modifiers  end
      FOldRange := FOldRange - [rsAfterIdentifierOrValue];
    end
    else
    // procedure foo; virtual; abstract;
    if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) then
      FRange := FRange + [rsInProcHeader]
    else
      Result := tkIdentifier;
  end
  else if ((CompilerMode = pcmMacPas) or not (rsCompilerModeSet in fRange)) and
    KeyComp('ObjcClass')
  then begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else if KeyComp('oldfpccall') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func85: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowedNoVar-[cfbtClass, cfbtClassSection]) and
     KeyComp('Forward')
  then begin
    Result := tkModifier;
    if TopPascalCodeFoldBlockType = cfbtProcedure then begin
      EndPascalCodeFoldBlock(True);
    end;
  end
  else
  if KeyComp('Library') then begin
    fRange := fRange - [rsInterface] + [rsImplementation];
    if TopPascalCodeFoldBlockType=cfbtNone then
      StartPascalCodeFoldBlock(cfbtProgram);
    Result := tkKey
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func86: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed) and
     KeyComp('VarArgs')
  then begin
    Result := tkKey;
    FRange := FRange + [rsInProcHeader];
  end
  else Result := tkIdentifier;
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
    if TopPascalCodeFoldBlockType=cfbtNone then
      StartPascalCodeFoldBlock(cfbtProgram);
    Result := tkKey;
  end
  else if KeyComp('Mwpascal') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func89: TtkTokenKind;
  function ScanForClassSection: Boolean;
  var
    FndLine: String;
    FndPos, FndLen: integer;
  begin
    Result := ScanAheadForNextToken(7, FndLine, FndPos, FndLen);
    if Result then
      Result := (FndLine[FndPos] in ['p', 'P']) and
                ( ( (FndLen = 7) and KeyCompEx(@FndLine[FndPos + 1], PChar('rivate'), 6) ) or
                  ( (FndLen = 9) and KeyCompEx(@FndLine[FndPos + 1], PChar('rotected'), 8) )
                );
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
  if KeyComp('strict') and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtClassConstBlock, cfbtClassTypeBlock]) and
     (fRange * [rsInProcHeader, rsAfterEqual, rsAfterEqualOrColon, rsVarTypeInSpecification] = []) and
     (fRange * [rsAfterSemiColon, rsAfterClass] <> []) and
     ScanForClassSection
  then begin
    CloseFolds(TopPascalCodeFoldBlockType, [cfbtClassConstBlock, cfbtClassTypeBlock]);
    fRange := fRange + [rsAfterSemiColon]; // flag for private/proctected (must be next)
    FOldRange := FOldRange - [rsAfterSemiColon];
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func91: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Downto') then
    Result := tkKey
  else
  if KeyComp('Private') and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtClassConstBlock, cfbtClassTypeBlock]) and
     (fRange * [rsInProcHeader, rsAfterEqual, rsAfterEqualOrColon, rsVarTypeInSpecification] = []) and
     (fRange * [rsAfterSemiColon, rsAfterClass] <> [])
  then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification] + [rsAfterSemiColon];
    FOldRange := FOldRange - [rsAfterSemiColon];
    tfb := CloseFolds(TopPascalCodeFoldBlockType(), [cfbtClassConstBlock, cfbtClassTypeBlock]);
    if (tfb=cfbtClassSection) then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtClassSection);
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func92: TtkTokenKind;
begin
  if D4syntax and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowedNoVar) and
     ( (rsAfterClassMembers in fRange) or not(TopPascalCodeFoldBlockType() in [cfbtClass, cfbtClassSection]) ) and
     KeyComp('overload')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('NoInline') then Result := tkModifier
  else
  if KeyComp('Inherited') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func94: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowedNoVar) and
     ( (rsAfterClassMembers in fRange) or not(TopPascalCodeFoldBlockType() in [cfbtClass, cfbtClassSection]) ) and
     KeyComp('Assembler')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('Readonly') then
  begin
    if (fRange * [rsProperty, rsAtPropertyOrReadWrite, rsAfterEqualOrColon] =  [rsProperty]) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      Result := tkKey;
      FOldRange := FOldRange - [rsAtPropertyOrReadWrite];
    end
    else
      Result := tkIdentifier;
  end else Result := tkIdentifier;
end;

function TSynPasSyn.Func95: TtkTokenKind;
begin
  if KeyComp('Absolute') and
     (TopPascalCodeFoldBlockType in cfbtVarConstType) and
     (fRange * [rsVarTypeInSpecification, rsAfterEqualOrColon, rsProperty] = [rsVarTypeInSpecification]) and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (FTokenState = tsNone) // FTokenState <> tsAfterAbsolute
  then begin
    Result := tkModifier;
    // prevent:          var foo absolute absolute; // at same address as variable named absolute
    // does not prevent: var foo absolute bar absolute abc; // not valid code
    FNextTokenState := tsAfterAbsolute;
  end
  else
  if KeyComp('Contains') and (TopPascalCodeFoldBlockType=cfbtPackage) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func96: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Published') and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtClassConstBlock, cfbtClassTypeBlock]) and
     (fRange * [rsInProcHeader, rsAfterEqual, rsAfterEqualOrColon, rsVarTypeInSpecification] = []) and
     (fRange * [rsAfterSemiColon, rsAfterClass] <> [])
  then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification] + [rsAfterSemiColon];
    FOldRange := FOldRange - [rsAfterSemiColon];
    tfb := CloseFolds(TopPascalCodeFoldBlockType(), [cfbtClassConstBlock, cfbtClassTypeBlock]);
    if (tfb=cfbtClassSection) then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtClassSection);
  end
  else
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     KeyComp('Override')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func97: TtkTokenKind;
begin
  if KeyComp('Threadvar') then begin
    Result := tkKey;
    if TopPascalCodeFoldBlockType() in cfbtVarConstTypeExt then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtVarBlock);
  end
  else
  if (rsInObjcProtocol in fRange) and
     KeyComp('required') and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection])
  then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification];
    StartPascalCodeFoldBlock(cfbtClassSection);
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func98: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  tfb := TopPascalCodeFoldBlockType;
  if (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (FTokenState <> tsAfterExternal) and
     ( ( (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
         (tfb in ProcModifierAllowed)
       ) or
       ( (fRange * [rsAfterSemiColon, rsInProcHeader, rsWasInProcHeader] = [rsAfterSemiColon]) and
         (tfb in [ cfbtVarBlock, cfbtLocalVarBlock])
       )
     ) and
     KeyComp('Export')
  then begin
    Result := tkModifier;
    FNextTokenState := tsAfterExternal;
  end
  else
    if KeyComp('Nodefault') then
    begin
      if rsProperty in fRange then
        Result := tkModifier
      else
        Result := tkIdentifier;
    end
    else
      Result := tkIdentifier;
end;

function TSynPasSyn.Func99: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  tfb := TopPascalCodeFoldBlockType;
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (FTokenState <> tsAfterExternal) and
     ( ( (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
         (tfb in ProcModifierAllowed)
       ) or
       ( (fRange * [rsAfterSemiColon, rsInProcHeader, rsWasInProcHeader] = [rsAfterSemiColon]) and
         (tfb in [ cfbtVarBlock, cfbtLocalVarBlock])
       )
     ) and
     KeyComp('External')
  then begin
    Result := tkModifier;
    FNextTokenState := tsAfterExternal;
    if tfb = cfbtProcedure then begin
      EndPascalCodeFoldBlock(True);
    end;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func100: TtkTokenKind;
begin
  if KeyComp('Automated') and // in old times: class section
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord]) and
     (fRange * [rsInProcHeader, rsAfterEqual, rsAfterEqualOrColon, rsVarTypeInSpecification] = []) and
     (fRange * [rsAfterSemiColon, rsAfterClass] <> [])
  then
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
  tfb: TPascalCodeFoldBlockType;
begin
  tfb := TopPascalCodeFoldBlockType;
  if KeyComp('Register') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (tfb in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('Platform') then begin
    if ( ( (tfb in cfbtVarConstType) and
           (FTokenState <> tsAfterAbsolute) and
           (fRange * [rsVarTypeInSpecification, rsAfterEqualOrColon] = [rsVarTypeInSpecification]) ) or
         ( (tfb in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection, cfbtClassConstBlock, cfbtClassTypeBlock]) and
           ( (fRange * [rsAfterClassMembers, rsInProcHeader] = [rsAfterClassMembers]) or
             (fRange * [rsAfterClassMembers, rsAfterEqualOrColon, rsVarTypeInSpecification] = [rsVarTypeInSpecification])
         ) ) or
         ( (tfb in [cfbtUnitSection, cfbtProgram, cfbtProcedure]) and
           (fRange * [rsInProcHeader] = []) ) or
         ( (tfb in [cfbtUnit, cfbtNone]) and
           (fRange * [rsInProcHeader] = []) and (FTokenState = tsAfterProcName) )
       ) and
       ( fRange *[rsAfterEqualOrColon, rsProperty] = [] ) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      Result := tkModifier;
      if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
         (tfb in [cfbtClass, cfbtClassSection]) and
         (CompilerMode = pcmDelphi)
      then
        FRange := FRange + [rsInProcHeader]; // virtual reintroduce overload can be after virtual
    end
    else
      Result := tkIdentifier;
  end
  else
  if FExtendedKeywordsMode and KeyComp('Continue') and
     (tfb in PascalStatementBlocks) and (fRange * [rsAfterEqualOrColon] = []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func102: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Function') then begin
    if (TopPascalCodeFoldBlockType in PascalStatementBlocks) and IsAnonymousFunc(8, True) then begin
      StartPascalCodeFoldBlock(cfbtAnonymousProcedure);
    end
    else begin
      if not(rsAfterEqualOrColon in fRange) then begin
        PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
        CloseBeginEndBlocksBeforeProc;

        if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
          EndPascalCodeFoldBlockLastLine;

        InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
        if ( (rsImplementation in fRange) and (not InClass) ) then
          StartPascalCodeFoldBlock(cfbtProcedure);

        if InClass then
          fRange := fRange + [rsAfterClassMembers];
        FNextTokenState := tsAtProcName;
      end;
    end;
    fRange := fRange + [rsInProcHeader];
    Result := tkKey;
  end
  else
  if (rsInObjcProtocol in fRange) and
     KeyComp('optional') and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtClassConstBlock, cfbtClassTypeBlock])
  then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification];
    StartPascalCodeFoldBlock(cfbtClassSection);
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func103: TtkTokenKind;
begin
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     KeyComp('Virtual')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func105: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Procedure') then begin
    if (TopPascalCodeFoldBlockType in PascalStatementBlocks) and IsAnonymousFunc(9, False) then begin
      StartPascalCodeFoldBlock(cfbtAnonymousProcedure);
    end
    else begin
      if not(rsAfterEqualOrColon in fRange) then begin
        PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
        CloseBeginEndBlocksBeforeProc;

        if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
          EndPascalCodeFoldBlockLastLine;

        InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
        if ( (rsImplementation in fRange) and (not InClass) ) then
          StartPascalCodeFoldBlock(cfbtProcedure);

        if InClass then
          fRange := fRange + [rsAfterClassMembers];
        FNextTokenState := tsAtProcName;
      end;
    end;
    fRange := fRange + [rsInProcHeader];
    Result := tkKey;
  end
  else if KeyComp('specialize') then begin
    Result := tkKey;
    if rsProperty in fRange then begin
      fRange := fRange + [rsAtPropertyOrReadWrite];
      FOldRange := FOldRange - [rsAtPropertyOrReadWrite];
    end
    else
    if rsInRaise in fRange then
      FNextTokenState := tsAfterRaise;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func106: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Protected') and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtClassConstBlock, cfbtClassTypeBlock]) and
     (fRange * [rsInProcHeader, rsAfterEqual, rsAfterEqualOrColon, rsVarTypeInSpecification] = []) and
     (fRange * [rsAfterSemiColon, rsAfterClass] <> [])
  then begin
    Result := tkKey;
    fRange := fRange - [rsAfterClassMembers, rsVarTypeInSpecification] + [rsAfterSemiColon];
    FOldRange := FOldRange - [rsAfterSemiColon];
    tfb := CloseFolds(TopPascalCodeFoldBlockType(), [cfbtClassConstBlock, cfbtClassTypeBlock]);
    if (tfb=cfbtClassSection) then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtClassSection);
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func108: TtkTokenKind;
var
  InClass: Boolean;
begin
  if KeyComp('Operator') then
  begin
    if not(rsAfterEqualOrColon in fRange) then
    begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;
      if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [{cfbtClass,} cfbtClassSection, cfbtRecord]; // only in records
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
      //FNextTokenState := tsAtProcName;
    end;
    fRange := fRange + [rsInProcHeader];
    Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func112: TtkTokenKind;
begin
  if KeyComp('Requires') and (TopPascalCodeFoldBlockType=cfbtPackage) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func117: TtkTokenKind;
begin
  if KeyComp('Exports') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func122: TtkTokenKind;
begin
  if KeyComp('Otherwise') then begin
    Result := tkKey;
    //DebugLn('  ### Otherwise');
    EndStatementLastLine(TopPascalCodeFoldBlockType, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfThen,cfbtIfElse]);
    if TopPascalCodeFoldBlockType = cfbtCase then begin
      StartPascalCodeFoldBlock(cfbtCaseElse, True);
      FTokenIsCaseLabel := True;
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func124: TtkTokenKind;
begin
  if ((CompilerMode = pcmMacPas) or not (rsCompilerModeSet in fRange)) and
     KeyComp('ObjcCategory')
  then begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass] - [rsVarTypeInSpecification, rsAfterEqual];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func125: TtkTokenKind;
begin
  if KeyComp('NoReturn') and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
  if KeyComp('Ms_abi_cdecl') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func126: TtkTokenKind;
begin
  if D4syntax and KeyComp('Implements') then
  begin
    if rsProperty in fRange then Result := tkKey else Result := tkIdentifier;
  end
  else
  if (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed) and
     KeyComp('NoStackFrame')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
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
    if (rsAfterEqual in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass] - [rsVarTypeInSpecification, rsAfterEqual];
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
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsAfterClassMembers, rsAfterSemiColon, rsAfterEqualOrColon, rsInProcHeader, rsProperty] =
               [rsAfterClassMembers, rsAfterSemiColon]) and
     KeyComp('Enumerator') and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord])
  then begin
    Result := tkModifier;
    if rsWasInProcHeader in fRange then
      FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func132: TtkTokenKind;
begin
  if D4syntax and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
     (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
     KeyComp('Reintroduce')
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func133: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Property') then begin
    Result := tkKey;
    fRange := fRange + [rsProperty, rsAtPropertyOrReadWrite];
    tfb := CloseFolds(TopPascalCodeFoldBlockType, [cfbtClassConstBlock, cfbtClassTypeBlock]);
    if tfb in [cfbtClass, cfbtClassSection, cfbtRecord] then
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
    if TopPascalCodeFoldBlockType in cfbtVarConstType then
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
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowedNoVar) and
     ( (rsAfterClassMembers in fRange) or not(TopPascalCodeFoldBlockType() in [cfbtClass, cfbtClassSection]) ) and
     KeyComp('WeakExternal') then
  begin
    Result := tkModifier;
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
  tfb: TPascalCodeFoldBlockType;
begin
  if KeyComp('Experimental') then begin
    tfb := TopPascalCodeFoldBlockType;
    if ( ( (tfb in cfbtVarConstType) and
           (FTokenState <> tsAfterAbsolute) and
           (fRange * [rsVarTypeInSpecification, rsAfterEqualOrColon] = [rsVarTypeInSpecification]) ) or
         ( (tfb in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection, cfbtClassConstBlock, cfbtClassTypeBlock]) and
           ( (fRange * [rsAfterClassMembers, rsInProcHeader] = [rsAfterClassMembers]) or
             (fRange * [rsAfterClassMembers, rsAfterEqualOrColon, rsVarTypeInSpecification] = [rsVarTypeInSpecification])
         ) ) or
         ( (tfb in [cfbtUnitSection, cfbtProgram, cfbtProcedure]) and
           (fRange * [rsInProcHeader] = []) ) or
         ( (tfb in [cfbtUnit, cfbtNone]) and
           (fRange * [rsInProcHeader] = []) and (FTokenState = tsAfterProcName) )
       ) and
       ( fRange *[rsAfterEqualOrColon, rsProperty] = [] ) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      Result := tkModifier;
      if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
         (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
         (CompilerMode = pcmDelphi)
      then
        FRange := FRange + [rsInProcHeader]; // virtual reintroduce overload can be after virtual
    end
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

      if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
      fRange := fRange + [rsInProcHeader];
      FNextTokenState := tsAtProcName;
    end;
    Result := tkKey;
  end else
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowedNoVar) and
     ( (rsAfterClassMembers in fRange) or not(TopPascalCodeFoldBlockType() in [cfbtClass, cfbtClassSection]) ) and
     KeyComp('compilerproc')
  then begin// fpc modifier
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func144: TtkTokenKind;
begin
  if ((CompilerMode = pcmMacPas) or not (rsCompilerModeSet in fRange)) and
     KeyComp('ObjcProtocol')
  then begin
    Result := tkKey;
    if (rsAfterEqualOrColon in fRange) and (PasCodeFoldRange.BracketNestLevel = 0) then
    begin
      fRange := fRange + [rsAtClass, rsInObjcProtocol] - [rsVarTypeInSpecification, rsAfterEqual];
      StartPascalCodeFoldBlock(cfbtClass);
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func151: TtkTokenKind;
var
  tfb: TPascalCodeFoldBlockType;
begin
  tfb := TopPascalCodeFoldBlockType;
  if KeyComp('Unimplemented') then begin
    if ( ( (tfb in cfbtVarConstType) and
           (FTokenState <> tsAfterAbsolute) and
           (fRange * [rsVarTypeInSpecification, rsAfterEqualOrColon] = [rsVarTypeInSpecification]) ) or
         ( (tfb in [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection, cfbtClassConstBlock, cfbtClassTypeBlock]) and
           ( (fRange * [rsAfterClassMembers, rsInProcHeader] = [rsAfterClassMembers]) or
             (fRange * [rsAfterClassMembers, rsAfterEqualOrColon, rsVarTypeInSpecification] = [rsVarTypeInSpecification])
         ) ) or
         ( (tfb in [cfbtUnitSection, cfbtProgram, cfbtProcedure]) and
           (fRange * [rsInProcHeader] = []) ) or
         ( (tfb in [cfbtUnit, cfbtNone]) and
           (fRange * [rsInProcHeader] = []) and (FTokenState = tsAfterProcName) )
       ) and
       ( fRange *[rsAfterEqualOrColon, rsProperty] = [] ) and
       (PasCodeFoldRange.BracketNestLevel = 0)
    then begin
      Result := tkModifier;
      if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader, rsAfterClassMembers] = [rsWasInProcHeader, rsAfterClassMembers]) and
         (TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection]) and
         (CompilerMode = pcmDelphi)
      then
        FRange := FRange + [rsInProcHeader]; // virtual reintroduce overload can be after virtual
    end
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

      if TopPascalCodeFoldBlockType in cfbtVarConstTypeExt then
        EndPascalCodeFoldBlockLastLine;

      InClass := TopPascalCodeFoldBlockType in [cfbtClass, cfbtClassSection, cfbtRecord];
      if ( (rsImplementation in fRange) and (not InClass) ) then
        StartPascalCodeFoldBlock(cfbtProcedure);

      if InClass then
        fRange := fRange + [rsAfterClassMembers];
      fRange := fRange + [rsInProcHeader];
      FNextTokenState := tsAtProcName;
    end;
    Result := tkKey;
  end else
    if KeyComp('Implementation') then begin
      PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
      CloseBeginEndBlocksBeforeProc;
      if TopPascalCodeFoldBlockType in cfbtVarConstType then
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
  if KeyComp('Ms_abi_default') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func168: TtkTokenKind;
begin
  if KeyComp('Initialization') then begin
    PasCodeFoldRange.BracketNestLevel := 0; // Reset in case of partial code
    CloseBeginEndBlocksBeforeProc;
    if TopPascalCodeFoldBlockType in cfbtVarConstType then
      EndPascalCodeFoldBlockLastLine;
    if TopPascalCodeFoldBlockType=cfbtUnitSection then EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtUnitSection);
    fRange := fRange - [rsInterface] + [rsImplementation];
    Result := tkKey;
  end
  else Result := tkIdentifier;
end;

function TSynPasSyn.Func170: TtkTokenKind;
begin
  if (FStringKeywordMode in [spsmDefault]) and KeyComp('UTF8String') then
    Result := tkKey
  else
    Result := tkIdentifier;
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
    if TopPascalCodeFoldBlockType in cfbtVarConstType then
      EndPascalCodeFoldBlockLastLine;
    StartPascalCodeFoldBlock(cfbtVarBlock);
  end
  else if KeyComp('Stringresource') then
    Result := tkKey else Result := tkIdentifier;
end;

function TSynPasSyn.Func111: TtkTokenKind;
begin
  if KeyComp('vectorcall') and
     (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func178: TtkTokenKind;
begin
  if KeyComp('Sysv_abi_cdecl') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.Func220: TtkTokenKind;
begin
  if KeyComp('Sysv_abi_default') and
     (PasCodeFoldRange.BracketNestLevel in [0, 1]) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then begin
    Result := tkModifier;
    FRange := FRange + [rsInProcHeader];
  end
  else
    Result := tkIdentifier;
end;

function TSynPasSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPasSyn.IdentKind(p: integer): TtkTokenKind;
begin
  fToIdent := p;
  FTokenHashKey := KeyHash;
  if FTokenHashKey <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[FTokenHashKey]()
  else
    Result := tkIdentifier;
end;

procedure TSynPasSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
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
      '"': fProcTable[I] := @DoubleQuoteProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := @IdentProc;
      '^': fProcTable[I] := @CaretProc;
      '{': fProcTable[I] := @BraceOpenProc;
      '}', '!', '('..'/', ':'..'@', '[', ']', '\', '`', '~':
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
end;

constructor TSynPasSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStringKeywordMode := spsmDefault;
  FExtendedKeywordsMode := False;
  CreateDividerDrawConfig;
  fD4syntax := true;
  fAsmAttri := TSynHighlighterAttributes.Create(@SYNS_AttrAssembler, SYNS_XML_AttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  FIDEDirectiveAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrIDEDirective, SYNS_XML_AttrIDEDirective);
  AddAttribute(FIDEDirectiveAttri);
  // FCurIDEDirectiveAttri, FCurCaseLabelAttri, FCurProcedureHeaderNameAttr
  // They are not available through the "Attribute" property (not added via AddAttribute
  // But they are returned via GetTokenAttribute, so they should have a name.
  FCurIDEDirectiveAttri := TSynSelectedColorMergeResult.Create(@SYNS_AttrIDEDirective, SYNS_XML_AttrIDEDirective);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fModifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrModifier, SYNS_XML_AttrModifier);
  fModifierAttri.Style:= [fsBold];
  AddAttribute(fModifierAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  FProcedureHeaderNameAttr := TSynHighlighterAttributesModifier.Create(@SYNS_AttrProcedureHeaderName, SYNS_XML_AttrProcedureHeaderName);
  AddAttribute(FProcedureHeaderNameAttr);
  FCaseLabelAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrCaseLabel, SYNS_XML_AttrCaseLabel);
  AddAttribute(FCaseLabelAttri);
  FCurCaseLabelAttri := TSynSelectedColorMergeResult.Create(@SYNS_AttrCaseLabel, SYNS_XML_AttrCaseLabel);
  FCurProcedureHeaderNameAttr := TSynSelectedColorMergeResult.Create(@SYNS_AttrProcedureHeaderName, SYNS_XML_AttrProcedureHeaderName);
  fDirectiveAttri := TSynHighlighterAttributes.Create(@SYNS_AttrDirective, SYNS_XML_AttrDirective);
  fDirectiveAttri.Style:= [fsItalic];
  AddAttribute(fDirectiveAttri);

  fPasDocKeyWordAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrPasDocKey, SYNS_XML_AttrPasDocKey);
  fPasDocKeyWordAttri.Clear;
  AddAttribute(fPasDocKeyWordAttri);
  fPasDocSymbolAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrPasDocSymbol, SYNS_XML_AttrPasDocSymbol);
  fPasDocSymbolAttri.Clear;
  AddAttribute(fPasDocSymbolAttri);
  fPasDocUnknownAttr := TSynHighlighterAttributesModifier.Create(@SYNS_AttrPasDocUnknown, SYNS_XML_AttrPasDocUnknown);
  fPasDocUnknownAttr.Clear;
  AddAttribute(fPasDocUnknownAttr);
  FCurPasDocAttri := TSynSelectedColorMergeResult.Create(@SYNS_AttrCaseLabel, SYNS_XML_AttrCaseLabel);
  FPasDocWordList := TStringList.Create;

  FCustomTokenMergedMarkup := TSynSelectedColorMergeResult.Create;

  CompilerMode:=pcmDelphi;
  SetAttributesOnChange(@DefHighlightChange);
  fPasDocKeyWordAttri.OnChange := @PasDocAttrChanged;
  fPasDocSymbolAttri.OnChange := @PasDocAttrChanged;
  fPasDocUnknownAttr.OnChange := @PasDocAttrChanged;

  InitIdent;
  MakeMethodTables;
  fRange := [];
  fAsmStart := False;
  fDefaultFilterInitialValue := SYNS_FilterPascal;
  fDefaultFilter := fDefaultFilterInitialValue;
end; { Create }

destructor TSynPasSyn.Destroy;
var
  i: Integer;
begin
  DestroyDividerDrawConfig;
  FreeAndNil(FCurCaseLabelAttri);
  FreeAndNil(FCurIDEDirectiveAttri);
  FreeAndNil(FCurProcedureHeaderNameAttr);
  FreeAndNil(FCurPasDocAttri);
  FreeAndNil(FCustomTokenMergedMarkup);
  FreeAndNil(FPasDocWordList);
  CustomTokenCount := 0;
  for i := 0 to 255 do
    FCustomTokenInfo[i].List.Free;
  inherited Destroy;
end;

procedure TSynPasSyn.Loaded;
begin
  if Byte(FTypeHelpers) = 99 then
    FTypeHelpers := FCompilerMode in [pcmDelphi];
  inherited Loaded;
end;

procedure TSynPasSyn.Loading;
begin
  Byte(FTypeHelpers) := 99; // Help detecting, if changed while loading.
  inherited Loading;
end;

procedure TSynPasSyn.SetLine(const NewValue: string; LineNumber:Integer);
begin
  //DebugLn(['TSynPasSyn.SetLine START LineNumber=',LineNumber,' Line="',NewValue,'"']);
  if FNeedCustomTokenBuild then
    RebuildCustomTokenInfo;

  fLineStr := NewValue;
  fLineLen:=length(fLineStr);
  fLine:=PChar(Pointer(fLineStr));
  Run := 0;
  FIsInSlash := False;
  Inherited SetLine(NewValue,LineNumber);
  PasCodeFoldRange.LastLineCodeFoldLevelFix := 0;
  PasCodeFoldRange.PasFoldFixLevel := 0;
  FStartCodeFoldBlockLevel := PasCodeFoldRange.MinimumNestFoldBlockLevel;
  FPasStartLevel := PasCodeFoldRange.MinimumCodeFoldBlockLevel;
  FSynPasRangeInfo.MinLevelIfDef := FSynPasRangeInfo.EndLevelIfDef;
  FSynPasRangeInfo.MinLevelRegion := FSynPasRangeInfo.EndLevelRegion;
  fLineNumber := LineNumber;
  FAtLineStart := True;
  if not IsCollectingNodeInfo then
    Next;
end; { SetLine }

procedure TSynPasSyn.SetNestedComments(const ANestedComments: boolean);
begin
  if FNestedComments = ANestedComments then Exit;
  FNestedComments := ANestedComments;
  PasCodeFoldRange.NestedComments:=FNestedComments;
end;

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

function TSynPasSyn.CheckPasDoc(APeekOnly: Boolean): boolean;
var
  p, r: LongInt;
  s: string;
begin
  Result := False;
  r := Run;
  inc(Run); // the @

  if fLine[Run] in ['(', ')', '-'] then begin
    inc(Run);
    Result := fPasDocSymbolAttri.IsEnabled;
    FIsPasDocSym := Result and not APeekOnly;
    if APeekOnly then
      Run := r;
    exit;
  end;

  p := Run;
  while fLine[Run] in ['A'..'Z', 'a'..'z'] do
    inc(Run);
  if p = Run then begin
    Run := r;
    exit;
  end;

  SetLength(s{%H-}, Run - p);
  move(fLine[p], s[1], Run - p);
  if FPasDocWordList.IndexOf(LowerCase(s)) >= 0 then begin
    Result := fPasDocKeyWordAttri.IsEnabled;
    FIsPasDocKey := Result and not APeekOnly;
  end
  else begin
    Result := fPasDocUnknownAttr.IsEnabled;
    FIsPasUnknown := Result and not APeekOnly;
  end;
  if APeekOnly or not Result then
    Run := r;
end;

procedure TSynPasSyn.BorProc;
var
  p: LongInt;
begin
  p:=Run;
  fTokenID := tkComment;
  if rsIDEDirective in fRange then
    fTokenID := tkIDEDirective;

  if FUsePasDoc and not(rsIDEDirective in fRange) and (fLine[Run] = '@') then begin
    if CheckPasDoc then
      exit;
  end;

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
    '@': begin
        if fLine[p+1] = '@' then
          inc(p)
        else
        if FUsePasDoc and not(rsIDEDirective in fRange) then begin
          Run := p;
          if CheckPasDoc(True) then
            exit;
          inc(p)
        end;
      end;
    end;
    Inc(p);
  until (p>=fLineLen);
  Run:=p;
end;

procedure TSynPasSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  if TextComp('modeswitch') then begin
    // modeswitch directive
    inc(Run,10);
    // skip space
    while (fLine[Run] in [' ',#9,#10,#13]) do inc(Run);
    if TextComp('nestedcomments') then
    begin
      inc(Run,14);
      // skip space
      while (fLine[Run] in [' ',#9,#10,#13]) do inc(Run);
      if fLine[Run] in ['+', '}'] then
        NestedComments:=True
      else
      if fLine[Run] = '-' then
        NestedComments:=False;
    end;
    if TextComp('typehelpers') then
    begin
      inc(Run,11);
      // skip space
      while (fLine[Run] in [' ',#9,#10,#13]) do inc(Run);
      if fLine[Run] in ['+', '}'] then
        TypeHelpers := True
      else
      if fLine[Run] = '-' then
        TypeHelpers := False;
    end;
  end;
  if TextComp('mode') then begin
    // $mode directive
    inc(Run,4);
    include(fRange, rsCompilerModeSet);
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
      CompilerMode:=pcmMacPas
    else
      exclude(fRange, rsCompilerModeSet);
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
                        (KeyCompEx(@Txt[i+1], PChar('fold'), 4))
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

  procedure StartDirectiveFoldBlock(ABlockType: TPascalCodeFoldBlockType); inline;
  begin
    dec(Run);
    inc(fStringLen); // include $
    StartCustomCodeFoldBlock(ABlockType);
    inc(Run);
  end;

  procedure EndDirectiveFoldBlock(ABlockType: TPascalCodeFoldBlockType); inline;
  begin
    dec(Run);
    inc(fStringLen); // include $
    EndCustomCodeFoldBlock(ABlockType);
    inc(Run);
  end;

  procedure EndStartDirectiveFoldBlock(ABlockType: TPascalCodeFoldBlockType); inline;
  begin
    dec(Run);
    inc(fStringLen); // include $
    EndCustomCodeFoldBlock(ABlockType);
    StartCustomCodeFoldBlock(ABlockType);
    inc(Run);
  end;

var
  nd: PSynFoldNodeInfo;
begin
  if (Run < fLineLen-1) and (fLine[Run+1] = '$') then begin
    // compiler directive
    fRange := fRange + [rsDirective];
    inc(Run, 2);
    fToIdent := Run;
    KeyHash;
    if (fLine[Run] in ['i', 'I']) and
       ( KeyComp('if') or KeyComp('ifc') or KeyComp('ifdef') or KeyComp('ifndef') or
         KeyComp('ifopt') )
    then
      StartDirectiveFoldBlock(cfbtIfDef)
    else
    if ( (fLine[Run] in ['e', 'E']) and ( KeyComp('endif') or KeyComp('endc') ) ) or
       KeyComp('ifend')
    then
      EndDirectiveFoldBlock(cfbtIfDef)
    else
    if (fLine[Run] in ['e', 'E']) and
       ( KeyComp('else') or KeyComp('elsec') or KeyComp('elseif') or KeyComp('elifc') )
    then
      EndStartDirectiveFoldBlock(cfbtIfDef)
    else
    if KeyComp('region') then begin
      StartDirectiveFoldBlock(cfbtRegion);
      if IsCollectingNodeInfo then
        // Scan ahead
        if ScanRegion then begin
          nd := CollectingNodeInfoList.LastItemPointer;
          if nd <> nil then
            nd^.FoldAction := nd^.FoldAction + [sfaDefaultCollapsed];
        end;
    end
    else if KeyComp('endregion') then
      EndDirectiveFoldBlock(cfbtRegion);
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
        StartDirectiveFoldBlock(cfbtRegion);
        if IsCollectingNodeInfo then
          // Scan ahead
          if ScanRegion then begin
            nd := CollectingNodeInfoList.LastItemPointer;
            if nd <> nil then
              nd^.FoldAction := nd^.FoldAction + [sfaDefaultCollapsed];
          end;
      end
      else if KeyComp('endregion') then
        EndDirectiveFoldBlock(cfbtRegion)
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
    if FUsePasDoc and (fLine[Run] = '@') and CheckPasDoc(True) then
      exit;
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
    if (TopPascalCodeFoldBlockType in cfbtVarConstTypeExt + [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCaseSection]) and
       ( (rsProperty in fRange) or not(rsAfterClassMembers in fRange) )
    then
      fRange := fRange + [rsVarTypeInSpecification];

    // modifiers "alias: 'foo';"
    if (PasCodeFoldRange.BracketNestLevel = 0) then begin
      if (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsInProcHeader]) and
         (TopPascalCodeFoldBlockType in ProcModifierAllowed)
      then
        FRange := FRange + [rsInProcHeader]
      else
      if TopPascalCodeFoldBlockType in [cfbtConstBlock, cfbtLocalConstBlock, cfbtClassConstBlock] then
        fRange := fRange + [rsInTypedConst];
    end;
  end;
end;

procedure TSynPasSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then begin
    inc(Run);
    // generic TFoo<..>= // no space between > and =
    if TopPascalCodeFoldBlockType in [cfbtTypeBlock, cfbtLocalTypeBlock, cfbtClassTypeBlock] then
      fRange := fRange + [rsAfterEqual, rsAfterEqualOrColon];
  end;
  //      DoAfterOperator;
  if fRange * [rsProperty, rsVarTypeInSpecification] = [rsProperty] then
    fRange := fRange + [rsAtPropertyOrReadWrite]
  else
  if rsInRaise in fRange then
    FNextTokenState := tsAfterRaise;
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
  if FLine[Run] in ['A'..'Z', 'a'..'z', '_'] then begin
    fTokenID := tkIdentifier;
    while Identifiers[fLine[Run]] do inc(Run);
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
  DoAfterOperator;
  FOldRange := FOldRange - [rsAtPropertyOrReadWrite];
end;

procedure TSynPasSyn.CaretProc;
var
  t: TPascalCodeFoldBlockType;
begin
  inc(Run);
  fTokenID := tkSymbol;

  t := TopPascalCodeFoldBlockType;
  if ( (t in PascalStatementBlocks - [cfbtAsm])                 //cfbtClass, cfbtClassSection,
       or
       ( (t in [cfbtVarBlock, cfbtLocalVarBlock, cfbtConstBlock, cfbtLocalConstBlock, cfbtClassConstBlock]) and
         (rsAfterEqual in fRange)
       )
       or
       ( (t in [cfbtProcedure, cfbtAnonymousProcedure, cfbtTypeBlock, cfbtLocalTypeBlock]) and
         (PasCodeFoldRange.BracketNestLevel > 0) and
         (fRange * [rsInProcHeader, rsAfterEqual] = [rsInProcHeader, rsAfterEqual])
       )
     ) and
     not(rsAfterIdentifierOrValue in fRange)
  then begin
    if Run<fLineLen then begin
      if (Run+1 < fLineLen) and (fLine[Run] = '{') and (fLine[Run+1] = '$')  then begin
        // "{$" directive takes precedence
        fTokenID := tkSymbol;
        exit;
      end;
      inc(Run);
    end;
    fTokenID := tkString;
  end
  else begin
    fRange := fRange + [rsAfterIdentifierOrValue];
    FOldRange := FOldRange - [rsAfterIdentifierOrValue];
  end;
end;

procedure TSynPasSyn.NullProc;
begin
  if (Run = 0) and (rsSlash in fRange) then begin
    fRange := fRange - [rsSlash];
    if TopPascalCodeFoldBlockType = cfbtSlashComment then
      EndPascalCodeFoldBlockLastLine;
  end;
  fTokenID := tkNull;
  if Run<fLineLen then inc(Run);
end;

procedure TSynPasSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  if Run<fLineLen then begin
    while (IsNumberChar[FLine[Run]]) do inc(Run);
    if (FLine[Run]='.') and not(fLine[Run+1]='.')  then begin
      inc(Run);
      while (IsNumberChar[FLine[Run]]) do inc(Run);
    end;
    if (FLine[Run]='e') or (fLine[Run]='E')  then begin
      inc(Run);
      if (FLine[Run]='+') or (fLine[Run]='-')  then inc(Run);
      while (IsNumberChar[FLine[Run]]) do inc(Run);
    end;
  end;
end;

procedure TSynPasSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['.', ')'] then
    inc(Run)
  else
  if fRange * [rsProperty, rsAfterClassMembers] <> [] then begin // Also happens for result-type of functions (if they have a dot)
    fRange := fRange + [rsAtPropertyOrReadWrite];
    FOldRange := FOldRange - [rsAtPropertyOrReadWrite];
  end;
  if (FTokenState = tsAfterProcName) then begin
    if rsInProcHeader in fRange then
      FTokenFlags := FTokenFlags + [tfProcName];
    FNextTokenState := tsAtProcName;
  end
  else
  if rsInRaise in fRange then
    FNextTokenState := tsAfterRaise;
end;

procedure TSynPasSyn.AnsiProc;
begin
  fTokenID := tkComment;
  if FUsePasDoc and (fLine[Run] = '@') then begin
    if CheckPasDoc then
      exit;
  end;

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
    if FUsePasDoc and (fLine[Run] = '@') then begin
      if fLine[Run+1] = '@' then
          inc(Run, 2)
      else
      if CheckPasDoc(True) then
        exit;
      Inc(Run);
    end
    else
      Inc(Run);
  until (Run>=fLineLen) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynPasSyn.RoundOpenProc;
begin
  Inc(Run);
  if Run>=fLineLen then begin
    fTokenID:=tkSymbol;
    if TopPascalCodeFoldBlockType = cfbtRecordCase then begin
      fStringLen := 1;
      Dec(Run);
      StartPascalCodeFoldBlock(cfbtRecordCaseSection, True); // TODO: only if case-label attr is set
      Inc(Run);
      PasCodeFoldRange.BracketNestLevel := 0
    end
    else
      PasCodeFoldRange.IncBracketNestLevel;
    exit;
  end;

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
          if FUsePasDoc and (fLine[Run] = '@') and CheckPasDoc(True) then
            exit;
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
        if TopPascalCodeFoldBlockType = cfbtRecordCase then begin
          fStringLen := 1;
          Dec(Run);
          StartPascalCodeFoldBlock(cfbtRecordCaseSection, True); // TODO: only if case-label attr is set
          Inc(Run);
          PasCodeFoldRange.BracketNestLevel := 0;
          fRange := fRange - [rsVarTypeInSpecification, rsAfterEqual, rsAfterEqualOrColon] + [rsAfterSemiColon];
        end
        else
          PasCodeFoldRange.IncBracketNestLevel;
      end;
  end;
end;

procedure TSynPasSyn.RoundCloseProc;
begin
  fTokenID := tkSymbol;
  fRange := fRange + [rsAfterIdentifierOrValue];
  FOldRange := FOldRange - [rsAfterIdentifierOrValue];
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (TopPascalCodeFoldBlockType in [cfbtRecordCase, cfbtRecordCaseSection])
  then begin
    // End of case-section can close ONE embedded case
    fStringLen := 1;
    if TopPascalCodeFoldBlockType = cfbtRecordCase then
      EndPascalCodeFoldBlock;
    if TopPascalCodeFoldBlockType = cfbtRecordCaseSection then
      EndPascalCodeFoldBlock;
  end
  else
    PasCodeFoldRange.DecBracketNestLevel;
  inc(Run);
end;

procedure TSynPasSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader])
  then
    FOldRange := FOldRange - [rsWasInProcHeader];
  PasCodeFoldRange.IncBracketNestLevel;
end;

procedure TSynPasSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  fRange := fRange + [rsAfterIdentifierOrValue];
  FOldRange := FOldRange - [rsAfterIdentifierOrValue];
  PasCodeFoldRange.DecBracketNestLevel;

  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsWasInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then
    FRange := FRange + [rsInProcHeader] - [rsWasInProcHeader]; // rsWasInProcHeader was removed from FOldRange
end;

procedure TSynPasSyn.EqualSignProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  fRange := fRange + [rsAfterEqualOrColon, rsAfterEqual];
  if (TopPascalCodeFoldBlockType in cfbtVarConstTypeExt + [cfbtClass, cfbtClassSection, cfbtRecord, cfbtRecordCaseSection]) and
     not(rsAfterClassMembers in fRange)
  then
    fRange := fRange + [rsVarTypeInSpecification];
  DoAfterOperator;
end;

procedure TSynPasSyn.SemicolonProc;
var
  tfb: TPascalCodeFoldBlockType;
  InSkipBlocks: Boolean;
begin
  fTokenID := tkSymbol;
  tfb := TopPascalCodeFoldBlockType;
  InSkipBlocks := rsSkipAllPasBlocks in fRange;
  Exclude(fRange, rsSkipAllPasBlocks);

  fStringLen := 1;
  if tfb = cfbtUses then
    EndPascalCodeFoldBlock;

  if (tfb = cfbtClass) and ((rsAfterClass in fRange) or InSkipBlocks) then begin
    EndPascalCodeFoldBlock(True, True);
    fRange := fRange - [rsInObjcProtocol];
  end;

  EndStatement(tfb, [cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfThen,cfbtIfElse]);

  Inc(Run);

  if (tfb in [cfbtCase, cfbtRecordCase]) then
    fRange := fRange + [rsAtCaseLabel];

  if (tfb in [cfbtClass, cfbtClassSection, cfbtClassConstBlock, cfbtClassTypeBlock]) and
     (fRange * [rsVarTypeInSpecification, rsAfterClassMembers] = [rsVarTypeInSpecification])
  then
    fRange := fRange + [rsAfterClassField];

  if (fRange * [rsProperty, rsInProcHeader] <> []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then begin
    if rsInProcHeader in fRange then
      fRange := fRange + [rsWasInProcHeader];
    fRange := fRange - [rsProperty, rsInProcHeader];
  end;

  if FTokenState in [tsAfterExternal, tsAfterCvar] then
    FNextTokenState := FTokenState
  else
  if rsInTypedConst in fRange then
    FNextTokenState := tsAfterTypedConst;

  fRange := fRange - [rsVarTypeInSpecification, rsAfterEqual, rsInTypedConst] + [rsAfterSemiColon];
  FOldRange := FOldRange - [rsAfterSemiColon];
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
    FIsInSlash := True;
    while not(fLine[Run] in [#0, #10, #13]) do
      if FUsePasDoc and (fLine[Run] = '@') then begin
        if fLine[Run+1] = '@' then
          inc(Run, 2)
        else
        if CheckPasDoc(True) then
          exit;
        Inc(Run);
      end
      else
        Inc(Run);
  end else begin
    Inc(Run);
    fTokenID := tkSymbol;
    DoAfterOperator;
  end;
end;

procedure TSynPasSyn.SlashContinueProc;
begin
  if FIsInSlash and (fLine[Run] = '@') then begin
    if CheckPasDoc then
      exit;
  end;

  if FIsInSlash or ((fLine[Run] = '/') and (fLine[Run + 1] = '/')) then begin
    FIsInSlash := True;
    // Continue fold block
    fTokenID := tkComment;
    while not(fLine[Run] in [#0, #10, #13]) do
      if FUsePasDoc and (fLine[Run] = '@') then begin
        if fLine[Run+1] = '@' then
          inc(Run, 2)
        else
        if CheckPasDoc(True) then
          exit;
        Inc(Run);
      end
      else
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
  if IsCombiningCodePoint(fLine+Run) then begin
    IdentProc;
    exit;
  end;

  fTokenID := tkSpace;

  if not IsSpaceChar[FLine[Run]] then
    exit;

  inc(Run);
  while IsSpaceChar[FLine[Run]] do inc(Run);
  if IsCombiningCodePoint(fLine+Run) then
    dec(Run);
end;

procedure TSynPasSyn.StringProc;
begin
  fTokenID := tkString;
  if FTokenState = tsAfterExternal then
    FNextTokenState := tsAfterExternal; // external 'foo' name 'bar'
  Inc(Run);
  while (not (fLine[Run] in [#0, #10, #13])) do begin
    if fLine[Run] = '''' then begin
      Inc(Run);
      if (fLine[Run] <> '''') then
        break;
    end;
    Inc(Run);
  end;

  // modifiers like "alias" take a string as argument
  if (PasCodeFoldRange.BracketNestLevel = 0) and
     (fRange * [rsInProcHeader, rsProperty, rsAfterEqualOrColon, rsWasInProcHeader] = [rsInProcHeader]) and
     (TopPascalCodeFoldBlockType in ProcModifierAllowed)
  then
    FRange := FRange + [rsInProcHeader];
end;

procedure TSynPasSyn.DoubleQuoteProc;
begin
  if (spmsmDoubleQuote in FStringMultilineMode) then begin
    Inc(Run);
    StringProc_MultiLineDQ();
  end
  else
    SymbolProc();
end;

procedure TSynPasSyn.StringProc_MultiLineDQ;
begin
  fTokenID := tkString;
  fRange := fRange + [rsAnsiMultiDQ];

  while (fLine[Run] <> #0) do
  begin
    if (fLine[Run] = '"') then
    begin
      Inc(Run);
      if (fLine[Run] <> '"') then
      begin
        fRange := fRange - [rsAnsiMultiDQ];
        Break;
      end;
    end;
    Inc(Run);
  end;
end;

procedure TSynPasSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  if rsProperty in fRange then begin
    fRange := fRange + [rsAtPropertyOrReadWrite];
    FOldRange := FOldRange - [rsAtPropertyOrReadWrite];
  end
  else
  if rsInRaise in fRange then
    FNextTokenState := tsAfterRaise;
end;

function TSynPasSyn.TypeHelpersIsStored: Boolean;
begin
  Result := FTypeHelpers = (FCompilerMode in [pcmDelphi]);
end;

procedure TSynPasSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPasSyn.Next;
var
  IsAtCaseLabel: Boolean;
  OldNestLevel, i: Integer;
  CustTk: TSynPasSynCustomTokenInfoList;
  CustTkList: PSynPasSynCustomTokenInfo;
  UpperTk: String;
  p: pointer;
begin
  fAsmStart := False;
  FIsPasDocKey := False;
  FIsPasDocSym := False;
  FIsPasUnknown := False;
  FTokenIsCaseLabel := False;
  fTokenPos := Run;
  FCustomTokenMarkup := nil;
  if Run>=fLineLen then begin
    NullProc;
    exit;
  end;
  if rsAnsiMultiDQ in fRange then
    StringProc_MultiLineDQ()
  else
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
        FNextTokenState := tsNone;
        FOldRange := fRange;
        OldNestLevel := PasCodeFoldRange.BracketNestLevel;
        if (PasCodeFoldRange.BracketNestLevel = 1) then // procedure foo; [attr...]
          FOldRange := FOldRange - [rsWasInProcHeader];
        FTokenFlags := [];
        //if rsAtEqual in fRange then
        //  fRange := fRange + [rsAfterEqualOrColon] - [rsAtEqual]
        //else

        IsAtCaseLabel := rsAtCaseLabel in fRange;

        FTokenHashKey := 0;
        fProcTable[fLine[Run]];

        if FTokenID in FCustomTokenInfo[FTokenHashKey and 255].MatchTokenKinds then begin
          CustTk := FCustomTokenInfo[FTokenHashKey and 255].List;
          if CustTk <> nil then begin
            UpperTk := '';
            p := CustTk.List;
            if p <> nil then begin
              CustTkList := PPSynPasSynCustomTokenInfo(p)^;
              for i := 0 to CustTk.Count - 1 do begin
                if (FTokenID in CustTkList^.MatchTokenKinds) then begin
                  if UpperTk = '' then
                    UpperTk := UpperCase(GetToken);
                  if (UpperTk = CustTkList^.Word) then begin
                    FCustomTokenMarkup := CustTkList^.Token.FMarkup;
                    break;
                  end
                end;
                inc(CustTkList);
              end;
            end;
          end;
        end;

        if (FTokenID = tkIdentifier) and (FTokenState = tsAtProcName) then begin
          if rsInProcHeader in fRange then
            FTokenFlags := FTokenFlags + [tfProcName];
          FNextTokenState := tsAfterProcName;
        end;

        if not (FTokenID in [tkSpace, tkComment, tkIDEDirective, tkDirective, tkNull]) then
          FTokenState := FNextTokenState;

        if (IsAtCaseLabel) and (rsAtCaseLabel in fRange) then begin
          FTokenIsCaseLabel := True;
          if (FTokenID = tkKey) then
            fRange := fRange - [rsAtCaseLabel];
        end;

        if not (FTokenID in [tkSpace, tkComment, tkIDEDirective, tkDirective]) then begin
          if (PasCodeFoldRange.BracketNestLevel = 0) and
             (OldNestLevel = 0)
          then
            fRange := fRange - [rsAfterClass];
          if (PasCodeFoldRange.BracketNestLevel > 0) or
             (OldNestLevel > 0)
          then
            FOldRange := FOldRange - [rsInTypeHelper];

          fRange := fRange -
            (FOldRange * [rsAfterEqualOrColon, rsAfterSemiColon,
                          rsAtPropertyOrReadWrite, rsAfterClassField,
                          rsAfterIdentifierOrValue, rsWasInProcHeader,
                          rsInTypeHelper]
            );

          if (FTokenID = tkIdentifier) and (rsInTypeHelper in FOldRange) then
            fRange := fRange + [rsAfterSemiColon];

          if rsAtClass in fRange then begin
            if FOldRange * [rsAtClass, rsAfterClass] <> [] then
              fRange := fRange + [rsAfterClass] - [rsAtClass]
            else
              fRange := fRange + [rsAfterClass];
          end
        end
        else begin
          if rsAtClass in fRange then
            fRange := fRange + [rsAfterClass];
        end;

        if (FTokenID = tkIdentifier) then
          fRange := fRange + [rsAfterIdentifierOrValue];
      end
  end;
  if FAtLineStart and not(FTokenID in [tkSpace, tkComment, tkIDEDirective]) then
    FAtLineStart := False;
  //DebugLn('TSynPasSyn.Next Run=%2d TkPos=%2d %12s Tk="%s" -- TS=%s  Rng=%s  F=%s  ()=%d', [Run, fTokenPos, dbgs(FTokenID), GetToken, dbgs(FTokenState), dbgs(fRange), dbgs(TopPascalCodeFoldBlockType), PasCodeFoldRange.BracketNestLevel]);
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
    SYN_ATTR_NUMBER: Result := fNumberAttri;
    SYN_ATTR_DIRECTIVE: Result := fDirectiveAttri;
    SYN_ATTR_ASM: Result := fAsmAttri;
  else
    Result := nil;
  end;
end;

function TSynPasSyn.GetEol: Boolean;
begin
  Result := (fTokenID = tkNull) and (Run >= fLineLen);
end;

function TSynPasSyn.GetToken: string;
begin
  SetString(Result, @fLine[fTokenPos], Run - fTokenPos);
end;

procedure TSynPasSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  if TokenLength>0 then begin
    TokenStart:=@fLine[fTokenPos];
  end else begin
    TokenStart:=nil;
  end;
end;

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
  tid: TtkTokenKind;
begin
  tid := GetTokenID;
  case tid of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkIDEDirective: begin
      FCurIDEDirectiveAttri.Assign(FCommentAttri);
      FCurIDEDirectiveAttri.Merge(FIDEDirectiveAttri);
      Result := FCurIDEDirectiveAttri;
    end;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkModifier: Result := fModifierAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkDirective: Result := fDirectiveAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;

  if FTokenIsCaseLabel and (tid in [tkIdentifier, tkKey, tkNumber, tkString])
  then begin
    FCurCaseLabelAttri.Assign(Result);
    FCurCaseLabelAttri.Merge(FCaseLabelAttri);
    Result := FCurCaseLabelAttri;
  end;

  if FIsPasDocKey then begin
    FCurPasDocAttri.Assign(Result);
    FCurPasDocAttri.Merge(fPasDocKeyWordAttri);
    Result := FCurPasDocAttri;
  end
  else
  if FIsPasDocSym then begin
    FCurPasDocAttri.Assign(Result);
    FCurPasDocAttri.Merge(fPasDocSymbolAttri);
    Result := FCurPasDocAttri;
  end
  else
  if FIsPasUnknown then begin
    FCurPasDocAttri.Assign(Result);
    FCurPasDocAttri.Merge(fPasDocUnknownAttr);
    Result := FCurPasDocAttri;
  end;

  if (tid in [tkIdentifier, tkSymbol]) and
     (fRange * [rsInProcHeader, rsAfterEqualOrColon, rsAfterEqual] = [rsInProcHeader]) and
     (tfProcName in FTokenFlags) and
     (FOldRange * [rsAfterEqualOrColon, rsAfterEqual] = []) and
     (PasCodeFoldRange.BracketNestLevel = 0)
  then begin
    FCurProcedureHeaderNameAttr.Assign(Result);
    FCurProcedureHeaderNameAttr.Merge(FProcedureHeaderNameAttr);
    Result := FCurProcedureHeaderNameAttr;
  end;

  if FCustomTokenMarkup <> nil then begin
    FCustomTokenMergedMarkup.Assign(Result);
    FCustomTokenMergedMarkup.Merge(FCustomTokenMarkup);
    Result := FCustomTokenMergedMarkup;
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

function TSynPasSyn.GetTokenLen: Integer;
begin
  Result := Run-fTokenPos;
end;

function TSynPasSyn.GetRange: Pointer;
begin
  // For speed reasons, we work with fRange instead of CodeFoldRange.RangeType
  // -> update now
  CodeFoldRange.RangeType:=Pointer(PtrUInt(Integer(fRange)));
  PasCodeFoldRange.TokenState := FTokenState;
  PasCodeFoldRange.TypeHelpers := TypeHelpers;
  // return a fixed copy of the current CodeFoldRange instance
  Result := inherited GetRange;
end;

procedure TSynPasSyn.SetRange(Value: Pointer);
begin
  //DebugLn(['TSynPasSyn.SetRange START']);
  inherited SetRange(Value);
  CompilerMode := PasCodeFoldRange.Mode;
  NestedComments := PasCodeFoldRange.NestedComments;
  TypeHelpers := PasCodeFoldRange.TypeHelpers;
  FTokenState := PasCodeFoldRange.TokenState;
  fRange := TRangeStates(Integer(PtrUInt(CodeFoldRange.RangeType)));
  FSynPasRangeInfo := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[LineIndex-1];
end;

procedure TSynPasSyn.StartAtLineIndex(LineNumber: Integer);
begin
  inherited StartAtLineIndex(LineNumber);
end;

function TSynPasSyn.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if fRange * [rsAnsi, rsBor] <> [] then
    Result := fCommentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

procedure TSynPasSyn.ResetRange;
begin
  fRange := [];
  FTokenState := tsNone;
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
  with TRegistry.Create do
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

function TSynPasSyn.IsLineStartingInDirective(ALineIndex: TLineIdx): Boolean;
var
  r: Pointer;
begin
  Result := False;
  if ALineIndex < 1 then exit;
  r := CurrentRanges[ALineIndex-1];
  if (r <> nil) and (r <> NullRange) then
    Result := rsDirective in TRangeStates(Integer(PtrUInt(TSynPasSynRange(r).RangeType)));
end;

function TSynPasSyn.FoldBlockEndLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
var
  inf: TSynPasRangeInfo;
  r, r2: Pointer;
begin
  Assert(CurrentRanges <> nil, 'TSynCustomFoldHighlighter.FoldBlockEndLevel requires CurrentRanges');
  Result := 0;
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit;

  inf.EndLevelIfDef := 0;
  inf.MinLevelIfDef := 0;
  inf.EndLevelRegion := 0;
  inf.MinLevelRegion := 0;
  if AFilter.FoldGroup  in [0, FOLDGROUP_REGION, FOLDGROUP_IFDEF] then
    inf := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[ALineIndex];

  if AFilter.FoldGroup  in [0, FOLDGROUP_PASCAL] then begin
    // All or Pascal
    r := CurrentRanges[ALineIndex];
    if (r <> nil) and (r <> NullRange) then begin
      r2 := TSynPasSynRange(CurrentRanges[ALineIndex + 1]);
      if sfbIncludeDisabled in AFilter.Flags then begin
        Result := TSynPasSynRange(r).NestFoldStackSize;
        if (r2 <> nil) and (r2 <> NullRange) then
          Result := Result + TSynPasSynRange(r2).LastLineCodeFoldLevelFix;
      end
      else begin
        Result := TSynPasSynRange(r).CodeFoldStackSize;
        if (r2 <> nil) and (r2 <> NullRange) then
          Result := Result + TSynPasSynRange(r2).PasFoldFixLevel;
      end;
    end;
  end;

  if AFilter.FoldGroup  in [0, FOLDGROUP_REGION] then begin
    // All or REGION
    if (  FFoldConfig[ord(cfbtRegion)].Enabled and
          (FFoldConfig[ord(cfbtRegion)].Modes * [fmFold, fmHide] <> [])
       ) or
       (sfbIncludeDisabled in AFilter.Flags)
    then
      Result := Result + inf.EndLevelRegion;
  end;

  if AFilter.FoldGroup  in [0, FOLDGROUP_IFDEF] then begin
    // All or IFDEF
    if ( FFoldConfig[ord(cfbtIfDef)].Enabled and
         (FFoldConfig[ord(cfbtIfDef)].Modes * [fmFold, fmHide] <> [])
       ) or
       (sfbIncludeDisabled in AFilter.Flags)
    then
      Result := Result + inf.EndLevelIfDef;
  end;
end;

function TSynPasSyn.FoldBlockMinLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
var
  inf: TSynPasRangeInfo;
  r, r2: Pointer;
begin
  Assert(CurrentRanges <> nil, 'TSynCustomFoldHighlighter.FoldBlockMinLevel requires CurrentRanges');
  Result := 0;
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit;

  inf.EndLevelIfDef := 0;
  inf.MinLevelIfDef := 0;
  inf.EndLevelRegion := 0;
  inf.MinLevelRegion := 0;
  if AFilter.FoldGroup  in [0, FOLDGROUP_REGION, FOLDGROUP_IFDEF] then
    inf := TSynHighlighterPasRangeList(CurrentRanges).PasRangeInfo[ALineIndex];

  if AFilter.FoldGroup  in [0, FOLDGROUP_PASCAL] then begin
    // All or Pascal
    (* Range.EndLevel can be smaller. because Range.MinLevel does not know the LastLineFix
       Using a copy of FoldBlockEndLevel *)
    r := CurrentRanges[ALineIndex];
    if (r <> nil) and (r <> NullRange) then begin
      r2 := TSynPasSynRange(CurrentRanges[ALineIndex + 1]);
      if sfbIncludeDisabled in AFilter.Flags then begin
        Result := TSynPasSynRange(r).NestFoldStackSize;
        if (r2 <> nil) and (r2 <> NullRange) then
          Result := Result + TSynPasSynRange(r2).LastLineCodeFoldLevelFix;
        // now Result = FoldBlockEndLevel
        Result := Min(Result, TSynPasSynRange(r).MinimumNestFoldBlockLevel);
      end
      else begin
        Result := TSynPasSynRange(r).CodeFoldStackSize;
        if (r2 <> nil) and (r2 <> NullRange) then
          Result := Result + TSynPasSynRange(r2).PasFoldFixLevel;
        // now Result = FoldBlockEndLevel
        Result := Min(Result, TSynPasSynRange(r).MinimumCodeFoldBlockLevel);
      end;
    end;
  end;

  if AFilter.FoldGroup  in [0, FOLDGROUP_REGION] then begin
    // All or REGION
    if (  FFoldConfig[ord(cfbtRegion)].Enabled and
          (FFoldConfig[ord(cfbtRegion)].Modes * [fmFold, fmHide] <> [])
       ) or
       (sfbIncludeDisabled in AFilter.Flags)
    then
      Result := Result + inf.MinLevelRegion;
  end;

  if AFilter.FoldGroup  in [0, FOLDGROUP_IFDEF] then begin
    // All or IFDEF
    if ( FFoldConfig[ord(cfbtIfDef)].Enabled and
         (FFoldConfig[ord(cfbtIfDef)].Modes * [fmFold, fmHide] <> [])
       ) or
       (sfbIncludeDisabled in AFilter.Flags)
    then
      Result := Result + inf.MinLevelIfDef;
  end;
end;

function TSynPasSyn.FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer; out
  AType: Pointer; const AFilter: TSynFoldBlockFilter): boolean;
var
  r, r2: Pointer;
  c: Integer;
  Fold: TSynCustomCodeFoldBlock;
begin
  Result := false;
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit;
  if ANestIndex < 0 then exit;

  if AFilter.FoldGroup = FOLDGROUP_PASCAL then begin
    if AFilter.Flags = [] then begin

      r := CurrentRanges[ALineIndex];
      if (r <> nil) and (r <> NullRange) then begin
        r2 := TSynPasSynRange(CurrentRanges[ALineIndex + 1]);
        c := TSynPasSynRange(r).CodeFoldStackSize;
        if (r2 <> nil) and (r2 <> NullRange) then
          c := c + TSynPasSynRange(r2).PasFoldFixLevel;

        if ANestIndex < c then begin
          c := TSynPasSynRange(r).NestFoldStackSize - 1 - ANestIndex;
          Fold := TSynPasSynRange(r).Top;
          while (Fold <> nil) and
                ( (c > 0) or (Fold.BlockType >= CountPascalCodeFoldBlockOffset) )
          do begin
            if (Fold.BlockType < CountPascalCodeFoldBlockOffset) then
              dec(c);
            Fold := Fold.Parent;
          end;
          if Fold <> nil then begin
            AType := Fold.BlockType;
            if AType >= CountPascalCodeFoldBlockOffset then
              AType := AType - PtrUInt(CountPascalCodeFoldBlockOffset);
            Result := True;
          end;
        end;

      end;

    end;
    if AFilter.Flags = [sfbIncludeDisabled] then begin

      r := CurrentRanges[ALineIndex];
      if (r <> nil) and (r <> NullRange) then begin
        r2 := TSynPasSynRange(CurrentRanges[ALineIndex + 1]);
        c := TSynPasSynRange(r).NestFoldStackSize;
        if (r2 <> nil) and (r2 <> NullRange) then
          c := c + TSynPasSynRange(r2).LastLineCodeFoldLevelFix;

        if ANestIndex < c then begin
          c := TSynPasSynRange(r).NestFoldStackSize - 1 - ANestIndex;
          Fold := TSynPasSynRange(r).Top;
          while (Fold <> nil) and (c > 0) do begin
            Fold := Fold.Parent;
            dec(c);
          end;
          if Fold <> nil then begin
            AType := Fold.BlockType;
            if AType >= CountPascalCodeFoldBlockOffset then
              AType := AType - PtrUInt(CountPascalCodeFoldBlockOffset);
            Result := True;
          end;
        end;

      end;
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

procedure TSynPasSyn.GetTokenBounds(out LogX1, LogX2: Integer);
begin
  LogX1 := Run;
  LogX2 := LogX1 + fStringLen;
end;

function TSynPasSyn.ScanAheadForNextToken(RunOffs: Integer; out
  AFndLine: String; out ATokStart, ATokLen: integer; MaxLineCnt: Integer
  ): Boolean;
var
  Txt: PChar;
  TxtPos, TxtLen: Integer; // TxtPos in PChar
  NestBrace1, NestBrace2: Integer;
  CurLineIdx: Integer;

  function SetFoundToken: Boolean;
  var
    TxtPos2: Integer;
  begin
    TxtPos2 := TxtPos + 1;
    if Identifiers[Txt[TxtPos]] then
      while (TxtPos2 < TxtLen) and Identifiers[Txt[TxtPos2]] do
        inc(TxtPos2)
    else
      while (TxtPos2 < TxtLen) and
            not( Identifiers[Txt[TxtPos2]] or IsSpaceChar[Txt[TxtPos2]] )
      do
        inc(TxtPos2);
    Result    := True;
    ATokStart := TxtPos + 1; // PChar to String index
    ATokLen   := TxtPos2 - TxtPos;
  end;

begin
  Result := False;
  AFndLine := fLineStr;
  Txt    := fLine;
  TxtPos := Run + RunOffs;
  TxtLen := fLineLen;
  CurLineIdx := LineIndex;

  NestBrace1 := 0;
  NestBrace2 := 0;
  while true do begin
    while TxtPos < TxtLen do begin
      case Txt[TxtPos] of
        '{' : if (NestBrace2 = 0) and (NestedComments or (NestBrace1 = 0)) then
                inc(NestBrace1);
        '}' : if (NestBrace2 = 0) then
                if NestBrace1 > 0
                then dec(NestBrace1)
                else exit(SetFoundToken);
        '(' : if (NestBrace1 = 0) then
                if (TxtPos+1 <= TxtLen) and (Txt[TxtPos+1] = '*') then begin
                  if NestedComments or (NestBrace2 = 0) then begin
                    inc(NestBrace2);
                    inc(TxtPos);
                  end
                end
                else
                if (NestBrace2 = 0) then
                  exit(SetFoundToken);
        '*' : if (NestBrace1 = 0) then
                if  (TxtPos+1 <= TxtLen) and (Txt[TxtPos+1] = ')') and (NestBrace2 > 0)
                  then begin
                    dec(NestBrace2);
                    inc(TxtPos);
                  end
                  else
                  if NestBrace2 = 0 then
                    exit(SetFoundToken);
        '/' : If (NestBrace1 = 0) and (NestBrace2 = 0) then begin
                if  (TxtPos+1 <= TxtLen) and (Txt[TxtPos+1] = '/')
                  then TxtPos := TxtLen
                  else exit(SetFoundToken);
              end;
        #1..#32: {do nothing};
        else
          If (NestBrace1 = 0) and (NestBrace2 = 0) then begin
            Result := SetFoundToken;
            exit;
          end;
      end;
      inc(TxtPos);
    end;
    dec(MaxLineCnt);
    if MaxLineCnt < 0 then
      exit;
    inc(CurLineIdx);
    if CurLineIdx < CurrentLines.Count then begin
      AFndLine := CurrentLines[CurLineIdx];
      Txt    := PChar(AFndLine);
      TxtPos := 0;
      TxtLen := length(Txt);
    end
    else
      break;
  end;
end;

function TSynPasSyn.IsAnonymousFunc(RunOffs: Integer; AnIsFunction: boolean
  ): Boolean;
var
  FndLine: String;
  FndPos, FndLen: integer;
begin
  Result := ScanAheadForNextToken(RunOffs, FndLine, FndPos, FndLen, 0);
  if not Result then
    exit;

  case FndLine[FndPos] of
    ':':      Result := AnIsFunction;
    '(', ';': Result := True;
    'a', 'A': Result := ( (FndLen = 3) and KeyCompEx(@FndLine[FndPos], PChar('asm'),   3) );
    'b', 'B': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('begin'), 5) );
    'c', 'C': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('const'), 5) ) or
                        ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('cdecl'), 5) );
    'i', 'I': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('inline'), 5) ) or
                        ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('interrupt'), 5) );
    'l', 'L': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('label'), 5) );
    'n', 'N': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('none'), 5) );
    'p', 'P': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('pascal'), 5) );
    'r', 'R': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('register'), 5) );
    's', 'S': Result := ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('safecall'), 5) ) or
                        ( (FndLen = 5) and KeyCompEx(@FndLine[FndPos], PChar('stdcall'), 5) );
    't', 'T': Result := ( (FndLen = 4) and KeyCompEx(@FndLine[FndPos], PChar('type'),  4) );
    'v', 'V': Result := ( (FndLen = 3) and KeyCompEx(@FndLine[FndPos], PChar('var'),   3) );
  end;
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
  act := [sfaOpenFold, sfaFold];
  if UseCloseNodes then act := [sfaCloseFold, sfaFold];
  case TPascalCodeFoldBlockType(PtrUInt(FoldNodeInfo[ALineIndex].NodeInfoEx(FoldIndex, act).FoldType)) of
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
  node := FoldNodeInfo[ALineIndex].NodeInfoEx(FoldIndex, [sfaOpenFold, sfaFold]);
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
  if (FoldBlockEndLevel(Result, atype) > FoldBlockMinLevel(Result, atype)) then
    dec(Result);
  // Amount of lines, that will become invisible (excludes the cfCollapsed line)
  Result := Result - ALineIndex;
end;

function TSynPasSyn.FoldEndLine(ALineIndex, FoldIndex: Integer): integer;
var
  lvl, cnt, atype : Integer;
  node: TSynFoldNodeInfo;
begin
  node := FoldNodeInfo[ALineIndex].NodeInfoEx(FoldIndex, [sfaOpenFold, sfaFold]);
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
  while (Result < cnt) and (FoldBlockMinLevel(Result, atype) >= lvl) do inc(Result);
  // check if fold last line of block (not mixed "end begin")
  // and not lastlinefix
  if (Result = cnt) then
    dec(Result);
end;

function TSynPasSyn.LastLinePasFoldLevelFix(Index: Integer; AType: Integer;
  AIncludeDisabled: Boolean): integer;
var
  r: TSynPasSynRange;
begin
  // AIncludeDisabled only works for Pascal Nodes
  case AType of
    2: Result := 0;
    3: Result := 0;
    else
      begin
        if (Index < 0) or (Index >= CurrentLines.Count) then
          exit(0);
        r := TSynPasSynRange(CurrentRanges[Index]);
        if (r <> nil) and (Pointer(r) <> NullRange) then begin
          if AIncludeDisabled then
            Result := r.LastLineCodeFoldLevelFix  // all pascal nodes (incl. not folded)
          else
            Result := r.PasFoldFixLevel
        end
        else
          Result := 0;
      end;
  end;
end;


procedure TSynPasSyn.DoInitNode(var Node: TSynFoldNodeInfo;
  FinishingABlock: Boolean; ABlockType: Pointer; aActions: TSynFoldActions;
  AIsFold: Boolean);
var
  PasBlockType: TPascalCodeFoldBlockType;
  EndOffs: Integer;
  OneLine: Boolean;
  t: TSynCustomFoldConfig;
begin
  PasBlockType := TPascalCodeFoldBlockType(PtrUint(ABlockType));

  if FinishingABlock then
    EndOffs := -1
  else
    EndOffs := +1;
  //inherited DoInitNode(Node, FinishingABlock, ABlockType, aActions, AIsFold);
  aActions := aActions + [sfaMultiLine];

  if (not FinishingABlock) and  (ABlockType <> nil) then begin
    if (PasBlockType in [cfbtIfThen,cfbtForDo,cfbtWhileDo,cfbtWithDo,cfbtIfElse]) then
      //Include( aActions, sfaOutlineKeepLevelOnSameLine);
      Include( aActions, sfaOutlineKeepLevel);

    //if (PasBlockType in [cfbtIfElse]) then
    //  Include( aActions, sfaOutlineMergeLevelOnWrongCol);

    if (PasBlockType in [cfbtClassSection]) then begin
      t := FFoldConfig[ord(cfbtClass)];
      if t.Enabled and (sfaOutline in t.FoldActions) then
        Include( aActions, sfaOutlineMergeParent);
    end;

    if (PasBlockType in [cfbtProcedure, cfbtAnonymousProcedure]) then begin
      t := FFoldConfig[ord(cfbtTopBeginEnd)];
      if t.Enabled and (sfaOutline in t.FoldActions) then
        aActions := aActions + [sfaOutlineKeepLevel,sfaOutlineNoColor];
    end;

    //if (PasBlockType in [cfbtProcedure, cfbtAnonymousProcedure]) and (InProcLevel > 0) then //nested
    //  aActions := aActions + [sfaOutlineForceIndent];

    if (PasBlockType in [cfbtExcept]) then begin
      t := FFoldConfig[ord(cfbtTry)];
      if t.Enabled and (sfaOutline in t.FoldActions) then
        Include( aActions, sfaOutlineMergeParent);
    end;

   // if (PasBlockType in [cfbtIfThen, cfbtClass,cfbtRecord]) then
    //  aActions := aActions + [sfaOutlineNoLine];
  end;

  Node.LineIndex := LineIndex;
  Node.LogXStart := Run;
  Node.LogXEnd := Run + fStringLen;
  Node.FoldType := ABlockType;
  Node.FoldTypeCompatible := Pointer(PtrUInt(PascalFoldTypeCompatibility[PasBlockType]));
  Node.FoldAction := aActions;
  case PasBlockType of
    cfbtRegion:
      begin
        node.FoldGroup := FOLDGROUP_REGION;
        if AIsFold then
          Node.FoldLvlStart := FSynPasRangeInfo.EndLevelRegion
        else
          Node.FoldLvlStart := 0;
        Node.NestLvlStart := FSynPasRangeInfo.EndLevelRegion;
        OneLine := FinishingABlock and (Node.FoldLvlStart > FSynPasRangeInfo.MinLevelRegion);
      end;
    cfbtIfDef:
      begin
        node.FoldGroup := FOLDGROUP_IFDEF;
        if AIsFold then
          Node.FoldLvlStart := FSynPasRangeInfo.EndLevelIfDef
        else
          Node.FoldLvlStart := 0;
        Node.NestLvlStart := FSynPasRangeInfo.EndLevelIfDef;
        OneLine := FinishingABlock and (Node.FoldLvlStart > FSynPasRangeInfo.MinLevelIfDef);
      end;
    else
      begin
        //inherited DoInitNode(Node, FinishingABlock, ABlockType, aActions, AIsFold);
        node.FoldGroup := FOLDGROUP_PASCAL;
        if AIsFold then begin
          Node.FoldLvlStart := PasCodeFoldRange.CodeFoldStackSize;
          Node.NestLvlStart := PasCodeFoldRange.NestFoldStackSize;
          OneLine := FinishingABlock and (Node.FoldLvlStart > PasCodeFoldRange.MinimumCodeFoldBlockLevel); // MinimumCodeFoldBlockLevel);
        end else begin
          Node.FoldLvlStart := PasCodeFoldRange.NestFoldStackSize; // CodeFoldStackSize; ? but fails test // Todo: zero?
          Node.NestLvlStart := PasCodeFoldRange.NestFoldStackSize;
          OneLine := FinishingABlock and (Node.NestLvlStart > PasCodeFoldRange.MinimumNestFoldBlockLevel); // MinimumCodeFoldBlockLevel);
        end;
      end;
  end;
  Node.NestLvlEnd := Node.NestLvlStart + EndOffs;
  if not (sfaFold in aActions) then
    EndOffs := 0;
  Node.FoldLvlEnd := Node.FoldLvlStart + EndOffs;
  if OneLine then  // find opening node
    RepairSingleLineNode(Node);
end;

procedure TSynPasSyn.StartCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
var
  act: TSynFoldActions;
  nd: TSynFoldNodeInfo;
  FoldBlock, BlockEnabled: Boolean;
  ConfigP: PSynCustomFoldConfig;
begin
  (* Currently no need to map / only IFDEF / REGION *)
  //ConfigP := @FFoldConfig[ord(PascalFoldTypeConfigMap[ABlockType])];
  ConfigP := @FFoldConfig[ord(ABlockType)];
  BlockEnabled := ConfigP^.Enabled;
  FoldBlock := BlockEnabled and (ConfigP^.Modes * [fmFold, fmHide] <> []);
  //if not ConfigP^.Enabled then exit;
  if IsCollectingNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    act := [sfaOpen, sfaOpenFold];
    if BlockEnabled then
      act := act + ConfigP^.FoldActions;
    if not FAtLineStart then
      act := act - [sfaFoldHide];
    DoInitNode(nd{%H-}, False, Pointer(PtrUInt(ABlockType)), act, FoldBlock);
    CollectingNodeInfoList.Add(nd);
  end;
  //if not FoldBlock then
  //  exit;
  case ABlockType of
    cfbtIfDef:
      inc(FSynPasRangeInfo.EndLevelIfDef);
    cfbtRegion:
      inc(FSynPasRangeInfo.EndLevelRegion);
  end;
end;

procedure TSynPasSyn.EndCustomCodeFoldBlock(ABlockType: TPascalCodeFoldBlockType);
var
  act: TSynFoldActions;
  nd: TSynFoldNodeInfo;
  FoldBlock, BlockEnabled: Boolean;
  ConfigP: PSynCustomFoldConfig;
begin
  (* Currently no need to map / only IFDEF / REGION *)
  //ConfigP := @FFoldConfig[ord(PascalFoldTypeConfigMap[ABlockType])];
  ConfigP := @FFoldConfig[ord(ABlockType)];
  // TODO: Why is "FoldBlock" = Enabled? Instead of Modes fmFold,fmHide?
  FoldBlock := ConfigP^.Enabled;
  //if not ConfigP^.Enabled then exit;
  if IsCollectingNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    BlockEnabled := ConfigP^.Enabled;
    act := [sfaClose, sfaCloseFold];
    if BlockEnabled then
      act := act + ConfigP^.FoldActions;
    if not FoldBlock then
      act := act - [sfaFold, sfaFoldFold, sfaFoldHide];
    DoInitNode(nd{%H-}, True, Pointer(PtrUInt(ABlockType)), act, FoldBlock); // + ConfigP^.FoldActions);
    CollectingNodeInfoList.Add(nd);
  end;
  //if not FoldBlock then
  //  exit;
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

function TSynPasSyn.CloseOneFold(ACurTfb: TPascalCodeFoldBlockType;
  ACloseFold: TPascalCodeFoldBlockType): TPascalCodeFoldBlockType;
begin
  if ACurTfb = ACloseFold then begin
    EndPascalCodeFoldBlock;
    Result:= TopPascalCodeFoldBlockType;
  end
  else
    Result := ACurTfb;
end;

function TSynPasSyn.CloseOneFold(ACurTfb: TPascalCodeFoldBlockType;
  ACloseFolds: TPascalCodeFoldBlockTypes): TPascalCodeFoldBlockType;
begin
  if ACurTfb in ACloseFolds then begin
    EndPascalCodeFoldBlock;
    Result:= TopPascalCodeFoldBlockType;
  end
  else
    Result := ACurTfb;
end;

function TSynPasSyn.CloseFolds(ACurTfb: TPascalCodeFoldBlockType;
  ACloseFolds: TPascalCodeFoldBlockTypes): TPascalCodeFoldBlockType;
begin
  while ACurTfb in ACloseFolds do begin
    EndPascalCodeFoldBlock;
    ACurTfb := TopPascalCodeFoldBlockType;
  end;
  Result := ACurTfb;
end;

procedure TSynPasSyn.CollectNodeInfo(FinishingABlock: Boolean;
  ABlockType: Pointer; LevelChanged: Boolean);
begin
  // nothing
end;

function TSynPasSyn.CreateFoldNodeInfoList: TLazSynFoldNodeInfoList;
begin
  Result := TLazSynFoldNodeInfoList.Create;
end;

procedure TSynPasSyn.ScanFoldNodeInfo();
var
  nd: PSynFoldNodeInfo;
  i: Integer;
begin
  fStringLen := 0;
  inherited ScanFoldNodeInfo;

  fStringLen := 0;
  i := LastLinePasFoldLevelFix(LineIndex+1, FOLDGROUP_PASCAL, True);  // all pascal nodes (incl. not folded)
  while i < 0 do begin
    EndPascalCodeFoldBlock;
    CollectingNodeInfoList.LastItemPointer^.FoldAction :=
      CollectingNodeInfoList.LastItemPointer^.FoldAction + [sfaCloseForNextLine];
    inc(i);
  end;
  if LineIndex = CurrentLines.Count - 1 then begin
    // last LineIndex, close all folds
    // Run (for LogXStart) is at LineIndex-end
    i := CollectingNodeInfoList.CountAll;
    while TopPascalCodeFoldBlockType <> cfbtNone do
      EndPascalCodeFoldBlock(True);
    while FSynPasRangeInfo.EndLevelIfDef > 0 do
      EndCustomCodeFoldBlock(cfbtIfDef);
    while FSynPasRangeInfo.EndLevelRegion > 0 do
      EndCustomCodeFoldBlock(cfbtRegion);
    while i < CollectingNodeInfoList.CountAll do begin
      nd := CollectingNodeInfoList.ItemPointer[i];
      nd^.FoldAction := nd^.FoldAction + [sfaLastLineClose];
      inc(i);
    end;
  end;
end;

function TSynPasSyn.StartPascalCodeFoldBlock(
  ABlockType: TPascalCodeFoldBlockType; ForceDisabled: Boolean
  ): TSynCustomCodeFoldBlock;
var
  p: PtrInt;
  FoldBlock, BlockEnabled: Boolean;
  act: TSynFoldActions;
  nd: TSynFoldNodeInfo;
  ConfigP: PSynCustomFoldConfig;
begin
  if rsSkipAllPasBlocks in fRange then exit(nil);
  ConfigP := @FFoldConfig[ord(PascalFoldTypeConfigMap[ABlockType])];
  BlockEnabled := ConfigP^.Enabled;
  if (not BlockEnabled) and (not ForceDisabled) and
     (not ConfigP^.IsEssential)
  then
    exit(nil);

  FoldBlock := BlockEnabled and (ConfigP^.Modes * [fmFold, fmHide] <> []);
  p := 0;
  // TODO: let inherited call CollectNodeInfo
  if IsCollectingNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    act := [sfaOpen, sfaOpenFold]; //TODO: sfaOpenFold not for cfbtIfThen
    if BlockEnabled then
      act := act + ConfigP^.FoldActions;
    if not FAtLineStart then
      act := act - [sfaFoldHide];
    DoInitNode(nd{%H-}, False, Pointer(PtrUInt(PascalFoldTypeConfigMap[ABlockType])), act, FoldBlock);
    CollectingNodeInfoList.Add(nd);
  end;
  if not FoldBlock then
    p := PtrInt(CountPascalCodeFoldBlockOffset);
  Result:=TSynCustomCodeFoldBlock(StartCodeFoldBlock(p+Pointer(PtrInt(ABlockType)), FoldBlock, True));
end;

procedure TSynPasSyn.EndPascalCodeFoldBlock(NoMarkup: Boolean;
  UndoInvalidOpen: Boolean);
var
  DecreaseLevel, BlockEnabled: Boolean;
  act: TSynFoldActions;
  BlockType: TPascalCodeFoldBlockType;
  nd: TSynFoldNodeInfo;
  ConfigP: PSynCustomFoldConfig;
begin
  Exclude(fRange, rsSkipAllPasBlocks);
  BlockType := TopPascalCodeFoldBlockType;
  fRange := fRange - [rsAfterEqual];
  DecreaseLevel := TopCodeFoldBlockType < CountPascalCodeFoldBlockOffset;
  // TODO: let inherited call CollectNodeInfo
  if IsCollectingNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    ConfigP := @FFoldConfig[ord(PascalFoldTypeConfigMap[BlockType])];
    BlockEnabled := ConfigP^.Enabled;
    act := [sfaClose, sfaCloseFold];
    if BlockEnabled then
      act := act + ConfigP^.FoldActions - [sfaFoldFold, sfaFoldHide]; // TODO: Why filter?
    if not DecreaseLevel then
      act := act - [sfaFold, sfaFoldFold, sfaFoldHide];
    if NoMarkup then
      exclude(act, sfaMarkup);
    if UndoInvalidOpen then
      act := act - [sfaMarkup, {sfaFold,} sfaFoldFold, sfaFoldHide, sfaOutline]; // sfaFold affects the EndOffset for the fold-lvl
    DoInitNode(nd{%H-}, True, Pointer(PtrUInt(PascalFoldTypeConfigMap[BlockType])), act, DecreaseLevel);
    CollectingNodeInfoList.Add(nd);
  end;
  EndCodeFoldBlock(DecreaseLevel);
end;

procedure TSynPasSyn.CloseBeginEndBlocksBeforeProc;
begin
  if not(TopPascalCodeFoldBlockType in PascalStatementBlocks + [cfbtAsm]) then
    exit;
  while TopPascalCodeFoldBlockType in PascalStatementBlocks + [cfbtAsm] do
    EndPascalCodeFoldBlockLastLine;
  if TopPascalCodeFoldBlockType in [cfbtProcedure, cfbtAnonymousProcedure] then
    EndPascalCodeFoldBlockLastLine; // This procedure did have a begin/end block, so it must end too
end;

procedure TSynPasSyn.SmartCloseBeginEndBlocks(SearchFor: TPascalCodeFoldBlockType);
var
  i: Integer;
  t: TPascalCodeFoldBlockType;
begin
  // Close unfinished blocks, IF the expected type is found
  // Only check a limited deep. Otherwhise assume, that the "SearchFor"-End node may be misplaced
  i := 0;
  while (i <= 2) do begin
    t := TopPascalCodeFoldBlockType(i);
    if not (t in PascalStatementBlocks + [cfbtAsm, SearchFor]) then
      exit;
    if (t = SearchFor) then break;
    inc(i);
  end;
  if i > 2 then
    exit;

  while i > 0 do begin
    EndPascalCodeFoldBlockLastLine;
    // Todo: FCatchNodeInfoList.CountAll > nc  can not happen
    //if FCatchNodeInfo and (FCatchNodeInfoList.CountAll > FCatchNodeInfoList.CountAll) then
    //  exclude(FCatchNodeInfoList.LastItemPointer^.FoldAction, sfaMarkup);
    dec(i);
  end;
end;

procedure TSynPasSyn.EndPascalCodeFoldBlockLastLine;
var
  i: Integer;
  nd: PSynFoldNodeInfo;
begin
  if IsCollectingNodeInfo then
    i := CollectingNodeInfoList.CountAll;
  EndPascalCodeFoldBlock;
  if FAtLineStart then begin
    // If we are not at linestart, new folds could have been opened => handle as normal close
    if (PasCodeFoldRange.NestFoldStackSize < FStartCodeFoldBlockLevel) and
      (FStartCodeFoldBlockLevel > 0)
    then begin
      PasCodeFoldRange.DecLastLineCodeFoldLevelFix;
      dec(FStartCodeFoldBlockLevel);
      if IsCollectingNodeInfo then CollectingNodeInfoList.Delete;
    end;
    // TODO this only happens if the above was true
    if (PasCodeFoldRange.CodeFoldStackSize < FPasStartLevel) and
      (FPasStartLevel > 0)
    then begin
      PasCodeFoldRange.DecLastLinePasFoldFix;
      dec(FPasStartLevel);
    end;
  end
  else if IsCollectingNodeInfo and (CollectingNodeInfoList.CountAll > {%H-}i) then begin
    nd := CollectingNodeInfoList.LastItemPointer;
    exclude(nd^.FoldAction, sfaMarkup); // not markup able
    nd^.LogXEnd := 0;
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
    m := PasCodeFoldRange.NestFoldStackSize;;
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
  CloseCnt :=  FoldBlockClosingCount(Index, FOLDGROUP_PASCAL, [sfbIncludeDisabled]);
  if (CloseCnt = 0) or
     (FoldBlockMinLevel(Index, FOLDGROUP_PASCAL, [sfbIncludeDisabled])
      <> FoldBlockEndLevel(Index, FOLDGROUP_PASCAL, [sfbIncludeDisabled]))
  then // not a mixed line
    exit;

  // SetRange[Index] has the folds at the start of this line
  // ClosedByNextLine: Folds closed by the next lines LastLineFix
  //                   must be taken from SetRange[Index+1] (end of this line)
  ClosedByNextLine := -LastLinePasFoldLevelFix(Index + 1, FOLDGROUP_PASCAL, True);
  // ClosedInLastLine: Folds Closed by this lines LastLineFix
  //                   must be ignored. (They are part of SetRange[Index] / this line)
  ClosedInLastLine := -LastLinePasFoldLevelFix(Index, FOLDGROUP_PASCAL, True);

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
      cfbtLocalVarBlock, cfbtLocalConstBlock, cfbtLocalTypeBlock:
        if CheckFoldNestLevel(FDividerDrawConfig[pddlVarLocal].MaxDrawDepth - 1,
                              i + 2, [cfbtProcedure], cfbtAll, c) then begin
          if c = 0
          then exit(FDividerDrawConfig[pddlVarLocal].TopSetting)
          else exit(FDividerDrawConfig[pddlVarLocal].NestSetting);
        end;
      cfbtVarBlock, cfbtConstBlock, cfbtTypeBlock:
        if FDividerDrawConfig[pddlVarGlobal].MaxDrawDepth > 0 then
          exit(FDividerDrawConfig[pddlVarGlobal].TopSetting);
      cfbtClass, cfbtRecord:
        begin
          if CheckFoldNestLevel(0, i + 1, [cfbtProcedure],
                                cfbtAll - cfbtVarConstType, c)
          then t := pddlStructGlobal
          else t := pddlStructLocal;
          if CheckFoldNestLevel(FDividerDrawConfig[t].MaxDrawDepth - 1,
                                i + 1, [cfbtClass, cfbtRecord],
                                cfbtAll - cfbtVarConstType, c) then begin
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
    FDividerDrawConfig[i].OnChange := @DefHighlightChange;
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

function TSynPasSyn.CreateFoldConfigInstance(Index: Integer
  ): TSynCustomFoldConfig;
var
  m: TSynCustomFoldConfigModes;
begin
  m := [];
  if TPascalCodeFoldBlockType(Index) in PascalWordTripletRanges then
    m := [fmMarkup];

  case TPascalCodeFoldBlockType(Index) of
    cfbtRegion, cfbtNestedComment, cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment:
      m := [fmFold, fmHide] + m;
    cfbtIfThen, cfbtForDo, cfbtWhileDo, cfbtWithDo, cfbtIfElse:
      m := m;
    cfbtFirstPrivate..high(TPascalCodeFoldBlockType):
      m := [];
    cfbtRecordCase, cfbtRecordCaseSection:
      m := [fmFold]; // no markup
    else
      m := [fmFold] + m;
  end;
  if not (TPascalCodeFoldBlockType(Index) in PascalNoOutlineRanges) then
    m := m + [fmOutline];

  Result := TSynCustomFoldConfig.Create(m, TPascalCodeFoldBlockType(Index) in cfbtEssential);
end;

function TSynPasSyn.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
var
  m: TSynCustomFoldConfigModes;
begin
  Result := inherited GetFoldConfigInstance(Index);

  m := Result.SupportedModes;

  if (TPascalCodeFoldBlockType(Index) in [cfbtIfThen, cfbtForDo, cfbtWhileDo, cfbtWithDo, cfbtIfElse]) then
    m := [];
  if TPascalCodeFoldBlockType(Index) in [cfbtSlashComment] then
    Result.Modes := [fmFold, fmHide] + m
  else
    Result.Modes := [fmFold] + m;

  if not (TPascalCodeFoldBlockType(Index) in PascalNoOutlineRanges) then
    Result.Modes := Result.Modes + [fmOutline];

  Result.Enabled := (TPascalCodeFoldBlockType(Index) in [cfbtBeginEnd..cfbtLastPublic]) and
    (Result.Modes <> []);
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
  // excluded cfbtNone; // maybe ord(cfbtLastPublic)+1 ?
  Result := ord(high(TPascalCodeFoldBlockType)) -
            ord(low(TPascalCodeFoldBlockType))
            - 1;
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
    tmpDirectiveAttri : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    s                 : TStringList;

  begin { ReadDelphiSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if (settingIndex < 0) or (settingIndex >= s.Count) then Result := false
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create(nil);
        tmpNumberAttri    := TSynHighlighterAttributes.Create(nil);
        tmpKeyAttri       := TSynHighlighterAttributes.Create(nil);
        tmpSymbolAttri    := TSynHighlighterAttributes.Create(nil);
        tmpAsmAttri       := TSynHighlighterAttributes.Create(nil);
        tmpCommentAttri   := TSynHighlighterAttributes.Create(nil);
        tmpDirectiveAttri := TSynHighlighterAttributes.Create(nil);
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create(nil);
        tmpSpaceAttri     := TSynHighlighterAttributes.Create(nil);
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpAsmAttri       .Assign(fAsmAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpDirectiveAttri .Assign(fDirectiveAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        Result := ReadDelphiSetting(s[settingIndex],fAsmAttri,'Assembler')
              and ReadDelphiSetting(s[settingIndex],fCommentAttri,'Comment')
              and ReadDelphiSetting(s[settingIndex],fDirectiveAttri,'Directive')
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
          fModifierAttri  .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fAsmAttri       .Assign(tmpAsmAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          FIDEDirectiveAttri.Assign(tmpCommentAttri);
          fDirectiveAttri .Assign(tmpDirectiveAttri);
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpAsmAttri       .Free;
        tmpCommentAttri   .Free;
        tmpDirectiveAttri .Free;
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
  Result := ['&', '_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynPasSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynPasSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

{begin}                                                                         //mh 2000-10-08
function TSynPasSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> fDefaultFilterInitialValue;
end;

procedure TSynPasSyn.DoAfterOperator;
begin
  if rsProperty in fRange then
    fRange := fRange + [rsAtPropertyOrReadWrite]
  else
  if rsInRaise in fRange then
    FNextTokenState := tsAfterRaise;
end;

procedure TSynPasSyn.EndStatement(ACurTfb: TPascalCodeFoldBlockType;
  ACloseFolds: TPascalCodeFoldBlockTypes);
begin
    fRange := fRange - [rsInRaise];
    while ACurTfb in ACloseFolds do begin
      EndPascalCodeFoldBlock(True);
      ACurTfb := TopPascalCodeFoldBlockType;
    end;
end;

procedure TSynPasSyn.EndStatementLastLine(ACurTfb: TPascalCodeFoldBlockType;
  ACloseFolds: TPascalCodeFoldBlockTypes);
begin
    fRange := fRange - [rsInRaise];
    while ACurTfb in ACloseFolds do begin
      EndPascalCodeFoldBlockLastLine;
      ACurTfb := TopPascalCodeFoldBlockType;
    end;
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
    KeywordsList.UseLocale := false;
    KeywordsList.CaseSensitive := true;
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

{ TSynPasSyn.TSynPasSynCustomTokenInfo }

class operator TSynPasSyn.TSynPasSynCustomTokenInfo. = (a, b: TSynPasSynCustomTokenInfo): boolean;
begin
  Result := (a.MatchTokenKinds = b.MatchTokenKinds) and
            (a.Token = b.Token) and
            (a.Word = b.Word);
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
  FPasFoldFixLevel := 0;
  FTokenState := tsNone;
end;

function TSynPasSynRange.Compare(Range: TSynCustomHighlighterRange): integer;
begin
  Result:=inherited Compare(Range);
  if Result<>0 then exit;

  Result:=ord(FTokenState)-ord(TSynPasSynRange(Range).FTokenState);
  if Result<>0 then exit;
  Result:=ord(FMode)-ord(TSynPasSynRange(Range).FMode);
  if Result<>0 then exit;
  Result:=ord(FNestedComments)-ord(TSynPasSynRange(Range).FNestedComments);
  if Result<>0 then exit;
  Result:=ord(FTypeHelpers)-ord(TSynPasSynRange(Range).FTypeHelpers);
  if Result<>0 then exit;
  Result := FBracketNestLevel - TSynPasSynRange(Range).FBracketNestLevel;
  if Result<>0 then exit;
  Result := FLastLineCodeFoldLevelFix - TSynPasSynRange(Range).FLastLineCodeFoldLevelFix;
  if Result<>0 then exit;
  Result := FPasFoldFixLevel - TSynPasSynRange(Range).FPasFoldFixLevel;
end;

procedure TSynPasSynRange.Assign(Src: TSynCustomHighlighterRange);
begin
  if (Src<>nil) and (Src<>TSynCustomHighlighterRange(NullRange)) then begin
    inherited Assign(Src);
    FTokenState:=TSynPasSynRange(Src).FTokenState;
    FMode:=TSynPasSynRange(Src).FMode;
    FNestedComments:=TSynPasSynRange(Src).FNestedComments;
    FTypeHelpers := TSynPasSynRange(Src).FTypeHelpers;
    FBracketNestLevel:=TSynPasSynRange(Src).FBracketNestLevel;
    FLastLineCodeFoldLevelFix := TSynPasSynRange(Src).FLastLineCodeFoldLevelFix;
    FPasFoldFixLevel := TSynPasSynRange(Src).FPasFoldFixLevel;
  end;
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

{ TSynPasSynCustomToken }

procedure TSynPasSynCustomToken.DoTokensChanged(Sender: TObject);
begin
  if FOnChange <> nil then
    FOnChange(Self);
end;

procedure TSynPasSynCustomToken.SetMatchTokenKinds(AValue: TtkTokenKinds);
begin
  if FMatchTokenKinds = AValue then Exit;
  FMatchTokenKinds := AValue;
  DoTokensChanged(Self);
end;

procedure TSynPasSynCustomToken.DoMarkupChaged(Sender: TObject);
begin
  if FOnMarkupChange <> nil then
    FOnMarkupChange(Self);
end;

constructor TSynPasSynCustomToken.Create;
begin
    FMarkup := TSynHighlighterAttributesModifier.Create;
    FMarkup.OnChange := @DoMarkupChaged;
    FTokens := TStringList.Create;
    TStringList(FTokens).OnChange := @DoTokensChanged;
    FMatchTokenKinds := [];
end;

destructor TSynPasSynCustomToken.Destroy;
begin
  inherited Destroy;
  FMarkup.Free;
  FTokens.Free;
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
  RegisterPlaceableHighlighter(TSynPasSyn);

finalization
  FreeAndNil(KeywordsList);

end.

