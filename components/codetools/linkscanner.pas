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
    TLinkScanner scans a source file, reacts to compiler directives, replaces
    macros and reads include files. It builds one source and a link list. The
    resulting source is called the cleaned source. A link points from a position
    of the cleaned source to its position in the real source.
    The link list makes it possible to find scanned code in the source files.
}
unit LinkScanner;

{$mode objfpc}{$H+}

{$I codetools.inc}

{ $DEFINE ShowIgnoreErrorAfter}

// debugging
{ $DEFINE ShowUpdateCleanedSrc}
{ $DEFINE VerboseIncludeSearch}
{ $DEFINE VerboseUpdateNeeded}

interface

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  // RTL + FCL
  Classes, SysUtils, math, Laz_AVL_Tree,
  // CodeTools
  CodeToolsStrConsts, CodeToolMemManager, FileProcs, ExprEval, SourceLog,
  KeywordFuncLists, BasicCodeTools,
  // LazUtils
  LazFileUtils, LazUtilities, LazDbgLog;

var
  FPCSystemUnitName: String = 'system';
  DelphiSystemUnitName: String = 'System';

const
  PascalCompilerDefine = ExternalMacroStart+'Compiler';
  MacroUseHeapTrc = ExternalMacroStart+'UseHeapTrcUnit';
  MacroUseLineInfo = ExternalMacroStart+'UseLineInfo';
  MacroUselnfodwrf = ExternalMacroStart+'Uselnfodwrf';
  MacroUseValgrind = ExternalMacroStart+'UseValgrind';
  MacroUseProfiler = ExternalMacroStart+'UseProfiler';
  MacroUseFPCylix = ExternalMacroStart+'UseFPCylix';
  MacroUseSysThrds = ExternalMacroStart+'UseSysThrds';
  MacroControllerUnit = ExternalMacroStart+'ControllerUnit';

type
  TLinkScanner = class;

//----------------------------------------------------------------------------
  TOnGetSource = function(Sender: TObject; Code: Pointer): TSourceLog
                 of object;
  TOnLoadSource = function(Sender: TObject; const AFilename: string;
                       OnlyIfExists: boolean): pointer of object;
  TOnGetSourceStatus = procedure(Sender: TObject; Code: Pointer;
                 var ReadOnly: boolean) of object;
  TOnDeleteSource = procedure(Sender: TObject; Code: Pointer; Pos, Len: integer)
                    of object;
  TOnGetFileName = function(Sender: TObject; Code: Pointer): string of object;
  TOnCheckFileOnDisk = function(Code: Pointer): boolean of object;
  TOnGetInitValues = function(Scanner: TLinkScanner; Code: Pointer;
                       out ChangeStep: integer): TExpressionEvaluator of object;
  TOnIncludeCode = procedure(ParentCode, IncludeCode: Pointer) of object;
  TOnSetWriteLock = procedure(Lock: boolean) of object;
  TLSOnGetGlobalChangeSteps = procedure(out SourcesChangeStep, FilesChangeStep: int64;
                                   out InitValuesChangeStep: integer) of object;

  { TSourceLink is used to map between the codefiles and the cleaned source }
  TSourceLinkKind = (
    slkCode,
    slkMissingIncludeFile,
    slkSkipStart,  // start of skipped code due to IFDEFs {#3
    slkSkipEnd,    // end of skipped code due to IFDEFs #3}
    slkCompilerString // e.g. {$I %FPCVERSION%}
    );
  TSourceLinkKinds = set of TSourceLinkKind;
  PSourceLink = ^TSourceLink;
  TSourceLink = record
    CleanedPos: integer;
    SrcPos: integer;
    Code: Pointer;
    Kind: TSourceLinkKind;
    Next: PSourceLink;
  end;

  TSourceLinkMacro = record
    Name: PChar;
    Code: Pointer;
    Src: string;
    SrcFilename: string;
    StartPos, EndPos: integer;
  end;
  PSourceLinkMacro = ^TSourceLinkMacro;

  { TSourceChangeStep is used to save the ChangeStep of every used file
    A ChangeStep is switching to or from an include file }
  PSourceChangeStep = ^TSourceChangeStep;
  TSourceChangeStep = record
    Code: Pointer;
    ChangeStep: integer;
    Next: PSourceChangeStep;
  end;
  
  TLinkScannerRange = (
    lsrNone, // undefined
    lsrInit, // init, but do not scan any code
    lsrSourceType, // read till source type (e.g. keyword program or unit)
    lsrSourceName, // read till source name
    lsrInterfaceStart, // read till keyword interface
    lsrMainUsesSectionStart, // uses section of interface/program
    lsrMainUsesSectionEnd, // uses section of interface/program
    lsrImplementationStart, // scan at least to interface end (e.g. till implementation keyword)
    lsrImplementationUsesSectionStart, // uses section of implementation
    lsrImplementationUsesSectionEnd, // uses section of implementation
    lsrInitializationStart,
    lsrFinalizationStart,
    lsrEnd // scan till 'end.'
    );

  TCommentStyle = (
    CommentNone,
    CommentCurly,  // {}
    CommentRound,  // (* *)
    CommentLine    // //
    );

  TCompilerMode = (
    cmFPC,
    cmDELPHI,
    cmDELPHIUNICODE,
    cmGPC,
    cmTP,
    cmOBJFPC,
    cmMacPas,
    cmISO,
    cmExtPas
    );
const
  // upper case
  CompilerModeNames: array[TCompilerMode] of string=(
    'FPC',
    'DELPHI',
    'DELPHIUNICODE',
    'GPC',
    'TP',
    'OBJFPC',
    'MACPAS',
    'ISO',
    'EXTPAS'
    );
type

  { TCompilerModeSwitch - see fpc/compiler/globtype.pas  tmodeswitch }
  TCompilerModeSwitch = (
    //cms_FPC,cms_ObjFPC,cms_Delphi,cms_TP7,cms_Mac,cms_ISO,cms_ExtPas,
    cmsAdd_pointer,        { ? }
    cmsClass,              { delphi class model }
    cmsObjpas,             { load objpas unit }
    cmsResult,             { result in functions }
    cmsString_pchar,       { pchar 2 string conversion }
    cmsCvar_support,       { cvar variable directive }
    cmsNested_comment,     { nested comments }
    cmsTp_procvar,         { tp style procvars (no @ needed) }
    cmsMac_procvar,        { macpas style procvars }
    cmsRepeat_forward,     { repeating forward declarations is needed }
    cmsPointer_2_procedure,{ allows the assignement of pointers to  procedure variables }
    cmsAutoderef,          { does auto dereferencing of struct. vars, e.g. a.b -> a^.b }
    cmsInitfinal,          { initialization/finalization for units }
    cmsDefault_ansistring, { ansistring turned on by default }
    cmsOut,                { support the calling convention OUT }
    cmsDefault_para,       { support default parameters }
    cmsHintdirective,      { support hint directives }
    cmsDuplicate_names,    { allow locals/paras to have duplicate names of globals }
    cmsProperty,           { allow properties }
    cmsDefault_inline,     { allow inline proc directive }
    cmsExcept,             { allow exception-related keywords }
    cmsObjectiveC1,        { support interfacing with Objective-C (1.0) }
    cmsObjectiveC2,        { support interfacing with Objective-C (2.0), includes cmsObjectiveC1 }
    cmsNestedProcVars,     { support nested procedural variables }
    cmsNonLocalGoto,       { support non local gotos (like iso pascal) }
    cmsAdvancedRecords,    { advanced record syntax with visibility sections, methods and properties }
    cmsISOLike_unary_minus,{ unary minus like in iso pascal: same precedence level as binary minus/plus }
    cmsSystemcodepage,     { use system codepage as compiler codepage by default, emit ansistrings with system codepage }
    cmsFinalFields,        { allows declaring fields as "final", which means they must be initialised
                             in the (class) constructor and are constant from then on (same as final
                             fields in Java) }
    cmsDefault_unicodestring, { makes the default string type in $h+ mode unicodestring rather than
                                ansistring; similarly, char becomes unicodechar rather than ansichar }
    cmsTypeHelpers,        { allows the declaration of "type helper" (non-Delphi) or "record helper"
                                  (Delphi) for primitive types }
    cmsClosures,           { Anonymous methods }
    cmsCBlocks,            { support for http://en.wikipedia.org/wiki/Blocks_(C_language_extension) }
    cmsISOlike_IO,         { I/O as it required by an ISO compatible compiler }
    cmsISOLike_Program_Para, { program parameters as it required by an ISO compatible compiler }
    cmsISOLike_Mod,        { mod operation as it is required by an iso compatible compiler }
    cmsArrayOperators,     { use Delphi compatible array operators instead of custom ones ("+") }
    // not yet in FPC, supported by pas2js:
    cmsPrefixedAttributes, { allow Delphi attributes, disable FPC [] proc modifier }
    cmsExternalClass,      { pas2js: allow  class external [pkgname] name [symbol] }
    cmsIgnoreAttributes,   { pas2js: ignore attributes }
    cmsOmitRTTI,           { pas2js: treat class section 'published' as 'public' and typeinfo does not work on symbols declared with this switch }
    msMultipleScopeHelpers { off=only one helper per type, on=all }
    );
  TCompilerModeSwitches = set of TCompilerModeSwitch;
const
  // see fpc/compiler/globals.pp
  DefaultCompilerModeSwitches: array[TCompilerMode] of TCompilerModeSwitches = (
    // cmFPC
    [cmsString_pchar,cmsNested_comment,cmsRepeat_forward,cmsCvar_support,
     cmsInitfinal,cmsHintdirective,cmsProperty,cmsDefault_inline,
     cmsResult],
    // cmDELPHI
    [cmsClass,cmsObjpas,cmsResult,cmsString_pchar,
     cmsPointer_2_procedure,cmsAutoderef,cmsTp_procvar,cmsInitfinal,cmsDefault_ansistring,
     cmsOut,cmsDefault_para,cmsDuplicate_names,cmsHintdirective,
     cmsProperty,cmsDefault_inline,cmsExcept,cmsAdvancedRecords,
     cmsClosures,cmsPrefixedAttributes,cmsArrayOperators],
    // cmDELPHIUNICODE
    [cmsClass,cmsObjpas,cmsResult,cmsString_pchar,
     cmsPointer_2_procedure,cmsAutoderef,cmsTp_procvar,cmsInitfinal,
     cmsOut,cmsDefault_para,cmsDuplicate_names,cmsHintdirective,
     cmsProperty,cmsDefault_inline,cmsExcept,cmsAdvancedRecords,
     cmsSystemcodepage,cmsDefault_unicodestring,
     cmsClosures,cmsPrefixedAttributes,cmsArrayOperators],
    // cmGPC
    [cmsTp_procvar],
    // cmTP
    [cmsResult,cmsTp_procvar,cmsDuplicate_names],
    // cmOBJFPC
    [cmsClass,cmsObjpas,cmsResult,cmsString_pchar,cmsNested_comment,
     cmsRepeat_forward,cmsCvar_support,cmsInitfinal,cmsOut,cmsDefault_para,
     cmsHintdirective,cmsProperty,cmsDefault_inline,cmsExcept],
    // cmMacPas
    [cmsCvar_support,cmsMac_procvar,cmsNestedProcVars,
     cmsNonLocalGoto,cmsISOLike_unary_minus,cmsDefault_inline],
    // cmISO
    [cmsTp_procvar,cmsDuplicate_names,cmsNestedProcVars,cmsNonLocalGoto,
     cmsISOLike_unary_minus],
    // cmExtPas
    [cmsTp_procvar,cmsDuplicate_names,cmsNestedProcVars,cmsNonLocalGoto,
     cmsISOLike_unary_minus,cmsISOlike_IO,
     cmsISOLike_Program_Para,
     cmsISOLike_Mod]
    );
  cmAllModesWithGeneric = [cmDELPHI,cmDELPHIUNICODE,cmOBJFPC];

  // upper case (see fpc/compiler/globtype.pas  modeswitchstr )
  CompilerModeSwitchNames: array[TCompilerModeSwitch] of string=(
    'POINTERARITHMETICS',
    'CLASS',
    'OBJPAS',
    'RESULT',
    'PCHARTOSTRING',
    'CVAR',
    'NESTEDCOMMENTS',
    'CLASSICPROCVARS',
    'MACPROCVARS',
    'REPEATFORWARD',
    'POINTERTOPROCVAR',
    'AUTODEREF',
    'INITFINAL',
    'ANSISTRINGS',
    'OUT',
    'DEFAULTPARAMETERS',
    'HINTDIRECTIVE',
    'DUPLICATELOCALS',
    'PROPERTIES',
    'ALLOWINLINE',
    'EXCEPTIONS',
    'OBJECTIVEC1',
    'OBJECTIVEC2',
    'NESTEDPROCVARS',
    'NONLOCALGOTO',
    'ADVANCEDRECORDS',
    'ISOUNARYMINUS',
    'SYSTEMCODEPAGE',
    'FINALFIELDS',
    'UNICODESTRINGS',
    'TYPEHELPERS',
    'CLOSURES',
    'CBLOCKS',
    'ISOIO',
    'ISOPROGRAMPARAS',
    'ISOMOD',
    'ARRAYOPERATORS',
    'PREFIXEDATTRIBUTES',
    'EXTERNALCLASS',
    'IGNOREATTRIBUTES',
    'OMITRTTI',
    'MULTIPLESCOPEHELPERS'
    );


type
  // see fpcsrc/compiler/globtype.pas toptimizerswitch
  TOptimizerSwitch = (
    cs_opt_none,
    cs_opt_level1,cs_opt_level2,cs_opt_level3,cs_opt_level4,
    cs_opt_regvar,cs_opt_uncertain,cs_opt_size,cs_opt_stackframe,
    cs_opt_peephole,cs_opt_loopunroll,cs_opt_tailrecursion,cs_opt_nodecse,
    cs_opt_nodedfa,cs_opt_loopstrength,cs_opt_scheduler,cs_opt_autoinline,cs_useebp,cs_userbp,
    cs_opt_reorder_fields,cs_opt_fastmath,
    cs_opt_dead_values,
    cs_opt_remove_emtpy_proc,
    cs_opt_constant_propagate,
    cs_opt_dead_store_eliminate,
    cs_opt_forcenostackframe
  );
  toptimizerswitches = set of toptimizerswitch;
const
  OptimizerSwitchStr : array[toptimizerswitch] of string[17] = ('',
    'LEVEL1','LEVEL2','LEVEL3','LEVEL4',
    'REGVAR','UNCERTAIN','SIZE','STACKFRAME',
    'PEEPHOLE','LOOPUNROLL','TAILREC','CSE',
    'DFA','STRENGTH','SCHEDULE','AUTOINLINE','USEEBP','USERBP',
    'ORDERFIELDS','FASTMATH','DEADVALUES','REMOVEEMPTYPROCS',
    'CONSTPROP',
    'DEADSTORE','FORCENOSTACKFRAME'
  );

type
  TPascalCompiler = (
    pcFPC,
    pcDelphi,
    pcPas2js);
const  
  // upper case
  PascalCompilerNames: array[TPascalCompiler] of string=(
    'FPC', 'DELPHI', 'PAS2JS'
    );
  PascalCompilerUnitExt: array[TPascalCompiler] of string = (
    'pp;pas;ppu', // + p  if TCompilerMode=cmMacPas
    'pas;dcu',
    'pas;pp;pcu;pju'
    );
  PascalCompilerSrcExt: array[TPascalCompiler] of string = (
    'pp;pas', // + p  if TCompilerMode=cmMacPas
    'pas',
    'pas;pp'
    );

type
  TLSSkippingDirective = (
    lssdNone,
    lssdTillElse,
    lssdTillEndIf
    );

  TLSDirectiveKind = (
    lsdkNone,
    // if
    lsdkIf,
    lsdkIfC,
    lsdkIfDef,
    lsdkIfNDef,
    lsdkIfOpt,
    // else
    lsdkElIfC,
    lsdkElse,
    lsdkElseC,
    lsdkElseIf,
    // end
    lsdkEndC,
    lsdkEndif,
    lsdkIfEnd,
    // misc
    lsdkDefine,
    lsdkInclude,
    lsdkIncludePath,
    lsdkLongSwitch,
    lsdkMacro,
    lsdkMode,
    lsdkModeSwitch,
    lsdkSetC,
    lsdkShortSwitch,
    lsdkThreading,
    lsdkUndef
    );
  TLSDirectiveKinds = set of TLSDirectiveKind;
const
  lsdkAllIf = [lsdkIf,lsdkIfC,lsdkIfDef,lsdkIfNDef,lsdkIfOpt];
  lsdkAllElse = [lsdkElIfC,lsdkElse,lsdkElseC,lsdkElseIf];
  lsdkAllEnd = [lsdkEndC,lsdkEndif,lsdkIfEnd];

type
  TLSDirectiveState = (
    lsdsActive, // was executed
    lsdsInactive,// was executed, but expression result was false and following code was skipped
    lsdsSkipped // was not executed
    );
  TLSDirectiveStates = set of TLSDirectiveState;

  TLSDirective = record
    CleanPos: integer;
    Level: integer;
    State: TLSDirectiveState;
    Code: Pointer; // TCodeBuffer
    SrcPos: integer; // 1-based position in Code
    Kind: TLSDirectiveKind;
  end;
  PLSDirective = ^TLSDirective;
  PPLSDirective = ^PLSDirective;

type
  { TMissingIncludeFile is a missing include file together with all
    params involved in the search }
  TMissingIncludeFile = class
  public
    IncludePath: string;
    Filename: string;
    constructor Create(const AFilename, AIncludePath: string);
    function CalcMemSize: PtrUInt;
  end;
  
  { TMissingIncludeFiles is a list of TMissingIncludeFile }
  TMissingIncludeFiles = class(TList)
  private
    function GetIncFile(Index: Integer): TMissingIncludeFile;
    procedure SetIncFile(Index: Integer; const AValue: TMissingIncludeFile);
  public
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function CalcMemSize: PtrUInt;
    property Items[Index: Integer]: TMissingIncludeFile
      read GetIncFile write SetIncFile; default;
  end;

  TDirectiveSequenceItemValue = record
    CleanPos: integer;
    Value: string;
  end;

  TSequenceDirective = (sdScopedEnums);

  TDirectiveSequenceItem = class
  private
    FItems: array of TDirectiveSequenceItemValue;
    FLastItem: integer;
  public
    constructor Create;
    procedure Add(const AValue: string; ACleanPos: integer);
    function FindValue(const ACleanPos: integer; out Value: string): Boolean;
    procedure Clear(FreeMemory: boolean);
    function CalcMemSize: PtrUInt;
  end;

  TDirectiveSequence = class
  private
    FDirectives: array[TSequenceDirective] of TDirectiveSequenceItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ADirective: TSequenceDirective; const ADirectiveValue: string;
      ACleanPos: Integer);
    function FindValue(ADirective: TSequenceDirective;
      ACleanPos: Integer; out Value: string): Boolean;
    procedure Clear(FreeMemory: boolean);

    function CalcMemSize: PtrUInt;
  end;

  { LinkScanner Token Types }
  TLSTokenType = (
    lsttNone,
    lsttSrcEnd, // no more tokens
    lsttWord,
    lsttEqual,
    lsttPoint,
    lsttSemicolon,
    lsttComma,
    lsttStringConstant,
    lsttEnd
    );

  { Error handling }

  { ELinkScannerError }

  ELinkScannerError = class(Exception)
    Sender: TLinkScanner;
    Id: int64;
    constructor Create(ASender: TLinkScanner; TheId: int64; const AMessage: string);
  end;
  
  ELinkScannerErrors = class of ELinkScannerError;
  
  TLinkScannerProgress = function(Sender: TLinkScanner): boolean of object;
  
  ELinkScannerAbort = class(ELinkScannerError);
  ELinkScannerConsistency = class(ELinkScannerError);

  { ELinkScannerEditError }

  ELinkScannerEditError = class(ELinkScannerError)
    Buffer: Pointer;
    BufferPos: integer;
    constructor Create(ASender: TLinkScanner; TheId: int64; const AMessage: string;
      ABuffer: Pointer; ABufferPos: integer);
  end;

  TLinkScannerState = (
    lssSourcesChanged,    // used source buffers changed
    lssInitValuesChanged, // used init values changed
    lssFilesChanged,      // used files on disk changed
    lssIgnoreMissingIncludeFiles
    );
  TLinkScannerStates = set of TLinkScannerState;
  
  { TLinkScanner }
  
  TLinkScanner = class(TObject)
  private
    FLinks: PSourceLink; // list of TSourceLink, sorted for CleanedPos
    FLinkCount: integer;
    FLinkCapacity: integer;
    FCleanedSrc: string;
    FLastCleanedSrcLen: integer;
    FOnGetSource: TOnGetSource;
    FOnGetFileName: TOnGetFileName;
    FOnGetSourceStatus: TOnGetSourceStatus;
    FOnLoadSource: TOnLoadSource;
    FOnDeleteSource: TOnDeleteSource;
    FOnCheckFileOnDisk: TOnCheckFileOnDisk;
    FOnGetInitValues: TOnGetInitValues;
    FOnIncludeCode: TOnIncludeCode;
    FOnProgress: TLinkScannerProgress;
    FIgnoreErrorAfterCode: Pointer;
    FIgnoreErrorAfterCursorPos: integer;
    FInitValues: TExpressionEvaluator;
    FInitValuesChangeStep: integer;
    FSourceChangeSteps: TFPList; // list of PSourceChangeStep sorted with Code
    FChangeStep: integer;
    FMainSourceFilename: string;
    FMainCode: pointer;
    FScanTill: TLinkScannerRange;
    FNestedComments: boolean; // for speed reasons keep this flag redundant with the CompilerModeSwitches
    FStates: TLinkScannerStates;
    FHiddenUsedUnits: string; // comma separated
    // global write lock
    FOnSetGlobalWriteLock: TOnSetWriteLock;
    FGlobalSourcesChangeStep: int64;
    FGlobalFilesChangeStep: int64;
    FGlobalInitValuesChangeStep: integer;
    function GetLinks(Index: integer): TSourceLink; inline;
    function GetLinkP(Index: integer): PSourceLink; inline;
    procedure SetLinks(Index: integer; const Value: TSourceLink);
    procedure SetSource(ACode: Pointer); // set current source
    procedure AddSourceChangeStep(ACode: pointer; AChangeStep: integer);
    procedure AddLink(ASrcPos: integer; ACode: Pointer;
                      AKind: TSourceLinkKind = slkCode);
    procedure IncreaseChangeStep; inline;
    procedure SetMainCode(const Value: pointer);
    procedure SetScanTill(const Value: TLinkScannerRange);
    function GetIgnoreMissingIncludeFiles: boolean;
    procedure SetIgnoreMissingIncludeFiles(const Value: boolean);
    function TokenIs(const AToken: string): boolean;
    function UpTokenIs(const AToken: string): boolean;
  private
    // parsing
    CommentStyle: TCommentStyle;
    CommentLevel: integer;
    CommentStartPos: integer;      // position of '{', '(*', '//'
    CommentInnerStartPos: integer; // position after '{', '(*', '//'
    CommentInnerEndPos: integer;   // position of '}', '*)', #10
    CommentEndPos: integer;        // postion after '}', '*)', #10
    CopiedSrcPos: integer;
    IfLevel: integer;
    procedure ReadNextToken;
    function ReturnFromIncludeFileAndIsEnd: boolean;
    function ReadIdentifier: string;
    function ReadUpperIdentifier: string;
    procedure ReadSpace; inline;
    procedure ReadCurlyComment;
    procedure ReadLineComment;
    procedure ReadRoundComment;
    procedure CommentEndNotFound(id: int64);
    procedure EndComment; inline;
    procedure IncCommentLevel; inline;
    procedure DecCommentLevel; inline;
    procedure HandleDirective;
    procedure UpdateCleanedSource(NewCopiedSrcPos: integer);
    function ReturnFromIncludeFile: boolean;
    function ParseKeyWord(StartPos: integer; LastTokenType: TLSTokenType): boolean;
    function DoEndToken: boolean; inline;
    function DoSourceTypeToken: boolean; inline;
    function DoInterfaceToken: boolean; inline;
    function DoImplementationToken: boolean; inline;
    function DoFinalizationToken: boolean; inline;
    function DoInitializationToken: boolean; inline;
    function DoUsesToken: boolean; inline;
    function TokenIsWord(p: PChar): boolean; inline;
  private
    // directives
    FDirectives: PLSDirective;
    FDirectivesCount: integer;
    FDirectivesCapacity: integer;
    FDirectivesSorted: PPLSDirective; // array of PLSDirective to items of FDirectives
    FDirectiveName: string;
    FDirectiveCleanPos: integer;
    FDirectivesStored: boolean;
    FMacrosOn: boolean;
    FMissingIncludeFiles: TMissingIncludeFiles;
    FIncludeStack: TFPList; // list of TSourceLink
    FOnGetGlobalChangeSteps: TLSOnGetGlobalChangeSteps;
    FSkippingDirectives: TLSSkippingDirective;
    FSkipIfLevel: integer;
    FStoreDirectives: integer;
    FCompilerMode: TCompilerMode;
    FCompilerModeSwitches: TCompilerModeSwitches;
    FPascalCompiler: TPascalCompiler;
    FMacros: PSourceLinkMacro;
    FMacroCount, fMacroCapacity: integer;
    FDirectiveSequence: TDirectiveSequence;
    function GetDirectives(Index: integer): PLSDirective; inline;
    function GetDirectivesSorted(Index: integer): PLSDirective; inline;
    procedure SetCompilerMode(const AValue: TCompilerMode);
    procedure SetPascalCompiler(const AValue: TPascalCompiler);
    procedure SkipTillEndifElse(SkippingUntil: TLSSkippingDirective);
    procedure SortDirectives;
    function InternalIfDirective: boolean;
    procedure EndSkipping;
    procedure AddSkipComment(IsStart: boolean);
    procedure SetDirectiveValueWithSequence(ADirective: TSequenceDirective;
      const ADirectiveValue: string);

    function IfdefDirective: boolean;
    function IfCDirective: boolean;
    function IfndefDirective: boolean;
    function IfDirective: boolean;
    function IfOptDirective: boolean;
    function EndifDirective: boolean;
    function EndCDirective: boolean;
    function IfEndDirective: boolean;
    function ElseDirective: boolean;
    function ElseCDirective: boolean;
    function ElseIfDirective: boolean;
    function ElIfCDirective: boolean;
    function DefineDirective: boolean;
    function UndefDirective: boolean;
    function SetCDirective: boolean;
    function IncludeDirective: boolean;
    function IncludePathDirective: boolean;
    function ShortSwitchDirective: boolean;
    function ReadNextSwitchDirective: boolean;
    function LongSwitchDirective: boolean;
    function LongSwitchDirectiveWithSequence(const ADirective: TSequenceDirective): boolean;
    function MacroDirective: boolean;
    function ModeDirective: boolean;
    function ModeSwitchDirective: boolean;
    function ThreadingDirective: boolean;
    function DoDirective(StartPos, DirLen: integer): boolean;
    
    function IncludeFile(const AFilename: string): boolean;
    procedure PushIncludeLink(ACleanedPos, ASrcPos: integer; ACode: Pointer);
    function PopIncludeLink: TSourceLink;
    function GetIncludeFileIsMissing: boolean;
    function MissingIncludeFilesNeedsUpdate: boolean;
    procedure ClearMissingIncludeFiles;

    // code macros
    procedure AddMacroValue(MacroName: PChar; ValueStart, ValueEnd: integer);
    procedure ClearMacros;
    function IndexOfMacro(MacroName: PChar; InsertPos: boolean): integer;
    procedure AddMacroSource(MacroID: integer);
  protected
    // error: the error is in range Succ(ScannedRange)
    LastErrorMessage: string;
    LastErrorSrcPos: integer;
    LastErrorCode: pointer;
    LastErrorIsValid: boolean;
    LastErrorBehindIgnorePosition: boolean;
    LastErrorCheckedForIgnored: boolean;
    LastErrorId: int64;
    CleanedIgnoreErrorAfterPosition: integer;// ignore if valid and >=
    procedure RaiseExceptionFmt(id: int64; const AMessage: string; const Args: array of const);
    procedure RaiseException(id: int64; const AMessage: string);
    procedure RaiseExceptionClass(id: int64; const AMessage: string;
      ExceptionClass: ELinkScannerErrors);
    procedure RaiseEditException(id: int64; const AMessage: string; ABuffer: Pointer;
      ABufferPos: integer);
    procedure RaiseConsistencyException(id: int64; const AMessage: string);
    procedure ClearLastError;
    procedure RaiseLastError;
    procedure DoCheckAbort;
  public
    // current values, positions, source, flags
    CleanedLen: integer;
    Src: string;     // current parsed source (= TCodeBuffer(Code).Source)
    SrcPos: integer; // current position (1-based in Src)
    TokenStart: integer; // start position of current token
    TokenType: TLSTokenType;
    SrcLen: integer; // length of current source
    Code: pointer;   // current code object (TCodeBuffer)
    Values: TExpressionEvaluator;
    SrcFilename: string;// current parsed filename (= TCodeBuffer(Code).Filename)
    IsUnit: boolean;
    SourceName: string;

    ScannedRange: TLinkScannerRange; // excluding the section with a syntax error

    function MainFilename: string;
    property ChangeStep: integer read FChangeStep; // see CTInvalidChangeStamp

    // links
    property Links[Index: integer]: TSourceLink read GetLinks write SetLinks;
    property LinkP[Index: integer]: PSourceLink read GetLinkP;
    property LinkCount: integer read FLinkCount;
    function LinkIndexAtCleanPos(ACleanPos: integer): integer;
    function LinkIndexAtCursorPos(ACursorPos: integer; ACode: Pointer): integer;
    function LinkSize(Index: integer): integer;
    function LinkSize_Inline(Index: integer): integer; inline;
    function LinkCleanedEndPos(Index: integer): integer;
    function LinkCleanedEndPos_Inline(Index: integer): integer; inline;
    function LinkSourceLog(Index: integer): TSourceLog;
    function FindFirstSiblingLink(LinkIndex: integer): integer;
    function FindParentLink(LinkIndex: integer): integer;
    function LinkIndexNearCursorPos(ACursorPos: integer; ACode: Pointer;
                                    var CursorInLink: boolean): integer;
    function CreateTreeOfSourceCodes: TAVLTree;

    // directives
    property Directives[Index: integer]: PLSDirective read GetDirectives;
    property DirectivesSorted[Index: integer]: PLSDirective read GetDirectivesSorted; // sorted for Code and SrcPos
    property DirectiveCount: integer read FDirectivesCount;
    procedure ClearDirectives(FreeMemory: boolean);
    function StoreDirectives: boolean; inline; // store directives on next Scan
    procedure DemandStoreDirectives; // increase internal counter to StoreDirectives
    procedure ReleaseStoreDirectives; // decrease internal counter to StoreDirectives
    property DirectivesStored: boolean read FDirectivesStored; // directives were stored on last scan
    function FindDirective(aCode: Pointer; aSrcPos: integer;
      out FirstSortedIndex, LastSortedIndex: integer): boolean;
    function FindFirstDirective(aCode: Pointer; aSrcPos: integer;
      const AllowedStates: TLSDirectiveStates): PLSDirective;
    function GetDirectiveValueAt(ADirective: TSequenceDirective; ACleanPos: integer): string;

    // source mapping (Cleaned <-> Original)
    function CleanedSrc: string;
    function CursorToCleanPos(ACursorPos: integer; ACode: pointer;
                    out ACleanPos: integer): integer; // 0=valid CleanPos
                          //-1=CursorPos was skipped, CleanPos between two links
                          // 1=CursorPos beyond scanned code
    function CleanedPosToCursor(ACleanedPos: integer; out ACursorPos: integer;
                                out ACode: Pointer): boolean;
    function CleanedPosToStr(ACleanedPos: integer): string;
    function LastErrorIsInFrontOfCleanedPos(ACleanedPos: integer): boolean;
    procedure RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos: integer);

    // ranges
    function WholeRangeIsWritable(CleanStartPos, CleanEndPos: integer;
                                  ErrorOnFail: boolean): boolean;
    procedure FindCodeInRange(CleanStartPos, CleanEndPos: integer;
                              UniqueSortedCodeList: TFPList);
    procedure DeleteRange(CleanStartPos,CleanEndPos: integer);

    // scanning
    procedure Scan(Range: TLinkScannerRange; CheckFilesOnDisk: boolean);
    function UpdateNeeded(Range: TLinkScannerRange;
                          CheckFilesOnDisk: boolean): boolean;
    procedure SetIgnoreErrorAfter(ACursorPos: integer; ACode: Pointer);
    procedure ClearIgnoreErrorAfter;
    function IgnoreErrAfterPositionIsInFrontOfLastErrMessage: boolean;
    function IgnoreErrorAfterCleanedPos: integer;// before using this, check if valid!
    function IgnoreErrorAfterValid: boolean;
    function CleanPosIsAfterIgnorePos(CleanPos: integer): boolean;
    function LoadSourceCaseLoUp(const AFilename: string; AllowVirtual: boolean = false): pointer;

    function SearchIncludeFile(AFilename: string; out NewCode: Pointer;
                         var MissingIncludeFile: TMissingIncludeFile): boolean;

    function GuessMisplacedIfdefEndif(StartCursorPos: integer;
                                      StartCode: pointer;
                                      out EndCursorPos: integer;
                                      out EndCode: Pointer): boolean;

    function GetHiddenUsedUnits: string; // comma separated

    // global write lock
    procedure ActivateGlobalWriteLock;
    procedure DeactivateGlobalWriteLock;
    property OnGetGlobalChangeSteps: TLSOnGetGlobalChangeSteps
                     read FOnGetGlobalChangeSteps write FOnGetGlobalChangeSteps;
    property OnSetGlobalWriteLock: TOnSetWriteLock
                         read FOnSetGlobalWriteLock write FOnSetGlobalWriteLock;

    // properties
    property OnGetSource: TOnGetSource read FOnGetSource write FOnGetSource;
    property OnLoadSource: TOnLoadSource read FOnLoadSource write FOnLoadSource;
    property OnDeleteSource: TOnDeleteSource read FOnDeleteSource write FOnDeleteSource;
    property OnGetSourceStatus: TOnGetSourceStatus
                               read FOnGetSourceStatus write FOnGetSourceStatus;
    property OnGetFileName: TOnGetFileName read FOnGetFileName write FOnGetFileName;
    property OnCheckFileOnDisk: TOnCheckFileOnDisk
                               read FOnCheckFileOnDisk write FOnCheckFileOnDisk;
    property OnGetInitValues: TOnGetInitValues
                                   read FOnGetInitValues write FOnGetInitValues;
    property OnIncludeCode: TOnIncludeCode read FOnIncludeCode write FOnIncludeCode;
    property OnProgress: TLinkScannerProgress read FOnProgress write FOnProgress;
    property IgnoreMissingIncludeFiles: boolean read GetIgnoreMissingIncludeFiles
                                             write SetIgnoreMissingIncludeFiles;
    property InitialValues: TExpressionEvaluator read FInitValues write FInitValues;
    property MainCode: pointer read FMainCode write SetMainCode;
    property IncludeFileIsMissing: boolean read GetIncludeFileIsMissing;
    property NestedComments: boolean read FNestedComments;
    property CompilerMode: TCompilerMode read FCompilerMode write SetCompilerMode;
    property CompilerModeSwitches: TCompilerModeSwitches
                         read FCompilerModeSwitches write FCompilerModeSwitches;
    property PascalCompiler: TPascalCompiler read FPascalCompiler write SetPascalCompiler;
    property ScanTill: TLinkScannerRange read FScanTill write SetScanTill;
        
    procedure Clear;
    procedure ConsistencyCheck;
    procedure WriteDebugReport;
    procedure CalcMemSize(Stats: TCTMemStats);
    constructor Create;
    destructor Destroy; override;
  end;

//----------------------------------------------------------------------------

  // memory system for PSourceLink(s)
  TPSourceLinkMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposePSourceLink(Link: PSourceLink);
    function NewPSourceLink: PSourceLink;
  end;

  // memory system for PSourceLink(s)
  TPSourceChangeStepMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposePSourceChangeStep(Step: PSourceChangeStep);
    function NewPSourceChangeStep: PSourceChangeStep;
  end;

const
  DirectiveSequenceName: array [TSequenceDirective] of string =
    ('SCOPEDENUMS');
var
  CompilerModeVars: array[TCompilerMode] of string;

  PSourceLinkMemManager: TPSourceLinkMemManager;
  PSourceChangeStepMemManager: TPSourceChangeStepMemManager;

function StrToCompilerMode(const aName: string): TCompilerMode;
function StrToPascalCompiler(const aName: string): TPascalCompiler;

procedure AddCodeToUniqueList(ACode: Pointer; UniqueSortedCodeList: TFPList);
function IndexOfCodeInUniqueList(ACode: Pointer;
                                 UniqueSortedCodeList: TList): integer;
function IndexOfCodeInUniqueList(ACode: Pointer;
                                 UniqueSortedCodeList: TFPList): integer;
function dbgs(r: TLinkScannerRange): string; overload;
function dbgs(const ModeSwitches: TCompilerModeSwitches): string; overload;
function dbgs(k: TSourceLinkKind): string; overload;
function dbgs(s: TLSDirectiveState): string; overload;
function dbgs(s: TLSDirectiveKind): string; overload;

implementation

// useful procs ----------------------------------------------------------------

function IndexOfCodeInUniqueList(ACode: Pointer;
  UniqueSortedCodeList: TList): integer;
var l,m,r: integer;
begin
  l:=0;
  r:=UniqueSortedCodeList.Count-1;
  m:=0;
  while r>=l do begin
    m:=(l+r) shr 1;
    if ACode<UniqueSortedCodeList[m] then
      r:=m-1
    else if ACode>UniqueSortedCodeList[m] then
      l:=m+1
    else begin
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
end;

function IndexOfCodeInUniqueList(ACode: Pointer;
  UniqueSortedCodeList: TFPList): integer;
var l,m,r: integer;
begin
  l:=0;
  r:=UniqueSortedCodeList.Count-1;
  m:=0;
  while r>=l do begin
    m:=(l+r) shr 1;
    if ACode<UniqueSortedCodeList[m] then
      r:=m-1
    else if ACode>UniqueSortedCodeList[m] then
      l:=m+1
    else begin
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
end;

function dbgs(r: TLinkScannerRange): string; overload;
begin
  WriteStr(Result, r);
end;

function dbgs(const ModeSwitches: TCompilerModeSwitches): string;
var
  ms: TCompilerModeSwitch;
begin
  Result:='';
  for ms:=Low(TCompilerModeSwitches) to high(TCompilerModeSwitches) do
    if ms in ModeSwitches then begin
      Result:=Result+CompilerModeSwitchNames[ms]+',';
    end;
  System.Delete(Result,length(Result),1); // cut comma
  Result:='['+Result+']';
end;

function dbgs(k: TSourceLinkKind): string;
begin
  case k of
  slkCode: Result:='Code';
  slkMissingIncludeFile: Result:='MissingInc';
  slkSkipStart: Result:='SkipStart';
  slkSkipEnd: Result:='SkipEnd';
  else Result:='?';
  end;
end;

function dbgs(s: TLSDirectiveState): string;
begin
  case s of
  lsdsActive: Result:='Active';
  lsdsInactive: Result:='Inactive';
  lsdsSkipped: Result:='Skipped';
  end;
end;

function dbgs(s: TLSDirectiveKind): string;
begin
  WriteStr(Result,s);
end;

function StrToCompilerMode(const aName: string): TCompilerMode;
begin
  for Result:=low(Result) to high(Result) do
    if SysUtils.CompareText(aName,CompilerModeNames[Result])=0 then
      exit;
  Result:=cmFPC;
end;

function StrToPascalCompiler(const aName: string): TPascalCompiler;
begin
  for Result:=low(Result) to high(Result) do
    if SysUtils.CompareText(aName,PascalCompilerNames[Result])=0 then
      exit;
  Result:=pcFPC;
end;

procedure AddCodeToUniqueList(ACode: Pointer; UniqueSortedCodeList: TFPList);
var l,m,r: integer;
begin
  l:=0;
  r:=UniqueSortedCodeList.Count-1;
  m:=0;
  while r>=l do begin
    m:=(l+r) shr 1;
    if ACode<UniqueSortedCodeList[m] then
      r:=m-1
    else if ACode>UniqueSortedCodeList[m] then
      l:=m+1
    else
      exit;
  end;
  if (m<UniqueSortedCodeList.Count) and (ACode>UniqueSortedCodeList[m]) then
    inc(m);
  UniqueSortedCodeList.Insert(m,ACode);
end;

function CompareUpToken(const UpToken: string; const Txt: string;
  TxtStartPos, TxtEndPos: integer): boolean;
var len, i: integer;
begin
  Result:=false;
  len:=TxtEndPos-TxtStartPos;
  if len<>length(UpToken) then exit;
  i:=1;
  while i<len do begin
    if (UpToken[i]<>UpChars[Txt[TxtStartPos]]) then exit;
    inc(i);
    inc(TxtStartPos);
  end;
  Result:=true;
end;

function CompareLSDirectiveCodeSrcPos(Item1, Item2: Pointer): Integer;
var
  Dir1: PLSDirective absolute Item1;
  Dir2: PLSDirective absolute Item2;
begin
  Result:=ComparePointers(Dir1^.Code,Dir2^.Code);
  if Result<>0 then exit;
  Result:=Dir1^.SrcPos-Dir2^.SrcPos;
end;

function CompareLSDirectiveCodeSrcPosCleanPos(Item1, Item2: Pointer): Integer;
var
  Dir1: PLSDirective absolute Item1;
  Dir2: PLSDirective absolute Item2;
begin
  Result:=ComparePointers(Dir1^.Code,Dir2^.Code);
  if Result<>0 then exit;
  Result:=Dir1^.SrcPos-Dir2^.SrcPos;
  if Result<>0 then exit;
  Result:=Dir1^.CleanPos-Dir2^.CleanPos;
end;

{ TDirectiveSequenceItem }

constructor TDirectiveSequenceItem.Create;
begin
  FLastItem := -1;
end;

procedure TDirectiveSequenceItem.Add(const AValue: string; ACleanPos: integer);
begin
  if (FLastItem >= 0) and (ACleanPos <= FItems[FLastItem].CleanPos) then
    raise Exception.Create('Internal error: TDirectiveSequenceItem.Add: ACleanPos not sorted.');
  if Length(FItems) = 0 then
    SetLength(FItems, 1)
  else if FLastItem = High(FItems) then
    SetLength(FItems, Length(FItems)+Min(128, Length(FItems)));
  Inc(FLastItem);
  FItems[FLastItem].CleanPos := ACleanPos;
  FItems[FLastItem].Value := AValue;
end;

function TDirectiveSequenceItem.CalcMemSize: PtrUInt;
var
  Item: TDirectiveSequenceItemValue;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(Length(FItems))*PtrUInt(SizeOf(TDirectiveSequenceItemValue));

  for Item in FItems do
    Inc(Result, MemSizeString(Item.Value));
end;

procedure TDirectiveSequenceItem.Clear(FreeMemory: boolean);
begin
  if FreeMemory then
    SetLength(FItems, 0);
  FLastItem := -1;
end;

function TDirectiveSequenceItem.FindValue(const ACleanPos: integer; out
  Value: string): Boolean;

  function BinarySearch: integer;
  var
    I, Max, Min: Integer;
    ResIndex, ResCleanPos: integer;
  begin
    Max := FLastItem;
    Min := 0;
    ResIndex := -1;
    ResCleanPos := -1;
    while (Min <= Max) do
    begin
      I := (Max+Min) div 2;
      if (FItems[I].CleanPos = ACleanPos) then
        Exit(I)
      else
      if (FItems[I].CleanPos < ACleanPos) then
      begin
        if ResCleanPos < FItems[I].CleanPos then
        begin
          //store the closest
          ResIndex := I;
          ResCleanPos := FItems[I].CleanPos;
        end;
        Min := I + 1;
      end else
      begin
        Max := I - 1;
      end;
    end;
    Result := ResIndex;
  end;
var
  ItemIndex: integer;
begin
  ItemIndex := BinarySearch;
  Result := ItemIndex >= 0;
  if Result then
    Value := FItems[ItemIndex].Value
  else
    Value := '';
end;

{ TDirectiveSequence }

constructor TDirectiveSequence.Create;
var
  I: TSequenceDirective;
begin
  for I := Low(FDirectives) to High(FDirectives) do
    FDirectives[I] := TDirectiveSequenceItem.Create;
end;

procedure TDirectiveSequence.Add(ADirective: TSequenceDirective;
  const ADirectiveValue: string; ACleanPos: Integer);
begin
  FDirectives[ADirective].Add(ADirectiveValue, ACleanPos);
end;

function TDirectiveSequence.CalcMemSize: PtrUInt;
var
  Item: TDirectiveSequenceItem;
begin
  Result:=PtrUInt(InstanceSize);

  for Item in FDirectives do
    Inc(Result, Item.CalcMemSize);
end;

procedure TDirectiveSequence.Clear(FreeMemory: boolean);
var
  Item: TDirectiveSequenceItem;
begin
  for Item in FDirectives do
    Item.Clear(FreeMemory);
end;

destructor TDirectiveSequence.Destroy;
var
  Item: TDirectiveSequenceItem;
begin
  for Item in FDirectives do
    Item.Free;

  inherited Destroy;
end;

function TDirectiveSequence.FindValue(ADirective: TSequenceDirective;
  ACleanPos: Integer; out Value: string): Boolean;
begin
  Result := FDirectives[ADirective].FindValue(ACleanPos, Value);
end;

{ TLinkScanner }

// inline
function TLinkScanner.GetLinks(Index: integer): TSourceLink;
begin
  Result:=FLinks[Index];
end;

// inline
function TLinkScanner.GetLinkP(Index: integer): PSourceLink;
begin
  Result:=@FLinks[Index];
end;

// inline
procedure TLinkScanner.IncreaseChangeStep;
begin
  if FChangeStep=$7fffffff then FChangeStep:=-$7fffffff
  else inc(FChangeStep);
end;

// inline
procedure TLinkScanner.ReadSpace;
begin
  while (SrcPos<=SrcLen) and (IsSpaceChar[Src[SrcPos]]) do inc(SrcPos);
end;

// inline
procedure TLinkScanner.EndComment;
begin
  CommentStyle:=CommentNone;
end;

// inline
procedure TLinkScanner.IncCommentLevel;
begin
  if FNestedComments then inc(CommentLevel)
  else CommentLevel:=1;
end;

// inline
procedure TLinkScanner.DecCommentLevel;
begin
  if FNestedComments then dec(CommentLevel)
  else CommentLevel:=0;
end;

// inline
function TLinkScanner.DoEndToken: boolean;
begin
  TokenType:=lsttEnd;
  Result:=true;
end;

// inline
function TLinkScanner.DoSourceTypeToken: boolean;
// program, unit, library, package
// unit unit1;
// unit a.b.unit1 platform;
// unit unit1 unimplemented;
begin
  if ScannedRange<>lsrInit then exit(false);
  Result:=true;
  ScannedRange:=lsrSourceType;
  IsUnit:=TokenIsWord('UNIT');
  if ScannedRange=ScanTill then exit;
  repeat
    ReadNextToken; // read identifier
    if TokenType=lsttWord then begin
      if SourceName<>'' then
        SourceName:=SourceName+'.';
      SourceName:=SourceName+GetIdentifier(@Src[TokenStart]);
      ReadNextToken; // read ';' or '.' or hint modifier
    end;
  until TokenType<>lsttPoint;
  ScannedRange:=lsrSourceName;
  if ScannedRange=ScanTill then exit;
end;

// inline
function TLinkScanner.DoInterfaceToken: boolean;
begin
  if ord(ScannedRange)>=ord(lsrInterfaceStart) then exit(false);
  ScannedRange:=lsrInterfaceStart;
  Result:=true;
end;

// inline
function TLinkScanner.DoFinalizationToken: boolean;
begin
  if ord(ScannedRange)>=ord(lsrFinalizationStart) then exit(false);
  ScannedRange:=lsrFinalizationStart;
  Result:=true;
end;

// inline
function TLinkScanner.DoInitializationToken: boolean;
begin
  if ord(ScannedRange)>=ord(lsrInitializationStart) then exit(false);
  ScannedRange:=lsrInitializationStart;
  Result:=true;
end;

function TLinkScanner.DoUsesToken: boolean;
// uses name, name in 'string';
begin
  if ord(ScannedRange)<=ord(lsrInterfaceStart) then
    ScannedRange:=lsrMainUsesSectionStart
  else if ScannedRange=lsrImplementationStart then
    ScannedRange:=lsrImplementationUsesSectionStart
  else
    exit(false);
  repeat
    // read unit name
    repeat
      ReadNextToken;
      if (TokenType<>lsttWord)
      or WordIsKeyWord.DoItCaseInsensitive(@Src[SrcPos]) then exit(false);
      ReadNextToken;
    until TokenType<>lsttPoint;
    if TokenIs('in') then begin
      // read "in" filename
      ReadNextToken;
      if TokenType=lsttStringConstant then
        ReadNextToken;
    end;
    if TokenType=lsttSemicolon then break;
    if TokenType<>lsttComma then begin
      // syntax error -> this token does not belong to the uses section
      SrcPos:=TokenStart;
      break;
    end;
  until false;
  ScannedRange:=succ(ScannedRange);   // lsrMainUsesSectionEnd, lsrImplementationUsesSectionEnd;
  Result:=true;
end;

// inline
function TLinkScanner.DoImplementationToken: boolean;
begin
  if ord(ScannedRange)>=ord(lsrImplementationStart) then exit(false);
  ScannedRange:=lsrImplementationStart;
  Result:=true;
end;

// inline
function TLinkScanner.TokenIsWord(p: PChar): boolean;
begin
  Result:=(TokenType=lsttWord) and (CompareIdentifiers(p,@Src[TokenStart])=0);
end;

// inline
function TLinkScanner.GetDirectives(Index: integer): PLSDirective;
begin
  Result:=@FDirectives[Index];
end;

// inline
function TLinkScanner.GetDirectivesSorted(Index: integer): PLSDirective;
begin
  Result:=FDirectivesSorted[Index];
end;

function TLinkScanner.GetDirectiveValueAt(ADirective: TSequenceDirective;
  ACleanPos: integer): string;
begin
  if not FDirectiveSequence.FindValue(ADirective, ACleanPos, Result) then
    Result := FInitValues.Variables[DirectiveSequenceName[ADirective]];
end;

// inline
function TLinkScanner.LinkSize_Inline(Index: integer): integer;
var
  Link: PSourceLink;
begin
  Link:=@FLinks[Index];
  if Index+1<LinkCount then
    Result:=Link[1].CleanedPos-Link^.CleanedPos
  else
    Result:=CleanedLen-Link^.CleanedPos+1;
end;

// inline
function TLinkScanner.LinkCleanedEndPos_Inline(Index: integer): integer;
var
  Link: PSourceLink;
begin
  Link:=@FLinks[Index];
  if Index+1<LinkCount then
    Result:=Link[1].CleanedPos
  else
    Result:=CleanedLen+1;
end;

// inline
function TLinkScanner.StoreDirectives: boolean;
begin
  Result:=FStoreDirectives>0;
end;

procedure TLinkScanner.SortDirectives;
var
  i: Integer;
begin
  // try to keep memory allocated
  ReAllocMem(FDirectivesSorted,SizeOf(Pointer)*FDirectivesCapacity);
  for i:=0 to FDirectivesCount-1 do
    FDirectivesSorted[i]:=@FDirectives[i];
  for i:=FDirectivesCount to FDirectivesCapacity-1 do
    FDirectivesSorted[i]:=nil;
  MergeSortWithLen(PPointer(FDirectivesSorted),FDirectivesCount,
                   @CompareLSDirectiveCodeSrcPosCleanPos);
end;

procedure TLinkScanner.AddLink(ASrcPos: integer; ACode: Pointer;
  AKind: TSourceLinkKind);
var
  NewCapacity: Integer;
  Link: PSourceLink;
begin
  if (LinkCount>0) and (FLinks[FLinkCount-1].CleanedPos=CleanedLen+1) then begin
    // last link is empty => remove
    {$IFDEF ShowUpdateCleanedSrc}
    Link:=@FLinks[FLinkCount-1];
    debugln(['TLinkScanner.AddLink removing empty link: ',dbgs(Link^.Kind)]);
    {$ENDIF}
    dec(FLinkCount);
  end else if FLinkCount=FLinkCapacity then begin
    NewCapacity:=FLinkCapacity*2;
    if NewCapacity<16 then NewCapacity:=16;
    ReAllocMem(FLinks,NewCapacity*SizeOf(TSourceLink));
    FLinkCapacity:=NewCapacity;
  end;
  Link:=@FLinks[FLinkCount];
  with Link^ do begin
    CleanedPos:=CleanedLen+1;
    SrcPos:=ASrcPos;
    Code:=ACode;
    Kind:=AKind;
  end;
  inc(FLinkCount);
end;

function TLinkScanner.CleanedSrc: string;
begin
  if length(FCleanedSrc)<>CleanedLen then begin
    SetLength(FCleanedSrc,CleanedLen);
  end;
  Result:=FCleanedSrc;
  if FLastCleanedSrcLen<CleanedLen then FLastCleanedSrcLen:=CleanedLen;
end;

procedure TLinkScanner.Clear;
var i: integer;
  PLink: PSourceLink;
  PStamp: PSourceChangeStep;
begin
  IsUnit:=false;
  SourceName:='';
  FHiddenUsedUnits:='';
  ClearMacros;
  ClearLastError;
  ClearDirectives(false);
  ClearMissingIncludeFiles;
  for i:=0 to FIncludeStack.Count-1 do begin
    PLink:=PSourceLink(FIncludeStack[i]);
    PSourceLinkMemManager.DisposePSourceLink(PLink);
  end;
  FIncludeStack.Clear;
  FLinkCount:=0;
  FCleanedSrc:='';
  for i:=0 to FSourceChangeSteps.Count-1 do begin
    PStamp:=PSourceChangeStep(FSourceChangeSteps[i]);
    PSourceChangeStepMemManager.DisposePSourceChangeStep(PStamp);
  end;
  FSourceChangeSteps.Clear;
  IncreaseChangeStep;
end;

constructor TLinkScanner.Create;
begin
  inherited Create;
  FInitValues:=TExpressionEvaluator.Create;
  Values:=TExpressionEvaluator.Create;
  FDirectiveSequence:=TDirectiveSequence.Create;
  IncreaseChangeStep;
  FSourceChangeSteps:=TFPList.Create;
  FMainCode:=nil;
  FMainSourceFilename:='';
  FIncludeStack:=TFPList.Create;
  FPascalCompiler:=pcFPC;
  FCompilerMode:=cmFPC;
  FCompilerModeSwitches:=DefaultCompilerModeSwitches[CompilerMode];
  FNestedComments:=cmsNested_comment in CompilerModeSwitches;
end;

destructor TLinkScanner.Destroy;
begin
  Clear;
  ClearDirectives(true);
  ReAllocMem(FMacros,0);
  FreeAndNil(FIncludeStack);
  FreeAndNil(FSourceChangeSteps);
  FreeAndNil(Values);
  FreeAndNil(FDirectiveSequence);
  FreeAndNil(FInitValues);
  ReAllocMem(FLinks,0);
  inherited Destroy;
end;

function TLinkScanner.LinkSize(Index: integer): integer;

  procedure IndexOutOfBounds;
  begin
    RaiseConsistencyException(20170422125948,'TLinkScanner.LinkSize  index '
       +IntToStr(Index)+' out of bounds: 0..'+IntToStr(LinkCount-1));
  end;

begin
  if (Index<0) or (Index>=LinkCount) then
    IndexOutOfBounds;
  Result:=LinkSize_Inline(Index);
end;

function TLinkScanner.LinkCleanedEndPos(Index: integer): integer;
begin
  Result:=LinkSize(Index)+FLinks[Index].CleanedPos;
end;

function TLinkScanner.LinkSourceLog(Index: integer): TSourceLog;
begin
  if Assigned(OnGetSource) then
    Result:=OnGetSource(Self,FLinks[Index].Code)
  else
    Result:=nil;
end;

function TLinkScanner.FindFirstSiblingLink(LinkIndex: integer): integer;
{ find link at the start of the code
  e.g. The resulting link SrcPos is always 1
  
   if LinkIndex is in the main code, the result will be 0
   if LinkIndex is in an include file, the result will be the first link of
   the include file. If the include file is included multiple times, it is
   treated as if they are different files.
}
var
  LastIndex: integer;
begin
  Result:=LinkIndex;
  if LinkIndex>=0 then begin
    LastIndex:=LinkIndex;
    while (Result>=0) do begin
      if FLinks[Result].Code=FLinks[LinkIndex].Code then begin
        if Links[Result].SrcPos>FLinks[LastIndex].SrcPos then begin
          // the include file was (in-)directly included by itself
          // -> skip
          Result:=FindParentLink(Result);
        end else if FLinks[Result].SrcPos=1 then begin
          // start found
          exit;
        end;
        LastIndex:=Result;
      end;
      dec(Result);
    end;
  end;
end;

function TLinkScanner.FindParentLink(LinkIndex: integer): integer;
// a parent link is the link of the include directive
// or in other words: the link in front of the first sibling link
begin
  Result:=FindFirstSiblingLink(LinkIndex);
  if Result>=0 then dec(Result);
end;

function TLinkScanner.LinkIndexNearCursorPos(ACursorPos: integer;
  ACode: Pointer; var CursorInLink: boolean): integer;
// returns the nearest link at cursorpos
// (either covering the cursorpos or in front)
var
  CurLinkSize: integer;
  BestLinkIndex: integer;
begin
  BestLinkIndex:=-1;
  Result:=0;
  CursorInLink:=false;
  while Result<LinkCount do begin
    if (ACode=FLinks[Result].Code) and (ACursorPos>=FLinks[Result].SrcPos) then
    begin
      CurLinkSize:=LinkSize(Result);
      if ACursorPos<FLinks[Result].SrcPos+CurLinkSize then begin
        CursorInLink:=true;
        exit;
      end else begin
        if (BestLinkIndex<0)
        or (FLinks[BestLinkIndex].SrcPos<FLinks[Result].SrcPos) then begin
          BestLinkIndex:=Result;
        end;
      end;
    end;
    inc(Result);
  end;
  Result:=BestLinkIndex;
end;

function TLinkScanner.CreateTreeOfSourceCodes: TAVLTree;
var
  CurCode: Pointer;
  i: Integer;
begin
  Result:=TAVLTree.Create(@ComparePointers);
  for i:=0 to LinkCount-1 do begin
    CurCode:=FLinks[i].Code;
    if CurCode=nil then continue;
    if Result.Find(CurCode)=nil then
      Result.Add(CurCode);
  end;
end;

procedure TLinkScanner.ClearDirectives(FreeMemory: boolean);
begin
  FDirectivesCount:=0;
  if FreeMemory then begin
    ReAllocMem(FDirectives,0);
    ReAllocMem(FDirectivesSorted,0);
    FDirectivesCapacity:=0;
  end else begin
    if FDirectivesSorted<>nil then
      FDirectivesSorted[0]:=nil;
  end;
  FDirectiveSequence.Clear(FreeMemory);
end;

procedure TLinkScanner.DemandStoreDirectives;
begin
  inc(FStoreDirectives);
end;

procedure TLinkScanner.ReleaseStoreDirectives;
begin
  if FStoreDirectives=0 then
    raise Exception.Create('');
  dec(FStoreDirectives);
  if FStoreDirectives=0 then
    ClearDirectives(true);
end;

function TLinkScanner.FindDirective(aCode: Pointer; aSrcPos: integer; out
  FirstSortedIndex, LastSortedIndex: integer): boolean;
var
  l: Integer;
  r: Integer;
  m: Integer;
  Dir: TLSDirective;
  cmp: Integer;
begin
  Dir.Code:=aCode;
  Dir.SrcPos:=aSrcPos;
  l:=0;
  r:=FDirectivesCount-1;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareLSDirectiveCodeSrcPos(@Dir,DirectivesSorted[m]);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else begin
      // found
      FirstSortedIndex:=m;
      LastSortedIndex:=m;
      while (FirstSortedIndex>0)
      and (CompareLSDirectiveCodeSrcPos(@Dir,DirectivesSorted[FirstSortedIndex-1])=0) do
        dec(FirstSortedIndex);
      while (LastSortedIndex+1<FDirectivesCount)
      and (CompareLSDirectiveCodeSrcPos(@Dir,DirectivesSorted[LastSortedIndex+1])=0) do
        inc(LastSortedIndex);
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
  FirstSortedIndex:=-1;
  LastSortedIndex:=-1;
end;

function TLinkScanner.FindFirstDirective(aCode: Pointer; aSrcPos: integer;
  const AllowedStates: TLSDirectiveStates): PLSDirective;
var
  FirstSortedIndex, LastSortedIndex, i: integer;
begin
  Result:=nil;
  if not FindDirective(aCode,aSrcPos,FirstSortedIndex,LastSortedIndex) then exit;
  for i:=FirstSortedIndex to LastSortedIndex do begin
    Result:=DirectivesSorted[i];
    if Result^.State in AllowedStates then exit;
  end;
  Result:=nil;
end;

function TLinkScanner.LinkIndexAtCleanPos(ACleanPos: integer): integer;

  procedure ConsistencyError1;
  begin
    raise Exception.Create(
      'TLinkScanner.LinkAtCleanPos Consistency-Error 1');
  end;

  procedure ConsistencyError2;
  begin
    raise Exception.Create(
      'TLinkScanner.LinkAtCleanPos Consistency-Error 2');
  end;

var l,r,m: integer;
begin
  Result:=-1;
  if (ACleanPos<1) or (ACleanPos>CleanedLen) then exit;
  // binary search through the links
  l:=0;
  r:=LinkCount-1;
  while l<=r do begin
    m:=(l+r) div 2;
    if m<LinkCount-1 then begin
      if ACleanPos<FLinks[m].CleanedPos then
        r:=m-1
      else if ACleanPos>=FLinks[m+1].CleanedPos then
        l:=m+1
      else begin
        Result:=m;
        exit;
      end;
    end else begin
      if ACleanPos>=FLinks[m].CleanedPos then begin
        Result:=m;
        exit;
      end else
        ConsistencyError2;
    end;
  end;
  ConsistencyError1;
end;

function TLinkScanner.LinkIndexAtCursorPos(ACursorPos: integer; ACode: Pointer
  ): integer;
var
  CurLinkSize: integer;
begin
  Result:=0;
  while Result<LinkCount do begin
    if (ACode=FLinks[Result].Code) and (ACursorPos>=FLinks[Result].SrcPos) then begin
      CurLinkSize:=LinkSize(Result);
      if ACursorPos<FLinks[Result].SrcPos+CurLinkSize then begin
        exit;
      end;
    end;
    inc(Result);
  end;
  Result:=-1;
end;

procedure TLinkScanner.SetSource(ACode: Pointer);

  procedure RaiseUnableToGetCode;
  begin
    RaiseConsistencyException(20170422125957,'unable to get source with Code='+DbgS(Code));
  end;

var SrcLog: TSourceLog;
begin
  if Assigned(FOnGetSource) then begin
    SrcLog:=FOnGetSource(Self,ACode);
    if SrcLog=nil then
      RaiseUnableToGetCode;
    SrcFilename:=FOnGetFileName(Self,ACode);
    AddSourceChangeStep(ACode,SrcLog.ChangeStep);
    Src:=SrcLog.Source;
    Code:=ACode;
    SrcPos:=1;
    TokenStart:=1;
    TokenType:=lsttNone;
    SrcLen:=length(Src);
    CopiedSrcPos:=0;
  end else begin
    RaiseUnableToGetCode;
  end;
end;

procedure TLinkScanner.HandleDirective;
var DirStart, DirLen: integer;
  CurDirective: PLSDirective;
begin
  FDirectiveCleanPos:=CommentStartPos-CopiedSrcPos+CleanedLen;
  if StoreDirectives then begin
    if FDirectivesCount=FDirectivesCapacity then begin
      // grow
      if FDirectivesCapacity=0 then
        FDirectivesCapacity:=16
      else
        FDirectivesCapacity:=FDirectivesCapacity*2;
      ReAllocMem(FDirectives,FDirectivesCapacity*SizeOf(TLSDirective));
    end;
    CurDirective:=@FDirectives[FDirectivesCount];
    CurDirective^.Kind:=lsdkNone;
    CurDirective^.Code:=Code;
    CurDirective^.SrcPos:=CommentStartPos;
    CurDirective^.CleanPos:=FDirectiveCleanPos;
    if FSkippingDirectives=lssdNone then
      CurDirective^.State:=lsdsActive
    else
      CurDirective^.State:=lsdsSkipped;
    CurDirective^.Level:=IfLevel;
    inc(FDirectivesCount);
  end;
  SrcPos:=CommentInnerStartPos+1;
  DirStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) do
    inc(SrcPos);
  DirLen:=SrcPos-DirStart;
  if DirLen>255 then DirLen:=255;
  FDirectiveName:=copy(Src,DirStart,DirLen);
  DoDirective(DirStart,DirLen);
  SrcPos:=CommentEndPos;
end;

function TLinkScanner.ReturnFromIncludeFileAndIsEnd: boolean;
begin
  Result:=false;
  if not ReturnFromIncludeFile then begin
    SrcPos:=SrcLen+1; // make sure SrcPos stands somewhere
    TokenStart:=SrcPos;
    TokenType:=lsttSrcEnd;
    Result:=true;
  end;
end;

{$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
{$R-}
procedure TLinkScanner.ReadNextToken;
var
  c1: char;
  c2: char;
  MacroID: LongInt;
  p: PChar;
begin
  //DebugLn([' TLinkScanner.ReadNextToken SrcPos=',SrcPos,' SrcLen=',SrcLen,' "',dbgstr(Src,SrcPos,5),'"']);
  if (SrcPos>SrcLen) and ReturnFromIncludeFileAndIsEnd then exit;
  //DebugLn([' TLinkScanner.ReadNextToken SrcPos=',SrcPos,' SrcLen=',SrcLen,' "',copy(Src,SrcPos,5),'"']);
  // Skip all spaces and comments
  p:=@Src[SrcPos];
  while true do begin
    case p^ of
    #0:
      begin
        SrcPos:=p-PChar(Src)+1;
        if (SrcPos>SrcLen) then begin
          if ReturnFromIncludeFileAndIsEnd then exit;
          if (SrcPos>SrcLen) then break;
        end else
          inc(SrcPos);
        p:=@Src[SrcPos];
      end;
    '{' :
      begin
        SrcPos:=p-PChar(Src)+1;
        ReadCurlyComment;
        p:=@Src[SrcPos];
      end;
    '/':
      if p[1]='/' then begin
        SrcPos:=p-PChar(Src)+1;
        ReadLineComment;
        p:=@Src[SrcPos];
      end else
        break;
    '(':
      if p[1]='*' then begin
        SrcPos:=p-PChar(Src)+1;
        ReadRoundComment;
        p:=@Src[SrcPos];
      end else
        break;
     ' ',#9,#10,#13:
        repeat
          inc(p);
        until not (p^ in [' ',#9,#10,#13]);
    else
      break;
    end;
  end;
  TokenStart:=p-PChar(Src)+1;
  // read token
  c1:=p^;
  case c1 of
  '_','A'..'Z','a'..'z':
    begin
      // keyword or identifier
      inc(p);
      while IsIdentChar[p^] do
        inc(p);
      TokenType:=lsttWord;
      SrcPos:=p-PChar(Src)+1;
      if FMacrosOn then begin
        MacroID:=IndexOfMacro(@Src[TokenStart],false);
        if MacroID>=0 then begin
          AddMacroSource(MacroID);
        end;
      end;
    end;
  '&':
    begin
      // identifier with "&" character or an octal number
      inc(p);
      case p^ of
        '_','A'..'Z','a'..'z'://identifier: &uses
        begin
          inc(p);
          while IsIdentChar[p^] do
            inc(p);
          TokenType:=lsttWord;
        end;
        '0'..'7'://octal number: &10
        begin
          inc(p);
          while IsOctNumberChar[p^] do
            inc(p);
          TokenType:=lsttNone;
        end;
      else
        TokenType:=lsttNone;
      end;
      SrcPos:=p-PChar(Src)+1;
    end;
  '''','#':
    begin
      TokenType:=lsttStringConstant;
      while true do begin
        case p^ of
        #0:
          begin
            SrcPos:=p-PChar(Src)+1;
            if SrcPos>SrcLen then break;
            inc(p);
          end;
        '#':
          begin
            inc(p);
            while IsNumberChar[p^] do
              inc(p);
          end;
        '''':
          begin
            inc(p);
            while true do begin
              case p^ of
              #0:
                begin
                  SrcPos:=p-PChar(Src)+1;
                  if SrcPos>SrcLen then break;
                  inc(p);
                end;
              '''':
                begin
                  inc(p);
                  break;
                end;
              #10,#13:
                break;
              else
                inc(p);
              end;
            end;
          end;
        else
          break;
        end;
      end;
      SrcPos:=p-PChar(Src)+1;
    end;
  '0'..'9':
    begin
      TokenType:=lsttNone;
      inc(p);
      while IsNumberChar[p^] do
        inc(p);
      if (p^='.') and (p[1]<>'.') then begin
        // real type number
        inc(p);
        while IsNumberChar[p^] do
          inc(p);
        if (p^ in ['E','e']) then begin
          // read exponent
          inc(p);
          if (p^ in ['-','+']) then inc(p);
          while IsNumberChar[p^] do
            inc(p);
        end;
      end;
      SrcPos:=p-PChar(Src)+1;
    end;
  '%': // boolean
    begin
      TokenType:=lsttNone;
      inc(p);
      while p^ in ['0'..'1'] do
        inc(p);
      SrcPos:=p-PChar(Src)+1;
    end;
  '$': // hex
    begin
      TokenType:=lsttNone;
      inc(p);
      while IsHexNumberChar[p^] do
        inc(p);
      SrcPos:=p-PChar(Src)+1;
    end;
  '=':
    begin
      SrcPos:=p-PChar(Src)+2;
      TokenType:=lsttEqual;
    end;
  '.':
    begin
      SrcPos:=p-PChar(Src)+2;
      TokenType:=lsttPoint;
    end;
  ';':
    begin
      SrcPos:=p-PChar(Src)+2;
      TokenType:=lsttSemicolon;
    end;
  ',':
    begin
      SrcPos:=p-PChar(Src)+2;
      TokenType:=lsttComma;
    end;
  else
    TokenType:=lsttNone;
    inc(p);
    c2:=p^;
    // test for double char operators
    //  :=, +=, -=, /=, *=, <>, <=, >=, **, ><, ..
    if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
    or ((c1='<') and (c2='>'))
    or ((c1='>') and (c2='<'))
    or ((c1='.') and (c2='.'))
    or ((c1='*') and (c2='*'))
    then inc(p);
    SrcPos:=p-PChar(Src)+1;
  end;
end;
{$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}

procedure TLinkScanner.Scan(Range: TLinkScannerRange; CheckFilesOnDisk: boolean);
var
  LastTokenType: TLSTokenType;
  cm: TCompilerMode;
  pc: TPascalCompiler;
  s: string;
  LastProgressPos: integer;
  CheckForAbort: boolean;
  NewSrcLen: Integer;
begin
  if (not UpdateNeeded(Range,CheckFilesOnDisk)) then begin
    // input is the same as last time -> output is the same
    // -> if there was an error and it was in a needed range, raise it again
    if LastErrorIsValid then begin
      // the error has happened in ScannedRange
      if ord(ScannedRange)>=ord(Range) then begin
        // error is behind needed range => ok
      end else if (not IgnoreErrorAfterValid)
          or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage)
      then
        RaiseLastError;
    end;
    exit;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan A -------- Range=',dbgs(Range));
  {$ENDIF}
  ScanTill:=Range;
  Clear;

  if Assigned(OnGetGlobalChangeSteps) then
    OnGetGlobalChangeSteps(FGlobalSourcesChangeStep,FGlobalFilesChangeStep,
                           FGlobalInitValuesChangeStep);
  FStates:=FStates-[lssSourcesChanged,lssFilesChanged,lssInitValuesChanged];

  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan B ');
  {$ENDIF}
  SetSource(FMainCode);
  NewSrcLen:=length(Src);
  if NewSrcLen<FLastCleanedSrcLen+1000 then
    NewSrcLen:=FLastCleanedSrcLen+1000;
  SetLength(FCleanedSrc,NewSrcLen);
  CleanedLen:=0;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan C ',dbgs(SrcLen));
  {$ENDIF}
  ScannedRange:=lsrNone;
  IsUnit:=false;
  SourceName:='';
  CommentStyle:=CommentNone;
  CommentLevel:=0;
  FPascalCompiler:=pcFPC;
  CompilerMode:=cmFPC;
  FNestedComments:=cmsNested_comment in DefaultCompilerModeSwitches[CompilerMode];
  IfLevel:=0;
  FSkippingDirectives:=lssdNone;
  FDirectivesStored:=StoreDirectives;
  //DebugLn('TLinkScanner.Scan D --------');

  // initialize Defines
  if Assigned(FOnGetInitValues) then
    FInitValues.Assign(FOnGetInitValues(Self,FMainCode,FInitValuesChangeStep));
  Values.Assign(FInitValues);

  // compiler
  s:=FInitValues.Variables[PascalCompilerDefine];
  if s<>'' then begin
    for pc:=Low(TPascalCompiler) to High(TPascalCompiler) do
      if (s=PascalCompilerNames[pc]) then
        PascalCompiler:=pc;
  end else if InitialValues.IsDefined('pas2js') then
    PascalCompiler:=pcPas2js
  else if InitialValues.IsDefined('delphi') and not InitialValues.IsDefined('fpc') then
    PascalCompiler:=pcDelphi;

  // compiler mode
  for cm:=Low(TCompilerMode) to High(TCompilerMode) do
    if FInitValues.IsDefined(CompilerModeVars[cm]) then begin
      CompilerMode:=cm;
      break;
    end;

  // nested comments override
  if Values.IsDefined(ExternalMacroStart+'NestedComments') then
    FNestedComments:=true;

  //DebugLn(['TLinkScanner.Scan ',MainFilename,' ',PascalCompilerNames[PascalCompiler],' ',CompilerModeNames[CompilerMode],' FNestedComments=',FNestedComments,' ModeSwitches=',dbgs(CompilerModeSwitches)]);

  //DebugLn(Values.AsString);
  FMacrosOn:=(Values.Variables['MACROS']<>'0');
  if Src='' then exit;
  // begin scanning
  AddLink(SrcPos,Code);
  LastTokenType:=lsttNone;
  LastProgressPos:=0;
  CheckForAbort:=Assigned(OnProgress);
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan F ',dbgs(SrcLen));
  {$ENDIF}
  ScannedRange:=lsrInit;
  if ScanTill=lsrInit then exit;
  try
    try
      ReadNextToken;
      if TokenIsWord('USES') then
        DoUsesToken
      else
        SrcPos:=TokenStart;
      while ord(ScanTill)>ord(ScannedRange) do begin
        // check every 100.000 bytes for abort
        if CheckForAbort and ((LastProgressPos-CopiedSrcPos)>100000) then begin
          LastProgressPos:=CopiedSrcPos;
          DoCheckAbort;
        end;
        //debugln(['TLinkScanner.Scan Token ',dbgstr(Src,TokenStart,SrcPos-TokenStart)]);
        ReadNextToken;
        if TokenType=lsttWord then
          ParseKeyWord(TokenStart,LastTokenType);

        //writeln('TLinkScanner.Scan G "',copy(Src,TokenStart,SrcPos-TokenStart),'" LastTokenType=',LastTokenType,' TokenType=',TokenType);
        if (LastTokenType=lsttEnd) and (TokenType=lsttPoint) then begin
          //DebugLn(['TLinkScanner.Scan END. ',MainFilename]);
          ScannedRange:=lsrEnd;
          break;
        end;
        if (SrcPos>SrcLen) and ReturnFromIncludeFileAndIsEnd then
          break;
        LastTokenType:=TokenType;
      end;
    finally
      {$IFDEF ShowUpdateCleanedSrc}
      DebugLn('TLinkScanner.Scan UpdatePos=',DbgS(SrcPos-1));
      {$ENDIF}
      if (SrcPos>CopiedSrcPos) then
        UpdateCleanedSource(SrcPos-1);
      if FDirectivesCount>0 then
        SortDirectives;
      if FSkippingDirectives<>lssdNone then begin
        {$IFDEF ShowUpdateCleanedSrc}
        DebugLn(['TLinkScanner.Scan missing $ENDIF']);
        {$ENDIF}
      end;
    end;
    IncreaseChangeStep;
    FLastCleanedSrcLen:=CleanedLen;
  except
    on E: ELinkScannerError do begin
      if (not IgnoreErrorAfterValid)
      or (not IgnoreErrAfterPositionIsInFrontOfLastErrMessage) then
        raise;
      {$IFDEF ShowIgnoreErrorAfter}
      DebugLn('TLinkScanner.Scan IGNORING ERROR: ',LastErrorMessage);
      {$ENDIF}
    end;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TLinkScanner.Scan END ',dbgs(CleanedLen),' ',dbgs(ScannedRange));
  {$ENDIF}
end;

procedure TLinkScanner.SetLinks(Index: integer; const Value: TSourceLink);
begin
  FLinks[Index]:=Value;
end;

procedure TLinkScanner.ReadCurlyComment;
// a normal pascal {} comment
var
  p: PChar;
begin
  CommentStyle:=CommentCurly;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  CommentInnerStartPos:=SrcPos+1;
  p:=@Src[SrcPos];
  inc(p);
  // HandleSwitches can dec CommentLevel
  while true do begin
    case p^ of
      #0:
        begin
          SrcPos:=p-PChar(Src)+1;
          if SrcPos>SrcLen then
            break;
        end;
      '{' :
        IncCommentLevel;
      '}' :
        begin
          DecCommentLevel;
          if CommentLevel=0 then begin
            inc(p);
            break;
          end;
        end;
    end;
    inc(p);
  end;
  SrcPos:=p-PChar(Src)+1;
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-1;
  if (CommentLevel>0) then CommentEndNotFound(20170422130048);
  // handle compiler switches
  if Src[CommentInnerStartPos]='$' then
    HandleDirective;
  EndComment;
end;

procedure TLinkScanner.ReadLineComment;
// a  // newline  comment
var
  p: PChar;
begin
  CommentStyle:=CommentLine;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  p:=@Src[SrcPos];
  inc(p,2);
  CommentInnerStartPos:=SrcPos;
  while not (p^ in [#0,#10,#13]) do inc(p);
  DecCommentLevel;
  if p^<>#0 then inc(p);
  SrcPos:=p-PChar(Src)+1;
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-1;
  // handle compiler switches (ignore)
  EndComment;
end;

procedure TLinkScanner.ReadRoundComment;
// a delphi comment (* *)
var
  p: PChar;
begin
  CommentStyle:=CommentLine;
  CommentStartPos:=SrcPos;
  IncCommentLevel;
  CommentInnerStartPos:=SrcPos+2;
  p:=@Src[SrcPos];
  inc(p,2);
  while true do begin
    case p^ of
    #0:
      begin
        SrcPos:=p-PChar(Src)+1;
        if SrcPos>SrcLen then
          break;
      end;
    '*':
      begin
        inc(p);
        if p^=')' then begin
          inc(p);
          DecCommentLevel;
          if CommentLevel=0 then break;
        end;
      end;
    '(':
      begin
        inc(p);
        if FNestedComments and (p^='*') then begin
          inc(p);
          IncCommentLevel;
        end;
      end;
    else
      inc(p);
    end;
  end;
  SrcPos:=p-PChar(Src)+1;
  CommentEndPos:=SrcPos;
  CommentInnerEndPos:=SrcPos-2;
  if (CommentLevel>0) then CommentEndNotFound(20170422130050);
  // handle compiler switches
  if Src[CommentInnerStartPos]='$' then
    HandleDirective;
  EndComment;
end;

procedure TLinkScanner.CommentEndNotFound(id: int64);
begin
  SrcPos:=CommentStartPos;
  RaiseException(id,ctsCommentEndNotFound);
end;

procedure TLinkScanner.UpdateCleanedSource(NewCopiedSrcPos: integer);
// add new parsed code to cleaned source string

  procedure RaiseInvalid;
  begin
    debugln(['TLinkScanner.UpdateCleanedSource inconsistency found: Srclen=',SrcLen,'=',length(Src),' FCleanedSrc=',CleanedLen,'/',length(FCleanedSrc),' CopiedSrcPos=',CopiedSrcPos,' NewCopiedSrcPos=',NewCopiedSrcPos,' AddLen=',NewCopiedSrcPos-CopiedSrcPos]);
    RaiseConsistencyException(20170422130054,'TLinkScanner.UpdateCleanedSource inconsistency found AddLen='+dbgs(NewCopiedSrcPos-CopiedSrcPos));
  end;

var AddLen: integer;
begin
  if NewCopiedSrcPos>SrcLen then NewCopiedSrcPos:=SrcLen+1;
  if NewCopiedSrcPos=CopiedSrcPos then exit;
  AddLen:=NewCopiedSrcPos-CopiedSrcPos;
  if AddLen<=0 then RaiseInvalid;
  if AddLen>length(FCleanedSrc)-CleanedLen then begin
    // expand cleaned source string by at least 1024
    SetLength(FCleanedSrc,length(FCleanedSrc)+SrcLen+1024);
  end;
  System.Move(Src[CopiedSrcPos+1],FCleanedSrc[CleanedLen+1],AddLen);
  inc(CleanedLen,AddLen);
  {$IFDEF ShowUpdateCleanedSrc}
  DebugLn('TLinkScanner.UpdateCleanedSource A ',
    DbgS(CopiedSrcPos),'-',DbgS(NewCopiedSrcPos),'="',
    StringToPascalConst(copy(Src,CopiedSrcPos+1,20)),
    '".."',StringToPascalConst(copy(Src,NewCopiedSrcPos-19,20)),'"');
  {$ENDIF}
  CopiedSrcPos:=NewCopiedSrcPos;
end;

procedure TLinkScanner.AddSourceChangeStep(ACode: pointer; AChangeStep: integer);

  procedure RaiseCodeNil;
  begin
    RaiseConsistencyException(20170422130109,'TLinkScanner.AddSourceChangeStep ACode=nil');
  end;

var l,r,m: integer;
  NewSrcChangeStep: PSourceChangeStep;
  c: pointer;
begin
  //DebugLn('[TLinkScanner.AddSourceChangeStep] ',DbgS(ACode));
  if ACode=nil then
    RaiseCodeNil;
  l:=0;
  r:=FSourceChangeSteps.Count-1;
  m:=0;
  c:=nil;
  while (l<=r) do begin
    m:=(l+r) shr 1;
    c:=PSourceChangeStep(FSourceChangeSteps[m])^.Code;
    if c<ACode then l:=m+1
    else if c>ACode then r:=m-1
    else exit;
  end;
  NewSrcChangeStep:=PSourceChangeStepMemManager.NewPSourceChangeStep;
  NewSrcChangeStep^.Code:=ACode;
  NewSrcChangeStep^.ChangeStep:=AChangeStep;
  if (FSourceChangeSteps.Count>0) and (c<ACode) then inc(m);
  FSourceChangeSteps.Insert(m,NewSrcChangeStep);
  //DebugLn('   ADDING ',DbgS(ACode),',',FSourceChangeSteps.Count);
end;

function TLinkScanner.TokenIs(const AToken: string): boolean;
var ATokenLen: integer;
  i: integer;
begin
  Result:=false;
  if (SrcPos<=SrcLen+1) and (TokenStart>=1) then begin
    ATokenLen:=length(AToken);
    if ATokenLen=SrcPos-TokenStart then begin
      for i:=1 to ATokenLen do
        if AToken[i]<>Src[TokenStart-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TLinkScanner.UpTokenIs(const AToken: string): boolean;
var ATokenLen: integer;
  i: integer;
begin
  Result:=false;
  if (SrcPos<=SrcLen+1) and (TokenStart>=1) then begin
    ATokenLen:=length(AToken);
    if ATokenLen=SrcPos-TokenStart then begin
      for i:=1 to ATokenLen do
        if AToken[i]<>UpChars[Src[TokenStart-1+i]] then exit;
      Result:=true;
    end;
  end;
end;

procedure TLinkScanner.ConsistencyCheck;
var i: integer;
begin
  if (FLinks=nil) xor (FLinkCapacity=0) then
    RaiseCatchableException('');
  if FLinks<>nil then begin
    for i:=0 to FLinkCount-1 do begin
      if (FLinks[i].Code=nil) and (FLinks[i].Kind=slkCode) then
        RaiseCatchableException('');
      if (FLinks[i].CleanedPos<1) or (FLinks[i].CleanedPos>SrcLen) then
        RaiseCatchableException('');
    end;
  end;
  if SrcLen<>length(Src) then
    RaiseCatchableException('');
  if Values<>nil then
    Values.ConsistencyCheck;
end;

procedure TLinkScanner.WriteDebugReport;
var i: integer;
begin
  // header
  DebugLn('');
  DebugLn('[TLinkScanner.WriteDebugReport]',
     ' ChangeStepCount=',dbgs(FSourceChangeSteps.Count),
     ' LinkCount=',dbgs(LinkCount),
     ' CleanedLen=',dbgs(CleanedLen));
  // time stamps
  for i:=0 to FSourceChangeSteps.Count-1 do begin
    DebugLn('  ChangeStep ',dbgs(i),': '
        ,' Code=',dbgs(PSourceChangeStep(FSourceChangeSteps[i])^.Code)
        ,' ChangeStep=',dbgs(PSourceChangeStep(FSourceChangeSteps[i])^.ChangeStep));
  end;
  // links
  for i:=0 to LinkCount-1 do begin
    DebugLn(['  Link ',i,':'
        ,' CleanedPos=',FLinks[i].CleanedPos
        ,' SrcPos=',FLinks[i].SrcPos
        ,' Code=',dbgs(FLinks[i].Code)
        ,' Kind=',dbgs(FLinks[i].Kind)
        ,' Src="',dbgstr(CleanedSrc,FLinks[i].CleanedPos,Min(50,LinkSize(i))),'"'
      ]);
  end;
end;

procedure TLinkScanner.CalcMemSize(Stats: TCTMemStats);
begin
  Stats.Add('TLinkScanner',
    PtrUInt(InstanceSize)
    +MemSizeString(FMainSourceFilename)
    +length(FDirectiveName)
    +MemSizeString(LastErrorMessage)
    +MemSizeString(SrcFilename));
  Stats.Add('TLinkScanner.CleanedSrc',MemSizeString(FCleanedSrc));
  // Note: Src belongs to the codebuffer

  if FLinks<>nil then
    Stats.Add('TLinkScanner.FLinks',
      FLinkCapacity*SizeOf(TSourceLink));
  if FInitValues<>nil then
    Stats.Add('TLinkScanner.FInitValues',
      FInitValues.CalcMemSize(false)); // FInitValues are copies of strings of TDefineTree
  if FSourceChangeSteps<>nil then
    Stats.Add('TLinkScanner.FSourceChangeSteps',
      FSourceChangeSteps.InstanceSize
               +FSourceChangeSteps.Capacity*SizeOf(TSourceChangeStep));
  if FIncludeStack<>nil then
    Stats.Add('TLinkScanner.FIncludeStack',
      FIncludeStack.InstanceSize+FIncludeStack.Capacity*SizeOf(TSourceLink));
  if Values<>nil then
    Stats.Add('TLinkScanner.Values',
      Values.CalcMemSize(true,FInitValues));
  if FDirectiveSequence<>nil then
    Stats.Add('TLinkScanner.FDirectiveSequence',
      FDirectiveSequence.CalcMemSize);
  if FMissingIncludeFiles<>nil then
    Stats.Add('TLinkScanner.FMissingIncludeFiles',
      FMissingIncludeFiles.InstanceSize);
end;

function TLinkScanner.UpdateNeeded(
  Range: TLinkScannerRange; CheckFilesOnDisk: boolean): boolean;
{ the clean source must be rebuilt if
   1. a former check says so
   2. scanrange increased
   3. unit source changed
   4. one of its include files changed
   5. init values changed (e.g. initial compiler defines)
   6. a missing include file can now be found
}
var i: integer;
  SrcLog: TSourceLog;
  NewInitValues: TExpressionEvaluator;
  NewInitValuesChangeStep: integer;
  SrcChange: PSourceChangeStep;
  CurSourcesChangeStep, CurFilesChangeStep: int64;
  CurInitValuesChangeStep: integer;
begin
  Result:=true;

  if Range=lsrNone then exit(false);

  if not Assigned(FOnCheckFileOnDisk) then CheckFilesOnDisk:=false;

  // use the last check result
  if [lssSourcesChanged,lssInitValuesChanged]*FStates<>[] then exit;
  if CheckFilesOnDisk and (lssFilesChanged in FStates) then exit;

  // check options
  if StoreDirectives and (not DirectivesStored) then exit;

  // check if range increased
  // Note: if there was an error, then a range increase will raise the same error
  // and no update is needed
  if (ord(Range)>ord(ScannedRange)) and (not LastErrorIsValid) then begin
    {$IFDEF VerboseUpdateNeeded}
    DebugLn(['TLinkScanner.UpdateNeeded because range increased from ',dbgs(ScannedRange),' to ',dbgs(Range),' ',ExtractFilename(MainFilename)]);
    {$ENDIF}
    exit;
  end;

  // do a quick test: check the global change steps for sources and values
  if Assigned(OnGetGlobalChangeSteps) then begin
    OnGetGlobalChangeSteps(CurSourcesChangeStep,CurFilesChangeStep,CurInitValuesChangeStep);
    if (CurSourcesChangeStep=FGlobalSourcesChangeStep)
    and (CurInitValuesChangeStep=FGlobalInitValuesChangeStep)
    and ((not CheckFilesOnDisk) or (CurFilesChangeStep=FGlobalSourcesChangeStep))
    then begin
      // sources and values did not change since last check
      //debugln(['TLinkScanner.UpdateNeeded global change steps still the same: ',MainFilename]);
      Result:=false;
      exit;
    end;
  end else begin
    CurSourcesChangeStep:=1;
    CurFilesChangeStep:=1;
    CurInitValuesChangeStep:=1;
  end;

  // check initvalues
  //if ExtractFileNameOnly(MainFilename)='androidr14' then
    //debugln(['TLinkScanner.UpdateNeeded FGlobalInitValuesChangeStep=',FGlobalInitValuesChangeStep,' CurInitValuesChangeStep=',CurInitValuesChangeStep]);
  if FGlobalInitValuesChangeStep<>CurInitValuesChangeStep then begin
    FGlobalInitValuesChangeStep:=CurInitValuesChangeStep;
    if Assigned(FOnGetInitValues) then begin
      NewInitValues:=FOnGetInitValues(Self,Code,NewInitValuesChangeStep);
      if (NewInitValues<>nil)
      and (NewInitValuesChangeStep<>FInitValuesChangeStep)
      and (not FInitValues.Equals(NewInitValues)) then begin
        {$IFDEF VerboseUpdateNeeded}
        //if ExtractFileNameOnly(MainFilename)='androidr14' then
          DebugLn(['TLinkScanner.UpdateNeeded because InitValues changed ',MainFilename]);
        {$ENDIF}
        Include(FStates,lssInitValuesChanged);
        exit;
      end;
    end;
  end;

  // check all used codebuffers
  if FGlobalSourcesChangeStep<>CurSourcesChangeStep then begin
    FGlobalSourcesChangeStep:=CurSourcesChangeStep;
    if Assigned(FOnGetSource) then begin
      for i:=0 to FSourceChangeSteps.Count-1 do begin
        SrcChange:=PSourceChangeStep(FSourceChangeSteps[i]);
        SrcLog:=FOnGetSource(Self,SrcChange^.Code);
        //debugln(['TLinkScanner.UpdateNeeded ',ExtractFilename(MainFilename),' i=',i,' File=',FOnGetFileName(Self,SrcLog),' Last=',SrcChange^.ChangeStep,' Now=',SrcLog.ChangeStep]);
        if SrcChange^.ChangeStep<>SrcLog.ChangeStep then begin
          {$IFDEF VerboseUpdateNeeded}
          DebugLn(['TLinkScanner.UpdateNeeded because source buffer changed: ',OnGetFileName(Self,SrcLog),' MainFilename=',MainFilename]);
          {$ENDIF}
          Include(FStates,lssSourcesChanged);
          exit;
        end;
      end;
    end;
  end;

  // check all file dates
  if CheckFilesOnDisk then begin
    if FGlobalFilesChangeStep<>CurFilesChangeStep then begin
      FGlobalFilesChangeStep:=CurFilesChangeStep;
      if Assigned(FOnGetSource) then begin
        // if files changed on disk, reload them
        for i:=0 to FSourceChangeSteps.Count-1 do begin
          SrcChange:=PSourceChangeStep(FSourceChangeSteps[i]);
          SrcLog:=FOnGetSource(Self,SrcChange^.Code);
          if FOnCheckFileOnDisk(SrcLog) then begin
            {$IFDEF VerboseUpdateNeeded}
            DebugLn(['TLinkScanner.UpdateNeeded because file on disk changed: ',OnGetFileName(Self,SrcLog),' MainFilename=',MainFilename]);
            {$ENDIF}
            Include(FStates,lssFilesChanged);
            exit;
          end;
        end;
      end;
    end;
  end;
  
  // check missing include files
  if CheckFilesOnDisk and MissingIncludeFilesNeedsUpdate then begin
    {$IFDEF VerboseUpdateNeeded}
    DebugLn(['TLinkScanner.UpdateNeeded because MissingIncludeFilesNeedsUpdate']);
    {$ENDIF}
    Include(FStates,lssFilesChanged);
    exit;
  end;

  // no update needed :)
  //DebugLn('TLinkScanner.UpdateNeeded END');
  Result:=false;
end;

procedure TLinkScanner.SetIgnoreErrorAfter(ACursorPos: integer; ACode: Pointer);
begin
  if (FIgnoreErrorAfterCode=ACode)
  and (FIgnoreErrorAfterCursorPos=ACursorPos) then exit;
  FIgnoreErrorAfterCode:=ACode;
  FIgnoreErrorAfterCursorPos:=ACursorPos;
  LastErrorCheckedForIgnored:=false;
  {$IFDEF ShowIgnoreErrorAfter}
  DbgOut('TLinkScanner.SetIgnoreErrorAfter ');
  if FIgnoreErrorAfterCode<>nil then
    DbgOut(OnGetFileName(Self,FIgnoreErrorAfterCode))
  else
    DbgOut('nil');
  DbgOut(' ',dbgs(FIgnoreErrorAfterCursorPos));
  DebugLn('');
  {$ENDIF}
end;

procedure TLinkScanner.ClearIgnoreErrorAfter;
begin
  SetIgnoreErrorAfter(0,nil);
end;

function TLinkScanner.IgnoreErrAfterPositionIsInFrontOfLastErrMessage: boolean;
var
  CleanResult: integer;
begin
  //DebugLn('TLinkScanner.IgnoreErrAfterPositionIsInFrontOfLastErrMessage');
  //DebugLn(['  LastErrorCheckedForIgnored=',LastErrorCheckedForIgnored,
  //  ' LastErrorBehindIgnorePosition=',LastErrorBehindIgnorePosition]);
  if LastErrorCheckedForIgnored then
    Result:=LastErrorBehindIgnorePosition
  else begin
    CleanedIgnoreErrorAfterPosition:=-1;
    if (FIgnoreErrorAfterCode<>nil) and (FIgnoreErrorAfterCursorPos>0) then
    begin
      CleanResult:=CursorToCleanPos(FIgnoreErrorAfterCursorPos,
                         FIgnoreErrorAfterCode,CleanedIgnoreErrorAfterPosition);
      {$IFDEF ShowIgnoreErrorAfter}
      DebugLn(['  CleanResult=',CleanResult,
        ' CleanedIgnoreErrorAfterPosition=',CleanedIgnoreErrorAfterPosition,
        ' FIgnoreErrorAfterCursorPos=',FIgnoreErrorAfterCursorPos,
        ' CleanedLen=',CleanedLen,
        ' LastErrorIsValid=',LastErrorIsValid]);
      {$ENDIF}
      if (CleanResult=0) or (CleanResult=-1)
      or (not LastErrorIsValid) then begin
        Result:=true;
      end else begin
        Result:=false;
      end;
    end else begin
      Result:=false;
    end;
    LastErrorBehindIgnorePosition:=Result;
    LastErrorCheckedForIgnored:=true;
  end;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.IgnoreErrAfterPositionIsInFrontOfLastErrMessage Result=',dbgs(Result));
  {$ENDIF}
end;

function TLinkScanner.IgnoreErrorAfterCleanedPos: integer;
begin
  if IgnoreErrAfterPositionIsInFrontOfLastErrMessage then
    Result:=CleanedIgnoreErrorAfterPosition
  else
    Result:=-1;
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.IgnoreErrorAfterCleanedPos Result=',dbgs(Result));
  {$ENDIF}
end;

function TLinkScanner.IgnoreErrorAfterValid: boolean;
begin
  Result:=(FIgnoreErrorAfterCode<>nil);
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.IgnoreErrorAfterValid Result=',dbgs(Result));
  {$ENDIF}
end;

function TLinkScanner.CleanPosIsAfterIgnorePos(CleanPos: integer): boolean;
var
  p: LongInt;
begin
  if IgnoreErrorAfterValid then begin
    p:=IgnoreErrorAfterCleanedPos;
    if p<1 then
      Result:=false
    else
      Result:=CleanPos>=p;
  end else begin
    Result:=false
  end;
end;

function TLinkScanner.LastErrorIsInFrontOfCleanedPos(ACleanedPos: integer
  ): boolean;
begin
  Result:=LastErrorIsValid and (CleanedLen<ACleanedPos);
  {$IFDEF ShowIgnoreErrorAfter}
  DebugLn('TLinkScanner.LastErrorIsInFrontOfCleanedPos Result=',dbgs(Result));
  {$ENDIF}
end;

procedure TLinkScanner.RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos: integer);
begin
  if LastErrorIsInFrontOfCleanedPos(ACleanedPos) then
    RaiseLastError;
end;

{-------------------------------------------------------------------------------
  function TLinkScanner.GuessMisplacedIfdefEndif
  Params: StartCursorPos: integer; StartCode: pointer;
          var EndCursorPos: integer; var EndCode: Pointer;
  Result: boolean;


-------------------------------------------------------------------------------}
function TLinkScanner.GuessMisplacedIfdefEndif(StartCursorPos: integer;
  StartCode: pointer;
  out EndCursorPos: integer; out EndCode: Pointer): boolean;
  
  type
    TIf = record
      StartPos: integer; // comment start e.g. {
      EndPos: integer;   // comment end  e.g. the char behind }
      Expression: string;
      HasElse: boolean;
    end;
    PIf = ^TIf;
    
    TTokenType = (ttNone,
                  ttCommentStart, ttCommentEnd, // '{' '}'
                  ttTPCommentStart, ttTPCommentEnd, // '(*' '*)'
                  ttDelphiCommentStart, // '//'
                  ttLineEnd
                  );
                  
    TTokenRange = (trCode, trComment, trTPComment, trDelphiComment);

    TToken = record
      StartPos: integer;
      EndPos: integer;
      TheType: TTokenType;
      Range: TTokenRange;
      NestedComments: boolean;
    end;
    
    TDirectiveType = (dtUnknown, dtIf, dtIfDef, dtIfNDef, dtIfOpt,
                      dtElse, dtEndif);
    
  function FindNextToken(const ASrc: string; var AToken: TToken): boolean;
  var
    ASrcLen: integer;
    OldRange: TTokenRange;
  begin
    Result:=true;
    AToken.StartPos:=AToken.EndPos;
    ASrcLen:=length(ASrc);
    OldRange:=AToken.Range;
    
    while (AToken.StartPos<=ASrcLen) do begin
      case ASrc[AToken.StartPos] of
      '{': // pascal comment start
        begin
          AToken.EndPos:=AToken.StartPos+1;
          AToken.TheType:=ttCommentStart;
          AToken.Range:=trComment;
          if (OldRange=trCode) then
            exit
          else if AToken.NestedComments then begin
            if (not FindNextToken(ASrc,AToken)) then begin
              Result:=false;
              exit;
            end;
            AToken.StartPos:=AToken.EndPos-1;
            AToken.Range:=OldRange;
          end;
        end;
        
      '(': // check if Turbo Pascal comment start
        if (AToken.StartPos<ASrcLen) and (ASrc[AToken.StartPos+1]='*') then
        begin
          AToken.EndPos:=AToken.StartPos+2;
          AToken.TheType:=ttTPCommentStart;
          AToken.Range:=trTPComment;
          if (OldRange=trCode) then
            exit
          else if AToken.NestedComments then begin
            if (not FindNextToken(ASrc,AToken)) then begin
              Result:=false;
              exit;
            end;
            AToken.StartPos:=AToken.EndPos-1;
            AToken.Range:=OldRange;
          end;
        end;
        
      '/': // check if Delphi comment start
        if (AToken.StartPos<ASrcLen) and (ASrc[AToken.StartPos+1]='/') then
        begin
          AToken.EndPos:=AToken.StartPos+2;
          AToken.TheType:=ttDelphiCommentStart;
          AToken.Range:=trDelphiComment;
          if (OldRange=trCode) then
            exit
          else if AToken.NestedComments then begin
            if (not FindNextToken(ASrc,AToken)) then begin
              Result:=false;
              exit;
            end;
            AToken.StartPos:=AToken.EndPos-1;
            AToken.Range:=OldRange;
          end;
        end;
        
      '}': // pascal comment end
        case AToken.Range of
        trComment:
          begin
            AToken.EndPos:=AToken.StartPos+1;
            AToken.TheType:=ttCommentEnd;
            AToken.Range:=trCode;
            exit;
          end;
          
        trCode:
          begin
            // error (comment was never openend)
            // -> skip rest of code
            AToken.StartPos:=ASrcLen;
          end;

        else
          // in different kind of comment -> ignore
        end;
        
      '*': // turbo pascal comment end
        if (AToken.StartPos<ASrcLen) and (ASrc[AToken.StartPos+1]=')') then
        begin
          case AToken.Range of
          trTPComment:
            begin
              AToken.EndPos:=AToken.StartPos+1;
              AToken.TheType:=ttTPCommentEnd;
              AToken.Range:=trCode;
              exit;
            end;

          trCode:
            begin
              // error (comment was never openend)
              // -> skip rest of code
              AToken.StartPos:=ASrcLen;
            end;

          else
            // in different kind of comment -> ignore
          end;
        end;

      #10,#13: // line end
        if AToken.Range in [trDelphiComment] then begin
          AToken.EndPos:=AToken.StartPos+1;
          if (AToken.StartPos<ASrcLen)
          and (ASrc[AToken.StartPos+1] in [#10,#13])
          and (ASrc[AToken.StartPos+1]<>ASrc[AToken.StartPos]) then
            inc(AToken.EndPos);
          AToken.TheType:=ttLineEnd;
          AToken.Range:=trCode;
          exit;
        end else begin
          // in different kind of comment -> ignore
        end;

      '''': // skip string constant
        begin
          inc(AToken.StartPos);
          while (AToken.StartPos<=ASrcLen) do begin
            if (not (ASrc[AToken.StartPos] in ['''',#10,#13])) then begin
              inc(AToken.StartPos);
            end else begin
              break;
            end;
          end;
        end;
        
      end;
      inc(AToken.StartPos);
    end;
    
    // at the end of the code
    AToken.EndPos:=AToken.StartPos;
    AToken.TheType:=ttNone;
    Result:=false;
  end;

  procedure FreeIfStack(var IfStack: TFPList);
  var
    i: integer;
    AnIf: PIf;
  begin
    if IfStack=nil then exit;
    for i:=0 to IfStack.Count-1 do begin
      AnIf:=PIf(IfStack[i]);
      AnIf^.Expression:='';
      Dispose(AnIf);
    end;
    IfStack.Free;
    IfStack:=nil;
  end;
  
  function InitGuessMisplaced(out CurToken: TToken; ACode: Pointer;
    out ASrc: string; out ASrcLen: integer): boolean;
  var
    ASrcLog: TSourceLog;
  begin
    Result:=false;
    
    // get source
    if (FOnGetSource=nil) then exit;
    ASrcLog:=FOnGetSource(Self,ACode);
    if ASrcLog=nil then exit;
    ASrc:=ASrcLog.Source;
    ASrcLen:=length(ASrc);

    CurToken.StartPos:=1;
    CurToken.EndPos:=1;
    CurToken.Range:=trCode;
    CurToken.TheType:=ttNone;
    CurToken.NestedComments:=NestedComments;
    Result:=true;
  end;
  
  function ReadDirectiveType(const ASrc: string;
    AToken: TToken): TDirectiveType;
  const
    DIR_RST: array[0..5] of TDirectiveType = (
      dtIfDef, dtIfNDef, dtIfOpt, dtIf, dtElse, dtEndif
    );
    DIR_TXT: array[0..5] of PChar = (
      'IFDEF', 'IFNDEF', 'IFOPT', 'IF', 'ELSE', 'ENDIF'
    );
  var
    ASrcLen, p: integer;
    n: Integer;
  begin
    Result:=dtUnknown;
    ASrcLen:=length(ASrc);
    p:=AToken.EndPos;
    if (p<ASrcLen) and (ASrc[p]='$') then
    begin
      // compiler directive
      inc(p);
      for n := Low(DIR_TXT) to High(DIR_TXT) do
      begin
        if CompareIdentifiers(@ASrc[p], DIR_TXT[n]) = 0
        then begin
          Result := DIR_RST[n];
          Exit;
        end;
      end;
    end;
  end;
  
  procedure PushIfOnStack(const ASrc: string; AToken: TToken; IfStack: TFPList);
  var
    NewIf: PIf;
  begin
    New(NewIf);
    FillChar(NewIf^,SizeOf(PIf),0);
    NewIf^.StartPos:=AToken.StartPos;
    FindNextToken(ASrc,AToken);
    NewIf^.EndPos:=AToken.EndPos;
    NewIf^.Expression:=copy(ASrc,NewIf^.StartPos+1,
                            AToken.EndPos-NewIf^.StartPos-1);
    NewIf^.HasElse:=false;
    IfStack.Add(NewIf);
  end;
  
  procedure PopIfFromStack(IfStack: TFPList);
  var Topif: PIf;
  begin
    TopIf:=PIf(IfStack[IfStack.Count-1]);
    Dispose(TopIf);
    IfStack.Delete(IfStack.Count-1);
  end;

  function GuessMisplacedIfdefEndifInCode(ACode: Pointer;
    var EndCursorPos: integer; var EndCode: Pointer): boolean;
  var
    ASrc: string;
    ASrcLen: integer;
    CurToken: TToken;
    IfStack: TFPList;
    DirectiveType: TDirectiveType;
  begin
    Result:=false;
    if not InitGuessMisplaced(CurToken,ACode,ASrc,ASrcLen) then exit;

    IfStack:=TFPList.Create;
    try
      repeat
        if (not FindNextToken(ASrc,CurToken)) then begin
          exit;
        end;
        if CurToken.Range in [trComment] then begin
          DirectiveType:=ReadDirectiveType(ASrc,CurToken);

          case DirectiveType of

          dtIf, dtIfDef, dtIfNDef, dtIfOpt:
            PushIfOnStack(ASrc,CurToken,IfStack);

          dtElse:
            begin
              if (IfStack.Count=0) or (PIf(IfStack[IfStack.Count-1])^.HasElse)
              then begin
                // this $ELSE has no $IF
                // -> misplaced directive found
                EndCursorPos:=CurToken.EndPos;
                EndCode:=ACode;
                DebugLn('GuessMisplacedIfdefEndif  $ELSE has no $IF');
                Result:=true;
                exit;
              end;
              PIf(IfStack[IfStack.Count-1])^.HasElse:=true;
            end;
            
          dtEndif:
            begin
              if (IfStack.Count=0) then begin
                // this $ENDIF has no $IF
                // -> misplaced directive found
                EndCursorPos:=CurToken.EndPos;
                EndCode:=ACode;
                DebugLn('GuessMisplacedIfdefEndif  $ENDIF has no $IF');
                Result:=true;
                exit;
              end;
              PopIfFromStack(IfStack);
            end;

          end;
        end;
      until CurToken.TheType=ttNone;
      if IfStack.Count>0 then begin
        // there is an $IF without $ENDIF
        // -> misplaced directive found
        EndCursorPos:=PIf(IfStack[IfStack.Count-1])^.StartPos+1;
        EndCode:=ACode;
        DebugLn('GuessMisplacedIfdefEndif  $IF without $ENDIF');
        Result:=true;
        exit;
      end;
    finally
      FreeIfStack(IfStack);
    end;
  end;
        
var
  LinkID, i, BestSrcPos: integer;
  LastCode: Pointer;
  SearchedCodes: TFPList;
begin
  Result:=false;
  EndCursorPos:=0;
  EndCode:=nil;
  if StartCode=nil then exit;
  
  // search link before start position
  LinkID:=-1;
  BestSrcPos:=0;
  i:=0;
  while i<LinkCount do begin
    if (StartCode=FLinks[i].Code) and (StartCursorPos>=FLinks[i].SrcPos) then begin
      if (LinkID<0) or (BestSrcPos<FLinks[i].SrcPos) then
        LinkID:=i;
    end;
    inc(i);
  end;
  if LinkID<0 then exit;
  
  // go through all following sources and guess misplaced ifdef/endif
  SearchedCodes:=TFPList.Create;
  try
    while LinkId<LinkCount do begin
      Result:=GuessMisplacedIfdefEndifInCode(FLinks[LinkID].Code,
        EndCursorPos,EndCode);
      if Result then exit;
      // search next code
      LastCode:=FLinks[LinkID].Code;
      SearchedCodes.Add(LastCode);
      repeat
        inc(LinkID);
        if LinkID>=LinkCount then exit;
        if FLinks[LinkID].Code=nil then continue;
      until (FLinks[LinkID].Code<>LastCode)
      and (SearchedCodes.IndexOf(FLinks[LinkID].Code)<0);
    end;
  finally
    SearchedCodes.Free;
  end;
end;

function TLinkScanner.GetHiddenUsedUnits: string;
var
  Controller: String;
  AnUnitName: String;
  p: Integer;
begin
  if FHiddenUsedUnits='' then begin
    // see fpc/compiler/pmodules.pp loaddefaultunits
    if PascalCompiler=pcDelphi then
      FHiddenUsedUnits:=DelphiSystemUnitName
    else
      FHiddenUsedUnits:=FPCSystemUnitName;
    if InitialValues.IsDefined('VER1_0')
    then begin
      if InitialValues.IsDefined('LINUX') then
        FHiddenUsedUnits:='SYSLINUX'
      else if InitialValues.IsDefined('BSD') then
        FHiddenUsedUnits:='SYSBSD'
      else if InitialValues.IsDefined('WIN32') then
        FHiddenUsedUnits:='SYSWIN32';
    end;
    if InitialValues.IsDefined(MacroUseLineInfo) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',lineinfo'
    else if InitialValues.IsDefined(MacroUselnfodwrf) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',lnfodwrf';
    if InitialValues.IsDefined(MacroUseValgrind) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',cmem';
    if InitialValues.IsDefined(MacroUseHeapTrc) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',HeapTrc';
    if InitialValues.IsDefined('FPC_HAS_WinLikeResources') then begin
      // ToDo: fpintres
      FHiddenUsedUnits:=FHiddenUsedUnits+',fpextres';
    end;
    if (cmsObjpas in CompilerModeSwitches) and (PascalCompiler=pcFPC) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',ObjPas';
    if (CompilerMode=cmMacPas) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',MacPas';
    if (CompilerMode=cmISO) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',ISO7185';
    if (CompilerMode=cmExtPas) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',ISO7185,ExtPas';
    if (cmsCBlocks in CompilerModeSwitches) and (PascalCompiler=pcFPC) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',BlockRTL';
    if (cmsDefault_unicodestring in CompilerModeSwitches) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',UUChar';
    if CompilerMode=cmISO then
      FHiddenUsedUnits:=FHiddenUsedUnits+',iso7185';
    if cmsObjectiveC1 in CompilerModeSwitches then
      FHiddenUsedUnits:=FHiddenUsedUnits+',ObjC,ObjCBase';
    if InitialValues.IsDefined(MacroUseProfiler) then
      // Note: only valid on i386_go32v2,i386_watcom
      FHiddenUsedUnits:=FHiddenUsedUnits+',profile';
    if InitialValues.IsDefined(MacroUseSysThrds) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',SysThrds';
    if InitialValues.IsDefined(MacroUseFPCylix) then
      FHiddenUsedUnits:=FHiddenUsedUnits+',fpcylix,dynlibs';
    Controller:=InitialValues[MacroControllerUnit];
    if (Controller<>'') and IsDottedIdentifier(Controller) then
      FHiddenUsedUnits:=FHiddenUsedUnits+','+Controller;

    // check if this is a hidden used unit
    AnUnitName:=ExtractFileNameOnly(MainFilename);
    if AnUnitName<>'' then begin
      if AnUnitName='system' then
        FHiddenUsedUnits:=''
      else begin
        p:=length(FHiddenUsedUnits);
        while p>=1 do begin
          while (p>1) and (FHiddenUsedUnits[p-1]<>',') do dec(p);
          if (CompareDottedIdentifiers(@FHiddenUsedUnits[p],PChar(AnUnitName))=0)
          or (IsUnit and (SourceName<>'')
              and (CompareDottedIdentifiers(@FHiddenUsedUnits[p],PChar(SourceName))=0))
          then begin
            // this unit is a hidden unit => remove this and all behind from list
            if p>1 then
              System.Delete(FHiddenUsedUnits,p-1,length(FHiddenUsedUnits))
            else
              FHiddenUsedUnits:='';
            break;
          end;
          dec(p); // skip comma
        end;
      end;
    end;

    //debugln(['TLinkScanner.GetHiddenUsedUnits ',dbgs(CompilerModeSwitches),' ',PascalCompilerNames[PascalCompiler],' Mode=',CompilerModeNames[CompilerMode],' units=',FHiddenUsedUnits]);
  end;
  Result:=FHiddenUsedUnits;
end;

procedure TLinkScanner.SetMainCode(const Value: pointer);
begin
  if FMainCode=Value then exit;
  FMainCode:=Value;
  FMainSourceFilename:=FOnGetFileName(Self,FMainCode);
  Clear;
end;

procedure TLinkScanner.SetScanTill(const Value: TLinkScannerRange);
var
  OldScanRange: TLinkScannerRange;
begin
  if FScanTill=Value then exit;
  OldScanRange:=FScanTill;
  FScanTill := Value;
  if ord(OldScanRange)<ord(FScanTill) then Clear;
end;

function TLinkScanner.ShortSwitchDirective: boolean;
// example: {$H+} or {$H+, R- comment}
var
  c: Char;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkShortSwitch;
  c:=UpChars[FDirectiveName[1]];
  FDirectiveName:=CompilerSwitchesNames[c];
  if FDirectiveName<>'' then begin
    if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
      if Src[SrcPos]='-' then
        Values.Variables[FDirectiveName] := '0'
      else
        Values.Variables[FDirectiveName] := '1';
      inc(SrcPos);
      Result:=ReadNextSwitchDirective;
    end else begin
      if c='I' then
        Result:=IncludeDirective
      else
        Result:=LongSwitchDirective;
    end;
  end else
    Result:=true;
end;

function TLinkScanner.DoDirective(StartPos, DirLen: integer): boolean;
var
  p: PChar;
begin
  Result:=false;
  if StartPos>SrcLen then exit;
  p:=@Src[StartPos];
  //DebugLn(['TLinkScanner.DoDirective ',copy(Src,StartPos,DirLen),' FSkippingDirectives=',ord(FSkippingDirectives)]);
  if FSkippingDirectives=lssdNone then begin
    if DirLen=1 then begin
      Result:=(CompilerSwitchesNames[UpChars[p^]]<>'')
              and ShortSwitchDirective;
    end else begin
      case UpChars[p^] of
      'A':
        case UpChars[p[1]] of
        'L': if CompareIdentifiers(p,'ALIGN')=0 then Result:=LongSwitchDirective;
        'S': if CompareIdentifiers(p,'ASSERTIONS')=0 then Result:=LongSwitchDirective;
        end;
      'B':
        if CompareIdentifiers(p,'BOOLEVAL')=0 then Result:=LongSwitchDirective;
      'D':
        case UpChars[p[1]] of
        'E':
          case UpChars[p[2]] of
          'F': if CompareIdentifiers(p,'DEFINE')=0 then Result:=DefineDirective;
          'B': if CompareIdentifiers(p,'DEBUGINFO')=0 then Result:=LongSwitchDirective;
          end;
        end;
      'E':
        case UpChars[p[1]] of
        'L':
          case UpChars[p[2]] of
          'I': if CompareIdentifiers(p,'ELIFC')=0 then Result:=ElIfCDirective;
          'S':
            case UpChars[p[3]] of
            'E':
              if CompareIdentifiers(p,'ELSE')=0 then Result:=ElseDirective
              else if CompareIdentifiers(p,'ELSEC')=0 then Result:=ElseCDirective
              else if CompareIdentifiers(p,'ELSEIF')=0 then Result:=ElseIfDirective;
            end;
          end;
        'N':
          if CompareIdentifiers(p,'ENDC')=0 then Result:=EndCDirective
          else if CompareIdentifiers(p,'ENDIF')=0 then Result:=EndIfDirective;
        'X':
          if CompareIdentifiers(p,'EXTENDEDSYNTAX')=0 then Result:=LongSwitchDirective;
        end;
      'I':
        case UpChars[p[1]] of
        'F':
          case UpChars[p[2]] of
          'C': if CompareIdentifiers(p,'IFC')=0 then Result:=IfCDirective;
          'D': if CompareIdentifiers(p,'IFDEF')=0 then Result:=IfDefDirective;
          'E': if CompareIdentifiers(p,'IFEND')=0 then Result:=IfEndDirective;
          'N': if CompareIdentifiers(p,'IFNDEF')=0 then Result:=IfndefDirective;
          'O': if CompareIdentifiers(p,'IFOPT')=0 then Result:=IfOptDirective;
          else if DirLen=2 then Result:=IfDirective;
          end;
        'N':
          if CompareIdentifiers(p,'INCLUDE')=0 then Result:=IncludeDirective
          else if CompareIdentifiers(p,'INCLUDEPATH')=0 then Result:=IncludePathDirective;
        'O': if CompareIdentifiers(p,'IOCHECKS')=0 then Result:=LongSwitchDirective;
        end;
      'L':
        if CompareIdentifiers(p,'LOCALSYMBOLS')=0 then Result:=LongSwitchDirective
        else if CompareIdentifiers(p,'LONGSTRINGS')=0 then Result:=LongSwitchDirective;
      'M':
        if CompareIdentifiers(p,'MODE')=0 then Result:=ModeDirective
        else if CompareIdentifiers(p,'MODESWITCH')=0 then Result:=ModeSwitchDirective
        else if CompareIdentifiers(p,'MACRO')=0 then Result:=MacroDirective;
      'O':
        if CompareIdentifiers(p,'OPENSTRINGS')=0 then Result:=LongSwitchDirective
        else if CompareIdentifiers(p,'OVERFLOWCHECKS')=0 then Result:=LongSwitchDirective;
      'R':
        if CompareIdentifiers(p,'RANGECHECKS')=0 then Result:=LongSwitchDirective
        else if CompareIdentifiers(p,'REFERENCEINFO')=0 then Result:=LongSwitchDirective;
      'S':
        if CompareIdentifiers(p,'SETC')=0 then Result:=SetCDirective
        else if CompareIdentifiers(p,'STACKFRAMES')=0 then Result:=LongSwitchDirective
        else if CompareIdentifiers(p,'SCOPEDENUMS')=0 then Result:=LongSwitchDirectiveWithSequence(sdScopedEnums);
      'T':
        if CompareIdentifiers(p,'THREADING')=0 then Result:=ThreadingDirective
        else if CompareIdentifiers(p,'TYPEADDRESS')=0 then Result:=LongSwitchDirective
        else if CompareIdentifiers(p,'TYPEINFO')=0 then Result:=LongSwitchDirective;
      'U':
        if CompareIdentifiers(p,'UNDEF')=0 then Result:=UndefDirective;
      'V':
        if CompareIdentifiers(p,'VARSTRINGCHECKS')=0 then Result:=LongSwitchDirective;
      end;
    end;
  end else begin
    // skipping code => read only IF directives
    case UpChars[p^] of
    'E':
      case UpChars[p[1]] of
      'L':
        case UpChars[p[2]] of
        'I': if CompareIdentifiers(p,'ELIFC')=0 then Result:=ElIfCDirective;
        'S':
          case UpChars[p[3]] of
          'E':
            if CompareIdentifiers(p,'ELSE')=0 then Result:=ElseDirective
            else if CompareIdentifiers(p,'ELSEC')=0 then Result:=ElseCDirective
            else if CompareIdentifiers(p,'ELSEIF')=0 then Result:=ElseIfDirective;
          end;
        end;
      'N':
        if CompareIdentifiers(p,'ENDC')=0 then Result:=EndCDirective
        else if CompareIdentifiers(p,'ENDIF')=0 then Result:=EndIfDirective;
      end;
    'I':
      case UpChars[p[1]] of
      'F':
        case UpChars[p[2]] of
        'C': if CompareIdentifiers(p,'IFC')=0 then Result:=IfCDirective;
        'D': if CompareIdentifiers(p,'IFDEF')=0 then Result:=IfDefDirective;
        'E': if CompareIdentifiers(p,'IFEND')=0 then Result:=IfEndDirective;
        'N': if CompareIdentifiers(p,'IFNDEF')=0 then Result:=IfndefDirective;
        'O': if CompareIdentifiers(p,'IFOPT')=0 then Result:=IfOptDirective;
        else if DirLen=2 then Result:=IfDirective;
        end;
      end;
    end;
  end;
end;

function TLinkScanner.LongSwitchDirective: boolean;
// example: {$ASSERTIONS ON comment}
var ValStart: integer;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkLongSwitch;
  ReadSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and IsWordChar[Src[SrcPos]] do
    inc(SrcPos);
  if CompareUpToken('ON',Src,ValStart,SrcPos) then
    Values.Variables[FDirectiveName] := '1'
  else if CompareUpToken('OFF',Src,ValStart,SrcPos) then
    Values.Variables[FDirectiveName] := '0'
  else if CompareUpToken('PRELOAD',Src,ValStart,SrcPos)
  and (FDirectiveName='ASSERTIONS') then
    Values.Variables[FDirectiveName] := 'PRELOAD'
  else if (FDirectiveName='LOCALSYMBOLS') then
    // ignore "localsymbols <something>"
  else if (FDirectiveName='RANGECHECKS') then
    // ignore "rangechecks <something>"
  else if (FDirectiveName='ALIGN') then
    // set record align size
  else begin
    RaiseExceptionFmt(20170422130112,ctsInvalidFlagValueForDirective,
        [copy(Src,ValStart,SrcPos-ValStart),FDirectiveName]);
  end;
  Result:=ReadNextSwitchDirective;
end;

function TLinkScanner.LongSwitchDirectiveWithSequence(
  const ADirective: TSequenceDirective): boolean;
var ValStart: integer;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkLongSwitch;
  ReadSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and IsWordChar[Src[SrcPos]] do
    inc(SrcPos);
  if CompareUpToken('ON',Src,ValStart,SrcPos) then
    SetDirectiveValueWithSequence(ADirective, '1')
  else if CompareUpToken('OFF',Src,ValStart,SrcPos) then
    SetDirectiveValueWithSequence(ADirective, '0')
  else begin
    RaiseExceptionFmt(20170422130115,ctsInvalidFlagValueForDirective,
        [copy(Src,ValStart,SrcPos-ValStart),FDirectiveName]);
  end;
  Result:=ReadNextSwitchDirective;
end;

function TLinkScanner.MacroDirective: boolean;
var
  ValStart: LongInt;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkMacro;
  ReadSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsWordChar[Src[SrcPos]]) do
    inc(SrcPos);
  if CompareUpToken('ON',Src,ValStart,SrcPos) then
    FMacrosOn:=true
  else if CompareUpToken('OFF',Src,ValStart,SrcPos) then
    FMacrosOn:=false
  else
    RaiseExceptionFmt(20170422130118,ctsInvalidFlagValueForDirective,
        [copy(Src,ValStart,SrcPos-ValStart),FDirectiveName]);
  Result:=true;
end;

function TLinkScanner.ModeDirective: boolean;
// $MODE DEFAULT, OBJFPC, TP, FPC, GPC, DELPHI
var ValStart: integer;
  AMode: TCompilerMode;
  ModeValid: boolean;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkMode;
  ReadSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsIdentChar[Src[SrcPos]]) do
    inc(SrcPos);
  // undefine all mode macros
  for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
    Values.Undefine(CompilerModeVars[AMode]);
  // define new mode macro
  if CompareUpToken('DEFAULT',Src,ValStart,SrcPos) then begin
    // set mode to initial mode
    for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
      if FInitValues.IsDefined(CompilerModeVars[AMode]) then begin
        CompilerMode:=AMode;
        break;
      end;
  end else begin
    ModeValid:=false;
    for AMode:=Low(TCompilerMode) to High(TCompilerMode) do
      if CompareUpToken(CompilerModeNames[AMode],Src,ValStart,SrcPos) then
      begin
        CompilerMode:=AMode;
        ModeValid:=true;
        break;
      end;
    if not ModeValid then
      RaiseExceptionFmt(20170422130122,ctsInvalidMode,[copy(Src,ValStart,SrcPos-ValStart)]);
  end;
  Result:=true;
end;

function TLinkScanner.ModeSwitchDirective: boolean;
// $MODESWITCH objectivec1
// $MODESWITCH objectivec1 on|off
// $MODESWITCH systemcodepage-
var
  p, ValStart: PChar;
  ModeSwitch: TCompilerModeSwitch;
  Switches: TCompilerModeSwitches;
  Enable: Boolean;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkModeSwitch;
  ReadSpace;
  p:=@Src[SrcPos];
  ValStart:=p;
  while IsIdentChar[p^] do inc(p);
  Result:=false;
  for ModeSwitch := Succ(Low(ModeSwitch)) to High(ModeSwitch) do begin
    if CompareIdentifiers(@CompilerModeSwitchNames[ModeSwitch][1],ValStart)=0
    then begin
      Result:=true;
      Switches:=[ModeSwitch];
      Enable:=true;
      if p^='-' then
        Enable:=false
      else if IsSpaceChar[p^] then begin
        repeat
          inc(p);
        until not IsSpaceChar[p^];
        if CompareIdentifiers(p,'off')=0 then
          Enable:=false;
      end;
      case ModeSwitch of
      cmsObjectiveC2: Include(Switches,cmsObjectiveC1);
      end;
      if Enable then begin
        FCompilerModeSwitches:=FCompilerModeSwitches+Switches;
        case ModeSwitch of
        cmsDefault_unicodestring:
          begin
            Values.Variables['FPC_UNICODESTRINGS'] := '1';
            Values.Variables['UNICODE'] := '1';
          end;
        end;
      end else begin
        FCompilerModeSwitches:=FCompilerModeSwitches-Switches;
        case ModeSwitch of
        cmsDefault_unicodestring:
          begin
            Values.Undefine('FPC_UNICODESTRINGS');
            Values.Undefine('UNICODE');
          end;
        end;
      end;
      exit;
    end;
  end;
  RaiseExceptionFmt(20170422130125,ctsInvalidModeSwitch,[GetIdentifier(ValStart)]);
end;

function TLinkScanner.ThreadingDirective: boolean;
// example: {$threading on}
var
  ValStart: integer;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkThreading;
  ReadSpace;
  ValStart:=SrcPos;
  while (SrcPos<=SrcLen) and (IsWordChar[Src[SrcPos]]) do
    inc(SrcPos);
  if CompareUpToken('ON',Src,ValStart,SrcPos) then begin
    // define
    Values.Variables[MacroUseSysThrds]:='1';
  end else begin
    // undefine
    Values.Undefine(MacroUseSysThrds);
  end;
  Result:=true;
end;

function TLinkScanner.ReadNextSwitchDirective: boolean;
var DirStart, DirLen: integer;
begin
  ReadSpace;
  if (SrcPos<=SrcLen) and (Src[SrcPos]=',') then begin
    inc(SrcPos);
    DirStart:=SrcPos;
    while (SrcPos<=SrcLen) and (IsIdentChar[Src[SrcPos]]) do
      inc(SrcPos);
    DirLen:=SrcPos-DirStart;
    if DirLen>255 then DirLen:=255;
    FDirectiveName:=copy(Src,DirStart,DirLen);
    Result:=DoDirective(DirStart,DirLen);
  end else
    Result:=true;
end;

function TLinkScanner.IfdefDirective: boolean;
// {$ifdef name comment}
var VariableName: string;
begin
  inc(IfLevel);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIfdef;
  if FSkippingDirectives<>lssdNone then exit(true);
  ReadSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') and (not Values.IsDefined(VariableName)) then
    SkipTillEndifElse(lssdTillElse);
  Result:=true;
end;

function TLinkScanner.IfCDirective: boolean;
// {$ifc expression} or indirectly called by {$elifc expression}
begin
  //DebugLn(['TLinkScanner.IfCDirective  FSkippingDirectives=',ord(FSkippingDirectives),' IfLevel=',IfLevel]);
  inc(IfLevel);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIfC;
  if FSkippingDirectives<>lssdNone then exit(true);
  Result:=InternalIfDirective;
end;

function TLinkScanner.ReadIdentifier: string;
var StartPos: integer;
begin
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) then begin
    inc(SrcPos);
    while (SrcPos<=SrcLen) and (IsIdentChar[Src[SrcPos]]) do
      inc(SrcPos);
    Result:=copy(Src,StartPos,SrcPos-StartPos);
  end else
    Result:='';
end;

function TLinkScanner.ReadUpperIdentifier: string;
var StartPos: integer;
begin
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (IsIdentStartChar[Src[SrcPos]]) then begin
    inc(SrcPos);
    while (SrcPos<=SrcLen) and (IsIdentChar[Src[SrcPos]]) do
      inc(SrcPos);
    Result:=UpperCaseStr(copy(Src,StartPos,SrcPos-StartPos));
  end else
    Result:='';
end;

function TLinkScanner.IfndefDirective: boolean;
// {$ifndef name comment}
var VariableName: string;
begin
  inc(IfLevel);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIfndef;
  if FSkippingDirectives<>lssdNone then exit(true);
  ReadSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') and (Values.IsDefined(VariableName)) then
    SkipTillEndifElse(lssdTillElse);
  Result:=true;
end;

function TLinkScanner.EndifDirective: boolean;
// {$endif comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130128,ctsAwithoutB,['$ENDIF','$IF'])
  end;
  
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkEndif;
  if IfLevel<=0 then
    RaiseAWithoutB;
  dec(IfLevel);
  if (IfLevel<FSkipIfLevel) and (FSkippingDirectives<>lssdNone) then begin
    {$IFDEF ShowUpdateCleanedSrc}
    debugln(['TLinkScanner.EndifDirective end skip']);
    {$ENDIF}
    EndSkipping;
  end;
  if StoreDirectives then begin
    FDirectives[FDirectivesCount-1].Level:=IfLevel;
  end;
  Result:=true;
end;

function TLinkScanner.EndCDirective: boolean;
// {$endc comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130131,ctsAwithoutB,['$ENDC','$IFC'])
  end;

begin
  //DebugLn(['TLinkScanner.EndCDirective  FSkippingDirectives=',ord(FSkippingDirectives),' IfLevel=',IfLevel]);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkEndC;
  if IfLevel<=0 then
    RaiseAWithoutB;
  dec(IfLevel);
  if (IfLevel<FSkipIfLevel) and (FSkippingDirectives<>lssdNone) then begin
    {$IFDEF ShowUpdateCleanedSrc}
    debugln(['TLinkScanner.EndCDirective end skip']);
    {$ENDIF}
    EndSkipping;
  end;
  Result:=true;
end;

function TLinkScanner.IfEndDirective: boolean;
// {$IfEnd comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130134,ctsAwithoutB,['$IfEnd','$ElseIf'])
  end;

begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIfEnd;
  if IfLevel<=0 then
    RaiseAWithoutB;
  dec(IfLevel);
  if (IfLevel<FSkipIfLevel) and (FSkippingDirectives<>lssdNone) then begin
    {$IFDEF ShowUpdateCleanedSrc}
    debugln(['TLinkScanner.IfEndDirective end skip']);
    {$ENDIF}
    EndSkipping;
  end;
  Result:=true;
end;

function TLinkScanner.ElseDirective: boolean;
// {$else comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130138,ctsAwithoutB,['$ELSE','$IF']);
  end;

begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkElse;
  if IfLevel=0 then
    RaiseAWithoutB;
  case FSkippingDirectives of
  lssdNone:
    // last block was executed, skip all other
    SkipTillEndifElse(lssdTillEndIf);
  lssdTillElse:
    if IfLevel=FSkipIfLevel then begin
      {$IFDEF ShowUpdateCleanedSrc}
      debugln(['TLinkScanner.ElseDirective skipped front, using ELSE part']);
      {$ENDIF}
      EndSkipping;
    end;
  end;
  Result:=true;
end;

function TLinkScanner.ElseCDirective: boolean;
// {$elsec comment}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130140,ctsAwithoutB,['$ELSEC','$IFC']);
  end;

begin
  //DebugLn(['TLinkScanner.ElseCDirective FSkippingDirectives=',ord(FSkippingDirectives),' IfLevel=',IfLevel]);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkElseC;
  if IfLevel=0 then
    RaiseAWithoutB;
  case FSkippingDirectives of
  lssdNone:
    // last block was executed, skip all other
    SkipTillEndifElse(lssdTillEndIf);
  lssdTillElse:
    if IfLevel=FSkipIfLevel then begin
      {$IFDEF ShowUpdateCleanedSrc}
      debugln(['TLinkScanner.ElseCDirective skipped front, using ELSEC part']);
      {$ENDIF}
      EndSkipping;
    end;
  end;
  Result:=true;
end;

function TLinkScanner.ElseIfDirective: boolean;
// {$elseif expression}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130143,ctsAwithoutB,['$ELSEIF','$IF']);
  end;

begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkElseIf;
  if IfLevel=0 then
    RaiseAWithoutB;
  case FSkippingDirectives of
  lssdNone:
    // last block was executed, skip all other
    SkipTillEndifElse(lssdTillEndIf);
  lssdTillElse:
    if IfLevel=FSkipIfLevel then
      exit(InternalIfDirective);
  end;
  Result:=true;
end;

function TLinkScanner.ElIfCDirective: boolean;
// {$elifc expression}

  procedure RaiseAWithoutB;
  begin
    RaiseExceptionFmt(20170422130146,ctsAwithoutB,['$ELIFC','$IFC']);
  end;

begin
  //DebugLn(['TLinkScanner.ElIfCDirective  FSkippingDirectives=',ord(FSkippingDirectives),' IfLevel=',IfLevel]);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkElseC;
  if IfLevel=0 then
    RaiseAWithoutB;
  case FSkippingDirectives of
  lssdNone:
    // last block was executed, skip all other
    SkipTillEndifElse(lssdTillEndIf);
  lssdTillElse:
    if IfLevel=FSkipIfLevel then
      exit(InternalIfDirective);
  end;
  Result:=true;
end;

function TLinkScanner.DefineDirective: boolean;
// {$define name} or {$define name:=value}
var VariableName, NewValue: string;
  NamePos: LongInt;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkDefine;
  ReadSpace;
  NamePos:=SrcPos;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then begin
    ReadSpace;
    if FMacrosOn and (SrcPos<SrcLen)
    and (Src[SrcPos]=':') and (Src[SrcPos+1]='=')
    then begin
      // makro => store the value
      inc(SrcPos,2);
      ReadSpace;
      NewValue:=copy(Src,SrcPos,CommentInnerEndPos-SrcPos);
      if CompareIdentifiers(PChar(NewValue),'false')=0 then
        NewValue:='0'
      else if CompareIdentifiers(PChar(NewValue),'true')=0 then
        NewValue:='1';
      Values.Variables[VariableName]:=NewValue;
      AddMacroValue(@Src[NamePos],SrcPos,CommentInnerEndPos);
    end else begin
      // flag
      Values.Variables[VariableName]:='1';
    end;
  end;
  Result:=true;
end;

function TLinkScanner.UndefDirective: boolean;
// {$undefine name}
var VariableName: string;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkUndef;
  ReadSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then
    Values.Undefine(VariableName);
  Result:=true;
end;

function TLinkScanner.SetCDirective: boolean;
// {$setc name} or {$setc name:=value}
var VariableName, NewValue: string;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkSetC;
  ReadSpace;
  VariableName:=ReadUpperIdentifier;
  if (VariableName<>'') then begin
    ReadSpace;
    if FMacrosOn and (SrcPos<SrcLen)
    and (Src[SrcPos]=':') and (Src[SrcPos+1]='=')
    then begin
      inc(SrcPos,2);
      ReadSpace;
      NewValue:=copy(Src,SrcPos,CommentInnerEndPos-SrcPos);
      if CompareIdentifiers(PChar(NewValue),'false')=0 then
        NewValue:='0'
      else if CompareIdentifiers(PChar(NewValue),'true')=0 then
        NewValue:='1';
      Values.Variables[VariableName]:=NewValue;
    end else begin
      Values.Variables[VariableName]:='1';
    end;
  end;
  Result:=true;
end;

function TLinkScanner.IncludeDirective: boolean;
// {$i filename} or {$include filename}
// filename can be 'filename with spaces'
var
  IncFilename: string;
begin
  Result:=false;
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkInclude;
  inc(SrcPos);
  if (Src[SrcPos]='%') then begin
    UpdateCleanedSource(CommentStartPos-1);
    // insert ''
    // ToDo: insert more useful string constant: %date%, %fpcversion%
    if 2>length(FCleanedSrc)-CleanedLen then begin
      // expand cleaned source string by at least 1024
      SetLength(FCleanedSrc,length(FCleanedSrc)+1024);
    end;
    AddLink(1,nil,slkCompilerString);
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:='''';
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:='''';
    // continue after directive
    CopiedSrcPos:=CommentEndPos-1;
    AddLink(CommentEndPos,Code);
  end else begin
    IncFilename:=Trim(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
    if (IncFilename<>'') and (IncFilename[1]='''') then begin
      if (IncFilename[length(IncFilename)]='''') then
        IncFilename:=copy(IncFilename,2,length(IncFilename)-2)
      else begin
        SrcPos:=CommentInnerEndPos;
        RaiseException(20170422130149,'missing ''');
      end;
    end;
    ForcePathDelims(IncFilename);
    {$IFDEF ShowUpdateCleanedSrc}
    DebugLn('TLinkScanner.IncludeDirective A IncFilename="',IncFilename,'" UpdatePos=',DbgS(CommentEndPos-1));
    {$ENDIF}
    UpdateCleanedSource(CommentEndPos-1);
    // put old position on stack
    PushIncludeLink(CleanedLen,CommentEndPos,Code);
    // load include file
    Result:=IncludeFile(IncFilename);
    if Result then begin
      if (SrcPos<=SrcLen) then
        CommentEndPos:=SrcPos
      else
        ReturnFromIncludeFile;
    end else begin
      PopIncludeLink;
    end;
  end;
  //DebugLn('[TLinkScanner.IncludeDirective] END ',CommentEndPos,',',SrcPos,',',SrcLen);
end;

function TLinkScanner.IncludePathDirective: boolean;
// {$includepath path_addition}
var AddPath, PathDivider: string;
begin
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIncludePath;
  inc(SrcPos);
  AddPath:=Trim(copy(Src,SrcPos,CommentInnerEndPos-SrcPos));
  PathDivider:=':';
  Values.Variables[ExternalMacroStart+'INCPATH']:=
    Values.Variables[ExternalMacroStart+'INCPATH']+PathDivider+AddPath;
  Result:=true;
end;

function TLinkScanner.LoadSourceCaseLoUp(const AFilename: string;
  AllowVirtual: boolean): pointer;
var
  Path, FileNameOnly: string;
  SecondaryFileNameOnly: String;
begin
  {$IFDEF VerboseIncludeSearch}
  debugln(['TLinkScanner.LoadSourceCaseLoUp AFilename="',AFilename,'" AllowVirtual=',AllowVirtual]);
  {$ENDIF}
  Result:=nil;
  Path:=ResolveDots(ExtractFilePath(AFilename));
  if (not AllowVirtual) and (Path<>'') and (not FilenameIsAbsolute(Path)) then
    exit;
  FileNameOnly:=ExtractFilename(AFilename);
  Result:=FOnLoadSource(Self,Path+FileNameOnly,true);
  if (Result<>nil) then exit;
  SecondaryFileNameOnly:=LowerCase(FileNameOnly);
  if (SecondaryFileNameOnly<>FileNameOnly) then begin
    Result:=FOnLoadSource(Self,Path+SecondaryFileNameOnly,true);
    if (Result<>nil) then exit;
  end;
  SecondaryFileNameOnly:=UpperCaseStr(FileNameOnly);
  if (SecondaryFileNameOnly<>FileNameOnly) then begin
    Result:=FOnLoadSource(Self,Path+SecondaryFileNameOnly,true);
    if (Result<>nil) then exit;
  end;
end;

function TLinkScanner.SearchIncludeFile(AFilename: string;
  out NewCode: Pointer; var MissingIncludeFile: TMissingIncludeFile): boolean;
var
  PathStart, PathEnd: integer;
  IncludePath, CurPath: string;
  ExpFilename: string;
  HasPathDelims: Boolean;

  function SearchPath(const APath, RelFilename: string): boolean;
  begin
    Result:=false;
    if APath='' then exit;
    {$IFDEF VerboseIncludeSearch}
    DebugLn('TLinkScanner.SearchPath CurIncPath="',APath,'" / "',RelFilename,'"');
    {$ENDIF}
    ExpFilename:=AppendPathDelim(APath)+RelFilename;
    if not FilenameIsAbsolute(ExpFilename) then
      ExpFilename:=ExtractFilePath(FMainSourceFilename)+ExpFilename;
    NewCode:=LoadSourceCaseLoUp(ExpFilename);
    Result:=NewCode<>nil;
  end;
  
  procedure SetMissingIncludeFile;
  begin
    if MissingIncludeFile=nil then
      MissingIncludeFile:=TMissingIncludeFile.Create(AFilename,'');
    MissingIncludeFile.IncludePath:=IncludePath;
  end;

  function SearchCasedInIncPath(const RelFilename: string): boolean;
  begin
    if FilenameIsAbsolute(FMainSourceFilename) then begin
      // main source has absolute filename
      // search in directory of unit
      ExpFilename:=ExtractFilePath(FMainSourceFilename)+RelFilename;
      NewCode:=LoadSourceCaseLoUp(ExpFilename);
      Result:=(NewCode<>nil);
      if Result then exit;
      // search in directory of source of include directive
      if FilenameIsAbsolute(SrcFilename)
      and (CompareFilenames(SrcFilename,FMainSourceFilename)<>0) then begin
        ExpFilename:=ExtractFilePath(SrcFilename)+RelFilename;
        NewCode:=LoadSourceCaseLoUp(ExpFilename);
        Result:=(NewCode<>nil);
        if Result then exit;
      end;
    end else begin
      // main source is virtual -> allow virtual include file
      NewCode:=LoadSourceCaseLoUp(RelFilename,true);
      Result:=(NewCode<>nil);
      if Result then exit;
    end;

    // then search the include file in directories defines in fpc.cfg (by -Fi option)
    if FindIncFileInCfgCache(AFilename,ExpFilename) then
    begin
      NewCode:=LoadSourceCaseLoUp(ExpFilename);
      Result:=(NewCode<>nil);
      if Result then exit;
    end;

    // then search the include file in the include path
    if not HasPathDelims then begin
      if MissingIncludeFile=nil then
        IncludePath:=Values.Variables[ExternalMacroStart+'INCPATH']
      else
        IncludePath:=MissingIncludeFile.IncludePath;

      {$IFDEF VerboseIncludeSearch}
      DebugLn('TLinkScanner.SearchIncludeFile IncPath="',IncludePath,'"');
      {$ENDIF}
      PathStart:=1;
      PathEnd:=PathStart;
      while PathEnd<=length(IncludePath) do begin
        if IncludePath[PathEnd]=';' then begin
          if PathEnd>PathStart then begin
            CurPath:=TrimFilename(copy(IncludePath,PathStart,PathEnd-PathStart));
            Result:=SearchPath(CurPath,RelFilename);
            if Result then exit;
          end;
          PathStart:=PathEnd+1;
          PathEnd:=PathStart;
        end else
          inc(PathEnd);
      end;
      if PathEnd>PathStart then begin
        CurPath:=TrimFilename(copy(IncludePath,PathStart,PathEnd-PathStart));
        Result:=SearchPath(CurPath,RelFilename);
        if Result then exit;
      end;
    end;
    Result:=false;
  end;

begin
  {$IFDEF VerboseIncludeSearch}
  DebugLn('TLinkScanner.SearchIncludeFile Filename="',AFilename,'"');
  {$ENDIF}
  NewCode:=nil;
  IncludePath:='';

  // beware of 'dir/file.inc'
  HasPathDelims:=(System.Pos('/',AFilename)>0) or (System.Pos('\',AFilename)>0);
  if HasPathDelims then
    ForcePathDelims(AFilename);
  AFilename:=ResolveDots(AFilename);

  if not Assigned(FOnLoadSource) then begin
    NewCode:=nil;
    SetMissingIncludeFile;
    Result:=false;
    exit;
  end;

  // if include filename is absolute then load it directly
  if FilenameIsAbsolute(AFilename) then begin
    NewCode:=LoadSourceCaseLoUp(AFilename);
    Result:=(NewCode<>nil);
    if not Result then SetMissingIncludeFile;
    exit;
  end;

  // first search without touching the extension
  {$IFDEF VerboseIncludeSearch}
  debugln(['TLinkScanner.SearchIncludeFile FMainSourceFilename="',FMainSourceFilename,'" SrcFile="',SrcFilename,'" AFilename="',AFilename,'"']);
  {$ENDIF}
  if SearchCasedInIncPath(AFilename) then exit(true);

  if ExtractFileExt(AFilename)='' then begin
    // search with the default file extensions
    if SearchCasedInIncPath(AFilename+'.inc') then exit(true);
    if SearchCasedInIncPath(AFilename+'.pp') then exit(true);
    if SearchCasedInIncPath(AFilename+'.pas') then exit(true);
  end;

  SetMissingIncludeFile;
  Result:=false;
end;

function TLinkScanner.IncludeFile(const AFilename: string): boolean;
var
  NewCode: Pointer;
  MissingIncludeFile: TMissingIncludeFile;
begin
  MissingIncludeFile:=nil;
  Result:=SearchIncludeFile(AFilename,NewCode,MissingIncludeFile);
  if Result then begin
    // change source
    if Assigned(FOnIncludeCode) then
      FOnIncludeCode(FMainCode,NewCode);
    SetSource(NewCode);
    AddLink(SrcPos,Code);
  end else begin
    if MissingIncludeFile<>nil then begin
      if FMissingIncludeFiles=nil then
        FMissingIncludeFiles:=TMissingIncludeFiles.Create;
      FMissingIncludeFiles.Add(MissingIncludeFile);
    end;
    if (not IgnoreMissingIncludeFiles) then begin
      // ToDo: add an event to let application improve the error message
      RaiseExceptionFmt(20170422130152,ctsIncludeFileNotFound,[AFilename])
    end else begin
      // add a dummy link
      AddLink(SrcPos,nil,slkMissingIncludeFile);
      AddLink(SrcPos,Code);
    end;
  end;
end;

function TLinkScanner.IfDirective: boolean;
// {$if expression} or indirectly called by {$elseif expression}
begin
  inc(IfLevel);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIf;
  if FSkippingDirectives<>lssdNone then exit(true);
  Result:=InternalIfDirective;
end;

function TLinkScanner.IfOptDirective: boolean;
// {$ifopt o+} or {$ifopt o-}
var Option, c: char;
  v: String;
begin
  inc(IfLevel);
  if StoreDirectives then
    FDirectives[FDirectivesCount-1].Kind:=lsdkIfOpt;
  if FSkippingDirectives<>lssdNone then exit(true);
  Result:=true;
  inc(SrcPos);
  Option:=UpChars[Src[SrcPos]];
  if (IsWordChar[Option]) and (CompilerSwitchesNames[Option]<>'')
  then begin
    inc(SrcPos);
    if (SrcPos<=SrcLen) then begin
      c:=Src[SrcPos];
      if c in ['+','-'] then begin
        v:=Values.Variables[CompilerSwitchesNames[Option]];
        if (c='-')<>((v='0') or (v='')) then
        begin
          SkipTillEndifElse(lssdTillElse);
          exit;
        end;
      end;
    end;
  end;
end;

procedure TLinkScanner.SetIgnoreMissingIncludeFiles(const Value: boolean);
begin
  if Value then
    Include(FStates,lssIgnoreMissingIncludeFiles)
  else
    Exclude(FStates,lssIgnoreMissingIncludeFiles);
end;

procedure TLinkScanner.PushIncludeLink(ACleanedPos, ASrcPos: integer;
  ACode: Pointer);
  
  procedure RaiseIncludeCircleDetected;
  begin
    RaiseException(20170422130154,ctsIncludeCircleDetected);
  end;
  
var NewLink: PSourceLink;
  i: integer;
begin
  for i:=0 to FIncludeStack.Count-1 do
    if PSourceLink(FIncludeStack[i])^.Code=ACode then
      RaiseIncludeCircleDetected;
  NewLink:=PSourceLinkMemManager.NewPSourceLink;
  with NewLink^ do begin
    CleanedPos:=ACleanedPos;
    SrcPos:=ASrcPos;
    Code:=ACode;
  end;
  FIncludeStack.Add(NewLink);
end;

function TLinkScanner.PopIncludeLink: TSourceLink;
var PLink: PSourceLink;
begin
  PLink:=PSourceLink(FIncludeStack[FIncludeStack.Count-1]);
  Result:=PLink^;
  PSourceLinkMemManager.DisposePSourceLink(PLink);
  FIncludeStack.Delete(FIncludeStack.Count-1);
end;

function TLinkScanner.GetIncludeFileIsMissing: boolean;
begin
  Result:=(FMissingIncludeFiles<>nil);
end;

function TLinkScanner.MissingIncludeFilesNeedsUpdate: boolean;
var
  i: integer;
  MissingIncludeFile: TMissingIncludeFile;
  NewCode: Pointer;
begin
  Result:=false;
  if (not IncludeFileIsMissing) or IgnoreMissingIncludeFiles then exit;
  { last scan missed an include file (i.e. was not in searchpath)
    -> Check all missing include files again }
  for i:=0 to FMissingIncludeFiles.Count-1 do begin
    MissingIncludeFile:=FMissingIncludeFiles[i];
    if SearchIncludeFile(MissingIncludeFile.Filename,NewCode,MissingIncludeFile)
    then begin
      Result:=true;
      exit;
    end;
  end;
end;

procedure TLinkScanner.ClearMissingIncludeFiles;
begin
  FreeAndNil(FMissingIncludeFiles);
end;

procedure TLinkScanner.AddMacroValue(MacroName: PChar; ValueStart, ValueEnd: integer);
var
  i: LongInt;
  Macro: PSourceLinkMacro;
begin
  i:=IndexOfMacro(MacroName,false);
  if i<0 then begin
    // insert new macro
    i:=IndexOfMacro(MacroName,true);
    if FMacroCount=fMacroCapacity then begin
      fMacroCapacity:=fMacroCapacity*2;
      if fMacroCapacity<4 then fMacroCapacity:=4;
      ReAllocMem(FMacros,SizeOf(TSourceLinkMacro)*fMacroCapacity);
    end;
    if i<FMacroCount then
      System.Move(FMacros[i],FMacros[i+1],
                  SizeOf(TSourceLinkMacro)*(FMacroCount-i));
    FillByte(FMacros[i],SizeOf(TSourceLinkMacro),0);
    inc(FMacroCount);
  end;
  Macro:=@FMacros[i];
  Macro^.Name:=MacroName;
  Macro^.Code:=Code;
  Macro^.Src:=Src;
  Macro^.SrcFilename:=SrcFilename;
  Macro^.StartPos:=ValueStart;
  Macro^.EndPos:=ValueEnd;
  //DebugLn(['TLinkScanner.AddMacroValue ',GetIdentifier(MacroName),' ',copy(Src,ValueStart,ValueEnd-ValueStart)]);
end;

procedure TLinkScanner.ClearMacros;
var
  i: Integer;
begin
  for i:=0 to FMacroCount-1 do begin
    with FMacros[i] do begin
      //DebugLn(['TLinkScanner.ClearMacros ',GetIdentifier(Name),' ',SrcFilename]);
      Src:='';
      SrcFilename:='';
    end;
  end;
  FMacroCount:=0;
end;

function TLinkScanner.IndexOfMacro(MacroName: PChar; InsertPos: boolean): integer;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: LongInt;
begin
  l:=0;
  r:=FMacroCount-1;
  m:=0;
  cmp:=0;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareIdentifierPtrs(MacroName,FMacros[m].Name);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else begin
      Result:=m;
      exit;
    end;
  end;
  if InsertPos then begin
    if cmp>0 then inc(m);
    Result:=m;
  end else begin
    Result:=-1;
  end;
end;

procedure TLinkScanner.AddMacroSource(MacroID: integer);
var
  Macro: PSourceLinkMacro;
  OldCode: Pointer;
  OldSrc: String;
  OldSrcFilename: String;
begin
  Macro:=@FMacros[MacroID];
  //DebugLn(['TLinkScanner.AddMacroSource ID=',MacroID,' ',GetIdentifier(Macro^.Name)]);
  // update cleaned source
  UpdateCleanedSource(TokenStart-1);
  // store old code pos
  OldCode:=Code;
  OldSrc:=Src;
  OldSrcFilename:=SrcFilename;
  //DebugLn(['TLinkScanner.AddMacroSource BEFORE CleanedSrc=',dbgstr(copy(FCleanedSrc,CleanedLen-19,20))]);
  // add macro source
  AddLink(Macro^.StartPos,Macro^.Code);
  Code:=Macro^.Code;
  Src:=Macro^.Src;
  SrcLen:=length(Src);
  SrcFilename:=Macro^.SrcFilename;
  CopiedSrcPos:=Macro^.StartPos-1;
  UpdateCleanedSource(Macro^.EndPos-1);
  //DebugLn(['TLinkScanner.AddMacroSource MACRO CleanedSrc=',dbgstr(copy(FCleanedSrc,CleanedLen-19,20))]);
  // restore code pos
  Code:=OldCode;
  Src:=OldSrc;
  SrcLen:=length(Src);
  SrcFilename:=OldSrcFilename;
  CopiedSrcPos:=SrcPos-1;
  AddLink(SrcPos,Code);
  // clear token type
  TokenType:=lsttNone;
  // SrcPos was not touched and still stands behind the macro name
  //DebugLn(['TLinkScanner.AddMacroSource END Token=',copy(Src,TokenStart,SrcPos-TokenStart)]);
end;

procedure TLinkScanner.AddSkipComment(IsStart: boolean);
begin
  //DebugLn(['TLinkScanner.AddSkipComment InFront="',dbgstr(CleanedSrc,CleanedLen-12,13),'" isstart=',IsStart]);
  // insert {#3 or #3}
  if 2>length(FCleanedSrc)-CleanedLen then begin
    // expand cleaned source string by at least 1024
    SetLength(FCleanedSrc,length(FCleanedSrc)+1024);
  end;
  if IsStart then begin
    AddLink(1,nil,slkSkipStart);
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:='{';
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:=#3;
  end else begin
    if (LinkCount>0) and (FLinks[FLinkCount-1].CleanedPos=CleanedLen+1) then begin
      // last link is empty
      dec(FLinkCount);
      {$IFDEF ShowUpdateCleanedSrc}
      debugln(['TLinkScanner.AddSkipComment SkipEnd: removing empty link: ',dbgs(FLinks[FLinkCount].Kind)]);
      {$ENDIF}
      if (LinkCount>0) and (FLinks[FLinkCount-1].Kind=slkSkipStart) then begin
        // remove unneeded SkipStart
        dec(FLinkCount);
        CleanedLen:=FLinks[FLinkCount].CleanedPos;
        exit;
      end;
    end;

    AddLink(1,nil,slkSkipEnd);
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:=#3;
    inc(CleanedLen);
    FCleanedSrc[CleanedLen]:='}';
  end;
  // SrcPos was not touched and still stands at the same position
  //DebugLn(['TLinkScanner.AddSkipComment END']);
end;

function TLinkScanner.ReturnFromIncludeFile: boolean;
var OldPos: TSourceLink;
begin
  if (SrcPos-1>CopiedSrcPos) then begin
    {$IFDEF ShowUpdateCleanedSrc}
    DebugLn('TLinkScanner.ReturnFromIncludeFile A UpdatePos=',DbgS(SrcPos-1));
    {$ENDIF}
    UpdateCleanedSource(SrcPos-1);
  end;
  while SrcPos>SrcLen do begin
    Result:=FIncludeStack.Count>0;
    if not Result then exit;
    OldPos:=PopIncludeLink;
    SetSource(OldPos.Code);
    SrcPos:=OldPos.SrcPos;
    CopiedSrcPos:=SrcPos-1;
    AddLink(SrcPos,Code);
  end;
  Result:=SrcPos<=SrcLen;
end;

function TLinkScanner.ParseKeyWord(StartPos: integer;
  LastTokenType: TLSTokenType): boolean;
var
  p: PChar;
begin
  if StartPos>SrcLen then exit(false);
  p:=@Src[StartPos];
  //writeln('TLinkScanner.ParseKeyWord ',copy(Src,StartPos,WordLen));
  case UpChars[p^] of
  'E': if CompareIdentifiers(p,'END')=0 then exit(DoEndToken);
  'F': if CompareIdentifiers(p,'FINALIZATION')=0 then exit(DoFinalizationToken);
  'I':
    case UpChars[p[1]] of
    'M': if CompareIdentifiers(p,'IMPLEMENTATION')=0 then exit(DoImplementationToken);
    'N':
      case UpChars[p[2]] of
      'I': if CompareIdentifiers(p,'INITIALIZATION')=0 then exit(DoInitializationToken);
      'T': if (LastTokenType<>lsttEqual)
              and (CompareIdentifiers(p,'INTERFACE')=0) then exit(DoInterfaceToken);
      end;
    end;
  'L': if CompareIdentifiers(p,'LIBRARY')=0 then exit(DoSourceTypeToken);
  'P':
    case UpChars[p[1]] of
    'R': if CompareIdentifiers(p,'PROGRAM')=0 then exit(DoSourceTypeToken);
    'A': if CompareIdentifiers(p,'PACKAGE')=0 then exit(DoSourceTypeToken);
    end;
  'U':
    case UpChars[p[1]] of
    'N': if CompareIdentifiers(p,'UNIT')=0 then exit(DoSourceTypeToken);
    'S': if CompareIdentifiers(p,'USES')=0 then exit(DoUsesToken);
    end;
  end;
  Result:=false;
end;

procedure TLinkScanner.SkipTillEndifElse(SkippingUntil: TLSSkippingDirective);

  procedure RaiseAlreadySkipping;
  begin
    raise Exception.Create('TLinkScanner.SkipTillEndifElse inconsistency: already skipping '
      +' Old='+dbgs(ord(FSkippingDirectives))
      +' New='+dbgs(ord(SkippingUntil)));
  end;

var
  p: PChar;
begin
  if FDirectivesCount>0 then
    FDirectives[FDirectivesCount-1].State:=lsdsInactive;
  if FSkippingDirectives<>lssdNone then begin
    FSkippingDirectives:=SkippingUntil;
    exit;
  end;
  FSkippingDirectives:=SkippingUntil;

  SrcPos:=CommentEndPos;
  {$IFDEF ShowUpdateCleanedSrc}
  DebugLn('TLinkScanner.SkipTillEndifElse A UpdatePos=',DbgS(SrcPos-1),' Src=',DbgStr(Src,SrcPos-15,15)+'|'+DbgStr(Src,SrcPos,15));
  {$ENDIF}
  UpdateCleanedSource(SrcPos-1);
  AddSkipComment(true);
  AddLink(SrcPos,Code);

  // parse till $else, $elseif or $endif
  FSkipIfLevel:=IfLevel;
  if (SrcPos<=SrcLen) then begin
    p:=@Src[SrcPos];
    while true do begin
      case p^ of
      '{':
        begin
          SrcPos:=p-PChar(Src)+1;
          ReadCurlyComment;
          if (FSkippingDirectives=lssdNone) or (SrcPos>SrcLen) then break;
          p:=@Src[SrcPos];
        end;
      '/':
        if p[1]='/' then begin
          SrcPos:=p-PChar(Src)+1;
          ReadLineComment;
          if (FSkippingDirectives=lssdNone) or (SrcPos>SrcLen) then break;
          p:=@Src[SrcPos];
        end else
          inc(p);
      '(':
        if p[1]='*' then begin
          SrcPos:=p-PChar(Src)+1;
          ReadRoundComment;
          if (FSkippingDirectives=lssdNone) or (SrcPos>SrcLen) then break;
          p:=@Src[SrcPos];
        end else
          inc(p);
      '''':
        begin
          // skip string constant
          inc(p);
          while not (p^ in ['''',#0,#10,#13]) do inc(p);
          if p^='''' then
            inc(p);
        end;
      #0:
        begin
          // FPC allows that corresponding IFDEF and ENDIF are in different files
          SrcPos:=p-PChar(Src)+1;
          if (SrcPos>SrcLen) then begin
            if not ReturnFromIncludeFile then begin
              CopiedSrcPos:=SrcLen+1;
              break;
            end;
            p:=@Src[SrcPos];
          end else
            inc(p);
        end;
      else
        inc(p);
      end;
    end;
    SrcPos:=p-PChar(Src)+1;
  end else begin
    CopiedSrcPos:=SrcLen+1;
  end;
  {$IFDEF ShowUpdateCleanedSrc}
  DebugLn('TLinkScanner.SkipTillEndifElse B Continuing after: ',
    ' Src=',DbgStr(copy(Src,CommentStartPos-15,15))+'|'+DbgStr(copy(Src,CommentStartPos,15)));
  {$ENDIF}

  FSkippingDirectives:=lssdNone;
end;

procedure TLinkScanner.SetCompilerMode(const AValue: TCompilerMode);
var
  OldModeSwitches, EnabledModeSwitches,
    DisabledModeSwitches: TCompilerModeSwitches;
begin
  if FCompilerMode=AValue then exit;
  Values.Undefine(CompilerModeVars[FCompilerMode]);
  FCompilerMode:=AValue;
  OldModeSwitches:=FCompilerModeSwitches;
  FCompilerModeSwitches:=DefaultCompilerModeSwitches[CompilerMode];
  FNestedComments:=cmsNested_comment in CompilerModeSwitches;
  Values.Variables[CompilerModeVars[FCompilerMode]]:='1';

  EnabledModeSwitches:=FCompilerModeSwitches-OldModeSwitches;
  DisabledModeSwitches:=OldModeSwitches-FCompilerModeSwitches;
  if cmsDefault_unicodestring in EnabledModeSwitches then begin
    Values.Variables['FPC_UNICODESTRINGS'] := '1';
    Values.Variables['UNICODE'] := '1';
  end else if cmsDefault_unicodestring in DisabledModeSwitches then begin
    Values.Undefine('FPC_UNICODESTRINGS');
    Values.Undefine('UNICODE');
  end;
end;

procedure TLinkScanner.SetPascalCompiler(const AValue: TPascalCompiler);
begin
  if FPascalCompiler=AValue then Exit;
  FPascalCompiler:=AValue;
  case PascalCompiler of
  pcPas2js: ;
  end;
end;

procedure TLinkScanner.SetDirectiveValueWithSequence(
  ADirective: TSequenceDirective; const ADirectiveValue: string);
begin
  Values.Variables[DirectiveSequenceName[ADirective]] := ADirectiveValue;
  FDirectiveSequence.Add(ADirective, ADirectiveValue, FDirectiveCleanPos);
end;

function TLinkScanner.GetIgnoreMissingIncludeFiles: boolean;
begin
  Result:=lssIgnoreMissingIncludeFiles in FStates;
end;

function TLinkScanner.InternalIfDirective: boolean;
// {$if expression} or {$ifc expression}
// or indirectly called by {$elifc expression} or {$elseif expression}

  procedure RaiseMissingExpr;
  begin
    RaiseException(20170422130156,'missing expression');
  end;

var
  ExprResult: Boolean;
begin
  //DebugLn(['TLinkScanner.InternalIfDirective FSkippingDirectives=',ord(FSkippingDirectives),' IfLevel=',IfLevel]);
  inc(SrcPos);
  if SrcPos>SrcLen then
    RaiseMissingExpr;
  ExprResult:=Values.EvalBoolean(@Src[SrcPos],CommentInnerEndPos-SrcPos);
  Result:=true;
  //DebugLn(['TLinkScanner.InternalIfDirective ExprResult=',ExprResult]);
  if Values.ErrorPosition>=0 then begin
    inc(SrcPos,Values.ErrorPosition);
    RaiseException(20170422130200,Values.ErrorMsg)
  end else if ExprResult then begin
    // expression evaluates to true => stop skipping and parse block
    if FDirectivesCount>0 then
      FDirectives[FDirectivesCount-1].State:=lsdsActive;
    if FSkippingDirectives<>lssdNone then begin
      {$IFDEF ShowUpdateCleanedSrc}
      debugln(['TLinkScanner.InternalIfDirective skipped front, using ELIFC part']);
      {$ENDIF}
      EndSkipping;
    end;
  end else begin
    // expression evaluates to false => skip this block
    SkipTillEndifElse(lssdTillElse);
  end;
end;

procedure TLinkScanner.EndSkipping;

  procedure ErrorNotSkipping;
  begin
    debugln(['ErrorNotSkipping internal error, please report this bug']);
    CTDumpStack;
  end;

var
  Dir: PLSDirective;
begin
  if FSkippingDirectives=lssdNone then begin
    ErrorNotSkipping;
    exit;
  end;
  FSkippingDirectives:=lssdNone;
  UpdateCleanedSource(CommentStartPos-1);
  AddSkipComment(false);
  AddLink(CommentStartPos,Code);
  //debugln(['TLinkScanner.EndSkipping ',StoreDirectives]);
  if StoreDirectives then begin
    // update cleaned position of directive
    Dir:=@FDirectives[FDirectivesCount-1];
    Dir^.CleanPos:=CleanedLen+1;
    Dir^.State:=lsdsActive;
  end;
  FSkipIfLevel:=-1;
end;

function TLinkScanner.CursorToCleanPos(ACursorPos: integer; ACode: pointer;
  out ACleanPos: integer): integer;
// 0=valid CleanPos
//-1=CursorPos was skipped, CleanPos is between two links
// 1=CursorPos beyond scanned code
// ACleanPos starts at 1
type
  TLinkQuality = (qNone,qSkipped,qSkippedInFront,qDisabled,qClean);
var
  i, BestCleanPos: integer;
  Link: PSourceLink;
  BestQuality: TLinkQuality;
  LinkEnd: Integer;
  Enabled: Boolean;
begin
  Result:=1;
  ACleanPos:=0;
  if ACode=nil then exit;

  i:=0;
  BestQuality:=qNone;
  BestCleanPos:=0;
  Enabled:=true;
  while i<LinkCount do begin
    Link:=@FLinks[i];
    if Link^.Kind=slkSkipStart then
      Enabled:=false
    else if Link^.Kind=slkSkipEnd then
      Enabled:=true;
    //DebugLn(['[TLinkScanner.CursorToCleanPos] A ACursorPos=',ACursorPos,', Code=',Link^.Code=ACode,', Link^.SrcPos=',Link^.SrcPos,', Link^.CleanedPos=',Link^.CleanedPos]);
    if (Link^.Code=ACode) then begin
      // link in same code found
      if (Link^.SrcPos<=ACursorPos) then begin
        ACleanPos:=ACursorPos-Link^.SrcPos+Link^.CleanedPos;
        //DebugLn(['[TLinkScanner.CursorToCleanPos] Same code ACursorPos=',ACursorPos,', Code=',Link^.Code=ACode,', Link^.SrcPos=',Link^.SrcPos,', Link^.CleanedPos=',Link^.CleanedPos,' EndCleanPos=',Link^.CleanedPos+LinkSize(i)]);
        LinkEnd:=LinkCleanedEndPos_Inline(i);
        if ACleanPos<LinkEnd then begin
          // link covers the cursor position
          //debugln(['TLinkScanner.CursorToCleanPos Found LinkStartInSrc="',dbgstr(LinkSourceLog(i).Source,Link^.SrcPos,40),'" LinkStartInCleanSrc="',dbgstr(FCleanedSrc,Link^.CleanedPos,40),'" CursorSrc="',dbgstr(copy(LinkSourceLog(i).Source,ACursorPos-20,20)),'|',dbgstr(copy(LinkSourceLog(i).Source,ACursorPos,20)),'" CleanCursorSrc="',dbgstr(FCleanedSrc,ACleanPos-20,20),'|',dbgstr(FCleanedSrc,ACleanPos,20),'"']);
          if Enabled then begin
            exit(0);  // position in parsed code
          end else begin
            // position in disabled code
            // Note: maybe this include file was parsed a second time and the
            // position is then enabled => save position and continue search
            if BestQuality<qDisabled then begin
              BestQuality:=qDisabled;
              BestCleanPos:=ACleanPos;
            end;
          end;
        end else begin
          // link is in front of the cursor
          if BestQuality<=qSkipped then begin
            // remember last link in front
            BestQuality:=qSkipped;
            BestCleanPos:=LinkEnd;
          end;
        end;
      end else begin
        // link is behind cursor
        if BestQuality=qSkipped then begin
          // the cursor lies in between two links
          BestQuality:=qSkippedInFront;
        end;
      end;
    end;
    inc(i);
  end;
  ACleanPos:=BestCleanPos;
  if (BestQuality=qSkippedInFront) and (ACleanPos<=CleanedLen) then begin
    // cursor position lies between two links
    Result:=-1;
  end else if BestQuality=qDisabled then begin
    // cursor position lies in disabled code (in the special comment #3)
    Result:=0;
  end else
    Result:=1; // default: CursorPos beyond/outside scanned code
end;

function TLinkScanner.CleanedPosToCursor(ACleanedPos: integer; out
  ACursorPos: integer; out ACode: Pointer): boolean;

  procedure ConsistencyCheckI(i: integer);
  begin
    raise Exception.Create(
      'TLinkScanner.CleanedPosToCursor Consistency-Error '+IntToStr(i));
  end;

  procedure ConsistencyCheckStart;
  begin
    raise Exception.Create(
      'TLinkScanner.CleanedPosToCursor Consistency-Error no start link with code found');
  end;

  procedure Found(LinkID: integer);
  begin
    ACode:=FLinks[LinkID].Code;
    if ACode=nil then begin
      repeat
        dec(LinkID);
        if LinkID<0 then
          ConsistencyCheckStart;
        ACode:=FLinks[LinkID].Code;
      until ACode<>nil;
    end;
    ACursorPos:=ACleanedPos-FLinks[LinkID].CleanedPos+FLinks[LinkID].SrcPos;
  end;

var l,r,m: integer;
begin
  Result:=(ACleanedPos>=1) and (ACleanedPos<=CleanedLen);
  if Result then begin
    // ACleanedPos in Cleaned Code -> binary search through the links
    l:=0;
    r:=LinkCount-1;
    while l<=r do begin
      m:=(l+r) div 2;
      if m<LinkCount-1 then begin
        if ACleanedPos<FLinks[m].CleanedPos then
          r:=m-1
        else if ACleanedPos>=FLinks[m+1].CleanedPos then
          l:=m+1
        else begin
          Found(m);
          exit;
        end;
      end else begin
        if ACleanedPos>=FLinks[m].CleanedPos then begin
          Found(m);
          exit;
        end else
          ConsistencyCheckI(2);
      end;
    end;
    ConsistencyCheckI(1);
  end;
  ACode:=nil;
  ACursorPos:=0;
end;

function TLinkScanner.CleanedPosToStr(ACleanedPos: integer): string;
var
  p: integer;
  ACode: Pointer;
begin
  if CleanedPosToCursor(ACleanedPos,p,ACode) then begin
    Result:=TSourceLog(ACode).AbsoluteToLineColStr(p);
  end else begin
    Result:='p='+IntToStr(ACleanedPos)+',y=?,x=?';
  end;
end;

function TLinkScanner.WholeRangeIsWritable(CleanStartPos, CleanEndPos: integer;
  ErrorOnFail: boolean): boolean;
  
  procedure EditError(const AMessage: string; ACode: Pointer);
  begin
    if ErrorOnFail then
      RaiseEditException(20170422130202,AMessage,ACode,0);
  end;
  
var
  ACode: Pointer;
  LinkIndex: integer;
  CodeIsReadOnly: boolean;
begin
  Result:=false;
  if (CleanStartPos<1) or (CleanStartPos>=CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (not Assigned(FOnGetSourceStatus)) then begin
    EditError('TLinkScanner.WholeRangeIsWritable: Invalid range',nil);
    exit;
  end;
  LinkIndex:=LinkIndexAtCleanPos(CleanStartPos);
  if LinkIndex<0 then begin
    EditError('TLinkScanner.WholeRangeIsWritable: position out of scan range',nil);
    exit;
  end;
  ACode:=FLinks[LinkIndex].Code;
  CodeIsReadOnly:=false;
  FOnGetSourceStatus(Self,ACode,CodeIsReadOnly);
  if CodeIsReadOnly then begin
    EditError(ctsfileIsReadOnly, ACode);
    exit;
  end;
  repeat
    inc(LinkIndex);
    if (LinkIndex>=LinkCount) or (FLinks[LinkIndex].CleanedPos>CleanEndPos) then
    begin
      Result:=true;
      exit;
    end;
    if ACode<>FLinks[LinkIndex].Code then begin
      ACode:=FLinks[LinkIndex].Code;
      if ACode<>nil then begin
        FOnGetSourceStatus(Self,ACode,CodeIsReadOnly);
        if CodeIsReadOnly then begin
          EditError(ctsfileIsReadOnly, ACode);
          exit;
        end;
      end;
    end;
  until false;
end;

procedure TLinkScanner.FindCodeInRange(CleanStartPos, CleanEndPos: integer;
  UniqueSortedCodeList: TFPList);
var ACode: Pointer;
  LinkIndex: integer;
  Link: PSourceLink;
begin
  if (CleanStartPos<1) or (CleanStartPos>CleanEndPos)
  or (CleanEndPos>CleanedLen+1) or (UniqueSortedCodeList=nil) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanStartPos);
  if LinkIndex<0 then exit;
  ACode:=FLinks[LinkIndex].Code;
  if ACode<>nil then
    AddCodeToUniqueList(ACode,UniqueSortedCodeList);
  repeat
    inc(LinkIndex);
    if (LinkIndex>=LinkCount) then
      exit;
    Link:=@FLinks[LinkIndex];
    if (Link^.CleanedPos>CleanEndPos) then
      exit;
    if (ACode<>Link^.Code) then begin
      ACode:=Link^.Code;
      if ACode<>nil then
        AddCodeToUniqueList(ACode,UniqueSortedCodeList);
    end;
  until false;
end;

procedure TLinkScanner.DeleteRange(CleanStartPos,CleanEndPos: integer);
{ delete all code in links (=parsed code) starting with the last link
  before you call this, test with WholeRangeIsWritable

  this can do unexpected things if
    - include files are included twice
    - compiler directives like IFDEF - ENDIF are partially destroyed
    
  ToDo: keep include directives
}
var
  LinkIndex, StartPos, Len, aLinkSize: integer;
  ACode: Pointer;
begin
  if CleanStartPos<1 then CleanStartPos:=1;
  if CleanEndPos>CleanedLen then CleanEndPos:=CleanedLen+1;
  if (CleanStartPos>=CleanEndPos) or (not Assigned(FOnDeleteSource)) then exit;
  LinkIndex:=LinkIndexAtCleanPos(CleanEndPos-1);
  //debugln(['TLinkScanner.DeleteRange CleanStartPos=',CleanStartPos,' CleanEndPos=',CleanEndPos,' LinkIndex=',LinkIndex]);
  while LinkIndex>=0 do begin
    StartPos:=CleanStartPos-FLinks[LinkIndex].CleanedPos;
    if StartPos<0 then StartPos:=0;
    aLinkSize:=LinkSize(LinkIndex);
    //debugln(['TLinkScanner.DeleteRange LinkIndex=',LinkIndex,' aLinkSize=',aLinkSize,' StartPosInLink=',StartPos]);
    Len:=CleanEndPos-FLinks[LinkIndex].CleanedPos;
    if Len>aLinkSize then Len:=aLinkSize;
    dec(Len,StartPos);
    inc(StartPos,FLinks[LinkIndex].SrcPos);
    //DebugLn(['[TLinkScanner.DeleteRange] Pos=',StartPos,'-',StartPos+Len,' ',dbgstr(copy(Src,StartPos,Len))]);
    ACode:=FLinks[LinkIndex].Code;
    if ACode<>nil then
      FOnDeleteSource(Self,ACode,StartPos,Len);
    if FLinks[LinkIndex].CleanedPos<=CleanStartPos then break;
    dec(LinkIndex);
  end;
end;

procedure TLinkScanner.ActivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(true);
end;

procedure TLinkScanner.DeactivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(false);
end;

procedure TLinkScanner.RaiseExceptionFmt(id: int64; const AMessage: string;
  const Args: array of const);
begin
  RaiseException(id,Format(AMessage,args));
end;

procedure TLinkScanner.RaiseException(id: int64; const AMessage: string);
begin
  RaiseExceptionClass(id,AMessage,ELinkScannerError);
end;

procedure TLinkScanner.RaiseExceptionClass(id: int64; const AMessage: string;
  ExceptionClass: ELinkScannerErrors);
begin
  LastErrorMessage:=AMessage;
  LastErrorSrcPos:=SrcPos;
  LastErrorCode:=Code;
  LastErrorCheckedForIgnored:=false;
  LastErrorId:=id;
  LastErrorIsValid:=true;
  raise ExceptionClass.Create(Self,id,AMessage);
end;

procedure TLinkScanner.RaiseEditException(id: int64; const AMessage: string;
  ABuffer: Pointer; ABufferPos: integer);
begin
  raise ELinkScannerEditError.Create(Self,id,AMessage,ABuffer,ABufferPos);
end;

procedure TLinkScanner.RaiseConsistencyException(id: int64;
  const AMessage: string);
begin
  RaiseExceptionClass(id,AMessage,ELinkScannerConsistency);
end;

procedure TLinkScanner.ClearLastError;
begin
  LastErrorIsValid:=false;
  LastErrorCheckedForIgnored:=false;
end;

procedure TLinkScanner.RaiseLastError;
begin
  SrcPos:=LastErrorSrcPos;
  Code:=LastErrorCode;
  RaiseException(20170422130205,LastErrorMessage);
end;

procedure TLinkScanner.DoCheckAbort;
begin
  if not Assigned(OnProgress) then exit;
  if OnProgress(Self) then exit;
  // raise abort exception
  RaiseExceptionClass(20170422130207,'Abort',ELinkScannerAbort);
end;

function TLinkScanner.MainFilename: string;
begin
  if Assigned(OnGetFileName) and (FMainCode<>nil) then
    Result:=OnGetFileName(Self,FMainCode)
  else
    Result:='';
end;

{ ELinkScannerError }

constructor ELinkScannerError.Create(ASender: TLinkScanner; TheId: int64;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
  id:=TheId;
end;

{ TPSourceLinkMemManager }

procedure TPSourceLinkMemManager.FreeFirstItem;
var Link: PSourceLink;
begin
  Link:=PSourceLink(FFirstFree);
  PSourceLink(FFirstFree):=Link^.Next;
  Dispose(Link);
end;

procedure TPSourceLinkMemManager.DisposePSourceLink(Link: PSourceLink);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Link to Free list
    FillChar(Link^,SizeOf(TSourceLink),0);
    Link^.Next:=PSourceLink(FFirstFree);
    PSourceLink(FFirstFree):=Link;
    inc(FFreeCount);
  end else begin
    // free list full -> free Link
    Dispose(Link);
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

function TPSourceLinkMemManager.NewPSourceLink: PSourceLink;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PSourceLink(FFirstFree);
    PSourceLink(FFirstFree):=Result^.Next;
    Result^.Next:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new PSourceLink
    New(Result);
    FillChar(Result^,SizeOf(TSourceLink),0);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

{ TPSourceChangeStep }

procedure TPSourceChangeStepMemManager.FreeFirstItem;
var Step: PSourceChangeStep;
begin
  Step:=PSourceChangeStep(FFirstFree);
  PSourceChangeStep(FFirstFree):=Step^.Next;
  Dispose(Step);
end;

procedure TPSourceChangeStepMemManager.DisposePSourceChangeStep(
  Step: PSourceChangeStep);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Link to Free list
    FillChar(Step^,SizeOf(TSourceChangeStep),0);
    Step^.Next:=PSourceChangeStep(FFirstFree);
    PSourceChangeStep(FFirstFree):=Step;
    inc(FFreeCount);
  end else begin
    // free list full -> free Step
    Dispose(Step);
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

function TPSourceChangeStepMemManager.NewPSourceChangeStep: PSourceChangeStep;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PSourceChangeStep(FFirstFree);
    PSourceChangeStep(FFirstFree):=Result^.Next;
    Result^.Next:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new PSourceChangeStep
    New(Result);
    FillChar(Result^,SizeOf(TSourceChangeStep),0);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

{ TMissingIncludeFile }

constructor TMissingIncludeFile.Create(const AFilename, AIncludePath: string);
begin
  inherited Create;
  Filename:=AFilename;
  IncludePath:=AIncludePath;
end;

function TMissingIncludeFile.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(IncludePath)
    +MemSizeString(Filename);
end;

{ TMissingIncludeFiles }

function TMissingIncludeFiles.GetIncFile(Index: Integer): TMissingIncludeFile;
begin
  Result:=TMissingIncludeFile(Get(Index));
end;

procedure TMissingIncludeFiles.SetIncFile(Index: Integer;
  const AValue: TMissingIncludeFile);
begin
  Put(Index,AValue);
end;

procedure TMissingIncludeFiles.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Clear;
end;

procedure TMissingIncludeFiles.Delete(Index: Integer);
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

function TMissingIncludeFiles.CalcMemSize: PtrUInt;
var
  i: Integer;
begin
  Result:=PtrUInt(InstanceSize)
    +SizeOf(Pointer)*PtrUInt(Capacity);
  for i:=0 to Count-1 do
    inc(Result,Items[i].CalcMemSize);
end;


//------------------------------------------------------------------------------
procedure InternalInit;
var
  CompMode: TCompilerMode;
begin
  for CompMode:=Low(TCompilerMode) to High(TCompilerMode) do
    CompilerModeVars[CompMode]:='FPC_'+CompilerModeNames[CompMode];
  PSourceLinkMemManager:=TPSourceLinkMemManager.Create;
  PSourceChangeStepMemManager:=TPSourceChangeStepMemManager.Create;
end;

procedure InternalFinal;
begin
  PSourceChangeStepMemManager.Free;
  PSourceLinkMemManager.Free;
end;

{ ELinkScannerEditError }

constructor ELinkScannerEditError.Create(ASender: TLinkScanner; TheId: int64;
  const AMessage: string; ABuffer: Pointer; ABufferPos: integer);
begin
  inherited Create(ASender,TheId,AMessage);
  Buffer:=ABuffer;
  BufferPos:=ABufferPos;
end;

initialization
  InternalInit;
  
finalization
  InternalFinal;

end.

