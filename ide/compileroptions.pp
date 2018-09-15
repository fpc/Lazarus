{ /***************************************************************************
                      compileroptions.pp  -  Lazarus IDE unit
                      ---------------------------------------
                   Compiler options sets the switches for the project
                   file for the FPC compiler.


                   Initial Revision  : Sat May 10 23:15:32 CST 1999


 ***************************************************************************/

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

}
unit CompilerOptions;

{$mode objfpc}
{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  InterfaceBase, Forms, Controls,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, Laz2_XMLCfg, Laz2_DOM, LazUtilities, LazTracer,
  LazStringUtils,
  // CodeTools
  FileProcs, DefineTemplates, CodeToolsCfgScript, CodeToolManager,
  KeywordFuncLists, BasicCodeTools, LinkScanner,
  // IDEIntf
  ProjectIntf, MacroIntf, IDEExternToolIntf, SrcEditorIntf, CompOptsIntf,
  IDEOptionsIntf,
  // IDE
  LazarusIDEStrConsts, IDEProcs, LazConf, TransferMacros, etFPCMsgParser,
  IDECmdLine, ModeMatrixOpts, CompOptsModes, EnvironmentOpts;

const
  DefaultCompilerPath = '$(CompPath)';
type

  TCheckCompileOptionsMsgLvl = (
    ccomlHints,
    ccomlWarning,
    ccomlErrors,
    ccomlNone
    );

  TIDEBuildMacros = class;

  { TIDEBuildMacro }

  TIDEBuildMacro = class(TLazBuildMacro)
  private
    FChangeStamp: integer;
  protected
    procedure SetIdentifier(const AValue: string); override;
    procedure SetDescription(const AValue: string); override;
    procedure SetValueDescriptions(const AValue: TStrings); override;
    procedure SetValues(const AValue: TStrings); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TLazBuildMacro); override;
    function Equals(Other: TLazBuildMacro): boolean; reintroduce;
    procedure LoadFromXMLConfig(aXMLConfig: TXMLConfig; const Path: string;
                                {%H-}DoSwitchPathDelims: boolean);
    procedure SaveToXMLConfig(aXMLConfig: TXMLConfig; const Path: string;
                              {%H-}UsePathDelim: TPathDelimSwitch);
    function CreateDiff(OtherMode: TLazBuildMacro; Tool: TCompilerDiffTool = nil): boolean;
    procedure IncreaseChangeStamp;
    property ChangeStamp: integer read FChangeStamp;
  end;

  { TIDEBuildMacros
    - every package and project has this list of build macros
      every build macro has
      - a list of possible values
      - and has a default value, or an expression to define the default
        the expression can use other build macros }
  TIDEBuildMacros = class(TLazBuildMacros)
  protected
    FItems: TFPList;// list of TIDEBuildMacro
    function GetItems(Index: integer): TLazBuildMacro; override;
  public
    function Add(Identifier: string): TLazBuildMacro; override;
    procedure Clear; override;
    function Count: integer; override;
    constructor Create(TheOwner: TObject); override;
    procedure Delete(Index: integer); override;
    destructor Destroy; override;
    function IndexOfIdentifier(Identifier: string): integer; override;
    function VarWithIdentifier(Identifier: string): TIDEBuildMacro; override;
    procedure Move(OldIndex, NewIndex: integer); override;
    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean);
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch);
    function CreateDiff(OtherProperties: TLazBuildMacros;
                        Tool: TCompilerDiffTool = nil): boolean;
    procedure Assign(Source: TLazBuildMacros);
  end;

const
  DefaultConditionals =
     '// example for adding linker options on Mac OS X'+LineEnding
    +'//if TargetOS=''darwin'' then'+LineEnding
    +'//  LinkerOptions := '' -framework OpenGL'';'+LineEnding
    +LineEnding
    +'// example for adding a unit and include path on Windows'+LineEnding
    +'//if SrcOS=''win'' then begin'+LineEnding
    +'//  UnitPath += '';win'';'+LineEnding
    +'//  IncPath += '';win'';'+LineEnding
    +'//end;'
    ;

type

  { TIDECfgScriptEngine }

  TIDECfgScriptEngine = class(TCTConfigScriptEngine)
  private
    FProjValuesAvailable: boolean;
  protected
    function IsCustomFunction(FunctionName: PChar): boolean; override;
    procedure RunCustomSimpleFunction(FunctionName: PChar;
      Value: PCTCfgScriptVariable); override;
  public
    property ProjValuesAvailable: boolean read FProjValuesAvailable write FProjValuesAvailable;
  end;

type
  TInheritedCompilerOption = (
    icoNone,
    icoUnitPath,
    icoNamespaces,
    icoIncludePath,
    icoObjectPath,
    icoLibraryPath,
    icoSrcPath,
    icoLinkerOptions,
    icoCustomOptions
    );
  TInheritedCompilerOptions = set of TInheritedCompilerOption;

  TInheritedCompOptsStrings = array[TInheritedCompilerOption] of string;

const
  icoAllSearchPaths = [icoUnitPath,icoIncludePath,icoObjectPath,icoLibraryPath,
                       icoSrcPath];

type
  { TParsedCompilerOptions }

  TParsedCompilerOptString = (
    pcosNone,
    pcosBaseDir,      // the base directory for the relative paths (only auto created packages can have macros in the BaseDir)
    pcosUnitPath,     // search path for pascal units
    pcosNamespaces,   // namespaces
    pcosIncludePath,  // search path for pascal include files
    pcosObjectPath,   // search path for .o files
    pcosLibraryPath,  // search path for libraries
    pcosSrcPath,      // additional search path for pascal source files
    pcosLinkerOptions,// additional linker options
    pcosCustomOptions,// additional options
    pcosOutputDir,    // the output directory
    pcosCompilerPath, // the filename of the compiler
    pcosDebugPath,    // additional debug search path
    pcosMsgFile       // fpc message file (errore.msg)
    );
  TParsedCompilerOptStrings = set of TParsedCompilerOptString;


const
  ParsedCompilerSearchPaths = [pcosUnitPath,pcosIncludePath,pcosObjectPath,
                               pcosLibraryPath,pcosSrcPath,pcosDebugPath];
  ParsedCompilerFilenames = [pcosCompilerPath,pcosMsgFile];
  ParsedCompilerDirectories = [pcosOutputDir];
  ParsedCompilerOutDirectories = [pcosOutputDir];
  ParsedCompilerFiles =
    ParsedCompilerSearchPaths+ParsedCompilerFilenames+ParsedCompilerDirectories;

  ParsedCompilerOptsVars: array[TParsedCompilerOptString] of string = (
    '',
    '',
    'UnitPath',
    'Namespaces',
    'IncPath',
    'ObjectPath',
    'LibraryPath',
    'SrcPath',
    'LinkerOptions',
    'CustomOptions',
    'OutputDir',
    'CompilerPath',
    'DebugPath',
    'MsgFile'
    );
  ParsedCompilerOptsUsageVars: array[TParsedCompilerOptString] of string = (
    '',
    '',
    'UsageUnitPath',
    'UsageNamespaces',
    'UsageIncPath',
    'UsageObjectPath',
    'UsageLibraryPath',
    'UsageSrcPath',
    'UsageLinkerOptions',
    'UsageCustomOptions',
    '',
    '',
    'UsageDebugPath',
    ''
    );
  InheritedToParsedCompilerOption: array[TInheritedCompilerOption] of
    TParsedCompilerOptString = (
      pcosNone,
      pcosUnitPath,      // icoUnitPath,
      pcosNamespaces,    // icoNamespaces,
      pcosIncludePath,   // icoIncludePath,
      pcosObjectPath,    // icoObjectPath,
      pcosLibraryPath,   // icoLibraryPath,
      pcosSrcPath,       // icoSrcPath,
      pcosLinkerOptions, // icoLinkerOptions,
      pcosCustomOptions  // icoCustomOptions
      );

  CompilerOptionMacroNormal = 0;
  CompilerOptionMacroPlatformIndependent = 1;

type
  TLocalSubstitutionEvent = function(const s: string;
                                PlatformIndependent: boolean): string of object;

  TInheritedCompOptsParseTypesStrings =
    array[TCompilerOptionsParseType] of TInheritedCompOptsStrings;

  { TParsedCompilerOptions }

  TParsedCompilerOptions = class
  private
    FInvalidateParseOnChange: boolean;
    FOnLocalSubstitute: TLocalSubstitutionEvent;
    FOutputDirectoryOverride: string;
    FOwner: TObject;
    procedure SetOutputDirectoryOverride(const AValue: string);
  public
    // parsed
    Values: array[TParsedCompilerOptString] of TParseString;
    ParsedErrorOption: TParsedCompilerOptString;
    ParsedErrorMsg: string;
    ParsedErrorStamp: integer; // see CompilerParseStamp
    // parsed except for platform macros
    ParsedPIValues: array[TParsedCompilerOptString] of string;
    ParsedPIStamp: array[TParsedCompilerOptString] of integer; // see CompilerParseStamp
    ParsingPI: array[TParsedCompilerOptString] of boolean;
    // macro values
    InheritedMacroValues: TCTCfgScriptVariables;
    InheritedMacroValuesStamp: integer; // see BuildMacroChangeStamp
    InheritedMacroValuesParsing: boolean;
    MacroValues: TIDECfgScriptEngine;
    MacroValuesStamp: integer; // see BuildMacroChangeStamp
    MacroValuesParsing: boolean;
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    function HasParsedError: boolean;
    procedure ParsedError(Option: TParsedCompilerOptString; Msg: string);
    function GetUnparsedWithConditionals(Option: TParsedCompilerOptString): string;
    function GetParsedValue(Option: TParsedCompilerOptString;
                            WithOverrides: boolean = true): string;
    function GetParsedPIValue(Option: TParsedCompilerOptString): string;// platform independent
    procedure SetUnparsedValue(Option: TParsedCompilerOptString;
                               const NewValue: string);
    function DoParseOption(const OptionText: string;
                           Option: TParsedCompilerOptString;
                           PlatformIndependent: boolean): string;
    procedure Assign(Src: TParsedCompilerOptions);
    procedure Clear;
    procedure InvalidateAll;
    procedure InvalidateFiles;
    procedure RenameMacro(const OldName, NewName: string;
                out Changed: TParsedCompilerOptStrings); // rename macro in UnparsedValues
  public
    property Owner: TObject read FOwner;
    property OnLocalSubstitute: TLocalSubstitutionEvent read FOnLocalSubstitute
                                                       write FOnLocalSubstitute;
    property InvalidateParseOnChange: boolean read FInvalidateParseOnChange
                                              write FInvalidateParseOnChange;
    property OutputDirectoryOverride: string read FOutputDirectoryOverride
                                             write SetOutputDirectoryOverride;
  end;

  TParseStringEvent =
    function(Options: TParsedCompilerOptions;
             const UnparsedValue: string; PlatformIndependent: boolean
             ): string of object;


  { TBaseCompilerOptions }

  TCompilerCmdLineOption = (
    ccloNoLinkerOpts,  // exclude linker options
    ccloAddVerboseAll,  // add -va
    ccloDoNotAppendOutFileOption, // do not add -o option
    ccloAbsolutePaths,
    ccloNoMacroParams, // no search paths, no linker options, no custom options
    ccloAddCompilerPath
    );
  TCompilerCmdLineOptions = set of TCompilerCmdLineOption;

  { TCompilationToolOptions }

  TCompilationToolOptions = class
  private
    FChangeStamp: int64;
    FCommand: string;
    FOnChanged: TNotifyEvent;
    FOwner: TObject;
    FParsers: TStrings;
    FParsedCommandStamp: integer;
    FParsedCommand: string;
    function GetHasParser(aParserName: string): boolean;
    procedure SetCommand(const AValue: string);
    procedure SetHasParser(aParserName: string; const AValue: boolean);
    procedure SetParsers(const AValue: TStrings);
  protected
    procedure SubstituteMacros(var s: string); virtual;
  public
    constructor Create(TheOwner: TObject); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function CreateDiff(CompOpts: TCompilationToolOptions;
                        Tool: TCompilerDiffTool = nil): boolean; virtual;
    procedure Assign(Src: TCompilationToolOptions); virtual;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch); virtual;
    function Execute(const WorkingDir, ToolTitle, CompileHint: string): TModalResult;
    function CreateExtTool(const WorkingDir, ToolTitle, CompileHint: string): TAbstractExternalTool;
    property ChangeStamp: int64 read FChangeStamp;
    procedure IncreaseChangeStamp;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    function GetParsedCommand: string; // resolved macros
    function HasCommands: boolean; // true if there is something to execute
  public
    property Owner: TObject read FOwner;
    property Command: string read FCommand write SetCommand;
    property Parsers: TStrings read FParsers write SetParsers;
    property HasParser[aParserName: string]: boolean read GetHasParser write SetHasParser;
  end;
  TCompilationToolClass = class of TCompilationToolOptions;

  TCompilerMsgIdFlag = record
    MsgId: integer;
    Flag: TCompilerFlagValue;
  end;
  PCompilerMsgIdFlag = ^TCompilerMsgIdFlag;

  { TCompilerMsgIDFlagsEnumerator }

  TCompilerMsgIDFlagsEnumerator = class
  protected
    FTree: TAvlTree;
    FCurrent: TAvlTreeNode;
    function GetCurrent: PCompilerMsgIdFlag; inline;
  public
    constructor Create(Tree: TAvlTree);
    function GetEnumerator: TCompilerMsgIDFlagsEnumerator; inline;
    function MoveNext: Boolean;
    property Current: PCompilerMsgIdFlag read GetCurrent;
  end;

  { TCompilerMsgIDFlags }

  TCompilerMsgIDFlags = class(TAbstractCompilerMsgIDFlags)
  private
    FChangeStamp: int64;
    fLastSavedStamp: int64;
    fTree: TAvlTree; // tree of TCompilerMsgIdFlag
    function FindNode(MsgId: integer): TAvlTreeNode;
  protected
    function GetValues(MsgId: integer): TCompilerFlagValue; override;
    function GetModified: boolean; override;
    procedure SetModified(AValue: boolean); override;
    procedure SetValues(MsgId: integer; AValue: TCompilerFlagValue); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Equals(Obj: TObject): boolean; override;
    procedure IncreaseChangeStamp;
    function GetEnumerator: TCompilerMsgIDFlagsEnumerator;
    function GetMsgIdList(Delim: char; aValue: TCompilerFlagValue; FPCMsgFile: TFPCMsgFilePoolItem = nil): string;
    function CreateDiff(Tool: TCompilerDiffTool; Other: TCompilerMsgIDFlags): boolean;
    function Count: SizeInt; inline;
    property ChangeStamp: int64 read FChangeStamp;
  end;

type
  { TBaseCompilerOptions }

  TBaseCompilerOptions = class(TLazCompilerOptions)
  private
    FDefaultMakeOptionsFlags: TCompilerCmdLineOptions;
    fInheritedOptions: TInheritedCompOptsParseTypesStrings;
    fInheritedOptParseStamps: integer;
    FParsedOpts: TParsedCompilerOptions;
    FStorePathDelim: TPathDelimSwitch;
    FOtherDefines: TStrings; // list of user selectable defines for custom options
    FFPCMsgFile: TFPCMsgFilePoolItem;

    // other tools
    fExecuteBefore: TCompilationToolOptions;
    fExecuteAfter: TCompilationToolOptions;
    FCreateMakefileOnBuild: boolean;

    procedure OnItemChanged(Sender: TObject);
    procedure SetCreateMakefileOnBuild(AValue: boolean);
  protected
    function GetCompilerPath: String; override;
    function GetBaseDirectory: string;
    function GetCustomOptions: string; override;
    function GetDebugPath: string; override;
    function GetIncludePaths: String; override;
    function GetLibraryPaths: String; override;
    function GetNamespaces: String; override;
    function GetObjectPath: string; override;
    function GetSrcPath: string; override;
    function GetUnitOutputDir: string; override;
    function GetUnitPaths: String; override;
    function GetExecuteBeforeCommand: string; override;
    function GetExecuteAfterCommand: string; override;
    procedure SetBaseDirectory(AValue: string);
    procedure SetCompilerPath(const AValue: String); override;
    procedure SetConditionals(AValue: string); override;
    procedure SetCustomOptions(const AValue: string); override;
    procedure SetIncludePaths(const AValue: String); override;
    procedure SetLibraryPaths(const AValue: String); override;
    procedure SetLinkerOptions(const AValue: String); override;
    procedure SetNamespaces(const AValue: String); override;
    procedure SetUnitPaths(const AValue: String); override;
    procedure SetUnitOutputDir(const AValue: string); override;
    procedure SetObjectPath(const AValue: string); override;
    procedure SetSrcPath(const AValue: string); override;
    procedure SetDebugPath(const AValue: string); override;
    procedure SetTargetCPU(const AValue: string); override;
    procedure SetTargetProc(const AValue: string); override;
    procedure SetTargetOS(const AValue: string); override;
    procedure SetTargetFileExt(const AValue: String); override;
    procedure SetTargetFilename(const AValue: String); override;
    procedure SetExecuteBeforeCommand(const ACommand: string); override;
    procedure SetExecuteAfterCommand(const ACommand: string); override;
  protected
    function GetModified: boolean; override;
    procedure SetModified(const AValue: boolean); override;
    procedure ClearInheritedOptions;
    procedure SetDefaultMakeOptionsFlags(const AValue: TCompilerCmdLineOptions);
  public
    constructor Create(const AOwner: TObject); override;
    constructor Create(const AOwner: TObject;
                       const AToolClass: TCompilationToolClass);
    destructor Destroy; override;
    procedure Clear; virtual;
    class function GetInstance: TAbstractIDEOptions; override;
    class function GetGroupCaption: string; override;

    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string); virtual;
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string); virtual;

    function LoadFromFile(AFilename: string): TModalResult;
    function SaveToFile(AFilename: string): TModalResult;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(CompOpts: TBaseCompilerOptions): boolean; virtual;
    procedure CreateDiffAsText(CompOpts: TBaseCompilerOptions; Diff: TStrings);
    function CreateDiff(CompOpts: TBaseCompilerOptions;
                        Tool: TCompilerDiffTool = nil): boolean; virtual;// true if differ

    procedure SetAlternativeCompile(const Command: string; ScanFPCMsgs: boolean); override;

    function MakeOptionsString(Flags: TCompilerCmdLineOptions): String; virtual;
    function GetSyntaxOptionsString(Kind: TPascalCompiler): string; virtual;
    function CreatePPUFilename(const SourceFileName: string): string; override;
    function CreateTargetFilename: string; virtual;
    function GetTargetFileExt: string; virtual;
    function GetTargetFilePrefix: string; virtual;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList // list of TAdditionalCompilerOptions
      ); virtual;
    function GetOwnerName: string; virtual;
    function GetInheritedOption(Option: TInheritedCompilerOption;
                                RelativeToBaseDir: boolean;
                                Parsed: TCompilerOptionsParseType = coptParsed
                                ): string; virtual;
    function GetDefaultMainSourceFileName: string; virtual;
    function CanBeDefaulForProject: boolean; virtual;
    function NeedsLinkerOpts: boolean;
    function HasCommands: boolean; // true if there is at least one commad to execute
    function HasCompilerCommand: boolean; virtual;
    function GetEffectiveTargetOS: string; override;
    function GetEffectiveTargetCPU: string; override;
    function GetEffectiveLCLWidgetType: string; override;
    function GetUnitPath(RelativeToBaseDir: boolean;
                         Parsed: TCompilerOptionsParseType = coptParsed;
                         WithBaseDir: boolean = true): string; override;
    function GetNamespacesParsed(Parsed: TCompilerOptionsParseType = coptParsed): string; override;
    function GetIncludePath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed;
                            WithBaseDir: boolean = true): string; override;
    function GetSrcPath(RelativeToBaseDir: boolean;
                        Parsed: TCompilerOptionsParseType = coptParsed;
                        WithBaseDir: boolean = true): string; override;
    function GetDebugPath(RelativeToBaseDir: boolean;
                          Parsed: TCompilerOptionsParseType = coptParsed;
                          WithBaseDir: boolean = true): string; override;
    function GetLibraryPath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed;
                            WithBaseDir: boolean = true): string; override;
    function GetUnitOutputDirectory(RelativeToBaseDir: boolean): string; override;
    function GetUnitOutPath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed): string;
    function GetObjectPath(RelativeToBaseDir: boolean;
                           Parsed: TCompilerOptionsParseType = coptParsed;
                           WithBaseDir: boolean = true): string; override;
    function GetPath(Option: TParsedCompilerOptString;
                     InheritedOption: TInheritedCompilerOption;
                     RelativeToBaseDir: boolean;
                     Parsed: TCompilerOptionsParseType;
                     WithBaseDir: boolean): string;
    function GetParsedPath(Option: TParsedCompilerOptString;
                           InheritedOption: TInheritedCompilerOption;
                           RelativeToBaseDir: boolean;
                           AddBaseDir: boolean = false): string;
    function GetParsedPIPath(Option: TParsedCompilerOptString;
                           InheritedOption: TInheritedCompilerOption;
                           RelativeToBaseDir: boolean): string;
    function GetUnparsedPath(Option: TParsedCompilerOptString;
                             InheritedOption: TInheritedCompilerOption;
                             RelativeToBaseDir: boolean): string;
    function ShortenPath(const SearchPath: string;
                         MakeAlwaysRelative: boolean): string;
    function GetCustomOptions(Parsed: TCompilerOptionsParseType): string;
    function TrimCustomOptions(o: string): string; override;
    function GetOptionsForCTDefines: string;
    // rename macro in paths and options, not in BuildMacros, not in dependencies
    procedure RenameMacro(const OldName, NewName: string;
              ChangeConditionals: boolean); virtual;
    procedure MergeToIncludePaths(const AddSearchPath: string);
    procedure MergeToLibraryPaths(const AddSearchPath: string);
    procedure MergeToNamespaces(const AddNamespaces: string);
    procedure MergeToUnitPaths(const AddSearchPath: string);
    procedure MergeToObjectPath(const AddSearchPath: string);
    procedure MergeToSrcPath(const AddSearchPath: string);
    procedure MergeToDebugPath(const AddSearchPath: string);
    procedure RemoveFromUnitPaths(const RemSearchPath: string);
    // compiler message types by id
    function IDEMessageFlags: TCompilerMsgIDFlags; inline;
  public
    // not stored properties
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
    property BaseDirectory: string read GetBaseDirectory write SetBaseDirectory;
    property DefaultMakeOptionsFlags: TCompilerCmdLineOptions
                 read FDefaultMakeOptionsFlags write SetDefaultMakeOptionsFlags;

    // stored properties
    property StorePathDelim: TPathDelimSwitch read FStorePathDelim write FStorePathDelim;
    property OtherDefines: TStrings read FOtherDefines;

    // compilation
    property ExecuteBefore: TCompilationToolOptions read fExecuteBefore;
    property ExecuteAfter: TCompilationToolOptions read fExecuteAfter;
    property CreateMakefileOnBuild: boolean read FCreateMakefileOnBuild
                                            write SetCreateMakefileOnBuild;
  end;

  TBaseCompilerOptionsClass = class of TBaseCompilerOptions;

  { TAdditionalCompilerOptions

    Additional Compiler options are used by packages to define, what a project
    or a package or the IDE needs to use the package.
  }
  TAdditionalCompilerOptions = class
  private
    fOwner: TObject;
    FParsedOpts: TParsedCompilerOptions;
  protected
    function GetBaseDirectory: string;
    function GetCustomOptions: string; virtual;
    function GetIncludePath: string; virtual;
    function GetLibraryPath: string; virtual;
    function GetLinkerOptions: string; virtual;
    function GetNamespaces: string; virtual;
    function GetObjectPath: string; virtual;
    function GetSrcPath: string; virtual;
    function GetUnitPath: string; virtual;
    procedure SetBaseDirectory(const AValue: string); virtual;
    procedure SetCustomOptions(const AValue: string); virtual;
    procedure SetIncludePath(const AValue: string); virtual;
    procedure SetLibraryPath(const AValue: string); virtual;
    procedure SetLinkerOptions(const AValue: string); virtual;
    procedure SetNamespaces(const AValue: string); virtual;
    procedure SetObjectPath(const AValue: string); virtual;
    procedure SetSrcPath(const AValue: string); virtual;
    procedure SetUnitPath(const AValue: string); virtual;
  public
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
    procedure AssignOptions(Source: TObject); virtual;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                AdjustPathDelims: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch);
    function GetOwnerName: string; virtual;
    function GetOption(AnOption: TInheritedCompilerOption): string;
    function GetBaseCompilerOptions: TBaseCompilerOptions; virtual;
  public
    property Owner: TObject read fOwner;
    property UnitPath: string read GetUnitPath write SetUnitPath;
    property Namespaces: string read GetNamespaces write SetNamespaces;
    property IncludePath: string read GetIncludePath write SetIncludePath;
    property SrcPath: string read GetSrcPath write SetSrcPath;
    property ObjectPath: string read GetObjectPath write SetObjectPath;
    property LibraryPath: string read GetLibraryPath write SetLibraryPath;
    property LinkerOptions: string read GetLinkerOptions write SetLinkerOptions;
    property CustomOptions: string read GetCustomOptions write SetCustomOptions;
    property BaseDirectory: string read GetBaseDirectory write SetBaseDirectory;
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
  end;


  { TCompilerOptions }

  TCompilerOptions = TBaseCompilerOptions;

const
  CompileReasonNames: array[TCompileReason] of string = (
    'Compile',
    'Build',
    'Run'
    );

var
  OnParseString: TParseStringEvent = nil;

function EnumToStr(opt: TParsedCompilerOptString): string; overload;
function ParseString(Options: TParsedCompilerOptions;
                     const UnparsedValue: string;
                     PlatformIndependent: boolean): string;
function GetMakefileMacroValue(const MacroName: string): string;
function TargetNeedsFPCOptionCG(TargetOS, TargetCPU: string): boolean;

procedure GatherInheritedOptions(AddOptionsList: TFPList;
  Parsed: TCompilerOptionsParseType;
  var InheritedOptionStrings: TInheritedCompOptsStrings);
function InheritedOptionsToCompilerParameters(
  var InheritedOptionStrings: TInheritedCompOptsStrings;
  Flags: TCompilerCmdLineOptions): string;
function MergeLinkerOptions(const OldOptions, AddOptions: string): string;
function MergeCustomOptions(const OldOptions, AddOptions: string): string;
function ConvertSearchPathToCmdLine(const switch, paths: String): String;
function ConvertOptionsToCmdLine(const Switch, OptionStr: string): string;

type
  TGetBuildMacroValues = function(Options: TBaseCompilerOptions;
             IncludeSelf: boolean): TCTCfgScriptVariables of object;
  TOnAppendCustomOptions = procedure(Sender: TObject;
             var CustomOptions: string; Types: TBuildMatrixGroupTypes) of object;
  TOnGetOutputDirectoryOverride = procedure(Sender: TObject;
             var OutDir: string; Types: TBuildMatrixGroupTypes) of object;
var
  GetBuildMacroValues: TGetBuildMacroValues = nil; // set by TPkgManager, do not change or free the variables
  OnAppendCustomOption: TOnAppendCustomOptions = nil; // set by MainBuildBoss
  OnGetOutputDirectoryOverride: TOnGetOutputDirectoryOverride = nil; // set by MainBuildBoss

function LoadXMLCompileReasons(const AConfig: TXMLConfig;
  const APath: String; const DefaultReasons: TCompileReasons): TCompileReasons;
procedure SaveXMLCompileReasons(const AConfig: TXMLConfig; const APath: String;
  const AFlags, DefaultFlags: TCompileReasons);

function EnumToStr(Flag: TCompilerFlagValue): string; overload;
function CompareCompMsgIdFlag(Data1, Data2: Pointer): integer;

var
  TestCompilerOptions: TNotifyEvent = nil;

implementation

const
  CompilerOptionsVersion = 11;
  // 11 Debugging/DebugInfoType/Value
  // 7  TargetProcessor/Value
  // 6  SyntaxMode/Value

function EnumToStr(opt: TParsedCompilerOptString): string;
begin
  Result:='';
  WriteStr(Result, opt);
end;

function ParseString(Options: TParsedCompilerOptions;
  const UnparsedValue: string; PlatformIndependent: boolean): string;
begin
  Result:=OnParseString(Options,UnparsedValue,PlatformIndependent);
end;

function GetMakefileMacroValue(const MacroName: string): string;
begin
  if SysUtils.CompareText('TargetCPU',MacroName)=0 then
    Result:='%(CPU_TARGET)'
  else if SysUtils.CompareText('TargetOS',MacroName)=0 then
    Result:='%(OS_TARGET)'
  else if SysUtils.CompareText('LCLWidgetType',MacroName)=0 then
    Result:='%(LCL_PLATFORM)'
  else
    Result:='';
end;

function TargetNeedsFPCOptionCG(TargetOS, TargetCPU: string): boolean;
begin
  Result:= (TargetCPU='x86_64')
    and ((TargetOS='linux') or (TargetOS='freebsd') or (TargetOS='netbsd')
      or (TargetOS='openbsd') or (TargetOS='solaris'));
end;

procedure GatherInheritedOptions(AddOptionsList: TFPList;
  Parsed: TCompilerOptionsParseType;
  var InheritedOptionStrings: TInheritedCompOptsStrings);
var
  i: Integer;
  AddOptions: TAdditionalCompilerOptions;
  o: TInheritedCompilerOption;
  UnparsedOption: String;
  CurOptions: String;
begin
  if AddOptionsList<>nil then begin
    for i:=0 to AddOptionsList.Count-1 do begin
      AddOptions:=TAdditionalCompilerOptions(AddOptionsList[i]);
      if (not (AddOptions is TAdditionalCompilerOptions)) then continue;

      case Parsed of
      coptParsed:
        begin
          // unit search path
          InheritedOptionStrings[icoUnitPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoUnitPath],
                            AddOptions.ParsedOpts.GetParsedValue(pcosUnitPath));
          // namespaces
          InheritedOptionStrings[icoNamespaces]:=
            MergeSearchPaths(InheritedOptionStrings[icoNamespaces],
                            AddOptions.ParsedOpts.GetParsedValue(pcosNamespaces));
          // include search path
          InheritedOptionStrings[icoIncludePath]:=
            MergeSearchPaths(InheritedOptionStrings[icoIncludePath],
                         AddOptions.ParsedOpts.GetParsedValue(pcosIncludePath));
          // src search path
          InheritedOptionStrings[icoSrcPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoSrcPath],
                             AddOptions.ParsedOpts.GetParsedValue(pcosSrcPath));
          // object search path
          InheritedOptionStrings[icoObjectPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoObjectPath],
                          AddOptions.ParsedOpts.GetParsedValue(pcosObjectPath));
          // library search path
          InheritedOptionStrings[icoLibraryPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoLibraryPath],
                         AddOptions.ParsedOpts.GetParsedValue(pcosLibraryPath));
          // linker options
          InheritedOptionStrings[icoLinkerOptions]:=
            MergeLinkerOptions(InheritedOptionStrings[icoLinkerOptions],
                       AddOptions.ParsedOpts.GetParsedValue(pcosLinkerOptions));
          // custom options
          InheritedOptionStrings[icoCustomOptions]:=
            MergeCustomOptions(InheritedOptionStrings[icoCustomOptions],
                       AddOptions.ParsedOpts.GetParsedValue(pcosCustomOptions));
        end;

      coptParsedPlatformIndependent:
        begin
          // unit search path
          InheritedOptionStrings[icoUnitPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoUnitPath],
                          AddOptions.ParsedOpts.GetParsedPIValue(pcosUnitPath));
          // namespaces
          InheritedOptionStrings[icoNamespaces]:=
            MergeSearchPaths(InheritedOptionStrings[icoNamespaces],
                          AddOptions.ParsedOpts.GetParsedPIValue(pcosNamespaces));
          // include search path
          InheritedOptionStrings[icoIncludePath]:=
            MergeSearchPaths(InheritedOptionStrings[icoIncludePath],
                       AddOptions.ParsedOpts.GetParsedPIValue(pcosIncludePath));
          // src search path
          InheritedOptionStrings[icoSrcPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoSrcPath],
                           AddOptions.ParsedOpts.GetParsedPIValue(pcosSrcPath));
          // object search path
          InheritedOptionStrings[icoObjectPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoObjectPath],
                        AddOptions.ParsedOpts.GetParsedPIValue(pcosObjectPath));
          // library search path
          InheritedOptionStrings[icoLibraryPath]:=
            MergeSearchPaths(InheritedOptionStrings[icoLibraryPath],
                       AddOptions.ParsedOpts.GetParsedPIValue(pcosLibraryPath));
          // linker options
          InheritedOptionStrings[icoLinkerOptions]:=
            MergeLinkerOptions(InheritedOptionStrings[icoLinkerOptions],
                     AddOptions.ParsedOpts.GetParsedPIValue(pcosLinkerOptions));
          // custom options
          InheritedOptionStrings[icoCustomOptions]:=
            MergeCustomOptions(InheritedOptionStrings[icoCustomOptions],
                     AddOptions.ParsedOpts.GetParsedPIValue(pcosCustomOptions));
        end;

      coptUnparsed:
        for o:=Low(TInheritedCompilerOption) to High(TInheritedCompilerOption)
        do begin
          UnparsedOption:=AddOptions.GetOption(o);
          if UnparsedOption<>'' then begin

            CurOptions:=InheritedOptionStrings[o];
            case o of
            icoNone: ;
            icoUnitPath,icoNamespaces,icoIncludePath,icoSrcPath,icoObjectPath,
            icoLibraryPath:
              CurOptions:=MergeWithDelimiter(CurOptions,UnparsedOption,';');
            icoLinkerOptions,icoCustomOptions:
              CurOptions:=MergeWithDelimiter(CurOptions,UnparsedOption,' ');
            else
              RaiseGDBException('GatherInheritedOptions');
            end;
            InheritedOptionStrings[o]:=CurOptions;
          end;
        end;
      end;
    end;
  end;
end;

function InheritedOptionsToCompilerParameters(
  var InheritedOptionStrings: TInheritedCompOptsStrings;
  Flags: TCompilerCmdLineOptions): string;
var
  CurLinkerOpts: String;
  CurIncludePath: String;
  CurLibraryPath: String;
  CurObjectPath: String;
  CurUnitPath: String;
  CurCustomOptions, CurNamespaces: String;
begin
  Result:='';

  // inherited Linker options
  if (not (ccloNoLinkerOpts in Flags)) then begin
    CurLinkerOpts:=InheritedOptionStrings[icoLinkerOptions];
    if CurLinkerOpts<>'' then
      Result := Result + ' ' + ConvertOptionsToCmdLine('-k', CurLinkerOpts);
  end;

  // include path
  CurIncludePath:=InheritedOptionStrings[icoIncludePath];
  if (CurIncludePath <> '') then
    Result := Result + ' ' + ConvertSearchPathToCmdLine('-Fi', CurIncludePath);

  // library path
  if (not (ccloNoLinkerOpts in Flags)) then begin
    CurLibraryPath:=InheritedOptionStrings[icoLibraryPath];
    if (CurLibraryPath <> '') then
      Result := Result + ' ' + ConvertSearchPathToCmdLine('-Fl', CurLibraryPath);
  end;

  // namespaces
  CurNamespaces:=InheritedOptionStrings[icoNamespaces];
  if CurNamespaces <> '' then
    Result := Result + ' -FN'+CurNamespaces;

  // object path
  CurObjectPath:=InheritedOptionStrings[icoObjectPath];
  if (CurObjectPath <> '') then
    Result := Result + ' ' + ConvertSearchPathToCmdLine('-Fo', CurObjectPath);

  // unit path
  CurUnitPath:=InheritedOptionStrings[icoUnitPath];
  // always add the current directory to the unit path, so that the compiler
  // checks for changed files in the directory
  CurUnitPath:=CurUnitPath+';.';
  Result := Result + ' ' + ConvertSearchPathToCmdLine('-Fu', CurUnitPath);

  // custom options
  CurCustomOptions:=InheritedOptionStrings[icoCustomOptions];
  if CurCustomOptions<>'' then
    Result := Result + ' ' +  SpecialCharsToSpaces(CurCustomOptions,true);
end;

function MergeLinkerOptions(const OldOptions, AddOptions: string): string;
begin
  Result:=MergeCustomOptions(OldOptions,AddOptions);
end;

function MergeCustomOptions(const OldOptions, AddOptions: string): string;
begin
  Result:=OldOptions;
  if AddOptions='' then exit;
  if (OldOptions<>'') and (OldOptions[length(OldOptions)]<>' ')
  and (AddOptions[1]<>' ') then
    Result+=' ';
  Result+=AddOptions;
end;

function ConvertSearchPathToCmdLine(
  const Switch, Paths: String): String;
var
  StartPos: Integer;
  l: Integer;
  EndPos: LongInt;
begin
  if Switch='' then
    RaiseGDBException('ConvertSearchPathToCmdLine no Switch');
  Result := '';
  if (Paths = '') then exit;

  l:=length(Paths);
  StartPos:=1;
  while StartPos<=l do begin
    while (StartPos<=l) and (Paths[StartPos]=' ') do inc(StartPos);
    EndPos:=StartPos;
    while (EndPos<=l) and (Paths[EndPos]<>';') do inc(EndPos);
    if StartPos<EndPos then begin
      if Result<>'' then
        Result:=Result+' ';
      Result:=Result
           +PrepareCmdLineOption(Switch + copy(Paths,StartPos,EndPos-StartPos));
    end;
    StartPos:=EndPos+1;
  end;
end;

function ConvertOptionsToCmdLine(const Switch,
  OptionStr: string): string;
var Startpos, EndPos: integer;
  p: Integer;
begin
  Result:='';
  StartPos:=1;
  while StartPos<=length(OptionStr) do begin
    while (StartPos<=length(OptionStr)) and (OptionStr[StartPos]<=' ') do
      inc(StartPos);
    EndPos:=StartPos;
    while (EndPos<=length(OptionStr)) and (OptionStr[EndPos]>' ') do begin
      if OptionStr[EndPos] in ['"',''''] then begin
        p:=EndPos;
        inc(EndPos);
        while (EndPos<=length(OptionStr)) and (OptionStr[EndPos]<>OptionStr[p]) do
          inc(EndPos);
      end;
      inc(EndPos);
    end;
    if EndPos>StartPos then begin
      Result:=Result+' '+Switch+copy(OptionStr,StartPos,EndPos-StartPos);
    end;
    StartPos:=EndPos;
  end;
end;

function LoadXMLCompileReasons(const AConfig: TXMLConfig; const APath: String;
  const DefaultReasons: TCompileReasons): TCompileReasons;
begin
  Result := [];
  if AConfig.GetValue(APath+'Compile',crCompile in DefaultReasons)
  then Include(Result, crCompile);
  if AConfig.GetValue(APath+'Build',crBuild in DefaultReasons)
  then Include(Result, crBuild);
  if AConfig.GetValue(APath+'Run',crRun in DefaultReasons)
  then Include(Result, crRun);
end;

procedure SaveXMLCompileReasons(const AConfig: TXMLConfig; const APath: String;
  const AFlags, DefaultFlags: TCompileReasons);
begin
  AConfig.SetDeleteValue(APath+'Compile', crCompile in AFlags, crCompile in DefaultFlags);
  AConfig.SetDeleteValue(APath+'Build', crBuild in AFlags, crBuild in DefaultFlags);
  AConfig.SetDeleteValue(APath+'Run', crRun in AFlags, crRun in DefaultFlags);
end;

function EnumToStr(Flag: TCompilerFlagValue): string;
begin
  case Flag of
  cfvHide: Result:='Hide';
  cfvShow: Result:='Show';
  else Result:='Default';
  end;
end;

function CompareCompMsgIdFlag(Data1, Data2: Pointer): integer;
var
  Flag1: PCompilerMsgIdFlag absolute Data1;
  Flag2: PCompilerMsgIdFlag absolute Data2;
begin
  if Flag1^.MsgId<Flag2^.MsgId then
    Result:=1
  else if Flag1^.MsgId>Flag2^.MsgId then
    Result:=-1
  else
    Result:=0;
end;

{ TCompilerMsgIDFlagsEnumerator }

function TCompilerMsgIDFlagsEnumerator.GetCurrent: PCompilerMsgIdFlag;
begin
  Result:=PCompilerMsgIdFlag(FCurrent.Data);
end;

constructor TCompilerMsgIDFlagsEnumerator.Create(Tree: TAvlTree);
begin
  FTree:=Tree;
end;

function TCompilerMsgIDFlagsEnumerator.
  GetEnumerator: TCompilerMsgIDFlagsEnumerator;
begin
  Result:=Self;
end;

function TCompilerMsgIDFlagsEnumerator.MoveNext: Boolean;
begin
  if FCurrent<>nil then
    FCurrent:=FCurrent.Successor
  else
    FCurrent:=FTree.FindLowest;
  Result:=FCurrent<>nil;
end;

{ TIDECfgScriptEngine }

function TIDECfgScriptEngine.IsCustomFunction(FunctionName: PChar): boolean;
begin
  case UpChars[FunctionName^] of
  'G':
    if (CompareIdentifiers(FunctionName,'GetIDEValue')=0)
    or (CompareIdentifiers(FunctionName,'GetEnv')=0)
    or (ProjValuesAvailable and (CompareIdentifiers(FunctionName,'GetProjValue')=0))
    then exit(true);
  end;
  Result:=false;
end;

procedure TIDECfgScriptEngine.RunCustomSimpleFunction(FunctionName: PChar;
  Value: PCTCfgScriptVariable);
var
  VarName: String;
  s: String;
begin
  case UpChars[FunctionName^] of
  'G':
    if (CompareIdentifiers(FunctionName,'GetIDEValue')=0) then
    begin
      VarName:=GetCTCSVariableAsString(Value);
      if CompareIdentifiers(PChar(VarName),'OS')=0 then
        SetCTCSVariableAsString(Value,GetCompiledTargetOS)
      else if CompareIdentifiers(PChar(VarName),'CPU')=0 then
        SetCTCSVariableAsString(Value,GetCompiledTargetCPU)
      else if CompareIdentifiers(PChar(VarName),'SrcOS')=0 then
        SetCTCSVariableAsString(Value,GetDefaultSrcOSForTargetOS(GetCompiledTargetOS))
      else if CompareIdentifiers(PChar(VarName),'SrcOS2')=0 then
        SetCTCSVariableAsString(Value,GetDefaultSrcOS2ForTargetOS(GetCompiledTargetOS))
      else if CompareIdentifiers(PChar(VarName),'LCLWidgetType')=0 then
        SetCTCSVariableAsString(Value,GetLCLWidgetTypeName)
      else
        ClearCTCSVariable(Value);
    end else if (CompareIdentifiers(FunctionName,'GetEnv')=0) then
    begin
      VarName:=GetCTCSVariableAsString(Value);
      SetCTCSVariableAsString(Value,GetEnvironmentVariableUTF8(VarName));
    end else if ProjValuesAvailable
    and (CompareIdentifiers(FunctionName,'GetProjValue')=0) then
    begin
      VarName:=GetCTCSVariableAsString(Value);
      if CompareIdentifiers(PChar(VarName),'FPC_FULLVERSION')=0 then
      begin
        s:='$(FPC_FULLVERSION)';
        GlobalMacroList.SubstituteStr(s);
        SetCTCSVariableAsNumber(Value,StrToIntDef(s,0));
      end;
    end;
  end;
end;


{ TBaseCompilerOptions }

// inline
function TBaseCompilerOptions.IDEMessageFlags: TCompilerMsgIDFlags;
begin
  Result:=TCompilerMsgIDFlags(MessageFlags);
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions Constructor
------------------------------------------------------------------------------}
constructor TBaseCompilerOptions.Create(const AOwner: TObject;
  const AToolClass: TCompilationToolClass);
begin
  inherited Create(AOwner);
  FParsedOpts := TParsedCompilerOptions.Create(Self);
  FOtherDefines := TStringList.Create;
  FExecuteBefore := AToolClass.Create(Self);
  FExecuteBefore.OnChanged := @OnItemChanged;
  FExecuteAfter := AToolClass.Create(Self);
  fExecuteAfter.OnChanged := @OnItemChanged;
  fBuildMacros := TIDEBuildMacros.Create(Self);
  fMessageFlags:=TCompilerMsgIDFlags.Create;
  Clear;
end;

constructor TBaseCompilerOptions.Create(const AOwner: TObject);
begin
  Create(AOwner, TCompilationToolOptions);
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions Destructor
------------------------------------------------------------------------------}
destructor TBaseCompilerOptions.Destroy;
begin
  if (FPCMsgFilePool<>nil) and (FFPCMsgFile<>nil) then
    FPCMsgFilePool.UnloadFile(FFPCMsgFile);
  FreeAndNil(fMessageFlags);
  FreeAndNil(fBuildMacros);
  FreeThenNil(fExecuteBefore);
  FreeThenNil(fExecuteAfter);
  FreeThenNil(FOtherDefines);
  FreeThenNil(FParsedOpts);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions LoadFromFile
------------------------------------------------------------------------------}
function TBaseCompilerOptions.LoadFromFile(AFilename: string): TModalResult;
var
  XMLConfig: TXMLConfig;
begin
  Result:=mrCancel;
  try
    XMLConfig := TXMLConfig.Create(AFilename);
    try
      LoadFromXMLConfig(XMLConfig,'CompilerOptions');
      Result:=mrOk;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TBaseCompilerOptions.LoadFromFile '+Classname+' '+AFilename+' '+E.Message);
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TBaseCompilerOptions.SetIncludePaths(const AValue: String);
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SetIncludePaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if IncludePath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosIncludePath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetIncludePaths ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetCompilerPath(const AValue: String);
begin
  if CompilerPath=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosCompilerPath,AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetCompilerPath ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetConditionals(AValue: string);
begin
  if FConditionals=AValue then exit;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetConditionals ']);
  debugln('old:"',dbgstr(FConditionals),'"');
  debugln('new:"',dbgstr(AValue),'"');
  {$ENDIF}
  FConditionals:=AValue;
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseBuildMacroChangeStamp;
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetDefaultMakeOptionsFlags(
  const AValue: TCompilerCmdLineOptions);
begin
  if FDefaultMakeOptionsFlags=AValue then exit;
  FDefaultMakeOptionsFlags:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetDefaultMakeOptionsFlags ']);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetSrcPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if SrcPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosSrcPath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetSrcPath ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetDebugPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if DebugPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosDebugPath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetDebugPath ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetCPU(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=GetFPCTargetCPU(AValue);
  if fTargetCPU=NewValue then exit;
  fTargetCPU:=NewValue;
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseBuildMacroChangeStamp;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetTargetCPU ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetProc(const AValue: string);
begin
  if fTargetProc=AValue then exit;
  fTargetProc:=AValue;
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseBuildMacroChangeStamp;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetTargetProc ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetOS(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=GetFPCTargetOS(AValue);
  if fTargetOS=NewValue then exit;
  fTargetOS:=NewValue;
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseBuildMacroChangeStamp;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetTargetOS ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetFileExt(const AValue: String);
begin
  if fTargetFileExt=AValue then exit;
  fTargetFileExt:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetTargetFileExt ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetFilename(const AValue: String);
begin
  if fTargetFilename=AValue then exit;
  fTargetFilename:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetTargetFilename ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

function TBaseCompilerOptions.GetModified: boolean;
begin
  Result:=(inherited GetModified) or MessageFlags.Modified;
end;

procedure TBaseCompilerOptions.OnItemChanged(Sender: TObject);
begin
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.OnItemChanged ',DbgSName(Sender)]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetCreateMakefileOnBuild(AValue: boolean);
begin
  if FCreateMakefileOnBuild=AValue then Exit;
  FCreateMakefileOnBuild:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetCreateMakefileOnBuild ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

function TBaseCompilerOptions.GetCompilerPath: String;
begin
  Result:=ParsedOpts.Values[pcosCompilerPath].UnparsedValue;
end;

function TBaseCompilerOptions.GetBaseDirectory: string;
begin
  Result:=ParsedOpts.Values[pcosBaseDir].UnparsedValue;
end;

function TBaseCompilerOptions.GetCustomOptions: string;
begin
  Result:=ParsedOpts.Values[pcosCustomOptions].UnparsedValue;
end;

function TBaseCompilerOptions.GetDebugPath: string;
begin
  Result:=ParsedOpts.Values[pcosDebugPath].UnparsedValue;
end;

function TBaseCompilerOptions.GetIncludePaths: String;
begin
  Result:=ParsedOpts.Values[pcosIncludePath].UnparsedValue;
end;

function TBaseCompilerOptions.GetLibraryPaths: String;
begin
  Result:=ParsedOpts.Values[pcosLibraryPath].UnparsedValue;
end;

function TBaseCompilerOptions.GetNamespaces: String;
begin
  Result:=ParsedOpts.Values[pcosNamespaces].UnparsedValue;
end;

function TBaseCompilerOptions.GetObjectPath: string;
begin
  Result:=ParsedOpts.Values[pcosObjectPath].UnparsedValue;
end;

function TBaseCompilerOptions.GetSrcPath: string;
begin
  Result:=ParsedOpts.Values[pcosSrcPath].UnparsedValue;
end;

function TBaseCompilerOptions.GetUnitOutputDir: string;
begin
  Result:=ParsedOpts.Values[pcosOutputDir].UnparsedValue;
end;

function TBaseCompilerOptions.GetUnitPaths: String;
begin
  Result:=ParsedOpts.Values[pcosUnitPath].UnparsedValue;
end;

procedure TBaseCompilerOptions.SetBaseDirectory(AValue: string);
begin
  if BaseDirectory=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosBaseDir,AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetBaseDirectory ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetCustomOptions(const AValue: string);
var
  NewValue: String;
begin
  // Keep line breaks for formatting in options dialog
  NewValue:=Trim(AValue);
  if CustomOptions=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosCustomOptions,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetCustomOptions ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetLibraryPaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if Libraries=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosLibraryPath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetLibraryPaths ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetLinkerOptions(const AValue: String);
begin
  if LinkerOptions=AValue then exit;
  fLinkerOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLinkerOptions,AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetLinkerOptions ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetNamespaces(const AValue: String);
begin
  if Namespaces=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosNamespaces,AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetNamespaces ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetUnitPaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if OtherUnitFiles=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosUnitPath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetUnitPaths ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetUnitOutputDir(const AValue: string);
begin
  if UnitOutputDirectory=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosOutputDir,AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetUnitOutputDir ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetObjectPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if ObjectPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosObjectPath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetObjectPath ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions LoadTheCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
var
  FileVersion: Integer;
  PathDelimChange: boolean;
  p: String;

  function f(const Filename: string): string;
  begin
    Result:=SwitchPathDelims(Filename,PathDelimChange);
  end;

  function sp(const SearchPath: string): string;
  begin
    Result:=SwitchPathDelims(SearchPath,PathDelimChange);
    Result:=MinimizeSearchPath(Result);
  end;

  procedure ReadSmaller;
  begin
    if FileVersion<2 then begin
      if aXMLConfig.GetValue(p+'Generate/Value', 1)<>1 then
        SmallerCode:=true;
    end else if FileVersion<8 then begin
      if aXMLConfig.GetValue(p+'Generate/Value','')='Smaller' then
        SmallerCode:=true;
    end else
      SmallerCode:=aXMLConfig.GetValue(p+'SmallerCode/Value',false);
  end;

  procedure ReadSmartLinkUnit;
  begin
    if FileVersion<3 then
      SmartLinkUnit := aXMLConfig.GetValue(p+'UnitStyle/Value', 1)=2
    else
      SmartLinkUnit := aXMLConfig.GetValue(p+'SmartLinkUnit/Value', false);
  end;

  procedure ReadLinkSmart;
  begin
    if FileVersion<3 then
      LinkSmart := aXMLConfig.GetValue(p+'LinkStyle/Value', 1)=3
    else
      LinkSmart := aXMLConfig.GetValue(p+'LinkSmart/Value', false);
  end;

  procedure ReadListOfMessageFlags(aPath: string; aValue: TCompilerFlagValue);
  var
    dNode: TDOMNode;
    i: Integer;
    Attr: TDOMNode;
    aName: DOMString;
    MsgId: Integer;
  begin
    dNode:=aXMLConfig.FindNode(aPath,false);
    if dNode<>nil then begin
      for i:=0 to dNode.Attributes.Length-1 do begin
        Attr:=dNode.Attributes.Item[i];
        aName:=Attr.NodeName;
        //debugln(['ReadListOfMessageFlags Attr=',aName,'=',Attr.NodeValue]);
        if LeftStr(aName,3)<>'idx' then continue;
        Delete(aName,1,3);
        MsgId:=StrToIntDef(aName,0);
        if MsgId<=0 then continue;
        if Attr.NodeValue<>'True' then continue;
        MessageFlags[MsgId]:=aValue;
      end;
    end;
  end;

var
  b: boolean;
  dit: TCompilerDbgSymbolType;
  i, Cnt: Integer;
  s: String;
begin
  { Load the compiler options from the XML file }
  p:=Path;
  FileVersion:=aXMLConfig.GetValue(p+'Version/Value', 0);
  StorePathDelim:=CheckPathDelim(aXMLConfig.GetValue(p+'PathDelim/Value', '/'),PathDelimChange);

  { Target }
  p:=Path+'Target/';
  TargetFileExt := f(aXMLConfig.GetValue(p+'FileExt', ''));
  TargetFilename := f(aXMLConfig.GetValue(p+'Filename/Value', ''));
  TargetFilenameApplyConventions := aXMLConfig.GetValue(p+'Filename/ApplyConventions', true);

  { SearchPaths }
  p:=Path+'SearchPaths/';
  IncludePath := sp(aXMLConfig.GetValue(p+'IncludeFiles/Value', ''));
  Libraries := sp(aXMLConfig.GetValue(p+'Libraries/Value', ''));
  OtherUnitFiles := sp(aXMLConfig.GetValue(p+'OtherUnitFiles/Value', ''));
  UnitOutputDirectory := sp(aXMLConfig.GetValue(p+'UnitOutputDirectory/Value', ''));
  ObjectPath := sp(aXMLConfig.GetValue(p+'ObjectPath/Value', ''));
  SrcPath := sp(aXMLConfig.GetValue(p+'SrcPath/Value', ''));

  { Conditionals }
  FConditionals:=ConvertLineEndings(UTF8Trim(
    aXMLConfig.GetValue(Path+'Conditionals/Value',DefaultConditionals),[]));
  TIDEBuildMacros(fBuildMacros).LoadFromXMLConfig(aXMLConfig,
                                       Path+'BuildMacros/',PathDelimChange);

  { Parsing }
  p:=Path+'Parsing/';
  AssemblerStyle := aXMLConfig.GetValue(p+'Style/Value', 0);

  { Syntax Options }
  if FileVersion>=5 then
    p:=Path+'Parsing/SyntaxOptions/'
  else
    p:=Path+'SymantecChecking/';
  if FileVersion<6 then begin
    if aXMLConfig.GetValue(p+'D2Extensions/Value', true) then
      FSyntaxMode:='ObjFPC';
    if aXMLConfig.GetValue(p+'TPCompatible/Value', false) then
      FSyntaxMode:='TP';
    if aXMLConfig.GetValue(p+'DelphiCompat/Value', false) then
      FSyntaxMode:='Delphi';
    if aXMLConfig.GetValue(p+'GPCCompat/Value', false) then
      FSyntaxMode:='GPC';
  end else begin
    FSyntaxMode:=aXMLConfig.GetValue(p+'SyntaxMode/Value', '');
    if FSyntaxMode='' then
      FSyntaxMode:='ObjFPC';
  end;
  CStyleOperators := aXMLConfig.GetValue(p+'CStyleOperator/Value', true);
  IncludeAssertionCode := aXMLConfig.GetValue(p+'IncludeAssertionCode/Value', false);
  AllowLabel := aXMLConfig.GetValue(p+'AllowLabel/Value', true);
  CPPInline := aXMLConfig.GetValue(p+'CPPInline/Value', true);
  CStyleMacros := aXMLConfig.GetValue(p+'CStyleMacros/Value', false);
  InitConstructor := aXMLConfig.GetValue(p+'InitConstructor/Value', false);
  StaticKeyword := aXMLConfig.GetValue(p+'StaticKeyword/Value', false);
  UseAnsiStrings := aXMLConfig.GetValue(p+'UseAnsiStrings/Value', FileVersion>=9);

  { CodeGeneration }
  p:=Path+'CodeGeneration/';
  ReadSmartLinkUnit;
  RelocatableUnit := aXMLConfig.GetValue(p+'RelocatableUnit/Value', false);
  IOChecks := aXMLConfig.GetValue(p+'Checks/IOChecks/Value', false);
  RangeChecks := aXMLConfig.GetValue(p+'Checks/RangeChecks/Value', false);
  OverflowChecks := aXMLConfig.GetValue(p+'Checks/OverflowChecks/Value', false);
  StackChecks := aXMLConfig.GetValue(p+'Checks/StackChecks/Value', false);
  EmulatedFloatOpcodes := aXMLConfig.GetValue(p+'EmulateFloatingPointOpCodes/Value', false);
  HeapSize := aXMLConfig.GetValue(p+'HeapSize/Value', 0);
  StackSize := aXMLConfig.GetValue(p+'StackSize/Value', 0);
  VerifyObjMethodCall := aXMLConfig.GetValue(p+'VerifyObjMethodCallValidity/Value', false);
  if FileVersion<7 then begin
    i:=aXMLConfig.GetValue(p+'TargetProcessor/Value', 0);
    case i of
    1: TargetProcessor:='PENTIUM';
    2: TargetProcessor:='PENTIUM2';
    3: TargetProcessor:='PENTIUM3';
    end;
  end else
    TargetProcessor := aXMLConfig.GetValue(p+'TargetProcessor/Value', '');
  TargetCPU := aXMLConfig.GetValue(p+'TargetCPU/Value', '');
  TargetOS := aXMLConfig.GetValue(p+'TargetOS/Value', '');
  OptimizationLevel := aXMLConfig.GetValue(p+'Optimizations/OptimizationLevel/Value', 1);
  VariablesInRegisters := aXMLConfig.GetValue(p+'Optimizations/VariablesInRegisters/Value', false);
  UncertainOptimizations := aXMLConfig.GetValue(p+'Optimizations/UncertainOptimizations/Value', false);
  ReadSmaller;

  { Linking }
  p:=Path+'Linking/';
  GenerateDebugInfo := aXMLConfig.GetValue(p+'Debugging/GenerateDebugInfo/Value', FileVersion >= 11); // Default = True, since version 11 (was False before)
  UseLineInfoUnit := aXMLConfig.GetValue(p+'Debugging/UseLineInfoUnit/Value', true);
  UseHeaptrc := aXMLConfig.GetValue(p+'Debugging/UseHeaptrc/Value', false);
  TrashVariables := aXMLConfig.GetValue(p+'Debugging/TrashVariables/Value', false);
  UseValgrind := aXMLConfig.GetValue(p+'Debugging/UseValgrind/Value', false);

  if (FileVersion < 11) and (aXMLConfig.GetValue(p+'Debugging/DebugInfoType/Value', '') = '') then begin
    // upgrading old setting
    DebugInfoType := dsAuto;
    if GenerateDebugInfo then
      DebugInfoType := dsStabs;
    if UseLineInfoUnit or UseHeaptrc or UseValgrind then
      GenerateDebugInfo := True; // LineInfo implies debug info
    b := aXMLConfig.GetValue(p+'Debugging/GenerateDwarf/Value', false);
    if b then begin
      GenerateDebugInfo := True;    // The old setting implied this
      DebugInfoType := dsDwarf2Set; // explicit dwarf, upgrade to +set
    end;
  end
  else begin
    try
      ReadStr(aXMLConfig.GetValue(p+'Debugging/DebugInfoType/Value', 'dsAuto'), dit);
      DebugInfoType := dit;
    except
      DebugInfoType := dsAuto;
    end;
  end;

  GenGProfCode := aXMLConfig.GetValue(p+'Debugging/GenGProfCode/Value', false);
  StripSymbols := aXMLConfig.GetValue(p+'Debugging/StripSymbols/Value', false);
  UseExternalDbgSyms := aXMLConfig.GetValue(p+'Debugging/UseExternalDbgSyms/Value', false);
  ReadLinkSmart;
  PassLinkerOptions := aXMLConfig.GetValue(p+'Options/PassLinkerOptions/Value', false);
  LinkerOptions := LineBreaksToSystemLineBreaks(
                f(aXMLConfig.GetValue(p+'Options/LinkerOptions/Value', '')));
  Win32GraphicApp := aXMLConfig.GetValue(p+'Options/Win32/GraphicApplication/Value', false);
  ExecutableType := CompilationExecutableTypeNameToType(
                    aXMLConfig.GetValue(p+'Options/ExecutableType/Value',''));
  //DebugLn('TBaseCompilerOptions.LoadFromXMLConfig ',CompilationExecutableTypeNames[ExecutableType]);

  { Messages }
  p:=Path+'Other/';
  fShowErrors := aXMLConfig.GetValue(p+'Verbosity/ShowErrors/Value', true);
  ShowWarn := aXMLConfig.GetValue(p+'Verbosity/ShowWarn/Value', true);
  ShowNotes := aXMLConfig.GetValue(p+'Verbosity/ShowNotes/Value', true);
  ShowHints := aXMLConfig.GetValue(p+'Verbosity/ShowHints/Value', true);
  fShowGenInfo := aXMLConfig.GetValue(p+'Verbosity/ShowGenInfo/Value', true);
  ShowLineNum := aXMLConfig.GetValue(p+'Verbosity/ShoLineNum/Value', false);
  ShowAll := aXMLConfig.GetValue(p+'Verbosity/ShowAll/Value', false);
  ShowDebugInfo := aXMLConfig.GetValue(p+'Verbosity/ShowDebugInfo/Value', false);
  ShowUsedFiles := aXMLConfig.GetValue(p+'Verbosity/ShowUsedFiles/Value', false);
  ShowTriedFiles := aXMLConfig.GetValue(p+'Verbosity/ShowTriedFiles/Value', false);
  ShowCompProc := aXMLConfig.GetValue(p+'Verbosity/ShowCompProc/Value', false);
  ShowCond := aXMLConfig.GetValue(p+'Verbosity/ShowCond/Value', false);
  ShowExecInfo := aXMLConfig.GetValue(p+'Verbosity/ShowExecInfo/Value', false);
  fShowSummary := aXMLConfig.GetValue(p+'Verbosity/ShowSummary/Value', false);
  ShowHintsForUnusedUnitsInMainSrc := aXMLConfig.GetValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', false);
  ShowHintsForSenderNotUsed := aXMLConfig.GetValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', false);
  WriteFPCLogo := aXMLConfig.GetValue(p+'WriteFPCLogo/Value', true);
  StopAfterErrCount := aXMLConfig.GetValue(p+'ConfigFile/StopAfterErrCount/Value', 1);

  ReadListOfMessageFlags(p+'CompilerMessages/IgnoredMessages',cfvHide);
  ReadListOfMessageFlags(p+'CompilerMessages/NonIgnoredMessages',cfvShow);

  { Other }
  p:=Path+'Other/';
  DontUseConfigFile := aXMLConfig.GetValue(p+'ConfigFile/DontUseConfigFile/Value', false);
  if FileVersion<=3 then
    CustomConfigFile := aXMLConfig.GetValue(p+'ConfigFile/AdditionalConfigFile/Value', false)
  else
    CustomConfigFile := aXMLConfig.GetValue(p+'ConfigFile/CustomConfigFile/Value', false);
  ConfigFilePath := f(aXMLConfig.GetValue(p+'ConfigFile/ConfigFilePath/Value', 'extrafpc.cfg'));
  CustomOptions := LineBreaksToSystemLineBreaks(aXMLConfig.GetValue(p+'CustomOptions/Value', ''));
  UseCommentsInCustomOptions := aXMLConfig.GetValue(p+'ConfigFile/UseCommentsInCustomOptions/Value', false);

  FOtherDefines.Clear;
  Cnt := aXMLConfig.GetValue(p+'OtherDefines/Count', 0);
  for i := 0 to Cnt-1 do
  begin
    s := aXMLConfig.GetValue(p+'OtherDefines/Define'+IntToStr(i)+'/Value', '');
    if s <> '' then
      FOtherDefines.Add(s);
  end;

  { Compilation }
  CompilerPath := f(aXMLConfig.GetValue(p+'CompilerPath/Value',DefaultCompilerPath));

  ExecuteBefore.LoadFromXMLConfig(aXMLConfig,p+'ExecuteBefore/',PathDelimChange);
  ExecuteAfter.LoadFromXMLConfig(aXMLConfig,p+'ExecuteAfter/',PathDelimChange);
  CreateMakefileOnBuild:=aXMLConfig.GetValue(p+'CreateMakefileOnBuild/Value',false);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SaveToFile
------------------------------------------------------------------------------}
function TBaseCompilerOptions.SaveToFile(AFilename: string): TModalResult;
var
  aXMLConfig: TXMLConfig;
begin
  Result:=mrCancel;
  try
    aXMLConfig := TXMLConfig.Create(AFilename);
    try
      SaveToXMLConfig(aXMLConfig,'CompilerOptions');
      Modified:=false;
      Result:=mrOk;
    finally
      aXMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TBaseCompilerOptions.SaveToFile '+Classname+' '+AFilename+' '+E.Message);
    end;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SaveTheCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SaveToXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
var
  UsePathDelim: TPathDelimSwitch;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure WriteListOfMessageFlags(aPath: string; aValue: TCompilerFlagValue);
  var
    Flag: PCompilerMsgIdFlag;
  begin
    for Flag in IDEMessageFlags do
      if Flag^.Flag=aValue then begin
        //debugln(['WriteListOfMessageFlags aPath=',aPath,' Flag.MsgId=',Flag^.MsgId]);
        aXMLConfig.SetValue(aPath+'/idx'+IntToStr(Flag^.MsgId), true);
      end;
  end;

var
  P, s: string;
  i: Integer;
begin
  { Save the compiler options to the XML file }
  p:=Path;
  UsePathDelim:=StorePathDelim;
  aXMLConfig.SetValue(p+'Version/Value', CompilerOptionsVersion);
  aXMLConfig.SetDeleteValue(p+'PathDelim/Value',
                                   PathDelimSwitchToDelim[UsePathDelim], '/');

  { Target }
  p:=Path+'Target/';
  aXMLConfig.SetDeleteValue(p+'FileExt', f(TargetFileExt),'');
  aXMLConfig.SetDeleteValue(p+'Filename/Value', f(TargetFilename),'');
  aXMLConfig.SetDeleteValue(p+'Filename/ApplyConventions', TargetFilenameApplyConventions,true);

  { SearchPaths }
  p:=Path+'SearchPaths/';
  aXMLConfig.SetDeleteValue(p+'IncludeFiles/Value', f(IncludePath),'');
  aXMLConfig.SetDeleteValue(p+'Libraries/Value', f(Libraries),'');
  aXMLConfig.SetDeleteValue(p+'OtherUnitFiles/Value', f(OtherUnitFiles),'');
  aXMLConfig.SetDeleteValue(p+'UnitOutputDirectory/Value', f(UnitOutputDirectory),'');
  aXMLConfig.SetDeleteValue(p+'ObjectPath/Value', f(ObjectPath),'');
  aXMLConfig.SetDeleteValue(p+'SrcPath/Value', f(SrcPath),'');

  { Conditionals }
  s:=Conditionals;
  if CompareTextIgnoringSpace(s,DefaultConditionals,true)=0 then
    s:='';
  aXMLConfig.SetDeleteValue(Path+'Conditionals/Value',s,'');
  TIDEBuildMacros(fBuildMacros).SaveToXMLConfig(aXMLConfig,
                                              Path+'BuildMacros/',UsePathDelim);

  { Parsing }
  p:=Path+'Parsing/';
  aXMLConfig.SetDeleteValue(p+'Style/Value', AssemblerStyle,0);

  { Syntax Options }
  p:=Path+'Parsing/SyntaxOptions/';
  aXMLConfig.SetDeleteValue(p+'SyntaxMode/Value', SyntaxMode,'ObjFPC');
  aXMLConfig.SetDeleteValue(p+'CStyleOperator/Value', CStyleOperators,true);
  aXMLConfig.SetDeleteValue(p+'IncludeAssertionCode/Value', IncludeAssertionCode,false);
  aXMLConfig.SetDeleteValue(p+'AllowLabel/Value', AllowLabel,true);
  aXMLConfig.SetDeleteValue(p+'CPPInline/Value', CPPInline,true);
  aXMLConfig.SetDeleteValue(p+'CStyleMacros/Value', CStyleMacros,false);
  aXMLConfig.SetDeleteValue(p+'InitConstructor/Value', InitConstructor,false);
  aXMLConfig.SetDeleteValue(p+'StaticKeyword/Value', StaticKeyword,false);
  aXMLConfig.SetDeleteValue(p+'UseAnsiStrings/Value', UseAnsiStrings,true);

  { CodeGeneration }
  p:=Path+'CodeGeneration/';
  aXMLConfig.SetDeleteValue(p+'SmartLinkUnit/Value', SmartLinkUnit,false);
  aXMLConfig.SetDeleteValue(p+'RelocatableUnit/Value', RelocatableUnit,false);
  aXMLConfig.SetDeleteValue(p+'Checks/IOChecks/Value', IOChecks,false);
  aXMLConfig.SetDeleteValue(p+'Checks/RangeChecks/Value', RangeChecks,false);
  aXMLConfig.SetDeleteValue(p+'Checks/OverflowChecks/Value', OverflowChecks,false);
  aXMLConfig.SetDeleteValue(p+'Checks/StackChecks/Value', StackChecks,false);
  aXMLConfig.SetDeleteValue(p+'EmulateFloatingPointOpCodes/Value', EmulatedFloatOpcodes,false);
  aXMLConfig.SetDeleteValue(p+'HeapSize/Value', HeapSize,0);
  aXMLConfig.SetDeleteValue(p+'StackSize/Value', StackSize,0);
  aXMLConfig.SetDeleteValue(p+'VerifyObjMethodCallValidity/Value', VerifyObjMethodCall,false);
  aXMLConfig.SetDeleteValue(p+'TargetProcessor/Value', TargetProcessor,'');
  aXMLConfig.SetDeleteValue(p+'TargetCPU/Value', TargetCPU,'');
  aXMLConfig.SetDeleteValue(p+'TargetOS/Value', TargetOS,'');
  aXMLConfig.SetDeleteValue(p+'Optimizations/OptimizationLevel/Value', OptimizationLevel,1);
  aXMLConfig.SetDeleteValue(p+'Optimizations/VariablesInRegisters/Value', VariablesInRegisters,false);
  aXMLConfig.SetDeleteValue(p+'Optimizations/UncertainOptimizations/Value', UncertainOptimizations,false);
  aXMLConfig.SetDeleteValue(p+'SmallerCode/Value', SmallerCode, false);

  { Linking }
  p:=Path+'Linking/';
  aXMLConfig.SetDeleteValue(p+'Debugging/GenerateDebugInfo/Value', GenerateDebugInfo, True); // Default = True, since version 11 (was False before)
  s:='';
  WriteStr(s, DebugInfoType);
  aXMLConfig.SetDeleteValue(p+'Debugging/DebugInfoType/Value', s, 'dsAuto');
  aXMLConfig.DeletePath(p+'Debugging/GenerateDwarf'); // old deprecated setting
  aXMLConfig.SetDeleteValue(p+'Debugging/UseLineInfoUnit/Value', UseLineInfoUnit,true);
  aXMLConfig.SetDeleteValue(p+'Debugging/UseHeaptrc/Value', UseHeaptrc,false);
  aXMLConfig.SetDeleteValue(p+'Debugging/TrashVariables/Value', TrashVariables,false);
  aXMLConfig.SetDeleteValue(p+'Debugging/UseValgrind/Value', UseValgrind,false);
  aXMLConfig.SetDeleteValue(p+'Debugging/GenGProfCode/Value', GenGProfCode,false);
  aXMLConfig.SetDeleteValue(p+'Debugging/StripSymbols/Value', StripSymbols,false);
  aXMLConfig.SetDeleteValue(p+'Debugging/UseExternalDbgSyms/Value', UseExternalDbgSyms,false);
  aXMLConfig.SetDeleteValue(p+'LinkSmart/Value', LinkSmart,false);
  aXMLConfig.SetDeleteValue(p+'Options/PassLinkerOptions/Value', PassLinkerOptions,false);
  aXMLConfig.SetDeleteValue(p+'Options/LinkerOptions/Value',
                               f(LineBreaksToSystemLineBreaks(LinkerOptions)),'');
  aXMLConfig.SetDeleteValue(p+'Options/Win32/GraphicApplication/Value', Win32GraphicApp,false);
  aXMLConfig.SetDeleteValue(p+'Options/ExecutableType/Value',
                                 CompilationExecutableTypeNames[ExecutableType],
                                 CompilationExecutableTypeNames[cetProgram]);
  //DebugLn('TBaseCompilerOptions.SaveCompilerOptions ',CompilationExecutableTypeNames[ExecutableType]);

  { Messages }
  p:=Path+'Other/';
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowErrors/Value', fShowErrors,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowWarn/Value', ShowWarn,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowNotes/Value', ShowNotes,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHints/Value', ShowHints,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowGenInfo/Value', fShowGenInfo,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShoLineNum/Value', ShowLineNum,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowAll/Value', ShowAll,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowDebugInfo/Value', ShowDebugInfo,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowUsedFiles/Value', ShowUsedFiles,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowTriedFiles/Value', ShowTriedFiles,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowCompProc/Value', ShowCompProc,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowCond/Value', ShowCond,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowExecInfo/Value', ShowExecInfo,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowSummary/Value', fShowSummary,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', ShowHintsForUnusedUnitsInMainSrc,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', ShowHintsForSenderNotUsed,false);
  aXMLConfig.SetDeleteValue(p+'WriteFPCLogo/Value', WriteFPCLogo,true);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/StopAfterErrCount/Value', StopAfterErrCount,1);

  WriteListOfMessageFlags(p+'CompilerMessages/IgnoredMessages',cfvHide);
  WriteListOfMessageFlags(p+'CompilerMessages/NonIgnoredMessages',cfvShow);

  { Other }
  p:=Path+'Other/';
  aXMLConfig.SetDeleteValue(p+'ConfigFile/DontUseConfigFile/Value', DontUseConfigFile,false);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/CustomConfigFile/Value', CustomConfigFile,false);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/ConfigFilePath/Value', f(ConfigFilePath),'extrafpc.cfg');
  aXMLConfig.SetDeleteValue(p+'CustomOptions/Value',
                            LineBreaksToSystemLineBreaks(CustomOptions),''); // do not touch / \ characters
  aXMLConfig.SetDeleteValue(p+'ConfigFile/UseCommentsInCustomOptions/Value', UseCommentsInCustomOptions,false);

  for i:=0 to FOtherDefines.Count-1 do
    aXMLConfig.SetDeleteValue(p+'OtherDefines/Define'+IntToStr(i)+'/Value',
                              FOtherDefines[i],'');
  aXMLConfig.SetDeleteValue(p+'OtherDefines/Count',FOtherDefines.Count,0);

  { Compilation }
  aXMLConfig.SetDeleteValue(p+'CompilerPath/Value', f(CompilerPath),DefaultCompilerPath);
  ExecuteBefore.SaveToXMLConfig(aXMLConfig,p+'ExecuteBefore/',UsePathDelim);
  ExecuteAfter.SaveToXMLConfig(aXMLConfig,p+'ExecuteAfter/',UsePathDelim);
  aXMLConfig.SetDeleteValue(p+'CreateMakefileOnBuild/Value',
                               CreateMakefileOnBuild,false);
  // write
  Modified := False;
end;

procedure TBaseCompilerOptions.SetModified(const AValue: boolean);
begin
  if Modified=AValue then exit;
  if AValue then begin
    IncreaseChangeStamp;
    if Assigned(OnModified) then
      OnModified(Self);
  end else begin
    FSavedChangeStamp:=ChangeStamp;
    fMessageFlags.Modified:=false;
  end;
end;

class function TBaseCompilerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := nil;
end;

class function TBaseCompilerOptions.GetGroupCaption: string;
begin
  Result := '';
end;

procedure TBaseCompilerOptions.ClearInheritedOptions;
var
  i: TInheritedCompilerOption;
  p: TCompilerOptionsParseType;
begin
  fInheritedOptParseStamps:=CTInvalidChangeStamp;
  for p:=Low(TCompilerOptionsParseType) to High(TCompilerOptionsParseType) do
    for i:=Low(TInheritedCompilerOption) to High(TInheritedCompilerOption) do
    begin
      fInheritedOptions[p][i]:='';
    end;
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions CreateTargetFilename
------------------------------------------------------------------------------}
function TBaseCompilerOptions.CreateTargetFilename: string;

  procedure AppendDefaultExt(var aFilename: string);
  var
    Ext: String;
  begin
    if (ExtractFileName(aFilename)='') then exit;
    Ext:=GetTargetFileExt;
    if Ext<>'' then begin
      aFilename:=ChangeFileExt(aFilename,Ext);
      //debugln ( 'Filename is ',AFilename,' in AppendDefaultExt' );
      exit;
    end;
  end;

  procedure PrependDefaultType(var AFilename: string);
  var
    Prefix: String;
    FileName: String;
    PathName: String;
    CurTargetOS: String;
    aSrcOS: String;
  begin
    //debugln ( 'Filename AFilename is ',AFilename, ' in PrependDefaultType' );
    if (ExtractFileName(AFilename)='')
    or (CompareText(copy(ExtractFileName(AFilename),1,3), 'lib') = 0) then exit;
    Prefix:=GetTargetFilePrefix;
    if Prefix<>'' then begin
      FileName := ExtractFileName(AFilename);
      PathName := ExtractFilePath(AFilename);
      //debugln ( 'Filename is ',FileName, ' in PrependDefaultType' );
      CurTargetOS:=TargetOS;
      if CurTargetOS='' then CurTargetOS:=GetCompiledTargetOS;
      aSrcOS:=GetDefaultSrcOSForTargetOS(CurTargetOS);
      if (CompareText(aSrcOS, 'unix') = 0)
      then begin
        AFilename:=PathName+Prefix+UTF8LowerCase(FileName);
      end else begin
        AFilename:=PathName+Prefix+FileName;
      end;
      //debugln ( 'AFilename is ',AFilename, ' in PrependDefaultType' );
      exit;
    end;
  end;

var
  UnitOutDir: String;
  OutFilename: String;
  Dir: String;
begin
  Result:=TargetFilename;
  if Assigned(ParsedOpts.OnLocalSubstitute) then
  begin
    Result:=ParsedOpts.OnLocalSubstitute(Result,false);
  end else begin
    Result:=ParseString(ParsedOpts,Result,false);
  end;
  if (Result<>'') and FilenameIsAbsolute(Result) then begin
    // fully specified target filename
  end else if Result<>'' then begin
    //debugln(['TBaseCompilerOptions.CreateTargetFilename ParsedOpts.OutputDirectoryOverride=',ParsedOpts.OutputDirectoryOverride]);
    if ParsedOpts.OutputDirectoryOverride<>'' then
    begin
      // the program is put into the output directory
      UnitOutDir:=GetUnitOutPath(false);
      if UnitOutDir='' then
        UnitOutDir:=BaseDirectory;
      Result:=AppendPathDelim(UnitOutDir)+ExtractFileName(Result);
    end else if BaseDirectory<>'' then begin
      // the program is put relative to the base directory
      Result:=CreateAbsolutePath(Result,BaseDirectory);
    end else begin
      // put into test directory
      Dir:=EnvironmentOptions.GetParsedTestBuildDirectory;
      Result:=CreateAbsolutePath(Result,Dir);
    end;
  end else begin
    // no target given => put into unit output directory
    // calculate output directory
    UnitOutDir:=GetUnitOutPath(false);
    if UnitOutDir='' then
      UnitOutDir:=BaseDirectory;
    if UnitOutDir='' then
      UnitOutDir:=EnvironmentOptions.GetParsedTestBuildDirectory;
    OutFilename:=ExtractFileNameOnly(GetDefaultMainSourceFileName);
    //debugln('TBaseCompilerOptions.CreateTargetFilename MainSourceFileName=',MainSourceFileName,' OutFilename=',OutFilename,' TargetFilename=',TargetFilename,' UnitOutDir=',UnitOutDir);
    Result:=CreateAbsolutePath(OutFilename,UnitOutDir);
  end;
  Result:=TrimFilename(Result);
  if TargetFilenameApplyConventions then begin
    AppendDefaultExt(Result);
    PrependDefaultType(Result);
  end;
end;

function TBaseCompilerOptions.GetTargetFileExt: string;
begin
  Result:=TargetFileExt;
  if Result<>'' then exit;
  case ExecutableType of
  cetProgram:
    Result:=GetExecutableExt(fTargetOS);
  cetLibrary:
    Result:=GetLibraryExt(fTargetOS);
  else
    RaiseGDBException('');
  end;
  //DebugLn('TBaseCompilerOptions.GetTargetFileExt ',Result,' ',dbgs(ord(ExecutableType)),' ',fTargetOS);
end;

function TBaseCompilerOptions.GetTargetFilePrefix: string;
begin
  case ExecutableType of
  cetLibrary:
    Result:=GetLibraryPrefix(fTargetOS);
  else
    Result:='';
  end;
  //DebugLn('TBaseCompilerOptions.GetTargetFilePrefix ',Result,' ',dbgs(ord(ExecutableType)),' ',fTargetOS);
end;

procedure TBaseCompilerOptions.GetInheritedCompilerOptions(
  var OptionsList: TFPList);
begin
  OptionsList:=nil;
end;

function TBaseCompilerOptions.GetOwnerName: string;
begin
  if Owner<>nil then
    Result:=Owner.ClassName
  else
    Result:='This compiler options object has no owner';
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.GetInheritedOption(
    Option: TInheritedCompilerOption; RelativeToBaseDir: boolean;
    Parsed: TCompilerOptionsParseType): string;
------------------------------------------------------------------------------}
function TBaseCompilerOptions.GetInheritedOption(
  Option: TInheritedCompilerOption; RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType): string;
var
  AddOptionsList: TFPList; // list of TAdditionalCompilerOptions
  p: TCompilerOptionsParseType;
begin
  if (fInheritedOptParseStamps<>CompilerParseStamp)
  then begin
    // update all three inherited options:
    // coptUnparsed, coptParsed and coptParsedPlatformIndependent
    ClearInheritedOptions;
    AddOptionsList:=nil;
    GetInheritedCompilerOptions(AddOptionsList);
    if AddOptionsList<>nil then begin
      for p:=Low(TCompilerOptionsParseType) to High(TCompilerOptionsParseType)
      do begin
        GatherInheritedOptions(AddOptionsList,p,fInheritedOptions[p]);
      end;
      AddOptionsList.Free;
    end;
    fInheritedOptParseStamps:=CompilerParseStamp;
  end;
  Result:=fInheritedOptions[Parsed][Option];
  if RelativeToBaseDir then begin
    if Option in [icoUnitPath,icoIncludePath,icoObjectPath,icoLibraryPath] then
      Result:=CreateRelativeSearchPath(Result,BaseDirectory);
  end;
end;

function TBaseCompilerOptions.GetDefaultMainSourceFileName: string;
begin
  Result:='';
end;

function TBaseCompilerOptions.CanBeDefaulForProject: boolean;
begin
  Result:=false;
end;

function TBaseCompilerOptions.NeedsLinkerOpts: boolean;
begin
  Result:=not (ccloNoLinkerOpts in fDefaultMakeOptionsFlags);
end;

function TBaseCompilerOptions.HasCommands: boolean;
begin
  Result:=true;
  if CreateMakefileOnBuild then exit;
  if HasCompilerCommand then exit;
  if ExecuteBefore.HasCommands then exit;
  if ExecuteAfter.HasCommands then exit;
  Result:=false;
end;

function TBaseCompilerOptions.HasCompilerCommand: boolean;
begin
  Result:=CompilerPath<>'';
end;

function TBaseCompilerOptions.GetEffectiveTargetOS: string;
var
  Vars: TCTCfgScriptVariables;
  UnitSet: TFPCUnitSetCache;
  CfgCache: TPCTargetConfigCache;
begin
  Result:='';
  Vars:=GetBuildMacroValues(Self,true);
  if Vars<>nil then
    Result:=GetFPCTargetOS(Vars.Values['TargetOS']);
  if Result='' then begin
    UnitSet:=CodeToolBoss.GetUnitSetForDirectory(BaseDirectory);
    if UnitSet<>nil then begin
      CfgCache:=UnitSet.GetConfigCache(false);
      if CfgCache<>nil then begin
        Result:=CfgCache.RealTargetOS;
      end;
    end;
  end;
  if Result='' then
    Result:=GetCompiledTargetOS;
end;

function TBaseCompilerOptions.GetEffectiveTargetCPU: string;
var
  Vars: TCTCfgScriptVariables;
  UnitSet: TFPCUnitSetCache;
  CfgCache: TPCTargetConfigCache;
begin
  Result:='';
  Vars:=GetBuildMacroValues(Self,true);
  if Vars<>nil then
    Result:=GetFPCTargetOS(Vars.Values['TargetCPU']);
  if Result='' then begin
    UnitSet:=CodeToolBoss.GetUnitSetForDirectory(BaseDirectory);
    if UnitSet<>nil then begin
      CfgCache:=UnitSet.GetConfigCache(false);
      if CfgCache<>nil then begin
        Result:=CfgCache.RealTargetCPU;
      end;
    end;
  end;
  if Result='' then
    Result:=GetCompiledTargetCPU;
end;

function TBaseCompilerOptions.GetEffectiveLCLWidgetType: string;
var
  Vars: TCTCfgScriptVariables;
begin
  Result:='';
  Vars:=GetBuildMacroValues(Self,true);
  if Vars<>nil then
    Result:=Vars.Values['LCLWidgetType'];
end;

function TBaseCompilerOptions.GetUnitPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosUnitPath,icoUnitPath,RelativeToBaseDir,Parsed,WithBaseDir);
end;

function TBaseCompilerOptions.GetNamespacesParsed(Parsed: TCompilerOptionsParseType
  ): string;
var
  CurNamespaces, InhNamespaces: String;
begin
  // this namespaces
  case Parsed of
  coptParsed: CurNamespaces:=ParsedOpts.GetParsedValue(pcosNamespaces);
  coptUnparsed: CurNamespaces:=ParsedOpts.Values[pcosNamespaces].UnparsedValue;
  coptParsedPlatformIndependent:
               CurNamespaces:=ParsedOpts.GetParsedPIValue(pcosNamespaces);
  else
    RaiseGDBException('');
  end;
  // inherited namespaces
  InhNamespaces:=GetInheritedOption(icoNamespaces,false,Parsed);

  // concatenate
  Result:=MergeWithDelimiter(CurNamespaces,InhNamespaces,';');

  // eliminate line breaks
  Result:=SpecialCharsToSpaces(Result,true);
end;

function TBaseCompilerOptions.GetIncludePath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosIncludePath,icoIncludePath,RelativeToBaseDir,Parsed,
                  WithBaseDir);
end;

function TBaseCompilerOptions.GetSrcPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosSrcPath,icoSrcPath,RelativeToBaseDir,Parsed,WithBaseDir);
end;

function TBaseCompilerOptions.GetDebugPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosDebugPath,icoNone,RelativeToBaseDir,Parsed,WithBaseDir);
end;

function TBaseCompilerOptions.GetLibraryPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosLibraryPath,icoLibraryPath,RelativeToBaseDir,Parsed,
                  WithBaseDir);
end;

function TBaseCompilerOptions.GetUnitOutputDirectory(RelativeToBaseDir: boolean
  ): string;
begin
  Result:=GetUnitOutPath(RelativeToBaseDir);
end;

function TBaseCompilerOptions.GetUnitOutPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType): string;
begin
  case Parsed of
  coptUnparsed: Result:=ParsedOpts.Values[pcosOutputDir].UnparsedValue;
  coptParsed: Result:=ParsedOpts.GetParsedValue(pcosOutputDir);
  coptParsedPlatformIndependent:
              Result:=ParsedOpts.GetParsedPIValue(pcosOutputDir);
  end;
  if (not RelativeToBaseDir) then
    CreateAbsolutePath(Result,BaseDirectory);
end;

function TBaseCompilerOptions.GetObjectPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosObjectPath,icoObjectPath,RelativeToBaseDir,Parsed,
                  WithBaseDir);
end;

function TBaseCompilerOptions.GetPath(Option: TParsedCompilerOptString;
  InheritedOption: TInheritedCompilerOption; RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
var
  AddPath: String;
begin
  case Parsed of
  coptUnparsed:
    Result:=GetUnparsedPath(Option,InheritedOption,RelativeToBaseDir);
  coptParsed:
    Result:=GetParsedPath(Option,InheritedOption,RelativeToBaseDir);
  coptParsedPlatformIndependent:
    Result:=GetParsedPIPath(Option,InheritedOption,RelativeToBaseDir);
  else
    RaiseGDBException('');
  end;
  if WithBaseDir then begin
    if RelativeToBaseDir then
      AddPath:='.'
    else
      AddPath:=BaseDirectory;
    if AddPath<>'' then
      Result:=MergeSearchPaths(Result,AddPath);
  end;
end;

function TBaseCompilerOptions.GetParsedPath(Option: TParsedCompilerOptString;
  InheritedOption: TInheritedCompilerOption;
  RelativeToBaseDir: boolean; AddBaseDir: boolean = false): string;
var
  CurrentPath: String;
  InheritedPath: String;
  ParsedBaseDir: String;
begin
  // the first path is searched first

  // current path
  if Option<>pcosNone then begin
    CurrentPath:=ParsedOpts.GetParsedValue(Option);
    {$IFDEF VerbosePkgUnitPath}
    if Option=pcosUnitPath then
      debugln('TBaseCompilerOptions.GetParsedPath GetParsedValue ',dbgsName(Self),' RelativeToBaseDir=',dbgs(RelativeToBaseDir),' CurrentPath="',CurrentPath,'"');
    {$ENDIF}

    if RelativeToBaseDir then
      CurrentPath:=CreateRelativeSearchPath(CurrentPath,BaseDirectory)
    else
      CurrentPath:=CreateAbsoluteSearchPath(CurrentPath,BaseDirectory);
    {$IFDEF VerbosePkgUnitPath}
    if Option=pcosUnitPath then
      debugln('TBaseCompilerOptions.GetParsedPath Absolute/Relative=',dbgs(RelativeToBaseDir),' SearchPath ',dbgsName(Self),' CurrentPath="',CurrentPath,'" BaseDirectory="',BaseDirectory,'"');
    {$ENDIF}
  end else begin
    CurrentPath:='';
  end;

  // inherited path
  if InheritedOption<>icoNone then begin
    InheritedPath:=GetInheritedOption(InheritedOption,RelativeToBaseDir,coptParsed);
    {$IFDEF VerbosePkgUnitPath}
    if Option=pcosUnitPath then
      debugln('TBaseCompilerOptions.GetParsedPath Inherited ',dbgsName(Self),' InheritedPath="',InheritedPath,'"');
    {$ENDIF}

    Result:=MergeSearchPaths(CurrentPath,InheritedPath);
    {$IFDEF VerbosePkgUnitPath}
    if Option=pcosUnitPath then
      debugln('TBaseCompilerOptions.GetParsedPath Total ',dbgsName(Self),' Result="',Result,'"');
    {$ENDIF}
  end else
    Result:=CurrentPath;

  if AddBaseDir then begin
    ParsedBaseDir:=ParsedOpts.GetParsedValue(pcosBaseDir);
    if ParsedBaseDir<>'' then
      Result:=MergeSearchPaths(Result,ParsedBaseDir);
  end;
end;

function TBaseCompilerOptions.GetParsedPIPath(Option: TParsedCompilerOptString;
  InheritedOption: TInheritedCompilerOption; RelativeToBaseDir: boolean
  ): string;
var
  CurrentPath: String;
  InheritedPath: String;
begin
  // current path
  CurrentPath:=ParsedOpts.GetParsedPIValue(Option);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetParsedPIPath GetParsedPIValue ',dbgsName(Self),' RelativeToBaseDir=',dbgs(RelativeToBaseDir),' CurrentPath="',CurrentPath,'" BaseDirectory="',BaseDirectory,'"');
  {$ENDIF}

  if RelativeToBaseDir then
    CurrentPath:=CreateRelativeSearchPath(CurrentPath,BaseDirectory)
  else
    CurrentPath:=CreateAbsoluteSearchPath(CurrentPath,BaseDirectory);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetParsedPIPath Abs/Rel ',dbgsName(Self),' CurrentPath="',CurrentPath,'"');
  {$ENDIF}

  // inherited path
  InheritedPath:=GetInheritedOption(InheritedOption,RelativeToBaseDir,
                                    coptParsedPlatformIndependent);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetParsedPIPath Inherited ',dbgsName(Self),' InheritedPath="',InheritedPath,'"');
  {$ENDIF}

  Result:=MergeSearchPaths(CurrentPath,InheritedPath);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetParsedPIPath Total ',dbgsName(Self),' Result="',Result,'"');
  {$ENDIF}
end;

function TBaseCompilerOptions.GetUnparsedPath(Option: TParsedCompilerOptString;
  InheritedOption: TInheritedCompilerOption; RelativeToBaseDir: boolean
  ): string;
var
  CurrentPath: String;
  InheritedPath: String;
begin
  // current path
  CurrentPath:=ParsedOpts.Values[Option].UnparsedValue;
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetUnparsedPath GetParsedValue ',dbgsName(Self),' RelativeToBaseDir=',dbgs(RelativeToBaseDir),' CurrentPath="',CurrentPath,'"');
  {$ENDIF}

  if (not RelativeToBaseDir) then
    CreateAbsoluteSearchPath(CurrentPath,BaseDirectory);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetUnparsedPath CreateAbsoluteSearchPath ',dbgsName(Self),' CurrentPath="',CurrentPath,'"');
  {$ENDIF}

  // inherited path
  InheritedPath:=GetInheritedOption(InheritedOption,RelativeToBaseDir,
                                    coptUnparsed);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetUnparsedPath Inherited ',dbgsName(Self),' InheritedPath="',InheritedPath,'"');
  {$ENDIF}

  Result:=MergeSearchPaths(CurrentPath,InheritedPath);
  {$IFDEF VerbosePkgUnitPath}
  if Option=pcosUnitPath then
    debugln('TBaseCompilerOptions.GetUnparsedPath Total ',dbgsName(Self),' Result="',Result,'"');
  {$ENDIF}
end;

function TBaseCompilerOptions.GetCustomOptions(
  Parsed: TCompilerOptionsParseType): string;
var
  CurCustomOptions: String;
  InhCustomOptions: String;
begin
  // custom options
  case Parsed of
  coptParsed: CurCustomOptions:=ParsedOpts.GetParsedValue(pcosCustomOptions);
  coptUnparsed: CurCustomOptions:=ParsedOpts.Values[pcosCustomOptions].UnparsedValue;
  coptParsedPlatformIndependent:
               CurCustomOptions:=ParsedOpts.GetParsedPIValue(pcosCustomOptions);
  else
    RaiseGDBException('');
  end;
  // inherited custom options
  InhCustomOptions:=GetInheritedOption(icoCustomOptions,true,Parsed);

  // concatenate
  Result:=MergeCustomOptions(InhCustomOptions,CurCustomOptions);

  // eliminate line breaks
  Result:=SpecialCharsToSpaces(Result,true);
end;

function TBaseCompilerOptions.TrimCustomOptions(o: string): string;
begin
  Result:=SpecialCharsToSpaces(o,true);
end;

function TBaseCompilerOptions.GetOptionsForCTDefines: string;

  procedure Add(s: string);
  begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+s;
  end;

begin
  Result:=GetCustomOptions(coptParsed);
  Add(GetSyntaxOptionsString(pcFPC));
end;

procedure TBaseCompilerOptions.RenameMacro(const OldName, NewName: string;
  ChangeConditionals: boolean);
var
  Changed: TParsedCompilerOptStrings;
  s: String;
begin
  ParsedOpts.RenameMacro(OldName,NewName,Changed);
  if Changed<>[] then begin

  end;
  if ChangeConditionals then
  begin
    s:=Conditionals;
    RenameCTCSVariable(s,OldName,NewName);
    Conditionals:=s;
  end;
end;

procedure TBaseCompilerOptions.MergeToIncludePaths(const AddSearchPath: string);
begin
  SetIncludePaths(MergeSearchPaths(GetIncludePaths,AddSearchPath));
end;

procedure TBaseCompilerOptions.MergeToLibraryPaths(const AddSearchPath: string);
begin
  SetLibraryPaths(MergeSearchPaths(GetLibraryPaths,AddSearchPath));
end;

procedure TBaseCompilerOptions.MergeToNamespaces(const AddNamespaces: string);
begin
  SetNamespaces(MergeWithDelimiter(GetNamespacesParsed,AddNamespaces,';'));
end;

procedure TBaseCompilerOptions.MergeToUnitPaths(const AddSearchPath: string);
begin
  SetUnitPaths(MergeSearchPaths(GetUnitPaths,AddSearchPath));
end;

procedure TBaseCompilerOptions.MergeToObjectPath(const AddSearchPath: string);
begin
  SetObjectPath(MergeSearchPaths(GetObjectPath,AddSearchPath));
end;

procedure TBaseCompilerOptions.MergeToSrcPath(const AddSearchPath: string);
begin
  SetSrcPath(MergeSearchPaths(GetSrcPath,AddSearchPath));
end;

procedure TBaseCompilerOptions.MergeToDebugPath(const AddSearchPath: string);
begin
  SetDebugPath(MergeSearchPaths(GetDebugPath,AddSearchPath));
end;

procedure TBaseCompilerOptions.RemoveFromUnitPaths(const RemSearchPath: string);
begin
  SetUnitPaths(RemoveSearchPaths(GetUnitPaths,RemSearchPath));
end;

function TBaseCompilerOptions.ShortenPath(const SearchPath: string;
  MakeAlwaysRelative: boolean): string;
begin
  Result:=TrimSearchPath(SearchPath,'');
  if MakeAlwaysRelative then
    Result:=CreateRelativeSearchPath(Result,BaseDirectory)
  else
    Result:=ShortenSearchPath(Result,BaseDirectory,BaseDirectory);
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.MakeOptionsString(
    const MainSourceFilename: string;
    Flags: TCompilerCmdLineOptions): String;

  Get all the options and create a string that can be passed to the compiler
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MakeOptionsString(
  Flags: TCompilerCmdLineOptions): String;
var
  switches, tempsw, quietsw, t: String;
  InhLinkerOpts: String;
  CurTargetFilename: String;
  CurTargetDirectory: String;
  CurIncludePath: String;
  CurLibraryPath: String;
  CurUnitPath: String;
  CurOutputDir: String;
  CurLinkerOptions: String;
  CurObjectPath: String;
  CurMainSrcFile: String;
  CurCustomOptions: String;
  OptimizeSwitches: String;
  Vars: TCTCfgScriptVariables;
  CurTargetOS: String;
  CurTargetCPU: String;
  CurSrcOS: String;
  dit: TCompilerDbgSymbolType;
  CompilerFilename: String;
  DefaultTargetOS: string;
  DefaultTargetCPU: string;
  RealCompilerFilename: String;
  s, CurNamespaces: string;
  CurFPCMsgFile: TFPCMsgFilePoolItem;
  Quiet: Boolean;
  Kind: TPascalCompiler;

  procedure EnableDisableVerbosityFlag(Enable: boolean; c: char);
  begin
    if Quiet or not Enable then
      quietsw+=c+'-'
    else
      tempsw+=c;
  end;

  procedure EnableVerbosityFlag(Enable: boolean; c: char);
  begin
    if Quiet then
      quietsw+=c+'-'
    else if Enable then
      tempsw+=c;
  end;

begin
  switches := '';

  { options of fpc 2.7.1 :

  Free Pascal Compiler version 2.7.1 [2012/01/23] for x86_64
  Copyright (c) 1993-2011 by Florian Klaempfl and others
  /usr/lib/fpc/2.7.1/ppcx64 [options] <inputfile> [options]
  Put + after a boolean switch option to enable it, - to disable it
    -a     The compiler doesn't delete the generated assembler file
        -al        List sourcecode lines in assembler file
        -an        List node info in assembler file
        -ap        Use pipes instead of creating temporary assembler files
        -ar        List register allocation/release info in assembler file
        -at        List temp allocation/release info in assembler file
    -A<x>  Output format:
        -Adefault  Use default assembler
        -Aas       Assemble using GNU AS
        -Agas      Assemble using GNU GAS
        -Agas-darwinAssemble darwin Mach-O64 using GNU GAS
        -Amasm     Win64 object file using ml64 (Microsoft)
        -Apecoff   PE-COFF (Win64) using internal writer
        -Aelf      ELF (Linux-64bit) using internal writer
    -b     Generate browser info
        -bl        Generate local symbol info
    -B     Build all modules
    -C<x>  Code generation options:
        -C3<x>     Turn on ieee error checking for constants
        -Ca<x>     Select ABI, see fpc -i for possible values
        -Cb        Generate big-endian code
        -Cc<x>     Set default calling convention to <x>
        -CD        Create also dynamic library (not supported)
        -Ce        Compilation with emulated floating point opcodes
        -Cf<x>     Select fpu instruction set to use, see fpc -i for possible values
        -CF<x>     Minimal floating point constant precision (default, 32, 64)
        -Cg        Generate PIC code
        -Ch<n>     <n> bytes heap (between 1023 and 67107840)
        -Ci        IO-checking
        -Cn        Omit linking stage
        -Co        Check overflow of integer operations
        -CO        Check for possible overflow of integer operations
        -Cp<x>     Select instruction set, see fpc -i for possible values
        -CP<x>=<y>  packing settings
           -CPPACKSET=<y> <y> set allocation: 0, 1 or DEFAULT or NORMAL, 2, 4 and 8
        -Cr        Range checking
        -CR        Verify object method call validity
        -Cs<n>     Set stack checking size to <n>
        -Ct        Stack checking (for testing only, see manual)
        -CX        Create also smartlinked library
    -d<x>  Defines the symbol <x>
    -D     Generate a DEF file
        -Dd<x>     Set description to <x>
        -Dv<x>     Set DLL version to <x>
    -e<x>  Set path to executable
    -E     Same as -Cn
    -fPIC  Same as -Cg
    -F<x>  Set file names and paths:
        -Fa<x>[,y] (for a program) load units <x> and [y] before uses is parsed
        -Fc<x>     Set input codepage to <x>
        -FC<x>     Set RC compiler binary name to <x>
        -Fd        Disable the compiler's internal directory cache
        -FD<x>     Set the directory where to search for compiler utilities
        -Fe<x>     Redirect error output to <x>
        -Ff<x>     Add <x> to framework path (Darwin only)
        -FE<x>     Set exe/unit output path to <x>
        -Fi<x>     Add <x> to include path
        -Fl<x>     Add <x> to library path
        -FL<x>     Use <x> as dynamic linker
        -Fm<x>     Load unicode conversion table from <x>.txt in the compiler dir
        -Fo<x>     Add <x> to object path
        -Fr<x>     Load error message file <x>
        -FR<x>     Set resource (.res) linker to <x>
        -Fu<x>     Add <x> to unit path
        -FU<x>     Set unit output path to <x>, overrides -FE
        -FW<x>     Store generated whole-program optimization feedback in <x>
        -Fw<x>     Load previously stored whole-program optimization feedback from <x>
    -g     Generate debug information (default format for target)
        -gc        Generate checks for pointers
        -gh        Use heaptrace unit (for memory leak/corruption debugging)
        -gl        Use line info unit (show more info with backtraces)
        -go<x>     Set debug information options
           -godwarfsets Enable DWARF 'set' type debug information (breaks gdb < 6.5)
           -gostabsabsincludes Store absolute/full include file paths in Stabs
           -godwarfmethodclassprefix Prefix method names in DWARF with class name
        -gp        Preserve case in stabs symbol names
        -gs        Generate Stabs debug information
        -gt        Trash local variables (to detect uninitialized uses)
        -gv        Generates programs traceable with Valgrind
        -gw        Generate DWARFv2 debug information (same as -gw2)
        -gw2       Generate DWARFv2 debug information
        -gw3       Generate DWARFv3 debug information
        -gw4       Generate DWARFv4 debug information (experimental)
    -i     Information
        -iD        Return compiler date
        -iV        Return short compiler version
        -iW        Return full compiler version
        -iSO       Return compiler OS
        -iSP       Return compiler host processor
        -iTO       Return target OS
        -iTP       Return target processor
    -I<x>  Add <x> to include path
    -k<x>  Pass <x> to the linker
    -l     Write logo
    -M<x>  Set language mode to <x>
        -Mfpc      Free Pascal dialect (default)
        -Mobjfpc   FPC mode with Object Pascal support
        -Mdelphi   Delphi 7 compatibility mode
        -Mtp       TP/BP 7.0 compatibility mode
        -Mmacpas   Macintosh Pascal dialects compatibility mode
    -n     Do not read the default config files
    -N<x>  Node tree optimizations
        -Nu        Unroll loops
    -o<x>  Change the name of the executable produced to <x>
    -O<x>  Optimizations:
        -O-        Disable optimizations
        -O1        Level 1 optimizations (quick and debugger friendly)
        -O2        Level 2 optimizations (-O1 + quick optimizations)
        -O3        Level 3 optimizations (-O2 + slow optimizations)
        -Oa<x>=<y> Set alignment
        -Oo[NO]<x> Enable or disable optimizations, see fpc -i for possible values
        -Op<x>     Set target cpu for optimizing, see fpc -i for possible values
        -OW<x>     Generate whole-program optimization feedback for optimization <x>, see fpc -i for possible values
        -Ow<x>     Perform whole-program optimization <x>, see fpc -i for possible values
        -Os        Optimize for size rather than speed
    -pg    Generate profile code for gprof (defines FPC_PROFILE)
    -R<x>  Assembler reading style:
        -Rdefault  Use default assembler for target
    -S<x>  Syntax options:
        -S2        Same as -Mobjfpc
        -Sc        Support operators like C (*=,+=,/= and -=)
        -Sa        Turn on assertions
        -Sd        Same as -Mdelphi
        -Se<x>     Error options. <x> is a combination of the following:
           <n> : Compiler halts after the <n> errors (default is 1)
           w : Compiler also halts after warnings
           n : Compiler also halts after notes
           h : Compiler also halts after hints
        -Sg        Enable LABEL and GOTO (default in -Mtp and -Mdelphi)
        -Sh        Use ansistrings by default instead of shortstrings
        -Si        Turn on inlining of procedures/functions declared as "inline"
        -Sk        Load fpcylix unit
        -SI<x>     Set interface style to <x>
           -SIcom     COM compatible interface (default)
           -SIcorba   CORBA compatible interface
        -Sm        Support macros like C (global)
        -So        Same as -Mtp
        -Ss        Constructor name must be init (destructor must be done)
        -Sx        Enable exception keywords (default in Delphi/ObjFPC modes)
        -Sy        @<pointer> returns a typed pointer, same as $T+
    -s     Do not call assembler and linker
        -sh        Generate script to link on host
        -st        Generate script to link on target
        -sr        Skip register allocation phase (use with -alr)
    -T<x>  Target operating system:
        -Tdarwin   Darwin/Mac OS X
        -Tlinux    Linux
        -Twin64    Win64 (64 bit Windows systems)
    -u<x>  Undefines the symbol <x>
    -U     Unit options:
        -Un        Do not check where the unit name matches the file name
        -Ur        Generate release unit files (never automatically recompiled)
        -Us        Compile a system unit
    -v<x>  Be verbose. <x> is a combination of the following letters:
        e : Show errors (default)       0 : Show nothing (except errors)
        w : Show warnings               u : Show unit info
        n : Show notes                  t : Show tried/used files
        h : Show hints                  c : Show conditionals
        i : Show general info           d : Show debug info
        l : Show linenumbers            r : Rhide/GCC compatibility mode
        s : Show time stamps            q : Show message numbers
        a : Show everything             x : Executable info (Win32 only)
        b : Write file names messages   p : Write tree.log with parse tree
            with full path              v : Write fpcdebug.txt with
                                            lots of debugging info
        m<x>,<y> : Don't show messages numbered <x> and <y>
    -W<x>  Target-specific options (targets)
        -WA        Specify native type application (Windows)
        -Wb        Create a bundle instead of a library (Darwin)
        -WB        Create a relocatable image (Windows)
        -WBxxxx    Set image base to xxxx (Windows)
        -WC        Specify console type application (EMX, OS/2, Windows)
        -WD        Use DEFFILE to export functions of DLL or EXE (Windows)
        -We        Use external resources (Darwin)
        -WG        Specify graphic type application (EMX, OS/2, Windows)
        -Wi        Use internal resources (Darwin)
        -WI        Turn on/off the usage of import sections (Windows)
        -WM<x>     Minimum Mac OS X deployment version: 10.4, 10.5.1, ... (Darwin)
        -WN        Do not generate relocation code, needed for debugging (Windows)
        -WR        Generate relocation code (Windows)
        -WX        Enable executable stack (Linux)
    -X     Executable options:
        -Xc        Pass --shared/-dynamic to the linker (BeOS, Darwin, FreeBSD, Linux)
        -Xd        Do not search default library path (sometimes required for cross-compiling when not using -XR)
        -Xe        Use external linker
        -Xg        Create debuginfo in a separate file and add a debuglink section to executable
        -XD        Try to link units dynamically      (defines FPC_LINK_DYNAMIC)
        -Xi        Use internal linker
        -Xm        Generate link map
        -XM<x>     Set the name of the 'main' program routine (default is 'main')
        -XP<x>     Prepend the binutils names with the prefix <x>
        -Xr<x>     Set the linker's rlink-path to <x> (needed for cross compile, see the ld manual for more information) (BeOS, Linux)
        -XR<x>     Prepend <x> to all linker search paths (BeOS, Darwin, FreeBSD, Linux, Mac OS, Solaris)
        -Xs        Strip all symbols from executable
        -XS        Try to link units statically (default, defines FPC_LINK_STATIC)
        -Xt        Link with static libraries (-static is passed to linker)
        -XX        Try to smartlink units             (defines FPC_LINK_SMART)

    -?     Show this help
    -h     Shows this help without waiting

  ------------------------------------------------------------------------------
  ppcx64 -i
    Free Pascal Compiler version 2.7.1

    Compiler Date      : 2012/01/23
    Compiler CPU Target: x86_64

    Supported targets:
      Linux for x86-64
      FreeBSD for x86-64
      Win64 for x64
      Darwin for x86_64
      Solaris for x86-64 (under development)

    Supported CPU instruction sets:
      ATHLON64

    Supported FPU instruction sets:
      SSE64
      SSE3

    Supported ABI targets:
      DEFAULT
      SYSV
      AIX
      EABI
      ARMEB

    Supported Optimizations:
      REGVAR
      STACKFRAME
      LOOPUNROLL
      TAILREC
      CSE

    Supported Whole Program Optimizations:
      All
      DEVIRTCALLS
      OPTVMTS
      SYMBOLLIVENESS

    Supported Microcontroller types:
  }
  Quiet:=ConsoleVerbosity<=-3; // lazbuild -q -q, lazarus -q -q -q

  CompilerFilename:=ParsedOpts.GetParsedValue(pcosCompilerPath);
  if IsCompilerExecutable(CompilerFilename,s,Kind) then
    RealCompilerFilename:=CompilerFilename
  else begin
    RealCompilerFilename:=EnvironmentOptions.GetParsedCompilerFilename;
    Kind:=pcFPC;
  end;

  if ccloAddCompilerPath in Flags then
    switches:=ConvertOptionsToCmdLine('',RealCompilerFilename);

  CurTargetOS:='';
  CurTargetCPU:='';
  if not (ccloNoMacroParams in Flags) then
  begin
    Vars:=GetBuildMacroValues(Self,true);
    if Vars<>nil then
    begin
      CurTargetOS:=GetFPCTargetOS(Vars.Values['TargetOS']);
      CurTargetCPU:=GetFPCTargetCPU(Vars.Values['TargetCPU']);
    end;
  end;
  CurSrcOS:=GetDefaultSrcOSForTargetOS(CurTargetOS);

  CodeToolBoss.CompilerDefinesCache.ConfigCaches.GetDefaultCompilerTarget(
    RealCompilerFilename,'',DefaultTargetOS,DefaultTargetCPU);

  { ------------------ Target --------------------- }

  { Target OS }
  if (CurTargetOS<>'')
  and ((TargetOS<>'') or (CurTargetOS<>DefaultTargetOS)) then
    switches := switches + ' -T' + CurTargetOS;
  { Target CPU }
  if (CurTargetCPU<>'')
  and ((TargetCPU<>'') or (CurTargetCPU<>DefaultTargetCPU)) then
    switches := switches + ' -P' + CurTargetCPU;
  { TargetProcessor }
  if TargetProcessor<>'' then
    Switches:=Switches+' -Cp'+UpperCase(TargetProcessor);

  { --------------- Parsing Tab ------------------- }

  { Assembler reading style  -Ratt = AT&T    -Rintel = Intel  -Rdirect = direct }
  case AssemblerStyle of
    1: switches := switches + ' -Rintel';
    2: switches := switches + ' -Ratt';
    3: switches := switches + ' -Rdirect';
  end;

  // Syntax Options
  tempsw:=GetSyntaxOptionsString(Kind);
  if (tempsw <> '') then
    switches := switches + ' ' + tempsw;

  { ----------- Code Generation Tab --------------- }

  { UnitStyle   '' = Static     'D' = Dynamic (not implemented)   'X' = smart linked }
  if SmartLinkUnit then
    switches := switches + ' -CX';
  if RelocatableUnit and (CurSrcOS='win') then
    switches := switches + ' -WR';
  if (not (ccloNoMacroParams in Flags))
  and TargetNeedsFPCOptionCG(CurTargetOS,CurTargetCPU) then
    switches := switches + ' -Cg'; // see bug 17412

  { Checks }
  tempsw := '';

  if IOChecks then
    tempsw := tempsw + 'i';
  if RangeChecks then
    tempsw := tempsw + 'r';
  if OverflowChecks then
    tempsw := tempsw + 'o';
  if StackChecks then
    tempsw := tempsw + 't';
  if EmulatedFloatOpcodes then
    tempsw := tempsw + 'e';
  if VerifyObjMethodCall then
    tempsw := tempsw + 'R';

  if (tempsw <> '') then
    switches := switches + ' -C' + tempsw;

  { Heap Size }
  if (HeapSize > 0) then
    switches := switches + ' ' + '-Ch' + IntToStr(HeapSize);

  { Stack Size }
  if (StackSize > 0) then
    switches := switches + ' ' + '-Cs' + IntToStr(StackSize);

  { Optimizations }
  OptimizeSwitches:='';
  if SmallerCode then
    OptimizeSwitches := OptimizeSwitches + 's';
  { OptimizationLevel     1 = Level 1    2 = Level 2    3 = Level 3 }
  if OptimizationLevel>0 then
    OptimizeSwitches := OptimizeSwitches + IntToStr(OptimizationLevel);
  if OptimizeSwitches<>'' then
    switches := switches + ' -O'+OptimizeSwitches;

  // uncertain
  if UncertainOptimizations then
    Switches := Switches + ' -OoUNCERTAIN';

  // registers
  if VariablesInRegisters then
    Switches := Switches + ' -OoREGVAR';

  { --------------- Linking Tab ------------------- }

  { Debugging }
  { Debug Info for GDB }
  if GenerateDebugInfo then begin

    dit := DebugInfoType;
    case dit of
      dsAuto:
        if Kind=pcFPC then begin
          if (not (ccloNoMacroParams in Flags)) and (CurTargetOS='darwin') then
            switches += ' -gw'
          else
            switches += ' -g';
        end;
      dsStabs:     switches := switches + ' -gs';
      dsDwarf2:    switches := switches + ' -gw2';
      dsDwarf2Set: switches := switches + ' -gw2 -godwarfsets';
      dsDwarf3:    switches := switches + ' -gw3';
    end;

    { Line Numbers in Run-time Error Backtraces - Use LineInfo Unit }
    if UseLineInfoUnit then
      switches := switches + ' -gl';

    { Use Heaptrc Unit }
    if UseHeaptrc and (not (ccloNoLinkerOpts in Flags)) then
      switches := switches + ' -gh';

    { Generate code for Valgrind }
    if UseValgrind and (not (ccloNoLinkerOpts in Flags)) then
      switches := switches + ' -gv';

    if UseExternalDbgSyms then
      switches := switches + ' -Xg';

  end
  else begin
    // no debug info wanted

    { Use Heaptrc Unit }
    if (UseHeaptrc) and (not (ccloNoLinkerOpts in Flags)) then
      switches := switches + ' -g-h'; // heaptrc, without -g
  end;

  { Trash variables }
  if TrashVariables then
    switches := switches + ' -gt';

  { Generate code gprof }
  if GenGProfCode then
    switches := switches + ' -pg';

  { Strip Symbols }
  if StripSymbols and (not (ccloNoLinkerOpts in Flags)) then
    switches := switches + ' -Xs';

  { Link Style
     -XD = Link with dynamic libraries
     -XS = Link with static libraries, default on non-win32 platforms
     -XX = Link smart
  }

  if (not (ccloNoLinkerOpts in Flags)) and LinkSmart then
    switches := switches + ' -XX';

  // additional Linker options
  if (not (ccloNoLinkerOpts in Flags))
  and (not (ccloNoMacroParams in Flags)) then
  begin
    if PassLinkerOptions then
    begin
      CurLinkerOptions:=ParsedOpts.GetParsedValue(pcosLinkerOptions);
      if (CurLinkerOptions<>'') then
        switches := switches + ' ' + ConvertOptionsToCmdLine('-k', CurLinkerOptions);
    end;

    // inherited Linker options
    InhLinkerOpts:=GetInheritedOption(icoLinkerOptions,
                                   not (ccloAbsolutePaths in Flags),coptParsed);
    //debugln(['TBaseCompilerOptions.MakeOptionsString InhLinkerOpts="',InhLinkerOpts,'"']);
    if InhLinkerOpts<>'' then
      switches := switches + ' ' + ConvertOptionsToCmdLine('-k', InhLinkerOpts);
  end;

  if Win32GraphicApp
  and ((CurSrcOS='win') or (CurTargetOS='macos') or (CurTargetOS='os2')) then
    switches := switches + ' -WG';

  { ---------------- Other Tab -------------------- }

  { Verbosity }
  if Quiet then
    switches := switches + ' -l-'
  else if WriteFPCLogo then
    switches := switches + ' -l';

  tempsw := '';
  quietsw := '';
  // the default fpc.cfg normally contains -viwn, if the user does not want
  // to see warnings pass -vw-
  tempsw := tempsw + 'e'; // always pass -ve, you cannot ignore errors
  EnableDisableVerbosityFlag(ShowWarn,'w');
  EnableDisableVerbosityFlag(ShowNotes,'n');
  EnableDisableVerbosityFlag(ShowHints,'h');
  // always pass -vi for IDE, (e.g. (3104) Compiling) needed to resolve filenames in fpc messages without path
  EnableVerbosityFlag(true,'i');
  // optional verbosity flags, usually off in fpc.cfg, pass them only if wanted
  EnableVerbosityFlag(ShowLineNum,'l');
  EnableVerbosityFlag(ShowDebugInfo,'d');
  EnableVerbosityFlag(ShowUsedFiles,'u');
  EnableVerbosityFlag(ShowTriedFiles,'t');
  EnableVerbosityFlag(ShowCompProc,'p');
  EnableVerbosityFlag(ShowCond,'c');
  EnableVerbosityFlag(ShowExecInfo,'x');

  if (ShowAll and not Quiet) or (ccloAddVerboseAll in Flags) then
    tempsw := 'a';
  tempsw := tempsw + 'bq'; // b = full file names, q = message ids

  if (tempsw <> '') then
    switches := switches + ' -v' + tempsw;
  if (quietsw <> '') then
    switches := switches + ' -v' + quietsw;

  // -vm flags allow to enable/disable types of messages
  // Passing a -vm ID, unknown by the current compiler will create an error
  // => check the compiler message file
  if IDEMessageFlags.Count>0 then begin
    if FPCMsgFilePool<>nil then begin
      CurFPCMsgFile:=FPCMsgFilePool.LoadCurrentEnglishFile(true,nil);
      if CurFPCMsgFile<>FFPCMsgFile then begin
        if FFPCMsgFile<>nil then
          FPCMsgFilePool.UnloadFile(FFPCMsgFile);
        FFPCMsgFile:=CurFPCMsgFile;
      end else
        FPCMsgFilePool.UnloadFile(CurFPCMsgFile);
    end;
    t := IDEMessageFlags.GetMsgIdList(',',cfvHide,FFPCMsgFile);
    if t <> '' then
      switches := switches + ' ' + PrepareCmdLineOption('-vm'+t);
    t := IDEMessageFlags.GetMsgIdList(',',cfvShow,FFPCMsgFile);
    if t <> '' then
      switches := switches + ' ' + PrepareCmdLineOption('-vm-'+t);
  end;

  if (StopAfterErrCount>1) then
    switches := switches + ' -Se'+IntToStr(StopAfterErrCount);

  { Ignore Config File }
  if DontUseConfigFile then
    switches := switches + ' -n';

  { Use Custom Config File     @ = yes and path }
  if not (ccloNoMacroParams in Flags)
  and (CustomConfigFile) and (ConfigFilePath<>'') then
    switches := switches + ' ' + PrepareCmdLineOption('@' + ConfigFilePath);

  { ------------- Search Paths ---------------- }
  CurOutputDir:='';
  if not (ccloNoMacroParams in Flags) then
  begin
    // include path
    CurIncludePath:=GetIncludePath(not (ccloAbsolutePaths in Flags),
                                   coptParsed,false);
    if (CurIncludePath <> '') then
      switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fi', CurIncludePath);

    // library path
    if (not (ccloNoLinkerOpts in Flags)) then begin
      CurLibraryPath:=GetLibraryPath(not (ccloAbsolutePaths in Flags),
                                     coptParsed,false);
      if (CurLibraryPath <> '') then
        switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fl', CurLibraryPath);
    end;

    // namespaces
    CurNamespaces:=GetNamespacesParsed(coptParsed);
    if CurNamespaces<>'' then
      switches := switches +' -FN'+CurNamespaces;

    // object path
    CurObjectPath:=GetObjectPath(not (ccloAbsolutePaths in Flags),
                                 coptParsed,false);
    if (CurObjectPath <> '') then
      switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fo', CurObjectPath);

    // unit path
    CurUnitPath:=GetUnitPath(not (ccloAbsolutePaths in Flags));
    //debugln('TBaseCompilerOptions.MakeOptionsString A ',dbgsName(Self),' CurUnitPath="',CurUnitPath,'"');
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fu', CurUnitPath);

    { CompilerPath - Nothing needs to be done with this one }

    { Unit output directory }
    if (UnitOutputDirectory<>'') then begin
      CurOutputDir:=ParsedOpts.GetParsedValue(pcosOutputDir);
    end;
  end;

  // output options -o, -FU, and -FE
  //  * -o to define the target file name.
  //  * -FU if the unit output directory is not empty
  //  * -FE if the target file name is not in the project directory (where the lpi file is)
  //  * if neither -FU nor -FE is passed fpc creates the ppu in the source directories

  CurMainSrcFile:=GetDefaultMainSourceFileName;
  CurTargetFilename:='';
  CurTargetDirectory:='';
  //DebugLn(['TBaseCompilerOptions.MakeOptionsString ',DbgSName(Self),' ',ccloDoNotAppendOutFileOption in Flags,' TargetFilename="',TargetFilename,'" CurMainSrcFile="',CurMainSrcFile,'" CurOutputDir="',CurOutputDir,'"']);
  if (not (ccloDoNotAppendOutFileOption in Flags))
    and (not (ccloNoMacroParams in Flags))
    and ((TargetFilename<>'') or (CurMainSrcFile<>'') or (CurOutputDir<>'')) then
  begin
    CurTargetFilename := CreateTargetFilename;
    if CurTargetFilename<>'' then
    begin
      CurTargetDirectory := ExtractFilePath(CurTargetFilename);
      if CurTargetDirectory<>'' then begin
        if (CurOutputDir='') // no -FU
        and (CompareFilenames(ChompPathDelim(CurTargetDirectory),ChompPathDelim(BaseDirectory))=0)
        then begin
          // if target file is in the base directory, do not use -FE switch
          // Without -FE and -FU switch the compiler puts .ppu files in the source
          // directories, which is Delphi compatible.
          // See bug http://bugs.freepascal.org/view.php?id=15535
          CurTargetDirectory:='';
        end else if CompareFilenames(ChompPathDelim(CurOutputDir),ChompPathDelim(CurTargetDirectory))=0 then
        begin
          // -FU and -FE are the same: do not add -FU
          CurOutputDir:='';
        end;
      end;
    end;
  end;

  if CurOutputDir<>'' then begin
    if not (ccloAbsolutePaths in Flags) then
      CurOutputDir:=CreateRelativePath(CurOutputDir,BaseDirectory,true);
    switches := switches + ' '+PrepareCmdLineOption('-FU' + CurOutputDir);
  end;
  if CurTargetDirectory <> '' then begin
    if not (ccloAbsolutePaths in Flags) then
      CurTargetDirectory:=CreateRelativePath(CurTargetDirectory,BaseDirectory,true);
    switches := switches + ' '+PrepareCmdLineOption('-FE' + CurTargetDirectory);
  end;
  if (CurTargetFilename<>'') then begin
    if not (ccloAbsolutePaths in Flags) then
      CurTargetFilename := CreateRelativePath(CurTargetFilename, BaseDirectory);
    if CurTargetFilename<>'' then
      switches := switches + ' '+PrepareCmdLineOption('-o' + CurTargetFilename);
  end;

  // append custom options as last, so they can override
  if not (ccloNoMacroParams in Flags) then
  begin
    //debugln(['TBaseCompilerOptions.MakeOptionsString ',DbgSName(Self)]);
    //DumpStack;
    CurCustomOptions:=GetCustomOptions(coptParsed);
    if CurCustomOptions<>'' then
      switches := switches+' '+CurCustomOptions;
  end;

  Result := switches;
end;

function TBaseCompilerOptions.GetSyntaxOptionsString(Kind: TPascalCompiler
  ): string;
var
  tempsw: String;
begin
  if SyntaxMode<>'' then
    Result:='-M'+SyntaxMode  // -M<x>  Set language mode to <x>
  else
    Result:='';

  tempsw := '';

  if (CStyleOperators) then
    tempsw := tempsw + 'c';
  if (IncludeAssertionCode) then
    tempsw := tempsw + 'a';
  if Kind=pcFPC then begin
    if (AllowLabel) then
      tempsw := tempsw + 'g';
    if (UseAnsiStrings) then
      tempsw := tempsw + 'h';
    if (CPPInline) then
      tempsw := tempsw + 'i';
    if (CStyleMacros) then
      tempsw := tempsw + 'm';
    if (InitConstructor) then
      tempsw := tempsw + 's';
    if (StaticKeyword) then
      tempsw := tempsw + 't';
  end;

  if (tempsw <> '') then begin
    if Result<>'' then
      Result:=Result+' ';
    Result := Result+'-S' + tempsw;
  end;
end;

function TBaseCompilerOptions.CreatePPUFilename(const SourceFileName: string
  ): string;
var
  UnitOutDir: String;
begin
  Result:=SourceFileName;
  IDEMacros.SubstituteMacros(Result);
  if Result='' then exit;
  if FilenameIsAbsolute(Result) then begin
    // fully specified target filename
  end else if (UnitOutputDirectory='')
  and (ParsedOpts.OutputDirectoryOverride='')
  and (ExtractFilePath(TargetFilename)='') then begin
    // the unit is put into the same directory as its source
    Result:=CreateAbsolutePath(Result,BaseDirectory);
  end else begin
    // the unit is put into the output directory
    UnitOutDir:=GetUnitOutPath(false);
    if UnitOutDir='' then
      UnitOutDir:=BaseDirectory;
    Result:=AppendPathDelim(UnitOutDir)+ExtractFileName(Result);
  end;
  Result:=ChangeFileExt(Result,'.ppu');
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions Clear
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.Clear;
begin
  // search paths
  IncludePath := '';
  Libraries := '';
  OtherUnitFiles := '';
  UnitOutputDirectory := '';
  ObjectPath:='';
  SrcPath:='';
  DebugPath:='';

  // parsing
  FSyntaxMode:='ObjFPC';
  fAssemblerStyle := 0;
  fCStyleOp := true;
  fIncludeAssertionCode := false;
  fAllowLabel := true;
  fCPPInline := true;
  fCMacros := false;
  fInitConst := false;
  fStaticKeyword := false;
  fUseAnsiStr := true;

  // code generation
  fSmartLinkUnit := false;
  fIOChecks := false;
  fRangeChecks := false;
  fOverflowChecks := false;
  fStackChecks := false;
  fHeapSize := 0;
  fStackSize := 0;
  fVerifyObjMethodCall := false;
  fTargetOS := '';
  fTargetCPU := '';
  fTargetProc := '';
  fOptLevel := 1;
  fVarsInReg := false;
  fUncertainOpt := false;
  FSmallerCode := false;

  // linking
  fGenDebugInfo := True;
  fDebugInfoType := dsAuto;
  fUseLineInfoUnit := true;
  fUseHeaptrc := false;
  fTrashVariables := false;
  fUseValgrind := false;
  fGenGProfCode := false;
  fStripSymbols := false;
  fLinkSmart := false;
  fPassLinkerOpt := false;
  LinkerOptions := '';
  Win32GraphicApp := false;
  ExecutableType := cetProgram;

  // messages
  fShowErrors := true;
  fShowWarn := true;
  fShowNotes := true;
  fShowHints := true;
  fShowGenInfo := true;
  fShowLineNum := false;
  fShowAll := false;
  fShowDebugInfo := false;
  fShowUsedFiles := false;
  fShowTriedFiles := false;
  fShowCompProc := false;
  fShowCond := false;
  fShowExecInfo := false;
  fShowSummary := false;
  fShowHintsForUnusedUnitsInMainSrc := false;
  fShowHintsForSenderNotUsed := false;
  fWriteFPCLogo := true;
  fStopAfterErrCount := 1;
  fMessageFlags.Clear;

  // other
  fDontUseConfigFile := false;
  fCustomConfigFile := false;
  fConfigFilePath := 'extrafpc.cfg';
  CustomOptions := '';

  // inherited
  ClearInheritedOptions;

  // compilation
  CompilerPath := DefaultCompilerPath;
  fExecuteBefore.Clear;
  fExecuteAfter.Clear;

  Modified := false;
end;

procedure TBaseCompilerOptions.Assign(Source: TPersistent);
var
  CompOpts: TBaseCompilerOptions;
begin
  if not (Source is TBaseCompilerOptions) then begin
    inherited Assign(Source);
    exit;
  end;
  CompOpts:=TBaseCompilerOptions(Source);

  // Target
  TargetFilename := CompOpts.TargetFilename;
  TargetFilenameApplyConventions := CompOpts.TargetFilenameApplyConventions;

  // Search Paths
  StorePathDelim := CompOpts.StorePathDelim;
  IncludePath := CompOpts.IncludePath;
  Libraries := CompOpts.Libraries;
  OtherUnitFiles := CompOpts.OtherUnitFiles;
  UnitOutputDirectory := CompOpts.UnitOutputDirectory;
  ObjectPath := CompOpts.ObjectPath;
  SrcPath := CompOpts.SrcPath;
  DebugPath := CompOpts.DebugPath;

  // conditionals
  Conditionals:=CompOpts.Conditionals;
  TIDEBuildMacros(BuildMacros).Assign(CompOpts.BuildMacros);

  // Parsing
  FSyntaxMode := CompOpts.FSyntaxMode;
  fAssemblerStyle := CompOpts.fAssemblerStyle;
  fCStyleOp := CompOpts.fCStyleOp;
  fIncludeAssertionCode := CompOpts.fIncludeAssertionCode;
  fAllowLabel := CompOpts.fAllowLabel;
  fCPPInline := CompOpts.fCPPInline;
  fCMacros := CompOpts.fCMacros;
  fInitConst := CompOpts.fInitConst;
  fStaticKeyword := CompOpts.fStaticKeyword;
  fUseAnsiStr := CompOpts.fUseAnsiStr;

  // Code Generation
  fSmartLinkUnit := CompOpts.SmartLinkUnit;
  fRelocatableUnit := CompOpts.RelocatableUnit;
  fIOChecks := CompOpts.fIOChecks;
  fRangeChecks := CompOpts.fRangeChecks;
  fOverflowChecks := CompOpts.fOverflowChecks;
  fStackChecks := CompOpts.fStackChecks;
  FEmulatedFloatOpcodes := CompOpts.fEmulatedFloatOpcodes;
  fHeapSize := CompOpts.fHeapSize;
  fStackSize := CompOpts.fStackSize;
  fVerifyObjMethodCall := CompOpts.VerifyObjMethodCall;
  fEmulatedFloatOpcodes := CompOpts.fEmulatedFloatOpcodes;
  fTargetOS := CompOpts.fTargetOS;
  fTargetCPU := CompOpts.fTargetCPU;
  fTargetProc := CompOpts.fTargetProc;
  fOptLevel := CompOpts.fOptLevel;
  fVarsInReg := CompOpts.fVarsInReg;
  fUncertainOpt := CompOpts.fUncertainOpt;
  FSmallerCode := CompOpts.FSmallerCode;

  // Linking
  fGenDebugInfo := CompOpts.fGenDebugInfo;
  FDebugInfoType := CompOpts.FDebugInfoType;
  fUseLineInfoUnit := CompOpts.fUseLineInfoUnit;
  fUseHeaptrc := CompOpts.fUseHeaptrc;
  fTrashVariables := CompOpts.fTrashVariables;
  fUseValgrind := CompOpts.fUseValgrind;
  fGenGProfCode := CompOpts.fGenGProfCode;
  fStripSymbols := CompOpts.fStripSymbols;
  fLinkSmart := CompOpts.fLinkSmart;
  fPassLinkerOpt := CompOpts.fPassLinkerOpt;
  LinkerOptions := CompOpts.fLinkerOptions;
  Win32GraphicApp := CompOpts.Win32GraphicApp;
  ExecutableType := CompOpts.ExecutableType;
  UseExternalDbgSyms := CompOpts.UseExternalDbgSyms;

  // Verbosity
  fShowErrors := CompOpts.fShowErrors;
  fShowWarn := CompOpts.fShowWarn;
  fShowNotes := CompOpts.fShowNotes;
  fShowHints := CompOpts.fShowHints;
  fShowGenInfo := CompOpts.fShowGenInfo;
  fShowLineNum := CompOpts.fShowLineNum;
  fShowAll := CompOpts.fShowAll;
  fShowDebugInfo := CompOpts.fShowDebugInfo;
  fShowUsedFiles := CompOpts.fShowUsedFiles;
  fShowTriedFiles := CompOpts.fShowTriedFiles;
  fShowCompProc := CompOpts.fShowCompProc;
  fShowCond := CompOpts.fShowCond;
  fShowExecInfo := CompOpts.fShowExecInfo;
  fShowSummary := CompOpts.FShowSummary;
  fShowHintsForUnusedUnitsInMainSrc := CompOpts.fShowHintsForUnusedUnitsInMainSrc;
  fShowHintsForSenderNotUsed := CompOpts.fShowHintsForSenderNotUsed;
  fWriteFPCLogo := CompOpts.fWriteFPCLogo;

  // Messages
  fMessageFlags.Assign(CompOpts.fMessageFlags);

  // Other
  fDontUseConfigFile := CompOpts.fDontUseConfigFile;
  fCustomConfigFile := CompOpts.fCustomConfigFile;
  fConfigFilePath := CompOpts.fConfigFilePath;
  fStopAfterErrCount := CompOpts.fStopAfterErrCount;
  CustomOptions := CompOpts.CustomOptions;

  // Inherited and parser options
  FDefaultMakeOptionsFlags := CompOpts.FDefaultMakeOptionsFlags;
  ClearInheritedOptions;
  ParsedOpts.Assign(CompOpts.ParsedOpts);
  FStorePathDelim := CompOpts.FStorePathDelim;
  FOtherDefines.Assign(CompOpts.FOtherDefines);

  // compilation
  CompilerPath := CompOpts.CompilerPath;
  ExecuteBefore.Assign(CompOpts.ExecuteBefore);
  ExecuteAfter.Assign(CompOpts.ExecuteAfter);
  CreateMakefileOnBuild:=CompOpts.CreateMakefileOnBuild;
end;

function TBaseCompilerOptions.IsEqual(CompOpts: TBaseCompilerOptions): boolean;
begin
  Result:= not CreateDiff(CompOpts,nil);
end;

procedure TBaseCompilerOptions.CreateDiffAsText(CompOpts: TBaseCompilerOptions;
  Diff: TStrings);
var
  Tool: TCompilerDiffTool;
begin
  Tool:=TCompilerDiffTool.Create(Diff);
  CreateDiff(CompOpts,Tool);
  Tool.Free;
end;

function TBaseCompilerOptions.CreateDiff(CompOpts: TBaseCompilerOptions;
  Tool: TCompilerDiffTool): boolean;

  function Done(Diff: boolean): boolean;
  begin
    if Diff then CreateDiff:=true;
    Result:=(Tool=nil) and Diff;
  end;

  function AddDiff(const PropertyName: string;
    const Old, New: TCompilationExecutableType): boolean;
  begin
    if Old=New then exit(false);
    Result:=true;
    Tool.AddDiffItem(PropertyName,CompilationExecutableTypeNames[New]);
  end;

begin
  Result:=false;
  //if Tool<>nil then debugln(['TBaseCompilerOptions.CreateDiff ',DbgSName(Self)]);
  if Done(Tool.AddPathsDiff('StorePathDelim',PathDelimSwitchToDelim[FStorePathDelim],
                            PathDelimSwitchToDelim[CompOpts.FStorePathDelim])) then exit;

  // target
  if Done(Tool.AddDiff('TargetFileExt',fTargetFileExt,CompOpts.fTargetFileExt)) then exit;
  if Done(Tool.AddDiff('TargetFilename',fTargetFilename,CompOpts.fTargetFilename)) then exit;
  if Done(Tool.AddDiff('TargetFilenameAppplyConventions',FTargetFilenameAppplyConventions,CompOpts.FTargetFilenameAppplyConventions)) then exit;

  // search paths
  if Tool<>nil then Tool.Path:='Paths';
  if Done(Tool.AddPathsDiff('IncludePaths',IncludePath,CompOpts.IncludePath)) then exit;
  if Done(Tool.AddPathsDiff('LibraryPaths',Libraries,CompOpts.Libraries)) then exit;
  if Done(Tool.AddPathsDiff('Namespaces',Namespaces,CompOpts.Namespaces)) then exit;
  if Done(Tool.AddPathsDiff('UnitPaths',OtherUnitFiles,CompOpts.OtherUnitFiles)) then exit;
  if Done(Tool.AddPathsDiff('UnitOutputDir',UnitOutputDirectory,CompOpts.UnitOutputDirectory)) then exit;
  if Done(Tool.AddPathsDiff('ObjectPath',ObjectPath,CompOpts.ObjectPath)) then exit;
  if Done(Tool.AddPathsDiff('SrcPath',SrcPath,CompOpts.SrcPath)) then exit;
  if Done(Tool.AddPathsDiff('DebugPath',DebugPath,CompOpts.DebugPath)) then exit;

  // conditionals
  if Done(Tool.AddPathsDiff('Conditionals',FConditionals,CompOpts.FConditionals)) then exit;
  if Tool<>nil then Tool.Path:='BuildModes';
  if Done(TIDEBuildMacros(fBuildMacros).CreateDiff(CompOpts.BuildMacros,Tool)) then exit;

  // parsing
  if Tool<>nil then Tool.Path:='Parsing';
  if Done(Tool.AddDiff('SyntaxMode',FSyntaxMode,CompOpts.FSyntaxMode)) then exit;
  if Done(Tool.AddDiff('AssemblerStyle',fAssemblerStyle,CompOpts.fAssemblerStyle)) then exit;
  if Done(Tool.AddDiff('CStyleOp',fCStyleOp,CompOpts.fCStyleOp)) then exit;
  if Done(Tool.AddDiff('IncludeAssertionCode',fIncludeAssertionCode,CompOpts.fIncludeAssertionCode)) then exit;
  if Done(Tool.AddDiff('AllowLabel',fAllowLabel,CompOpts.fAllowLabel)) then exit;
  if Done(Tool.AddDiff('CPPInline',fCPPInline,CompOpts.fCPPInline)) then exit;
  if Done(Tool.AddDiff('CMacros',fCMacros,CompOpts.fCMacros)) then exit;
  if Done(Tool.AddDiff('InitConst',fInitConst,CompOpts.fInitConst)) then exit;
  if Done(Tool.AddDiff('StaticKeyword',fStaticKeyword,CompOpts.fStaticKeyword)) then exit;
  if Done(Tool.AddDiff('UseAnsiStr',fUseAnsiStr,CompOpts.fUseAnsiStr)) then exit;

  // code generation
  if Tool<>nil then Tool.Path:='Code';
  if Done(Tool.AddDiff('SmartLinkUnit',fSmartLinkUnit,CompOpts.SmartLinkUnit)) then exit;
  if Done(Tool.AddDiff('Relocatable',fRelocatableUnit,CompOpts.RelocatableUnit)) then exit;
  if Done(Tool.AddDiff('IOChecks',fIOChecks,CompOpts.fIOChecks)) then exit;
  if Done(Tool.AddDiff('RangeChecks',fRangeChecks,CompOpts.fRangeChecks)) then exit;
  if Done(Tool.AddDiff('OverflowChecks',fOverflowChecks,CompOpts.fOverflowChecks)) then exit;
  if Done(Tool.AddDiff('StackChecks',fStackChecks,CompOpts.fStackChecks)) then exit;
  if Done(Tool.AddDiff('EmulatedFloatOpcodes',FEmulatedFloatOpcodes,CompOpts.FEmulatedFloatOpcodes)) then exit;
  if Done(Tool.AddDiff('HeapSize',fHeapSize,CompOpts.fHeapSize)) then exit;
  if Done(Tool.AddDiff('StackSize',fStackSize,CompOpts.fStackSize)) then exit;
  if Done(Tool.AddDiff('VerifyObjMethodCall',fVerifyObjMethodCall,CompOpts.fVerifyObjMethodCall)) then exit;
  if Done(Tool.AddDiff('EmulatedFloatOpcodes',fEmulatedFloatOpcodes,CompOpts.fEmulatedFloatOpcodes)) then exit;
  if Done(Tool.AddDiff('TargetOS',fTargetOS,CompOpts.fTargetOS)) then exit;
  if Done(Tool.AddDiff('TargetCPU',fTargetCPU,CompOpts.fTargetCPU)) then exit;
  if Done(Tool.AddDiff('TargetProc',fTargetProc,CompOpts.fTargetProc)) then exit;
  if Done(Tool.AddDiff('OptLevel',fOptLevel,CompOpts.fOptLevel)) then exit;
  if Done(Tool.AddDiff('VarsInReg',fVarsInReg,CompOpts.fVarsInReg)) then exit;
  if Done(Tool.AddDiff('UncertainOpt',fUncertainOpt,CompOpts.fUncertainOpt)) then exit;
  if Done(Tool.AddDiff('SmallerCode',FSmallerCode,CompOpts.FSmallerCode)) then exit;

  // linking
  if Tool<>nil then Tool.Path:='Linking';
  if Done(Tool.AddDiff('GenDebugInfo',fGenDebugInfo,CompOpts.fGenDebugInfo)) then exit;
  if Done(Tool.AddDiff('DebugInfoType',DebugInfoTypeStr,CompOpts.DebugInfoTypeStr)) then exit;
  if Done(Tool.AddDiff('UseLineInfoUnit',fUseLineInfoUnit,CompOpts.fUseLineInfoUnit)) then exit;
  if Done(Tool.AddDiff('UseHeaptrc',fUseHeaptrc,CompOpts.fUseHeaptrc)) then exit;
  if Done(Tool.AddDiff('TrashVariables',fTrashVariables,CompOpts.fTrashVariables)) then exit;
  if Done(Tool.AddDiff('UseValgrind',fUseValgrind,CompOpts.fUseValgrind)) then exit;
  if Done(Tool.AddDiff('GenGProfCode',fGenGProfCode,CompOpts.fGenGProfCode)) then exit;
  if Done(Tool.AddDiff('StripSymbols',fStripSymbols,CompOpts.fStripSymbols)) then exit;
  if Done(Tool.AddDiff('LinkSmart',fLinkSmart,CompOpts.fLinkSmart)) then exit;
  if Done(Tool.AddDiff('PassLinkerOpt',fPassLinkerOpt,CompOpts.fPassLinkerOpt)) then exit;
  if Done(Tool.AddDiff('LinkerOptions',fLinkerOptions,CompOpts.fLinkerOptions)) then exit;
  if Done(Tool.AddDiff('Win32GraphicApp',FWin32GraphicApp,CompOpts.FWin32GraphicApp)) then exit;
  if Done(AddDiff('ExecutableType',FExecutableType,CompOpts.FExecutableType)) then exit;

  // verbosity
  if Tool<>nil then Tool.Path:='Verbosity';
  if Done(Tool.AddDiff('ShowErrors',fShowErrors,CompOpts.fShowErrors)) then exit;
  if Done(Tool.AddDiff('ShowWarn',fShowWarn,CompOpts.fShowWarn)) then exit;
  if Done(Tool.AddDiff('ShowNotes',fShowNotes,CompOpts.fShowNotes)) then exit;
  if Done(Tool.AddDiff('ShowHints',fShowHints,CompOpts.fShowHints)) then exit;
  if Done(Tool.AddDiff('ShowGenInfo',fShowGenInfo,CompOpts.fShowGenInfo)) then exit;
  if Done(Tool.AddDiff('ShowLineNum',fShowLineNum,CompOpts.fShowLineNum)) then exit;
  if Done(Tool.AddDiff('ShowAll',fShowAll,CompOpts.fShowAll)) then exit;
  if Done(Tool.AddDiff('ShowDebugInfo',fShowDebugInfo,CompOpts.fShowDebugInfo)) then exit;
  if Done(Tool.AddDiff('ShowUsedFiles',fShowUsedFiles,CompOpts.fShowUsedFiles)) then exit;
  if Done(Tool.AddDiff('ShowTriedFiles',fShowTriedFiles,CompOpts.fShowTriedFiles)) then exit;
  if Done(Tool.AddDiff('ShowCompProc',fShowCompProc,CompOpts.fShowCompProc)) then exit;
  if Done(Tool.AddDiff('ShowCond',fShowCond,CompOpts.fShowCond)) then exit;
  if Done(Tool.AddDiff('ShowExecInfo',fShowExecInfo,CompOpts.fShowExecInfo)) then exit;
  if Done(Tool.AddDiff('ShowSummary',fShowSummary,CompOpts.fShowSummary)) then exit;
  if Done(Tool.AddDiff('ShowHintsForUnusedUnitsInMainSrc',fShowHintsForUnusedUnitsInMainSrc,CompOpts.fShowHintsForUnusedUnitsInMainSrc)) then exit;
  if Done(Tool.AddDiff('ShowHintsForSenderNotUsed',fShowHintsForSenderNotUsed,CompOpts.fShowHintsForSenderNotUsed)) then exit;
  if Done(Tool.AddDiff('WriteFPCLogo',fWriteFPCLogo,CompOpts.fWriteFPCLogo)) then exit;

  // messages
  if Tool<>nil then Tool.Path:='Messages';
  if Done(IDEMessageFlags.CreateDiff(Tool,CompOpts.IDEMessageFlags)) then exit;

  // other
  if Tool<>nil then Tool.Path:='Other';
  if Done(Tool.AddDiff('DontUseConfigFile',fDontUseConfigFile,CompOpts.fDontUseConfigFile)) then exit;
  if Done(Tool.AddDiff('CustomConfigFile',fCustomConfigFile,CompOpts.fCustomConfigFile)) then exit;
  if Done(Tool.AddDiff('ConfigFilePath',fConfigFilePath,CompOpts.fConfigFilePath)) then exit;
  if Done(Tool.AddDiff('StopAfterErrCount',fStopAfterErrCount,CompOpts.fStopAfterErrCount)) then exit;
  if Done(Tool.AddDiff('CustomOptions',CustomOptions,CompOpts.CustomOptions)) then exit;
  if Done(Tool.AddDiff('OtherDefines',OtherDefines.Text,CompOpts.OtherDefines.Text)) then exit;

  // compilation
  if Tool<>nil then Tool.Path:='Compilation';
  if Done(Tool.AddDiff('CompilerPath',CompilerPath,CompOpts.CompilerPath)) then exit;
  if Done(ExecuteBefore.CreateDiff(CompOpts.ExecuteBefore,Tool)) then exit;
  if Done(ExecuteAfter.CreateDiff(CompOpts.ExecuteAfter,Tool)) then exit;
  if Done(Tool.AddDiff('CreateMakefileOnBuild',fCreateMakefileOnBuild,CompOpts.fCreateMakefileOnBuild)) then exit;
  if Result then debugln(['TBaseCompilerOptions.CreateDiff END']);
end;

procedure TBaseCompilerOptions.SetAlternativeCompile(const Command: string;
  ScanFPCMsgs: boolean);
begin
  CompilerPath:='';
  ExecuteBefore.Command:=Command;
  if ScanFPCMsgs then
    ExecuteBefore.Parsers.Text:=SubToolFPC+LineEnding+SubToolMake
  else
    ExecuteBefore.Parsers.Clear;
end;

function TBaseCompilerOptions.GetExecuteBeforeCommand: string;
begin
  Result := ExecuteBefore.Command;
end;

function TBaseCompilerOptions.GetExecuteAfterCommand: string;
begin
  Result := ExecuteAfter.Command;
end;

procedure TBaseCompilerOptions.SetExecuteBeforeCommand(const ACommand: string);
begin
  ExecuteBefore.Command := ACommand;
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetExecuteAfterCommand(const ACommand: string);
begin
  ExecuteAfter.Command := ACommand;
  IncreaseChangeStamp;
end;


{ TAdditionalCompilerOptions }

procedure TAdditionalCompilerOptions.SetCustomOptions(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosCustomOptions,AValue);
end;

procedure TAdditionalCompilerOptions.SetSrcPath(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosSrcPath,AValue);
end;

function TAdditionalCompilerOptions.GetUnitPath: string;
begin
  Result:=FParsedOpts.Values[pcosUnitPath].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetIncludePath: string;
begin
  Result:=FParsedOpts.Values[pcosIncludePath].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetBaseDirectory: string;
begin
  Result:=FParsedOpts.Values[pcosBaseDir].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetCustomOptions: string;
begin
  Result:=FParsedOpts.Values[pcosCustomOptions].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetLibraryPath: string;
begin
  Result:=FParsedOpts.Values[pcosLibraryPath].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetLinkerOptions: string;
begin
  Result:=FParsedOpts.Values[pcosLinkerOptions].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetNamespaces: string;
begin
  Result:=FParsedOpts.Values[pcosNamespaces].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetObjectPath: string;
begin
  Result:=FParsedOpts.Values[pcosObjectPath].UnparsedValue;
end;

function TAdditionalCompilerOptions.GetSrcPath: string;
begin
  Result:=FParsedOpts.Values[pcosSrcPath].UnparsedValue;
end;

procedure TAdditionalCompilerOptions.SetBaseDirectory(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosBaseDir,AValue);
end;

procedure TAdditionalCompilerOptions.SetIncludePath(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosIncludePath,AValue);
end;

procedure TAdditionalCompilerOptions.SetLibraryPath(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosLibraryPath,AValue);
end;

procedure TAdditionalCompilerOptions.SetLinkerOptions(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosLinkerOptions,AValue);
end;

procedure TAdditionalCompilerOptions.SetNamespaces(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosNamespaces,AValue);
end;

procedure TAdditionalCompilerOptions.SetObjectPath(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosObjectPath,AValue);
end;

procedure TAdditionalCompilerOptions.SetUnitPath(const AValue: string);
begin
  ParsedOpts.SetUnparsedValue(pcosUnitPath,AValue);
end;

constructor TAdditionalCompilerOptions.Create(TheOwner: TObject);
begin
  fOwner:=TheOwner;
  FParsedOpts:=TParsedCompilerOptions.Create(Self);
  Clear;
end;

destructor TAdditionalCompilerOptions.Destroy;
begin
  FreeThenNil(FParsedOpts);
  inherited Destroy;
end;

procedure TAdditionalCompilerOptions.Clear;
begin
  UnitPath:='';
  Namespaces:='';
  SrcPath:='';
  IncludePath:='';
  CustomOptions:='';
  LibraryPath:='';
  LinkerOptions:='';
  ObjectPath:='';
end;

procedure TAdditionalCompilerOptions.AssignOptions(Source: TObject);
var
  Src: TAdditionalCompilerOptions;
begin
  if not (Source is TAdditionalCompilerOptions) then
    raise Exception.Create('TAdditionalCompilerOptions.AssignOptions: Can not copy from '+DbgSName(Source));
  Src:=TAdditionalCompilerOptions(Source);
  UnitPath:=Src.UnitPath;
  Namespaces:=Src.Namespaces;
  IncludePath:=Src.IncludePath;
  SrcPath:=Src.SrcPath;
  ObjectPath:=Src.ObjectPath;
  LibraryPath:=Src.LibraryPath;
  LinkerOptions:=Src.LinkerOptions;
  CustomOptions:=Src.CustomOptions;
  BaseDirectory:=Src.BaseDirectory;
end;

procedure TAdditionalCompilerOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; AdjustPathDelims: boolean);

  function f(const Filename: string): string;
  begin
    Result:=SwitchPathDelims(Filename,AdjustPathDelims);
  end;

begin
  Clear;
  CustomOptions:=f(XMLConfig.GetValue(Path+'CustomOptions/Value',''));
  IncludePath:=f(XMLConfig.GetValue(Path+'IncludePath/Value',''));
  LibraryPath:=f(XMLConfig.GetValue(Path+'LibraryPath/Value',''));
  LinkerOptions:=f(XMLConfig.GetValue(Path+'LinkerOptions/Value',''));
  Namespaces:=f(XMLConfig.GetValue(Path+'Namespaces/Value',''));
  ObjectPath:=f(XMLConfig.GetValue(Path+'ObjectPath/Value',''));
  UnitPath:=f(XMLConfig.GetValue(Path+'UnitPath/Value',''));
  SrcPath:=f(XMLConfig.GetValue(Path+'SrcPath/Value',''));
end;

procedure TAdditionalCompilerOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

begin
  XMLConfig.SetDeleteValue(Path+'CustomOptions/Value',f(CustomOptions),'');
  XMLConfig.SetDeleteValue(Path+'IncludePath/Value',f(IncludePath),'');
  XMLConfig.SetDeleteValue(Path+'LibraryPath/Value',f(LibraryPath),'');
  XMLConfig.SetDeleteValue(Path+'LinkerOptions/Value',f(LinkerOptions),'');
  XMLConfig.SetDeleteValue(Path+'Namespaces/Value',Namespaces,'');
  XMLConfig.SetDeleteValue(Path+'ObjectPath/Value',f(ObjectPath),'');
  XMLConfig.SetDeleteValue(Path+'UnitPath/Value',f(UnitPath),'');
  XMLConfig.SetDeleteValue(Path+'SrcPath/Value',f(SrcPath),'');
end;

function TAdditionalCompilerOptions.GetOwnerName: string;
begin
  if fOwner<>nil then
    Result:=fOwner.Classname
  else
    Result:='Has no owner';
end;

function TAdditionalCompilerOptions.GetOption(AnOption: TInheritedCompilerOption
  ): string;
begin
  Result:='';
  case AnOption of
    icoNone: Result:='';
    icoUnitPath: Result:=UnitPath;
    icoNamespaces: Result:=Namespaces;
    icoIncludePath: Result:=IncludePath;
    icoObjectPath: Result:=ObjectPath;
    icoLibraryPath: Result:=LibraryPath;
    icoSrcPath: Result:=SrcPath;
    icoLinkerOptions: Result:=LinkerOptions;
    icoCustomOptions: Result:=CustomOptions;
  else
    RaiseGDBException(''); // inconsistency detected
  end;
end;

function TAdditionalCompilerOptions.GetBaseCompilerOptions: TBaseCompilerOptions;
begin
  Result:=nil;
end;

{ TParsedCompilerOptions }

procedure TParsedCompilerOptions.SetOutputDirectoryOverride(const AValue: string);
begin
  if FOutputDirectoryOverride=AValue then exit;
  FOutputDirectoryOverride:=AValue;
  if InvalidateParseOnChange then
    IncreaseCompilerParseStamp;// the output dir is used by other packages
  if FOutputDirectoryOverride<>'' then
    DebugLn(['TParsedCompilerOptions.SetOutputDirectoryOverride New=',FOutputDirectoryOverride])
  else
    DebugLn(['TParsedCompilerOptions.SetOutputDirectoryOverride using default']);
end;

constructor TParsedCompilerOptions.Create(TheOwner: TObject);
begin
  FOwner:=TheOwner;
  InheritedMacroValues:=TCTCfgScriptVariables.Create;
  MacroValues:=TIDECfgScriptEngine.Create;
  Clear;
end;

destructor TParsedCompilerOptions.Destroy;
begin
  FreeAndNil(InheritedMacroValues);
  FreeAndNil(MacroValues);
  inherited Destroy;
end;

function TParsedCompilerOptions.HasParsedError: boolean;
begin
  Result:=(ParsedErrorStamp<>CTInvalidChangeStamp)
      and (ParsedErrorStamp=CompilerParseStamp);
end;

procedure TParsedCompilerOptions.ParsedError(Option: TParsedCompilerOptString;
  Msg: string);
begin
  if HasParsedError then exit;
  ParsedErrorMsg:=Msg;
  ParsedErrorOption:=Option;
  ParsedErrorStamp:=CompilerParseStamp;
end;

function TParsedCompilerOptions.GetUnparsedWithConditionals(
  Option: TParsedCompilerOptString): string;
var
  Opts: TBaseCompilerOptions;
  VarName: String;
  Vars: TCTCfgScriptVariables;
  MoreOptions: String;
begin
  Result:=Values[Option].UnparsedValue;
  Opts:=nil;
  VarName:='';
  if (Owner is TBaseCompilerOptions) then
  begin
    Opts:=TBaseCompilerOptions(Owner);
    VarName:=ParsedCompilerOptsVars[Option];
  end else if (Owner is TAdditionalCompilerOptions) then
  begin
    Opts:=TAdditionalCompilerOptions(Owner).GetBaseCompilerOptions;
    VarName:=ParsedCompilerOptsUsageVars[Option];
  end;
  if (VarName='') or (Opts=nil) then exit;
  Vars:=GetBuildMacroValues(Opts,true);
  if Vars=nil then exit;
  case Option of
  pcosUnitPath,pcosIncludePath,pcosObjectPath,pcosLibraryPath,pcosSrcPath,
  pcosDebugPath:
    Result:=MergeSearchPaths(Result,GetForcedPathDelims(Vars[VarName]));
  pcosLinkerOptions:
    Result:=MergeLinkerOptions(Result,Vars[VarName]);
  pcosNamespaces:
    Result:=MergeWithDelimiter(Result,Vars[VarName],';');
  pcosCustomOptions:
    begin
      Result:=MergeCustomOptions(Result,Vars[VarName]);
      // add project/global overrides
      if (Owner is TBaseCompilerOptions) and Assigned(OnAppendCustomOption) then
      begin
        MoreOptions:='';
        OnAppendCustomOption(Opts,MoreOptions,bmgtAll);
        if Assigned(OnLocalSubstitute) then
          MoreOptions:=OnLocalSubstitute(MoreOptions,false);
        MoreOptions:=SpecialCharsToSpaces(MoreOptions,true);
        Result:=MergeCustomOptions(Result,MoreOptions);
      end;
    end;
  pcosOutputDir,pcosCompilerPath:
    if Vars.IsDefined(PChar(VarName)) then
      Result:=GetForcedPathDelims(Vars[VarName]);
  end
end;

function TParsedCompilerOptions.GetParsedValue(Option: TParsedCompilerOptString;
  WithOverrides: boolean): string;
var
  s: String;
begin
  if WithOverrides then begin
    if (Option=pcosOutputDir) and (OutputDirectoryOverride<>'') then begin
      Result:=OutputDirectoryOverride;
      exit;
    end;
  end;
  if Values[Option].ParseStamp<>CompilerParseStamp then begin
    if Values[Option].Parsing then begin
      DebugLn('TParsedCompilerOptions.GetParsedValue Circle in Options: ',EnumToStr(Option),' Unparsed="',Values[Option].UnparsedValue,'"');
      ParsedError(Option, lisEndlessLoopInMacros);
      exit('');
    end;
    Values[Option].Parsing:=true;
    try
      s:=DoParseOption(GetUnparsedWithConditionals(Option),Option,false);
      Values[Option].ParsedValue:=s;
      Values[Option].ParseStamp:=CompilerParseStamp;
    finally
      Values[Option].Parsing:=false;
    end;
  end;
  Result:=Values[Option].ParsedValue;
end;

function TParsedCompilerOptions.GetParsedPIValue(
  Option: TParsedCompilerOptString): string;
var
  s: String;
begin
  if ParsedPIStamp[Option]<>CompilerParseStamp then begin
    if ParsingPI[Option] then begin
      DebugLn('TParsedCompilerOptions.GetParsedPIValue Circle in Options: ',EnumToStr(Option));
      exit('');
    end;
    ParsingPI[Option]:=true;
    try
      s:=DoParseOption(GetUnparsedWithConditionals(Option),Option,true);
      ParsedPIValues[Option]:=s;
      ParsedPIStamp[Option]:=CompilerParseStamp;
      //if Option=pcosCustomOptions then begin
      //  DebugLn('TParsedCompilerOptions.GetParsedValue PARSED ',dbgs(ParsedStamp[Option]),' ',dbgs(CompilerParseStamp),' new="',ParsedValues[Option],'"');
      //end;
    finally
      ParsingPI[Option]:=false;
    end;
  end;
  Result:=ParsedPIValues[Option];
end;

procedure TParsedCompilerOptions.SetUnparsedValue(
  Option: TParsedCompilerOptString; const NewValue: string);
begin
  if NewValue=Values[Option].UnparsedValue then exit;
  if InvalidateParseOnChange then IncreaseCompilerParseStamp;
  if Option=pcosBaseDir then
    InvalidateFiles
  else begin
    Values[Option].ParseStamp:=CTInvalidChangeStamp;
    ParsedPIStamp[Option]:=CTInvalidChangeStamp;
  end;
  Values[Option].UnparsedValue:=NewValue;
end;

function TParsedCompilerOptions.DoParseOption(const OptionText: string;
  Option: TParsedCompilerOptString; PlatformIndependent: boolean): string;

  function GetBaseDir: string;
  begin
    if PlatformIndependent then
      Result:=GetParsedPIValue(pcosBaseDir)
    else
      Result:=GetParsedValue(pcosBaseDir);
    if Result='' then
      Result:=EnvironmentOptions.GetParsedTestBuildDirectory;
  end;

  procedure MakeFilenameAbsolute(var aFilename: string);
  var
    BaseDirectory: String;
  begin
    aFilename:=TrimFilename(aFilename);
    if (aFilename<>'') and (not FilenameIsAbsolute(aFilename)) then begin
      BaseDirectory:=GetBaseDir;
      if (BaseDirectory<>'') then aFilename:=TrimFilename(BaseDirectory+aFilename);
    end;
  end;

var
  s: String;
  BaseDirectory, h: String;
begin
  s:=OptionText;

  // apply overrides
  if not PlatformIndependent then begin
    if Option=pcosOutputDir then begin
      if Assigned(OnGetOutputDirectoryOverride) then
        OnGetOutputDirectoryOverride(Self,s,bmgtAll);
    end;
  end;

  // parse locally (macros depending on owner, like pkgdir and build macros)
  if Assigned(OnLocalSubstitute) then
  begin
    //DebugLn(['TParsedCompilerOptions.DoParseOption local "',s,'" ...']);
    s:=OnLocalSubstitute(s,PlatformIndependent)
  end else
  begin
    //DebugLn(['TParsedCompilerOptions.DoParseOption global "',s,'" ...']);
    s:=ParseString(Self,s,PlatformIndependent);
  end;
  //DebugLn(['TParsedCompilerOptions.DoParseOption complete "',s,'" ...']);
  // improve
  if Option=pcosBaseDir then
    // base directory
    s:=AppendPathDelim(TrimFilename(s))
  else if Option in ParsedCompilerFilenames then
  begin
    // make filename absolute
    if ExtractFilePath(s)='' then begin
      h:=FileUtil.FindDefaultExecutablePath(s,GetBaseDir);
      if h<>'' then s:=h;
    end;
    MakeFilenameAbsolute(s);
  end
  else if Option in ParsedCompilerDirectories then
  begin
    // make directory absolute
    s:=TrimFilename(s);
    if Option<>pcosBaseDir then
      MakeFilenameAbsolute(s);
    s:=AppendPathDelim(s);
  end
  else if Option in ParsedCompilerSearchPaths then
  begin
    // make search paths absolute
    BaseDirectory:=GetBaseDir;
    s:=TrimSearchPath(s,BaseDirectory);
  end else if Option=pcosCustomOptions then begin
    s:=SpecialCharsToSpaces(s,true);
  end;
  Result:=s;
end;

procedure TParsedCompilerOptions.Assign(Src: TParsedCompilerOptions);
begin
  FInvalidateParseOnChange := Src.FInvalidateParseOnChange;
//  FOnLocalSubstitute := Src.FOnLocalSubstitute;
  FOutputDirectoryOverride := Src.FOutputDirectoryOverride;
  Values := Src.Values;
  ParsedErrorOption := Src.ParsedErrorOption;
  ParsedErrorMsg := Src.ParsedErrorMsg;
  ParsedErrorStamp := Src.ParsedErrorStamp;
  // parsed except for platform macros
  ParsedPIValues := Src.ParsedPIValues;
  ParsedPIStamp := Src.ParsedPIStamp;
  ParsingPI := Src.ParsingPI;
  // macro values
//  InheritedMacroValues.Assign(Src.InheritedMacroValues);
  InheritedMacroValuesStamp := Src.InheritedMacroValuesStamp;
  InheritedMacroValuesParsing := Src.InheritedMacroValuesParsing;
//  MacroValues: TIDECfgScriptEngine;
  MacroValuesStamp := Src.MacroValuesStamp;
  MacroValuesParsing := Src.MacroValuesParsing;
end;

procedure TParsedCompilerOptions.Clear;
var
  Option: TParsedCompilerOptString;
begin
  InvalidateAll;
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
  begin
    Values[Option].ParsedValue:='';
    ParsedPIValues[Option]:='';
    Values[Option].UnparsedValue:='';
  end;
  InheritedMacroValues.Clear;
  MacroValues.Variables.Clear;
  MacroValues.ClearErrors;
end;

procedure TParsedCompilerOptions.InvalidateAll;
var
  Option: TParsedCompilerOptString;
begin
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
  begin
    Values[Option].ParseStamp:=CTInvalidChangeStamp;
    ParsedPIStamp[Option]:=CTInvalidChangeStamp;
  end;
  InheritedMacroValuesStamp:=CTInvalidChangeStamp;
  MacroValuesStamp:=CTInvalidChangeStamp;
  ParsedErrorStamp:=CTInvalidChangeStamp;
end;

procedure TParsedCompilerOptions.InvalidateFiles;
var
  Option: TParsedCompilerOptString;
begin
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
    if (Option in ParsedCompilerFiles) then begin
      Values[Option].ParseStamp:=CTInvalidChangeStamp;
      ParsedPIStamp[Option]:=CTInvalidChangeStamp;
    end;
end;

procedure TParsedCompilerOptions.RenameMacro(const OldName, NewName: string;
  out Changed: TParsedCompilerOptStrings);
var
  o: TParsedCompilerOptString;
  s: String;
begin
  Changed:=[];
  for o:=Low(Values) to High(Values) do
  begin
    s:=Values[o].UnparsedValue;
    RenameIDEMacroInString(s,OldName,NewName);
    if s<>Values[o].UnparsedValue then begin
      SetUnparsedValue(o,s);
      Include(Changed,o)
    end;
  end;
end;

{ TCompilationToolOptions }

procedure TCompilationToolOptions.SetCommand(const AValue: string);
begin
  if FCommand=AValue then exit;
  FCommand:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TCompilationToolOptions.SetCommand ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

function TCompilationToolOptions.GetHasParser(aParserName: string): boolean;
begin
  Result:=FParsers.IndexOf(aParserName)>=0;
end;

procedure TCompilationToolOptions.SetHasParser(aParserName: string;
  const AValue: boolean);
var
  i: Integer;
begin
  i:=FParsers.IndexOf(aParserName);
  if i>=0 then begin
    if AValue then exit;
    FParsers.Delete(i);
  end else begin
    if not AValue then exit;
    FParsers.Add(aParserName);
  end;
  IncreaseChangeStamp;
end;

procedure TCompilationToolOptions.SetParsers(const AValue: TStrings);
begin
  if FParsers.Equals(AValue) then Exit;
  {$IFDEF VerboseIDEModified}
  debugln(['TCompilationToolOptions.SetParsers ',AValue.Text]);
  {$ENDIF}
  FParsers.Assign(AValue);
  IncreaseChangeStamp;
end;

procedure TCompilationToolOptions.SubstituteMacros(var s: string);
begin
  IDEMacros.SubstituteMacros(s);
end;

constructor TCompilationToolOptions.Create(TheOwner: TObject);
begin
  FOwner:=TheOwner;
  FParsers:=TStringList.Create;
end;

destructor TCompilationToolOptions.Destroy;
begin
  FreeAndNil(FParsers);
  inherited Destroy;
end;

procedure TCompilationToolOptions.Clear;
begin
  Command:='';
  Parsers.Clear;
end;

procedure TCompilationToolOptions.Assign(Src: TCompilationToolOptions);
begin
  Command:=Src.Command;
  Parsers.Assign(Src.Parsers);
end;

procedure TCompilationToolOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
begin
  //debugln(['TCompilationToolOptions.LoadFromXMLConfig ',Command,' Path=',Path,' DoSwitchPathDelims=',DoSwitchPathDelims]);
  Command:=SwitchPathDelims(XMLConfig.GetValue(Path+'Command/Value',''),
                            DoSwitchPathDelims);
  LoadStringList(XMLConfig,Parsers,Path+'Parsers/');
  if Parsers.Count=0 then begin
    // read old format
    HasParser[SubToolFPC]:=XMLConfig.GetValue(Path+'ScanForFPCMsgs/Value',false);
    HasParser[SubToolMake]:=XMLConfig.GetValue(Path+'ScanForMakeMsgs/Value',false);
    HasParser[SubToolDefault]:=XMLConfig.GetValue(Path+'ShowAllMessages/Value',false);
  end;
end;

procedure TCompilationToolOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);
var
  i: Integer;
  s: String;
  NeedNewFormat: Boolean;
begin
  //debugln(['TCompilationToolOptions.SaveToXMLConfig ',Command,' Path=',Path]);
  XMLConfig.SetDeleteValue(Path+'Command/Value',
                           SwitchPathDelims(Command,UsePathDelim),'');

  // Parsers
  NeedNewFormat:=false;
  for i:=0 to Parsers.Count-1 do begin
    s:=Parsers[i];
    if (CompareText(s,SubToolFPC)=0)
    or (CompareText(s,SubToolMake)=0)
    or (CompareText(s,SubToolDefault)=0)
    then continue;
    NeedNewFormat:=true;
    break;
  end;
  if NeedNewFormat then
    SaveStringList(XMLConfig,Parsers,Path+'Parsers/')
  else begin
    // save backward compatible
    XMLConfig.SetDeleteValue(Path+'ScanForFPCMsgs/Value',
                             HasParser[SubToolFPC],false);
    XMLConfig.SetDeleteValue(Path+'ScanForMakeMsgs/Value',
                             HasParser[SubToolMake],false);
    XMLConfig.SetDeleteValue(Path+'ShowAllMessages/Value',
                             HasParser[SubToolDefault],false);
  end;
end;

function TCompilationToolOptions.CreateDiff(CompOpts: TCompilationToolOptions;
  Tool: TCompilerDiffTool): boolean;

  function Done(Diff: boolean): boolean;
  begin
    if Diff then CreateDiff:=true;
    Result:=(Tool=nil) and Diff;
  end;

begin
  Result:=false;
  if Done(Tool.AddDiff('Command',Command,CompOpts.Command)) then exit;
  if Done(Tool.AddStringsDiff('Parsers',Parsers,CompOpts.Parsers)) then exit;
end;

function TCompilationToolOptions.Execute(const WorkingDir, ToolTitle,
  CompileHint: string): TModalResult;
var
  ExtTool: TAbstractExternalTool;
begin
  if Command='' then exit(mrOk);
  if SourceEditorManagerIntf<>nil then
    SourceEditorManagerIntf.ClearErrorLines;

  ExtTool:=CreateExtTool(WorkingDir,ToolTitle,CompileHint);
  if ExtTool=nil then exit(mrOk);
  ExtTool.Reference(Self,ClassName);
  try
    // run
    ExtTool.Execute;
    ExtTool.WaitForExit;
    if ExtTool.ErrorMessage='' then
      Result:=mrOk
    else
      Result:=mrCancel;
  finally
    ExtTool.Release(Self);
  end;
end;

function TCompilationToolOptions.CreateExtTool(const WorkingDir, ToolTitle,
  CompileHint: string): TAbstractExternalTool;
var
  CurCommand: String;
  ProgramFilename: string;
  Params: string;
  Filename: String;
  ok: Boolean;
  i: Integer;
begin
  CurCommand:=GetParsedCommand;
  //debugln(['TCompilationToolOptions.CreateExtTool CurCommand=[',CurCommand,']']);
  if CurCommand='' then
    exit(nil);
  SplitCmdLine(CurCommand,ProgramFilename,Params);
  //debugln(['TCompilationToolOptions.CreateExtTool Prg=[',ProgramFilename,'] Params=[',Params,']']);
  if not FilenameIsAbsolute(ProgramFilename) then begin
    Filename:=FindProgram(ProgramFilename,WorkingDir,true);
    //debugln(['TCompilationToolOptions.CreateExtTool Found=[',Filename,']']);
    if Filename<>'' then ProgramFilename:=Filename;
  end;
  Result:=ExternalToolList.Add(ToolTitle);
  ok:=false;
  try
    Result.Hint:=CompileHint;
    Result.Process.CurrentDirectory:=WorkingDir;
    Result.Process.Executable:=ProgramFilename;
    Result.CmdLineParams:=Params;
    for i:=0 to Parsers.Count-1 do
      Result.AddParserByName(Parsers[i]);
    if Result.ParserCount=0 then
      Result.AddParsers(SubToolDefault);
    ok:=true;
  finally
    if not ok then
      FreeAndNil(Result);
  end;
end;

procedure TCompilationToolOptions.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp64(FChangeStamp);
  if assigned(OnChanged) then OnChanged(Self);
end;

function TCompilationToolOptions.GetParsedCommand: string;
begin
  if FParsedCommandStamp<>CompilerParseStamp then begin
    FParsedCommandStamp:=CompilerParseStamp;
    FParsedCommand:=Command;
    //debugln(['TCompilationToolOptions.GetParsedCommand Unparsed="',FParsedCommand,'"']);
    SubstituteMacros(FParsedCommand);
    //debugln(['TCompilationToolOptions.GetParsedCommand Parsed="',FParsedCommand,'"']);
  end;
  Result:=FParsedCommand;
end;

function TCompilationToolOptions.HasCommands: boolean;
begin
  Result:=true;
  if GetParsedCommand<>'' then exit;
  Result:=false;
end;

{ TIDEBuildMacro }

procedure TIDEBuildMacro.SetIdentifier(const AValue: string);
begin
  if FIdentifier=AValue then exit;
  if not IsValidIdent(AValue) then
    raise Exception.Create('TIDEBuildMacro.SetIdentifier invalid identifier: '+AValue);
  FIdentifier:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TIDEBuildMacro.SetIdentifier ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
  IncreaseBuildMacroChangeStamp;
end;

procedure TIDEBuildMacro.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  {$IFDEF VerboseIDEModified}
  debugln(['TIDEBuildMacro.SetDescription ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TIDEBuildMacro.SetValueDescriptions(const AValue: TStrings);
begin
  if (FValueDescriptions=AValue) or FValueDescriptions.Equals(AValue) then exit;
  FValueDescriptions.Assign(AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TIDEBuildMacro.SetValueDescriptions ',AValue.Text]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TIDEBuildMacro.SetValues(const AValue: TStrings);
begin
  if (FValues=AValue) or FValues.Equals(AValue) then exit;
  FValues.Assign(AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TIDEBuildMacro.SetValues ',AValue.Text]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

constructor TIDEBuildMacro.Create;
begin
  FChangeStamp:=CTInvalidChangeStamp;
  FValues:=TStringList.Create;
  FValueDescriptions:=TStringList.Create;
  FDefaultValue:='';
end;

destructor TIDEBuildMacro.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FValueDescriptions);
  inherited Destroy;
end;

procedure TIDEBuildMacro.Assign(Source: TLazBuildMacro);
begin
  Identifier:=Source.Identifier;
  Description:=Source.Description;
  ValueDescriptions:=Source.ValueDescriptions;
  Values:=Source.Values;
end;

function TIDEBuildMacro.Equals(Other: TLazBuildMacro): boolean;
begin
  Result:=false;
  if Identifier<>Other.Identifier then exit;
  if Description<>Other.Description then exit;
  if not Values.Equals(Other.Values) then exit;
  if not ValueDescriptions.Equals(Other.ValueDescriptions) then exit;
  Result:=true;
end;

procedure TIDEBuildMacro.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
begin
  FIdentifier:=AXMLConfig.GetValue(Path+'Identifier/Value','');
  if not IsValidIdent(FIdentifier) then FIdentifier:='';
  FDescription:=ConvertLineEndings(AXMLConfig.GetValue(Path+'Description/Value',''));
  LoadStringList(AXMLConfig,FValues,Path+'Values/');
  LoadStringList(AXMLConfig,FValueDescriptions,Path+'ValueDescriptions/');
  FDefaultValue:=ConvertLineEndings(AXMLConfig.GetValue(Path+'Default/Value',''));

  while ValueDescriptions.Count>Values.Count do
    ValueDescriptions.Delete(ValueDescriptions.Count-1);
  while ValueDescriptions.Count<Values.Count do
    ValueDescriptions.Add('');
end;

procedure TIDEBuildMacro.SaveToXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);
begin
  AXMLConfig.SetDeleteValue(Path+'Identifier/Value',FIdentifier,'');
  AXMLConfig.SetDeleteValue(Path+'Description/Value',
                                    LineBreaksToDelimiter(FDescription,#10),'');
  SaveStringList(AXMLConfig,FValues,Path+'Values/');
  SaveStringList(AXMLConfig,FValueDescriptions,Path+'ValueDescriptions/');
  AXMLConfig.SetDeleteValue(Path+'DefaultValue/Value',
                                   LineBreaksToDelimiter(FDefaultValue,#10),'');
end;

function TIDEBuildMacro.CreateDiff(OtherMode: TLazBuildMacro;
  Tool: TCompilerDiffTool): boolean;

  function Done(Diff: boolean): boolean;
  begin
    if Diff then CreateDiff:=true;
    Result:=(Tool=nil) and Diff;
  end;

begin
  Result:=false;
  if Done(Tool.AddDiff('Identifier',Identifier,OtherMode.Identifier)) then exit;
  if Done(Tool.AddDiff('Description',Description,OtherMode.Description)) then exit;
  if Done(Tool.AddStringsDiff('Values',Values,OtherMode.Values)) then exit;
  if Done(Tool.AddStringsDiff('ValueDescriptions',ValueDescriptions,OtherMode.ValueDescriptions)) then exit;
end;

procedure TIDEBuildMacro.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

{ TIDEBuildMacros }

function TIDEBuildMacros.GetItems(Index: integer): TLazBuildMacro;
begin
  Result:=TLazBuildMacro(FItems[Index]);
end;

function TIDEBuildMacros.Add(Identifier: string): TLazBuildMacro;
begin
  if IndexOfIdentifier(Identifier)>=0 then
    raise Exception.Create('TIDEBuildMacros.Add identifier already exists');
  Result:=TIDEBuildMacro.Create;
  Result.Identifier:=Identifier;
  FItems.Add(Result);
end;

procedure TIDEBuildMacros.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TIDEBuildMacros.Count: integer;
begin
  Result:=FItems.Count;
end;

constructor TIDEBuildMacros.Create(TheOwner: TObject);
begin
  inherited Create(TheOwner);
  FItems:=TFPList.Create;
end;

procedure TIDEBuildMacros.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

destructor TIDEBuildMacros.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TIDEBuildMacros.IndexOfIdentifier(Identifier: string): integer;
begin
  Result:=FItems.Count-1;
  while (Result>=0) and (SysUtils.CompareText(Identifier,Items[Result].Identifier)<>0) do
    dec(Result);
end;

function TIDEBuildMacros.VarWithIdentifier(Identifier: string): TIDEBuildMacro;
var
  i: LongInt;
begin
  i:=IndexOfIdentifier(Identifier);
  if i<0 then
    Result:=nil
  else
    Result:=TIDEBuildMacro(Items[i]);
end;

procedure TIDEBuildMacros.Move(OldIndex, NewIndex: integer);
begin
  FItems.Move(OldIndex,NewIndex);
end;

procedure TIDEBuildMacros.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
var
  NewItem: TIDEBuildMacro;
  NewCount: LongInt;
  i: Integer;
begin
  Clear;
  NewCount:=AXMLConfig.GetValue(Path+'Count/Value',0);
  for i:=0 to NewCount-1 do begin
    NewItem:=TIDEBuildMacro.Create;
    NewItem.LoadFromXMLConfig(AXMLConfig,Path+'Item'+IntToStr(i+1)+'/',DoSwitchPathDelims);
    if IsValidIdent(NewItem.Identifier) then
      FItems.Add(NewItem)
    else
      NewItem.Free;
  end;
end;

procedure TIDEBuildMacros.SaveToXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);
var
  i: Integer;
begin
  AXMLConfig.SetDeleteValue(Path+'Count/Value',Count,0);
  for i:=0 to Count-1 do
    TIDEBuildMacro(Items[i]).SaveToXMLConfig(AXMLConfig,
                                    Path+'Item'+IntToStr(i+1)+'/',UsePathDelim);
end;

function TIDEBuildMacros.CreateDiff(OtherProperties: TLazBuildMacros;
  Tool: TCompilerDiffTool): boolean;
var
  i: Integer;
  OtherMacro: TLazBuildMacro;
begin
  Result:=Tool.AddDiff('BuildMacros/Count',Count,OtherProperties.Count);
  if (Tool=nil) and Result then exit;
  for i:=0 to OtherProperties.Count-1 do begin
    OtherMacro:=OtherProperties.Items[i];
    if i>=Count then
    begin
      if Tool=nil then exit(true);
      Tool.AddDiffItem('BuildMacros/'+OtherMacro.Identifier,'new');
    end else begin
      if Tool=nil then
      begin
        if not TIDEBuildMacro(Items[i]).Equals(OtherMacro) then exit(true);
      end else
      begin
        Tool.Path:='BuildMacros/'+OtherMacro.Identifier;
        if TIDEBuildMacro(Items[i]).CreateDiff(OtherProperties.Items[i],Tool) then
          Result:=true;
      end;
    end;
  end;
  if Tool<>nil then
    for i:=OtherProperties.Count to Count-1 do
      Tool.AddDiffItem('BuildMacros/'+Items[i].Identifier,'deleted');
end;

procedure TIDEBuildMacros.Assign(Source: TLazBuildMacros);
var
  i: Integer;
  Item: TLazBuildMacro;
begin
  Clear;
  for i:=0 to Source.Count-1 do begin
    Item:=Add(Source[i].Identifier);
    TIDEBuildMacro(Item).Assign(Source[i]);
  end;
end;

{ TCompilerMsgIDFlags }

function TCompilerMsgIDFlags.Count: SizeInt;
begin
  Result:=fTree.Count;
end;

function TCompilerMsgIDFlags.FindNode(MsgId: integer): TAvlTreeNode;
var
  Flag: TCompilerMsgIdFlag;
begin
  Flag.MsgId:=MsgId;
  Result:=fTree.FindKey(@Flag,@CompareCompMsgIdFlag);
end;

function TCompilerMsgIDFlags.GetValues(MsgId: integer): TCompilerFlagValue;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(MsgId);
  if Node<>nil then
    Result:=PCompilerMsgIdFlag(Node.Data)^.Flag
  else
    Result:=cfvNone;
end;

function TCompilerMsgIDFlags.GetModified: boolean;
begin
  Result:=FChangeStamp<>fLastSavedStamp;
end;

procedure TCompilerMsgIDFlags.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    fLastSavedStamp:=FChangeStamp;
end;

procedure TCompilerMsgIDFlags.SetValues(MsgId: integer;
  AValue: TCompilerFlagValue);
var
  Node: TAvlTreeNode;
  Flag: PCompilerMsgIdFlag;
begin
  Node:=FindNode(MsgId);
  if (Node<>nil) then begin
    Flag:=PCompilerMsgIdFlag(Node.Data);
    if Flag^.Flag=AValue then
      exit; // no change
    if AValue=cfvNone then begin
      // change to default -> do not store default values => delete
      Dispose(Flag);
      fTree.Delete(Node);
    end
    else
      Flag^.Flag:=AValue; // switch
  end else if AValue=cfvNone then begin
    // no change
    exit;
  end else begin
    // add new value
    New(Flag);
    Flag^.MsgId:=MsgId;
    Flag^.Flag:=AValue;
    fTree.Add(Flag);
    fTree.ConsistencyCheck;
  end;
  {$IFDEF VerboseIDEModified}
  debugln(['TCompilerMsgIDFlags.SetValues ']);
  {$ENDIF}
  IncreaseChangeStamp;
end;

constructor TCompilerMsgIDFlags.Create;
begin
  fTree:=TAvlTree.Create(@CompareCompMsgIdFlag);
end;

destructor TCompilerMsgIDFlags.Destroy;
begin
  Clear;
  FreeAndNil(fTree);
  inherited Destroy;
end;

procedure TCompilerMsgIDFlags.Clear;
var
  Node: TAvlTreeNode;
  Flag: PCompilerMsgIdFlag;
begin
  Node:=fTree.FindLowest;
  while Node<>nil do begin
    Flag:=PCompilerMsgIdFlag(Node.Data);
    Dispose(Flag);
    Node:=Node.Successor;
  end;
  fTree.Clear;
end;

procedure TCompilerMsgIDFlags.Assign(Source: TPersistent);
var
  Src: TCompilerMsgIDFlags;
  Node: TAvlTreeNode;
  SrcFlag, Flag: PCompilerMsgIdFlag;
begin
  if Source is TCompilerMsgIDFlags then begin
    Src:=TCompilerMsgIDFlags(Source);
    if Equals(Src) then exit;
    // copy node structure and Data references
    fTree.Assign(Src.fTree);
    // clone data
    Node:=fTree.FindLowest;
    while Node<>nil do begin
      SrcFlag:=PCompilerMsgIdFlag(Node.Data);
      New(Flag);
      Flag^:=SrcFlag^;
      Node.Data:=Flag;
      Node:=Node.Successor;
    end;
    {$IFDEF VerboseIDEModified}
    debugln(['TCompilerMsgIDFlags.Assign ']);
    {$ENDIF}
    IncreaseChangeStamp;
  end else
    inherited Assign(Source);
end;

function TCompilerMsgIDFlags.Equals(Obj: TObject): boolean;
var
  Other: TCompilerMsgIDFlags;
  MyNode: TAvlTreeNode;
  OtherNode: TAvlTreeNode;
  MyFlag: PCompilerMsgIdFlag;
  OtherFlag: PCompilerMsgIdFlag;
begin
  if Obj=Self then exit(true);
  if Obj is TCompilerMsgIDFlags then begin
    Other:=TCompilerMsgIDFlags(Obj);
    Result:=false;
    if Count<>Other.Count then exit;
    MyNode:=fTree.FindLowest;
    OtherNode:=Other.fTree.FindLowest;
    while MyNode<>nil do begin
      if OtherNode=nil then exit;
      MyFlag:=PCompilerMsgIdFlag(MyNode.Data);
      OtherFlag:=PCompilerMsgIdFlag(OtherNode.Data);
      if (MyFlag^.MsgId<>OtherFlag^.MsgId)
      or (MyFlag^.Flag<>OtherFlag^.Flag) then exit;
      MyNode:=MyNode.Successor;
      OtherNode:=OtherNode.Successor;
    end;
    if OtherNode<>nil then exit;
    Result:=true;
  end
  else
    Result:=inherited Equals(Obj);
end;

procedure TCompilerMsgIDFlags.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp64(FChangeStamp);
end;

function TCompilerMsgIDFlags.GetEnumerator: TCompilerMsgIDFlagsEnumerator;
begin
  Result:=TCompilerMsgIDFlagsEnumerator.Create(fTree);
end;

function TCompilerMsgIDFlags.GetMsgIdList(Delim: char;
  aValue: TCompilerFlagValue; FPCMsgFile: TFPCMsgFilePoolItem): string;
var
  Flag: PCompilerMsgIdFlag;
begin
  Result:='';
  for Flag in Self do begin
    if Flag^.Flag<>aValue then continue;
    if (FPCMsgFile<>nil) and (FPCMsgFile.GetMsg(Flag^.MsgId)=nil) then continue;
    if Result<>'' then
      Result+=Delim;
    Result+=IntToStr(Flag^.MsgId);
  end;
end;

function TCompilerMsgIDFlags.CreateDiff(Tool: TCompilerDiffTool;
  Other: TCompilerMsgIDFlags): boolean;
var
  Node: TAvlTreeNode;
  Flag: PCompilerMsgIdFlag;
  OtherFlag: TCompilerFlagValue;
begin
  Result:=false;
  if Tool=nil then
    exit(Equals(Other));
  Result:=Result or Tool.AddDiff('Count',Count,Other.Count);
  // first all in here
  Node:=fTree.FindLowest;
  while Node<>nil do begin
    Flag:=PCompilerMsgIdFlag(Node.Data);
    OtherFlag:=Other[Flag^.MsgId];
    if Flag^.Flag<>OtherFlag then begin
      Result:=Result or Tool.AddDiff('message id '+IntToStr(Flag^.MsgId),EnumToStr(Flag^.Flag),EnumToStr(OtherFlag));
    end;
    Node:=Node.Successor;
  end;
  // then all not here
  Node:=Other.fTree.FindLowest;
  while Node<>nil do begin
    Flag:=PCompilerMsgIdFlag(Node.Data);
    if Values[Flag^.MsgId]=cfvNone then
      Result:=Result or Tool.AddDiff('message id '+IntToStr(Flag^.MsgId),EnumToStr(cfvNone),EnumToStr(Flag^.Flag));
    Node:=Node.Successor;
  end;
end;

initialization
  CompilerParseStamp:=1;
  CompilerParseStampIncreased:=nil;
  BuildMacroChangeStamp:=1;

end.

