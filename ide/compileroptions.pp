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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  ToDo:
  - create Makefile:
     - create a special .compiled file
     - an IDE created with make, without extra options should not recompile a
       package with this special .compiled file
  - writable package output directory: set it on load package or when output dir changes
     - use the same macros to get the same diversity
     - use a readonly directory if package is valid
  - make synedit a package
  - make IDEIntf a package
  - make LCL a package
  - make FCL a package
  - when adding/removing search path: do it for all build modes
    - add unit to project
    - remove unit from project
    - move unit in project
  - make lazbuild lcl independent, independent of packages except one
    - license gpl2
    - create package lazbuildsystem with some units
    - move
  - remove TGlobalCompilerOptions
    - implement switching active project
  - i18n for descriptions
  - keyword help for a build macro

}
unit CompilerOptions;

{$mode objfpc}
{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

uses
  Classes, SysUtils, FileProcs, FileUtil, InterfaceBase, LCLProc, Forms,
  Controls, Laz_XMLCfg, ExprEval, DefineTemplates, CodeToolsCfgScript,
  // IDEIntf
  ProjectIntf, MacroIntf, IDEExternToolIntf, SrcEditorIntf, IDEOptionsIntf,
  // IDE
  IDEProcs, IDEMsgIntf, LazConf, TransferMacros, CompOptsModes;

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
                                DoSwitchPathDelims: boolean);
    procedure SaveToXMLConfig(aXMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch);
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

  { TGlobalCompilerOptions - compiler options overrides }

  TGlobalCompilerOptions = class
  private
    FTargetCPU: string;
    FTargetOS: string;
    procedure SetTargetCPU(const AValue: string);
    procedure SetTargetOS(const AValue: string);
  public
    property TargetCPU: string read FTargetCPU write SetTargetCPU;
    property TargetOS: string read FTargetOS write SetTargetOS;
  end;


type
  TInheritedCompilerOption = (
    icoNone,
    icoUnitPath,
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
    pcosIncludePath,  // search path for pascal include files
    pcosObjectPath,   // search path for .o files
    pcosLibraryPath,  // search path for libraries
    pcosSrcPath,      // additional search path for pascal source files
    pcosLinkerOptions,// additional linker options
    pcosCustomOptions,// additional options
    pcosOutputDir,    // the output directory
    pcosCompilerPath, // the filename of the compiler
    pcosDebugPath     // additional debug search path
    );
  TParsedCompilerOptStrings = set of TParsedCompilerOptString;


const
  ParsedCompilerSearchPaths = [pcosUnitPath,pcosIncludePath,pcosObjectPath,
                               pcosLibraryPath,pcosSrcPath,pcosDebugPath];
  ParsedCompilerFilenames = [pcosCompilerPath];
  ParsedCompilerDirectories = [pcosOutputDir];
  ParsedCompilerOutDirectories = [pcosOutputDir];
  ParsedCompilerFiles =
    ParsedCompilerSearchPaths+ParsedCompilerFilenames+ParsedCompilerDirectories;
    
  ParsedCompilerOptStringNames: array[TParsedCompilerOptString] of string = (
    'pcosNone',
    'pcosBaseDir', // only auto created packages can have macros in the BaseDir
    'pcosUnitPath',
    'pcosIncludePath',
    'pcosObjectPath',
    'pcosLibraryPath',
    'pcosSrcPath',
    'pcosLinkerOptions',
    'pcosCustomOptions',
    'pcosOutputDir',
    'pcosCompilerPath',
    'pcosDebugPath'
    );
  ParsedCompilerOptsVars: array[TParsedCompilerOptString] of string = (
    '',
    '',
    'UnitPath',
    'IncPath',
    'ObjectPath',
    'LibraryPath',
    'SrcPath',
    'LinkerOptions',
    'CustomOptions',
    'OutputDir',
    'CompilerPath',
    'DebugPath'
    );
  ParsedCompilerOptsUsageVars: array[TParsedCompilerOptString] of string = (
    '',
    '',
    'UsageUnitPath',
    'UsageIncPath',
    'UsageObjectPath',
    'UsageLibraryPath',
    'UsageSrcPath',
    'UsageLinkerOptions',
    'UsageCustomOptions',
    '',
    '',
    'UsageDebugPath'
    );
  InheritedToParsedCompilerOption: array[TInheritedCompilerOption] of
    TParsedCompilerOptString = (
      pcosNone,
      pcosUnitPath,      // icoUnitPath,
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
  TGetWritableOutputDirectory = procedure(var s: string) of object;
  
  TCompilerOptionsParseType = (
    coptUnparsed,  // no macros resolved
    coptParsed,    // all macros resolved
    coptParsedPlatformIndependent // all but platform macros resolved
    );
    
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
    UnparsedValues: array[TParsedCompilerOptString] of string;
    // parsed
    ParsedValues: array[TParsedCompilerOptString] of string;
    ParsedStamp: array[TParsedCompilerOptString] of integer; // see CompilerParseStamp
    Parsing: array[TParsedCompilerOptString] of boolean;
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
    MacroValues: TCTConfigScriptEngine;
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
    ccloNoMacroParams // no search paths, no linker options, no custom options
    );
  TCompilerCmdLineOptions = set of TCompilerCmdLineOption;
  
  { TCompilationToolOptions }

  TCompilationToolOptions = class
  private
    FChangeStamp: int64;
    FCommand: string;
    FOnChanged: TNotifyEvent;
    FScanForFPCMessages: boolean;
    FScanForMakeMessages: boolean;
    FShowAllMessages: boolean;
    procedure SetCommand(const AValue: string);
    procedure SetScanForFPCMessages(const AValue: boolean);
    procedure SetScanForMakeMessages(const AValue: boolean);
    procedure SetShowAllMessages(const AValue: boolean);
  public
    procedure Clear; virtual;
    function CreateDiff(CompOpts: TCompilationToolOptions;
                        Tool: TCompilerDiffTool = nil): boolean; virtual;
    procedure Assign(Src: TCompilationToolOptions); virtual;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch); virtual;
    function Execute(const WorkingDir, ToolTitle: string): TModalResult;
    property ChangeStamp: int64 read FChangeStamp;
    procedure IncreaseChangeStamp;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    property Command: string read FCommand write SetCommand;
    property ScanForFPCMessages: boolean read FScanForFPCMessages write SetScanForFPCMessages;
    property ScanForMakeMessages: boolean read FScanForMakeMessages write SetScanForMakeMessages;
    property ShowAllMessages: boolean read FShowAllMessages write SetShowAllMessages;
  end;
  TCompilationToolClass = class of TCompilationToolOptions;

  TCompilerMessagesList = class; 
  
  { TCompilerMessageConfig }
  
  TCompilerMessageConfig = class
  private
    fOwner  : TCompilerMessagesList; 
  public
    MsgIndex : integer;
    MsgText  : String;
    Ignored  : Boolean;
    MsgType  : TFPCErrorType;
    constructor Create(AOwner: TCompilerMessagesList); 
    function GetUserText: string; overload;
    function GetUserText(const ReplaceParams: array of string): string; overload; 
  end;

  { TCompilerMessagesList }
  
  TCompilerMessagesList = class
  private
    fItems      : TFPList;
    fHash       : array of array of TCompilerMessageConfig;
  protected
    fUsedMsgFile  : string; 
    fUpdating     : Integer; 
    FErrorNames   : array [TFPCErrorType] of string;

    procedure ClearHash;
    procedure AddHash(Msg: TCompilerMessageConfig);
    function FindHash(AIndex: integer):TCompilerMessageConfig ; 

    function GetMsgConfigByIndex(AIndex: Integer): TCompilerMessageConfig; 
    function GetMsgConfig(i: Integer): TCompilerMessageConfig; virtual;
    procedure SetMsgIgnored(i: Integer; const AValue: Boolean); virtual;
    function GetMsgIgnored(i: Integer): Boolean; virtual;

    procedure GetIgnoredArray(var b: array of Boolean);    // array must be large enough 
    procedure SetIgnoredArray(const b: array of Boolean);  // to store b[MaxMsgIndex], or function fail

    function GetCount: Integer; 
    function GetErrorNames(errtype: TFPCErrorType): string;
  public
    constructor Create; 
    destructor Destroy; override;
    procedure Clear; virtual; 

    procedure Assign(Src: TCompilerMessagesList);  virtual; 

    procedure BeginUpdate; virtual; 
    procedure EndUpdate; virtual; 

    function LoadMsgFile(const FileName: string): Boolean; virtual;

    function Add(AMsgIndex: Integer; AMsgType: TFPCErrorType; const AMsgText: string; AIgnored: Boolean=false): TCompilerMessageConfig; virtual;

    procedure SetDefault(KeepIgnored: Boolean=true); virtual;
    function GetParams(MsgIndex: Integer; var prms: array of string; out PrmCount: Integer): Integer; virtual;

    property Msg[i: Integer]: TCompilerMessageConfig read GetMsgConfig;
    property MsgByIndex[AIndex: Integer]:  TCompilerMessageConfig read GetMsgConfigByIndex;
    property MsgIgnored[i: Integer]: Boolean read GetMsgIgnored write SetMsgIgnored;
    property Count: Integer read GetCount; 
    property UsedMsgFile : string read fUsedMsgFile; 
    property ErrorNames[errtype: TFPCErrorType]: string read GetErrorNames;
  end;
  
  { TBaseCompilerOptions }

  TBaseCompilerOptions = class(TLazCompilerOptions)
  private
    FDefaultMakeOptionsFlags: TCompilerCmdLineOptions;
    fInheritedOptions: TInheritedCompOptsParseTypesStrings;
    fInheritedOptParseStamps: integer;
    FParsedOpts: TParsedCompilerOptions;
    FStorePathDelim: TPathDelimSwitch;
    FUseAsDefault: Boolean;

    // Compilation
    fExecuteBefore: TCompilationToolOptions;
    fExecuteAfter: TCompilationToolOptions;
    FCreateMakefileOnBuild: boolean;
    
    // Compiler Messags
    fUseCustomMessages: Boolean; // use messages customization 
    fUseMsgFile: Boolean;  // use specified file for messages 
    fMsgFileName: String;  // messages file name 
    fCompilerMessages: TCompilerMessagesList;

    procedure OnItemChanged(Sender: TObject);
  protected
    function GetCompilerPath: String;
    function GetBaseDirectory: string;
    function GetCustomOptions: string; override;
    function GetDebugPath: string; override;
    function GetIncludePaths: String; override;
    function GetLibraryPaths: String; override;
    function GetObjectPath: string; override;
    function GetSrcPath: string; override;
    function GetUnitOutputDir: string; override;
    function GetUnitPaths: String; override;
    procedure SetBaseDirectory(const AValue: string);
    procedure SetCompilerPath(const AValue: String); override;
    procedure SetConditionals(const AValue: string); override;
    procedure SetCustomOptions(const AValue: string); override;
    procedure SetIncludePaths(const AValue: String); override;
    procedure SetLibraryPaths(const AValue: String); override;
    procedure SetLinkerOptions(const AValue: String); override;
    procedure SetUnitPaths(const AValue: String); override;
    procedure SetUnitOutputDir(const AValue: string); override;
    procedure SetObjectPath(const AValue: string); override;
    procedure SetSrcPath(const AValue: string); override;
    procedure SetDebugPath(const AValue: string); override;
    procedure SetTargetCPU(const AValue: string); override;
    procedure SetTargetProc(const AValue: string); override;
    procedure SetTargetOS(const AValue: string); override;
    procedure SetTargetFilename(const AValue: String); override;
    procedure SetLCLWidgetType(const AValue: string); override;
    procedure SetModified(const AValue: boolean); override;
  protected
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

    function MakeOptionsString(Globals: TGlobalCompilerOptions;
                               Flags: TCompilerCmdLineOptions): String;
    function MakeOptionsString(const MainSourceFileName: string;
                               Globals: TGlobalCompilerOptions;
                               Flags: TCompilerCmdLineOptions): String; virtual;
    function GetSyntaxOptionsString: string; virtual;
    function CreateTargetFilename(const MainSourceFileName: string): string; virtual;
    function GetTargetFileExt: string; virtual;
    function GetTargetFilePrefix: string; virtual;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList); virtual;
    function GetOwnerName: string; virtual;
    function GetInheritedOption(Option: TInheritedCompilerOption;
                                RelativeToBaseDir: boolean;
                                Parsed: TCompilerOptionsParseType = coptParsed
                                ): string; virtual;
    function GetDefaultMainSourceFileName: string; virtual;
    function CanBeDefaulForProject: boolean; virtual;
    function NeedsLinkerOpts: boolean;
    function GetUnitPath(RelativeToBaseDir: boolean;
                         Parsed: TCompilerOptionsParseType = coptParsed;
                         WithBaseDir: boolean = true): string;
    function GetIncludePath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed;
                            WithBaseDir: boolean = true): string;
    function GetSrcPath(RelativeToBaseDir: boolean;
                        Parsed: TCompilerOptionsParseType = coptParsed;
                        WithBaseDir: boolean = true): string;
    function GetDebugPath(RelativeToBaseDir: boolean;
                          Parsed: TCompilerOptionsParseType = coptParsed;
                          WithBaseDir: boolean = true): string;
    function GetLibraryPath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed;
                            WithBaseDir: boolean = true): string;
    function GetUnitOutPath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed): string;
    function GetObjectPath(RelativeToBaseDir: boolean;
                           Parsed: TCompilerOptionsParseType = coptParsed;
                           WithBaseDir: boolean = true): string;
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
    function GetEffectiveLCLWidgetType: string; virtual;

    procedure RenameMacro(const OldName, NewName: string;
              ChangeConditionals: boolean); virtual; // rename macro in paths and options, not in BuildMacros, not in dependencies
  public
    // not stored properties
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
    property BaseDirectory: string read GetBaseDirectory write SetBaseDirectory;
    property DefaultMakeOptionsFlags: TCompilerCmdLineOptions
                 read FDefaultMakeOptionsFlags write SetDefaultMakeOptionsFlags;

    // for dialog only
    property UseAsDefault: Boolean read FUseAsDefault write FUseAsDefault;

    // stored properties
    property StorePathDelim: TPathDelimSwitch read FStorePathDelim write FStorePathDelim;

    // compilation
    property CompilerPath: String read GetCompilerPath write SetCompilerPath;
    property ExecuteBefore: TCompilationToolOptions read fExecuteBefore;
    property ExecuteAfter: TCompilationToolOptions read fExecuteAfter;
    property CreateMakefileOnBuild: boolean read FCreateMakefileOnBuild
                                            write FCreateMakefileOnBuild;
                                            
    // compiler messages
    property CompilerMessages: TCompilerMessagesList read fCompilerMessages;
    property UseMsgFile: Boolean read fUseMsgFile write fUseMsgFile;
    property MsgFileName: String read fMsgFileName write fMsgFileName;
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
    function GetObjectPath: string; virtual;
    function GetSrcPath: string; virtual;
    function GetUnitPath: string; virtual;
    procedure SetBaseDirectory(const AValue: string); virtual;
    procedure SetCustomOptions(const AValue: string); virtual;
    procedure SetIncludePath(const AValue: string); virtual;
    procedure SetLibraryPath(const AValue: string); virtual;
    procedure SetLinkerOptions(const AValue: string); virtual;
    procedure SetObjectPath(const AValue: string); virtual;
    procedure SetSrcPath(const AValue: string); virtual;
    procedure SetUnitPath(const AValue: string); virtual;
  public
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
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
  LCLWidgetLinkerAddition: array[TLCLPlatform] of string = (
{$IFDEF DARWIN}
    ' -Fl/usr/X11R6/lib -Fl/sw/lib', // gtk
{$ELSE}
    '', // gtk
{$ENDIF}
{$IFDEF DARWIN}
    ' -Fl/usr/X11R6/lib -Fl/sw/lib -Fl/sw/lib/pango-ft219/lib', // gtk2
{$ELSE}
    '', // gtk2
{$ENDIF}
    '', // win32
    '', // wince
{$IFDEF DARWIN}
    ' -k-framework -kCarbon -k-framework -kOpenGL -k''-dylib_file'' -k''/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib''', // carbon
{$ELSE}
    '', //carbon
{$ENDIF}
    '', // qt
    '', // fpGUI
    '', // noGUI
{$IFDEF DARWIN}
    ' -k-framework -kCocoa' // Cocoa
{$ELSE}
    '' // Cocoa
{$ENDIF}
  );

type
  TRunCompilerWithOptions = function(ExtTool: TIDEExternalToolOptions;
                ACompilerOptions: TBaseCompilerOptions): TModalResult of object;
var
  OnParseString: TParseStringEvent = nil;
  RunCompilerWithOptions: TRunCompilerWithOptions = nil;

function ParseString(Options: TParsedCompilerOptions;
                     const UnparsedValue: string;
                     PlatformIndependent: boolean): string;
function GetMakefileMacroValue(const MacroName: string): string;

procedure GatherInheritedOptions(AddOptionsList: TFPList;
  Parsed: TCompilerOptionsParseType;
  var InheritedOptionStrings: TInheritedCompOptsStrings);
function InheritedOptionsToCompilerParameters(
  var InheritedOptionStrings: TInheritedCompOptsStrings;
  Flags: TCompilerCmdLineOptions): string;
function MergeLinkerOptions(const OldOptions, AddOptions: string): string;
function MergeCustomOptions(const OldOptions, AddOptions: string): string;
function ConvertSearchPathToCmdLine(const switch, paths: String): String;
function ConvertOptionsToCmdLine(const Delim, Switch, OptionStr: string): string;

type
  TGetBuildMacroValues = function(Options: TBaseCompilerOptions;
             IncludeSelf: boolean): TCTCfgScriptVariables of object;
var
  GetBuildMacroValues: TGetBuildMacroValues = nil; // set by TPkgManager, do not change or free the variables

function LoadXMLCompileReasons(const AConfig: TXMLConfig;
  const APath: String; const DefaultReasons: TCompileReasons): TCompileReasons;
procedure SaveXMLCompileReasons(const AConfig: TXMLConfig; const APath: String;
  const AFlags, DefaultFlags: TCompileReasons);

const
  MaxMsgParams = 4; 
  MaxMsgIndex = 20000; 

  symFile   = '$FileName';
  symClass  = '$Class';
  symName   = '$Name';
  symItem   = '$Item';
  symLineNo = '$LineNum';

var
  TestCompilerOptions: TNotifyEvent = nil;

implementation

const
  CompilerOptionsVersion = 9;

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
            icoUnitPath,icoIncludePath,icoSrcPath,icoObjectPath,icoLibraryPath:
              begin
                if CurOptions<>'' then
                  UnparsedOption:=';'+UnparsedOption;
                CurOptions:=CurOptions+UnparsedOption;
              end;
            icoLinkerOptions,icoCustomOptions:
              begin
                if CurOptions<>'' then
                  UnparsedOption:=' '+UnparsedOption;
                CurOptions:=CurOptions+UnparsedOption;
              end;
            else
              RaiseException('GatherInheritedOptions');
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
  CurCustomOptions: String;
begin
  Result:='';
  
  // inherited Linker options
  if (not (ccloNoLinkerOpts in Flags)) then begin
    CurLinkerOpts:=InheritedOptionStrings[icoLinkerOptions];
    if CurLinkerOpts<>'' then
      Result := Result + ' ' + ConvertOptionsToCmdLine(' ','-k', CurLinkerOpts);
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
  Result:=OldOptions;
  if AddOptions='' then exit;
  if (OldOptions<>'') and (OldOptions[length(OldOptions)]<>' ')
  and (AddOptions[1]<>' ') then
    Result:=Result+' '+AddOptions
  else
    Result:=Result+AddOptions;
end;

function MergeCustomOptions(const OldOptions, AddOptions: string): string;
begin
  Result:=OldOptions;
  if AddOptions='' then exit;
  if (OldOptions<>'') and (OldOptions[length(OldOptions)]<>' ')
  and (AddOptions[1]<>' ') then
    Result:=Result+' '+AddOptions
  else
    Result:=Result+AddOptions;
end;

function ConvertSearchPathToCmdLine(
  const Switch, Paths: String): String;
var
  StartPos: Integer;
  l: Integer;
  EndPos: LongInt;
begin
  if Switch='' then
    RaiseException('ConvertSearchPathToCmdLine no Switch');
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

function ConvertOptionsToCmdLine(const Delim, Switch,
  OptionStr: string): string;
var Startpos, EndPos: integer;
begin
  Result:='';
  StartPos:=1;
  while StartPos<=length(OptionStr) do begin
    EndPos:=StartPos;
    while (EndPos<=length(OptionStr)) and (pos(OptionStr[EndPos],Delim)=0) do
      inc(EndPos);
    if EndPos>StartPos then begin
      Result:=Result+' '+Switch+copy(OptionStr,StartPos,EndPos-StartPos);
    end;
    StartPos:=EndPos+1;
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


{ TBaseCompilerOptions }

{------------------------------------------------------------------------------
  TBaseCompilerOptions Constructor
------------------------------------------------------------------------------}
constructor TBaseCompilerOptions.Create(const AOwner: TObject;
  const AToolClass: TCompilationToolClass);
begin
  inherited Create(AOwner);
  FParsedOpts := TParsedCompilerOptions.Create(Self);
  FExecuteBefore := AToolClass.Create;
  FExecuteBefore.OnChanged:=@OnItemChanged;
  FExecuteAfter := AToolClass.Create;
  fExecuteAfter.OnChanged:=@OnItemChanged;
  fBuildMacros := TIDEBuildMacros.Create(Self);
  FCompilerMessages:=TCompilerMessagesList.Create;
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
  FreeAndNil(FCompilerMessages);
  FreeAndNil(fBuildMacros);
  FreeThenNil(fExecuteBefore);
  FreeThenNil(fExecuteAfter);
  FreeThenNil(FParsedOpts);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions LoadFromFile
------------------------------------------------------------------------------}
function TBaseCompilerOptions.LoadFromFile(AFilename: string
  ): TModalResult;
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
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetCompilerPath(const AValue: String);
begin
  if CompilerPath=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosCompilerPath,AValue);
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetConditionals(const AValue: string);
begin
  if FConditionals=AValue then exit;
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
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetSrcPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if SrcPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosSrcPath,NewValue);
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetDebugPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if DebugPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosDebugPath,NewValue);
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
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetProc(const AValue: string);
begin
  if fTargetProc=AValue then exit;
  fTargetProc:=AValue;
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseBuildMacroChangeStamp;
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
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetTargetFilename(const AValue: String);
begin
  if fTargetFilename=AValue then exit;
  fTargetFilename:=AValue;
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetLCLWidgetType(const AValue: string);
begin
  if LCLWidgetType=AValue then exit;
  inherited SetLCLWidgetType(AValue);
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseBuildMacroChangeStamp;
end;

procedure TBaseCompilerOptions.OnItemChanged(Sender: TObject);
begin
  IncreaseChangeStamp;
end;

function TBaseCompilerOptions.GetCompilerPath: String;
begin
  Result:=ParsedOpts.UnparsedValues[pcosCompilerPath];
end;

function TBaseCompilerOptions.GetBaseDirectory: string;
begin
  Result:=ParsedOpts.UnparsedValues[pcosBaseDir];
end;

function TBaseCompilerOptions.GetCustomOptions: string;
begin
  Result:=ParsedOpts.UnparsedValues[pcosCustomOptions];
end;

function TBaseCompilerOptions.GetDebugPath: string;
begin
  Result:=ParsedOpts.UnparsedValues[pcosDebugPath];
end;

function TBaseCompilerOptions.GetIncludePaths: String;
begin
  Result:=ParsedOpts.UnparsedValues[pcosIncludePath];
end;

function TBaseCompilerOptions.GetLibraryPaths: String;
begin
  Result:=ParsedOpts.UnparsedValues[pcosLibraryPath];
end;

function TBaseCompilerOptions.GetObjectPath: string;
begin
  Result:=ParsedOpts.UnparsedValues[pcosObjectPath];
end;

function TBaseCompilerOptions.GetSrcPath: string;
begin
  Result:=ParsedOpts.UnparsedValues[pcosSrcPath];
end;

function TBaseCompilerOptions.GetUnitOutputDir: string;
begin
  Result:=ParsedOpts.UnparsedValues[pcosOutputDir];
end;

function TBaseCompilerOptions.GetUnitPaths: String;
begin
  Result:=ParsedOpts.UnparsedValues[pcosUnitPath];
end;

procedure TBaseCompilerOptions.SetBaseDirectory(const AValue: string);
begin
  if BaseDirectory=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosBaseDir,AValue);
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
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetLibraryPaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if Libraries=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosLibraryPath,NewValue);
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetLinkerOptions(const AValue: String);
begin
  if LinkerOptions=AValue then exit;
  fLinkerOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLinkerOptions,AValue);
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetUnitPaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if OtherUnitFiles=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosUnitPath,NewValue);
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetUnitOutputDir(const AValue: string);
begin
  if UnitOutputDirectory=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosOutputDir,AValue);
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetObjectPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if ObjectPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosObjectPath,NewValue);
  IncreaseChangeStamp;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions LoadTheCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
var
  p: String;
  PathDelimChange: boolean;
  FileVersion: Integer;
  i: LongInt;
  
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

begin
  { Load the compiler options from the XML file }
  p:=Path;
  FileVersion:=aXMLConfig.GetValue(p+'Version/Value', 0);
  StorePathDelim:=CheckPathDelim(aXMLConfig.GetValue(p+'PathDelim/Value', '/'),PathDelimChange);

  { Target }
  p:=Path+'Target/';
  TargetFilename := f(aXMLConfig.GetValue(p+'Filename/Value', ''));
  TargetFilenameAppplyConventions := aXMLConfig.GetValue(p+'Filename/ApplyConventions', true);

  { SearchPaths }
  p:=Path+'SearchPaths/';
  IncludePath := sp(aXMLConfig.GetValue(p+'IncludeFiles/Value', ''));
  Libraries := sp(aXMLConfig.GetValue(p+'Libraries/Value', ''));
  OtherUnitFiles := sp(aXMLConfig.GetValue(p+'OtherUnitFiles/Value', ''));
  UnitOutputDirectory := sp(aXMLConfig.GetValue(p+'UnitOutputDirectory/Value', ''));
  ObjectPath := sp(aXMLConfig.GetValue(p+'ObjectPath/Value', ''));
  SrcPath := sp(aXMLConfig.GetValue(p+'SrcPath/Value', ''));

  { Conditionals }
  FConditionals:=aXMLConfig.GetValue(Path+'Conditionals/Value','');
  TIDEBuildMacros(fBuildMacros).LoadFromXMLConfig(aXMLConfig,
                                       Path+'BuildMacros/',PathDelimChange);
  // ToDo: replace this with conditional compiler options
  LCLWidgetType := aXMLConfig.GetValue(p+'LCLWidgetType/Value', '');

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
  IOChecks := aXMLConfig.GetValue(p+'Checks/IOChecks/Value', false);
  RangeChecks := aXMLConfig.GetValue(p+'Checks/RangeChecks/Value', false);
  OverflowChecks := aXMLConfig.GetValue(p+'Checks/OverflowChecks/Value', false);
  StackChecks := aXMLConfig.GetValue(p+'Checks/StackChecks/Value', false);
  EmulatedFloatOpcodes := aXMLConfig.GetValue(p+'EmulateFloatingPointOpCodes/Value', false);
  HeapSize := aXMLConfig.GetValue(p+'HeapSize/Value', 0);
  VerifyObjMethodCall := aXMLConfig.GetValue(p+'VerifyObjMethodCallValidity/Value', false);
  ReadSmaller;
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
  VariablesInRegisters := aXMLConfig.GetValue(p+'Optimizations/VariablesInRegisters/Value', false);
  UncertainOptimizations := aXMLConfig.GetValue(p+'Optimizations/UncertainOptimizations/Value', false);
  OptimizationLevel := aXMLConfig.GetValue(p+'Optimizations/OptimizationLevel/Value', 1);

  { Linking }
  p:=Path+'Linking/';
  GenerateDebugInfo := aXMLConfig.GetValue(p+'Debugging/GenerateDebugInfo/Value', false);
  UseLineInfoUnit := aXMLConfig.GetValue(p+'Debugging/UseLineInfoUnit/Value', true);
  GenerateDwarf := aXMLConfig.GetValue(p+'Debugging/GenerateDwarf/Value', false);
  UseHeaptrc := aXMLConfig.GetValue(p+'Debugging/UseHeaptrc/Value', false);
  UseValgrind := aXMLConfig.GetValue(p+'Debugging/UseValgrind/Value', false);
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
  //DebugLn('TBaseCompilerOptions.LoadTheCompilerOptions ',CompilationExecutableTypeNames[ExecutableType]);

  { Messages }
  p:=Path+'Other/';
  ShowErrors := aXMLConfig.GetValue(p+'Verbosity/ShowErrors/Value', true);
  ShowWarn := aXMLConfig.GetValue(p+'Verbosity/ShowWarn/Value', true);
  ShowNotes := aXMLConfig.GetValue(p+'Verbosity/ShowNotes/Value', true);
  ShowHints := aXMLConfig.GetValue(p+'Verbosity/ShowHints/Value', true);
  ShowGenInfo := aXMLConfig.GetValue(p+'Verbosity/ShowGenInfo/Value', true);
  ShowLineNum := aXMLConfig.GetValue(p+'Verbosity/ShoLineNum/Value', false);
  ShowAll := aXMLConfig.GetValue(p+'Verbosity/ShowAll/Value', false);
  ShowAllProcsOnError := aXMLConfig.GetValue(p+'Verbosity/ShowAllProcsOnError/Value', false);
  ShowDebugInfo := aXMLConfig.GetValue(p+'Verbosity/ShowDebugInfo/Value', false);
  ShowUsedFiles := aXMLConfig.GetValue(p+'Verbosity/ShowUsedFiles/Value', false);
  ShowTriedFiles := aXMLConfig.GetValue(p+'Verbosity/ShowTriedFiles/Value', false);
  ShowDefMacros := aXMLConfig.GetValue(p+'Verbosity/ShowDefMacros/Value', false);
  ShowCompProc := aXMLConfig.GetValue(p+'Verbosity/ShowCompProc/Value', false);
  ShowCond := aXMLConfig.GetValue(p+'Verbosity/ShowCond/Value', false);
  ShowExecInfo := aXMLConfig.GetValue(p+'Verbosity/ShowExecInfo/Value', false);
  ShowNothing := aXMLConfig.GetValue(p+'Verbosity/ShowNothing/Value', false);
  ShowSummary := aXMLConfig.GetValue(p+'Verbosity/ShowSummary/Value', false);
  ShowHintsForUnusedUnitsInMainSrc := aXMLConfig.GetValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', false);
  ShowHintsForSenderNotUsed := aXMLConfig.GetValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', false);
  WriteFPCLogo := aXMLConfig.GetValue(p+'WriteFPCLogo/Value', true);
  StopAfterErrCount := aXMLConfig.GetValue(p+'ConfigFile/StopAfterErrCount/Value', 1);

  if fCompilerMessages.Count = 0 then fCompilerMessages.SetDefault; 
  UseMsgFile := aXMLConfig.GetValue(p+'CompilerMessages/UseMsgFile/Value', False);
  MsgFileName := aXMLConfig.GetValue(p+'CompilerMessages/MsgFileName/Value', '');
  if UseMsgFile and FileExists(MsgFileName) then
    fCompilerMessages.LoadMsgFile(MsgFileName);

  for i := 0 to fCompilerMessages.Count - 1 do begin
    with fCompilerMessages.Msg[i] do 
      Ignored := aXMLConfig.GetValue(p+'CompilerMessages/IgnoredMessages/idx'+IntToStr(MsgIndex), false);
  end; 

  if UseMsgFile then 
    with aXMLConfig do begin
      // ErrorNames should be stored, because the Message file is not read (or parsed)
      // on project opening. So errors needs to be initialized properly from the CompilerOptions.xml
      CompilerMessages.fErrorNames[etHint]:=GetValue(p+'CompilerMessages/ErrorNames/Hint', FPCErrorTypeNames[etHint]);
      CompilerMessages.fErrorNames[etNote]:=GetValue(p+'CompilerMessages/ErrorNames/Note', FPCErrorTypeNames[etNote]);
      CompilerMessages.fErrorNames[etWarning]:=GetValue(p+'CompilerMessages/ErrorNames/Warning', FPCErrorTypeNames[etWarning]);
      CompilerMessages.fErrorNames[etError]:=GetValue(p+'CompilerMessages/ErrorNames/Error', FPCErrorTypeNames[etError]);
      CompilerMessages.fErrorNames[etFatal]:=GetValue(p+'CompilerMessages/ErrorNames/Fatal', FPCErrorTypeNames[etFatal]);
    end;
  
  
  { Other }
  p:=Path+'Other/';
  DontUseConfigFile := aXMLConfig.GetValue(p+'ConfigFile/DontUseConfigFile/Value', false);
  if FileVersion<=3 then
    CustomConfigFile := aXMLConfig.GetValue(p+'ConfigFile/AdditionalConfigFile/Value', false)
  else
    CustomConfigFile := aXMLConfig.GetValue(p+'ConfigFile/CustomConfigFile/Value', false);
  ConfigFilePath := f(aXMLConfig.GetValue(p+'ConfigFile/ConfigFilePath/Value', 'extrafpc.cfg'));
  CustomOptions := LineBreaksToSystemLineBreaks(aXMLConfig.GetValue(p+'CustomOptions/Value', ''));

  { Compilation }
  CompilerPath := f(aXMLConfig.GetValue(p+'CompilerPath/Value','$(CompPath)'));

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

var
  P: string;
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
  aXMLConfig.SetDeleteValue(p+'Filename/Value', f(TargetFilename),'');
  aXMLConfig.SetDeleteValue(p+'Filename/ApplyConventions', TargetFilenameAppplyConventions,true);

  { SearchPaths }
  p:=Path+'SearchPaths/';
  aXMLConfig.SetDeleteValue(p+'IncludeFiles/Value', f(IncludePath),'');
  aXMLConfig.SetDeleteValue(p+'Libraries/Value', f(Libraries),'');
  aXMLConfig.SetDeleteValue(p+'OtherUnitFiles/Value', f(OtherUnitFiles),'');
  aXMLConfig.SetDeleteValue(p+'UnitOutputDirectory/Value', f(UnitOutputDirectory),'');
  aXMLConfig.SetDeleteValue(p+'ObjectPath/Value', f(ObjectPath),'');
  aXMLConfig.SetDeleteValue(p+'SrcPath/Value', f(SrcPath),'');

  { Conditionals }
  aXMLConfig.SetDeleteValue(Path+'Conditionals/Value',Conditionals,'');
  TIDEBuildMacros(fBuildMacros).SaveToXMLConfig(aXMLConfig,
                                              Path+'BuildMacros/',UsePathDelim);
  // ToDo: remove
  aXMLConfig.SetDeleteValue(p+'LCLWidgetType/Value', LCLWidgetType,'');

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
  aXMLConfig.SetDeleteValue(p+'Checks/IOChecks/Value', IOChecks,false);
  aXMLConfig.SetDeleteValue(p+'Checks/RangeChecks/Value', RangeChecks,false);
  aXMLConfig.SetDeleteValue(p+'Checks/OverflowChecks/Value', OverflowChecks,false);
  aXMLConfig.SetDeleteValue(p+'Checks/StackChecks/Value', StackChecks,false);
  aXMLConfig.SetDeleteValue(p+'EmulateFloatingPointOpCodes/Value', EmulatedFloatOpcodes,false);
  aXMLConfig.SetDeleteValue(p+'HeapSize/Value', HeapSize,0);
  aXMLConfig.SetDeleteValue(p+'VerifyObjMethodCallValidity/Value', VerifyObjMethodCall,false);
  aXMLConfig.SetDeleteValue(p+'SmallerCode/Value', SmallerCode, false);
  aXMLConfig.SetDeleteValue(p+'TargetProcessor/Value', TargetProcessor,'');
  aXMLConfig.SetDeleteValue(p+'TargetCPU/Value', TargetCPU,'');
  aXMLConfig.SetDeleteValue(p+'TargetOS/Value', TargetOS,'');
  aXMLConfig.SetDeleteValue(p+'Optimizations/VariablesInRegisters/Value', VariablesInRegisters,false);
  aXMLConfig.SetDeleteValue(p+'Optimizations/UncertainOptimizations/Value', UncertainOptimizations,false);
  aXMLConfig.SetDeleteValue(p+'Optimizations/OptimizationLevel/Value', OptimizationLevel,1);

  { Linking }
  p:=Path+'Linking/';
  aXMLConfig.SetDeleteValue(p+'Debugging/GenerateDebugInfo/Value', GenerateDebugInfo,false);
  aXMLConfig.SetDeleteValue(p+'Debugging/UseLineInfoUnit/Value', UseLineInfoUnit,true);
  aXMLConfig.SetDeleteValue(p+'Debugging/GenerateDwarf/Value', GenerateDwarf, false);
  aXMLConfig.SetDeleteValue(p+'Debugging/UseHeaptrc/Value', UseHeaptrc,false);
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
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowErrors/Value', ShowErrors,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowWarn/Value', ShowWarn,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowNotes/Value', ShowNotes,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHints/Value', ShowHints,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowGenInfo/Value', ShowGenInfo,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShoLineNum/Value', ShowLineNum,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowAll/Value', ShowAll,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowAllProcsOnError/Value', ShowAllProcsOnError,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowDebugInfo/Value', ShowDebugInfo,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowUsedFiles/Value', ShowUsedFiles,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowTriedFiles/Value', ShowTriedFiles,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowDefMacros/Value', ShowDefMacros,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowCompProc/Value', ShowCompProc,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowCond/Value', ShowCond,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowExecInfo/Value', ShowExecInfo,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowNothing/Value', ShowNothing,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowSummary/Value', ShowSummary,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', ShowHintsForUnusedUnitsInMainSrc,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', ShowHintsForSenderNotUsed,false);
  aXMLConfig.SetDeleteValue(p+'WriteFPCLogo/Value', WriteFPCLogo,true);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/StopAfterErrCount/Value', StopAfterErrCount,1);

  for i := 0 to CompilerMessages.Count - 1 do begin
    with CompilerMessages.Msg[i] do 
      aXMLConfig.SetDeleteValue(p+'CompilerMessages/IgnoredMessages/idx'+IntToStr(MsgIndex), Ignored, false);
  end;
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/UseMsgFile/Value', UseMsgFile, False);
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/MsgFileName/Value', MsgFileName, '');
  
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/ErrorNames/Hint',    CompilerMessages.ErrorNames[etHint],    FPCErrorTypeNames[etHint]);
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/ErrorNames/Note',    CompilerMessages.ErrorNames[etNote],    FPCErrorTypeNames[etNote]);
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/ErrorNames/Warning', CompilerMessages.ErrorNames[etWarning], FPCErrorTypeNames[etWarning]);
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/ErrorNames/Error',   CompilerMessages.ErrorNames[etError],   FPCErrorTypeNames[etError]);
  aXMLConfig.SetDeleteValue(p+'CompilerMessages/ErrorNames/Fatal',   CompilerMessages.ErrorNames[etFatal],   FPCErrorTypeNames[etFatal]);

  { Other }
  p:=Path+'Other/';
  aXMLConfig.SetDeleteValue(p+'ConfigFile/DontUseConfigFile/Value', DontUseConfigFile,false);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/CustomConfigFile/Value', CustomConfigFile,false);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/ConfigFilePath/Value', f(ConfigFilePath),'extrafpc.cfg');
  aXMLConfig.SetDeleteValue(p+'CustomOptions/Value',
                            LineBreaksToSystemLineBreaks(CustomOptions),''); // do not touch / \ characters

  { Compilation }
  aXMLConfig.SetDeleteValue(p+'CompilerPath/Value', f(CompilerPath),'');
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
  fInheritedOptParseStamps:=InvalidParseStamp;
  for p:=Low(TCompilerOptionsParseType) to High(TCompilerOptionsParseType) do
    for i:=Low(TInheritedCompilerOption) to High(TInheritedCompilerOption) do
    begin
      fInheritedOptions[p][i]:='';
    end;
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions CreateTargetFilename
------------------------------------------------------------------------------}
function TBaseCompilerOptions.CreateTargetFilename(
  const MainSourceFileName: string): string;
  
  procedure AppendDefaultExt;
  var
    Ext: String;
  begin
    if (ExtractFileName(Result)='') or (ExtractFileExt(Result)<>'') then exit;
    Ext:=GetTargetFileExt;
    if Ext<>'' then begin
      Result:=Result+Ext;
      //debugln ( 'Filename result is ',Result,' in AppendDefaultExt' );
      exit;
    end;
  end;

  procedure PrependDefaultType;
  var
    Prefix: String;
    FileName: String;
    PathName: String;
    CurTargetOS: String;
    aSrcOS: String;
 begin
    //debugln ( 'Filename result is ',Result, ' in PrependDefaultType' );
    if (ExtractFileName(Result)='') or
    (CompareText(copy(ExtractFileName(Result),1,3), 'lib') = 0) then exit;
    Prefix:=GetTargetFilePrefix;
    if Prefix<>'' then begin
      FileName := ExtractFileName(Result);
      PathName := ExtractFilePath(Result);
      //debugln ( 'Filename is ',FileName, ' in PrependDefaultType' );
      CurTargetOS:=TargetOS;
      if CurTargetOS='' then CurTargetOS:=GetDefaultTargetOS;
      aSrcOS:=GetDefaultSrcOSForTargetOS(CurTargetOS);
      if (CompareText(aSrcOS, 'unix') = 0)
      then begin
        Result:=PathName+Prefix+UTF8LowerCase(FileName);
      end else begin
        Result:=PathName+Prefix+FileName;
      end;
      //debugln ( 'Result is ',Result, ' in PrependDefaultType' );
      exit;
    end;
  end;

var
  UnitOutDir: String;
  OutFilename: String;
begin
  Result:=TargetFilename;
  IDEMacros.SubstituteMacros(Result);
  if (Result<>'') and FilenameIsAbsolute(Result) then begin
    // fully specified target filename
  end else if Result<>'' then begin
    // TargetFilename is relative to BaseDirectory
    Result:=CreateAbsolutePath(Result,BaseDirectory);
  end else begin
    // no target given => put into unit output directory
    // calculate output directory
    UnitOutDir:=GetUnitOutPath(false);
    if UnitOutDir='' then
      UnitOutDir:=BaseDirectory;
    OutFilename:=ExtractFileNameOnly(MainSourceFileName);
    //debugln('TBaseCompilerOptions.CreateTargetFilename MainSourceFileName=',MainSourceFileName,' OutFilename=',OutFilename,' TargetFilename=',TargetFilename);

    Result:=CreateAbsolutePath(OutFilename,UnitOutDir);
  end;
  Result:=TrimFilename(Result);
  if TargetFilenameAppplyConventions then begin
    AppendDefaultExt;
    PrependDefaultType;
  end;
end;

function TBaseCompilerOptions.GetTargetFileExt: string;
begin
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
  OptionsList: TFPList;
  p: TCompilerOptionsParseType;
begin
  if (fInheritedOptParseStamps<>CompilerParseStamp)
  then begin
    // update inherited options
    ClearInheritedOptions;
    OptionsList:=nil;
    GetInheritedCompilerOptions(OptionsList);
    if OptionsList<>nil then begin
      for p:=Low(TCompilerOptionsParseType) to High(TCompilerOptionsParseType)
      do begin
        GatherInheritedOptions(OptionsList,p,fInheritedOptions[p]);
      end;
      OptionsList.Free;
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

function TBaseCompilerOptions.GetUnitPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithBaseDir: boolean): string;
begin
  Result:=GetPath(pcosUnitPath,icoUnitPath,RelativeToBaseDir,Parsed,WithBaseDir);
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

function TBaseCompilerOptions.GetUnitOutPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType): string;
begin
  case Parsed of
  coptUnparsed: Result:=ParsedOpts.UnparsedValues[pcosOutputDir];
  coptParsed: Result:=ParsedOpts.GetParsedValue(pcosOutputDir);
  coptParsedPlatformIndependent:
              Result:=ParsedOpts.GetParsedPIValue(pcosOutputDir);
  end;
  if (not RelativeToBaseDir) then
    CreateAbsoluteSearchPath(Result,BaseDirectory);
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
  CurrentPath:=ParsedOpts.UnparsedValues[Option];
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
  coptUnparsed: CurCustomOptions:=ParsedOpts.UnparsedValues[pcosCustomOptions];
  coptParsedPlatformIndependent:
               CurCustomOptions:=ParsedOpts.GetParsedPIValue(pcosCustomOptions);
  else
    RaiseGDBException('');
  end;
  // inherited custom options
  InhCustomOptions:=GetInheritedOption(icoCustomOptions,true,Parsed);
  // concatenate
  if CurCustomOptions<>'' then
    Result:=CurCustomOptions+' '+InhCustomOptions
  else
    Result:=InhCustomOptions;
  if Result='' then exit;
  
  // eliminate line breaks
  Result:=SpecialCharsToSpaces(Result,true);
end;

function TBaseCompilerOptions.TrimCustomOptions(o: string): string;
var
  i: Integer;
begin
  Result:=Trim(o);
  for i:=length(Result) downto 1 do
    if Result[i] in [#0..#31,#127] then System.Delete(Result,i,1);
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
  Add(GetSyntaxOptionsString);
end;

function TBaseCompilerOptions.GetEffectiveLCLWidgetType: string;
begin
  Result:=LCLWidgetType;
  if (Result='') or (Result='default') then
    Result:= LCLPlatformDirNames[GetDefaultLCLWidgetType];
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
  TBaseCompilerOptions MakeOptionsString
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MakeOptionsString(Globals: TGlobalCompilerOptions;
  Flags: TCompilerCmdLineOptions): String;
begin
  Result:=MakeOptionsString(GetDefaultMainSourceFileName,Globals,Flags);
end;

function GetIgnoredMsgsIndexes(msglist: TCompilerMessagesList; const Separator: string): string;
var
  i : integer;
begin
  Result := '';
  if not Assigned(msglist) then Exit;
  for i := 0 to msglist.Count - 1 do 
    if msglist.Msg[i].Ignored then begin
      if Result <> '' then 
        Result := Result + Separator + IntToStr(msglist.Msg[i].MsgIndex)
      else 
        Result := IntToStr(msglist.Msg[i].MsgIndex);
    end;
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.MakeOptionsString(
    const MainSourceFilename: string;
    Globals: TGlobalCompilerOptions;
    Flags: TCompilerCmdLineOptions): String;

  Get all the options and create a string that can be passed to the compiler
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MakeOptionsString(
  const MainSourceFilename: string; Globals: TGlobalCompilerOptions;
  Flags: TCompilerCmdLineOptions): String;
var
  switches, tempsw, t: String;
  InhLinkerOpts: String;
  NewTargetFilename: String;
  NewTargetDirectory: String;
  CurIncludePath: String;
  CurLibraryPath: String;
  CurUnitPath: String;
  CurOutputDir: String;
  CurLinkerOptions: String;
  CurObjectPath: String;
  CurMainSrcFile: String;
  CurCustomOptions: String;
  OptimizeSwitches: String;
  LinkerAddition: String;
begin
  CurMainSrcFile:=MainSourceFileName;
  if CurMainSrcFile='' then
    CurMainSrcFile:=GetDefaultMainSourceFileName;

  switches := '';

  { options of fpc 2.2.2 :

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
      -Anasmcoff COFF (Go32v2) file using Nasm
      -Anasmelf  ELF32 (Linux) file using Nasm
      -Anasmwin32Win32 object file using Nasm
      -AnasmwdosxWin32/WDOSX object file using Nasm
      -Awasm     Obj file using Wasm (Watcom)
      -Anasmobj  Obj file using Nasm
      -Amasm     Obj file using Masm (Microsoft)
      -Atasm     Obj file using Tasm (Borland)
      -Aelf      ELF (Linux) using internal writer
      -Acoff     COFF (Go32v2) using internal writer
      -Apecoff   PE-COFF (Win32) using internal writer
  -b     Generate browser info
      -bl        Generate local symbol info
  -B     Build all modules
  -C<x>  Code generation options:
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
      -Cs<n>     Set stack size to <n>
      -Ct        Stack checking
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
  -g     Generate debug information (default format for target)
      -gc        Generate checks for pointers
      -gh        Use heaptrace unit (for memory leak/corruption debugging)
      -gl        Use line info unit (show more info with backtraces)
      -go<x>     Set debug information options
         -godwarfsets Enable Dwarf set debug information (breaks gdb < 6.5)
      -gp        Preserve case in stabs symbol names
      -gs        Generate stabs debug information
      -gt        Trash local variables (to detect uninitialized uses)
      -gv        Generates programs traceable with valgrind
      -gw        Generate dwarf-2 debug information (same as -gw2)
      -gw2       Generate dwarf-2 debug information
      -gw3       Generate dwarf-3 debug information
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
      -Os        Optimize for size rather than speed
  -pg    Generate profile code for gprof (defines FPC_PROFILE)
  -R<x>  Assembler reading style:
      -Rdefault  Use default assembler for target
      -Ratt      Read AT&T style assembler
      -Rintel    Read Intel style assembler
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
      -St        Allow static keyword in objects
      -Sx        Enable exception keywords (default in Delphi/ObjFPC modes)
  -s     Do not call assembler and linker
      -sh        Generate script to link on host
      -st        Generate script to link on target
      -sr        Skip register allocation phase (use with -alr)
  -T<x>  Target operating system:
      -Temx      OS/2 via EMX (including EMX/RSX extender)
      -Tfreebsd  FreeBSD
      -Tgo32v2   Version 2 of DJ Delorie DOS extender
      -Tlinux    Linux
      -Tnetbsd   NetBSD
      -Tnetware  Novell Netware Module (clib)
      -Tnetwlibc Novell Netware Module (libc)
      -Topenbsd  OpenBSD
      -Tos2      OS/2 / eComStation
      -Tsunos    SunOS/Solaris
      -Tsymbian  Symbian OS
      -Twatcom   Watcom compatible DOS extender
      -Twdosx    WDOSX DOS extender
      -Twin32    Windows 32 Bit
      -Twince    Windows CE
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
      a : Show everything             x : Executable info (Win32 only)
      b : Write file names messages with full path
      v : Write fpcdebug.txt with     p : Write tree.log with parse tree
          lots of debugging info
  -W<x>  Target-specific options (targets)
      -Wb        Create a bundle instead of a library (Darwin)
      -WB        Create a relocatable image (Windows)
      -WC        Specify console type application (EMX, OS/2, Windows)
      -WD        Use DEFFILE to export functions of DLL or EXE (Windows)
      -WF        Specify full-screen type application (EMX, OS/2)
      -WG        Specify graphic type application (EMX, OS/2, Windows)
      -WN        Do not generate relocation code, needed for debugging (Windows)
      -WR        Generate relocation code (Windows)
  -X     Executable options:
      -Xc        Pass --shared/-dynamic to the linker (BeOS, Darwin, FreeBSD, Linux)
      -Xd        Do not use standard library search path (needed for cross compile)
      -Xe        Use external linker
      -Xg        Create debuginfo in a separate file and add a debuglink section to executable
      -XD        Try to link units dynamically      (defines FPC_LINK_DYNAMIC)
      -Xi        Use internal linker
      -Xm        Generate link map
      -XM<x>     Set the name of the 'main' program routine (default is 'main')
      -XP<x>     Prepend the binutils names with the prefix <x>
      -Xr<x>     Set library search path to <x> (needed for cross compile) (BeOS, Linux)
      -XR<x>     Prepend <x> to all linker search paths (BeOS, Darwin, FreeBSD, Linux, Mac OS, Solaris)
      -Xs        Strip all symbols from executable
      -XS        Try to link units statically (default, defines FPC_LINK_STATIC)
      -Xt        Link with static libraries (-static is passed to linker)
      -XX        Try to smartlink units             (defines FPC_LINK_SMART)

  -?     Show this help
  -h     Shows this help without waiting
  }
  
  

  { --------------- Parsing Tab ------------------- }

  { Assembler reading style  -Ratt = AT&T    -Rintel = Intel  -Rdirect = direct }
  case AssemblerStyle of
    1: switches := switches + '-Rintel';
    2: switches := switches + '-Ratt';
    3: switches := switches + '-Rdirect';
  end;
  
  // Syntax Options
  tempsw:=GetSyntaxOptionsString;
  if (tempsw <> '') then
    switches := switches + ' ' + tempsw;

  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
  -Un = Do not check the unit name
  -Us = Compile a system unit
}

  { ----------- Code Generation Tab --------------- }

  { UnitStyle   '' = Static     'D' = Dynamic (not implemented)   'X' = smart linked }
  if SmartLinkUnit then
    switches := switches + ' -CX';

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

  if (tempsw <> '') then begin
    switches := switches + ' -C' + tempsw;
  end;

  { Heap Size }
  if (HeapSize > 0) then
    switches := switches + ' ' + '-Ch' + IntToStr(HeapSize);

  { Optimizations }
  OptimizeSwitches:='';
  if SmallerCode then
    OptimizeSwitches := OptimizeSwitches + 's';
  { OptimizationLevel     1 = Level 1    2 = Level 2    3 = Level 3 }
  case (OptimizationLevel) of
    1:  OptimizeSwitches := OptimizeSwitches + '1';
    2:  OptimizeSwitches := OptimizeSwitches + '2';
    3:  OptimizeSwitches := OptimizeSwitches + '3';
  end;
  if OptimizeSwitches<>'' then
    switches := switches + ' -O'+OptimizeSwitches;

  // uncertain
  if (UncertainOptimizations) then
    Switches := Switches + ' -OoUNCERTAIN';

  // registers
  if (VariablesInRegisters) then
    Switches := Switches + ' -OoREGVAR';

  { TargetProcessor }
  if TargetProcessor<>'' then
    Switches:=Switches+' -Op'+UpperCase(TargetProcessor);

  if not (ccloNoMacroParams in Flags) then
  begin
    { Target OS
       GO32V1 = DOS and version 1 of the DJ DELORIE extender (no longer maintained).
       GO32V2 = DOS and version 2 of the DJ DELORIE extender.
       LINUX = LINUX.
       OS2 = OS/2 (2.x) using the EMX extender.
       WIN32 = Windows 32 bit.
        ... }
    { Target OS }
    if (Globals<>nil) and (Globals.TargetOS<>'') then
      switches := switches + ' -T' + Globals.TargetOS
    else if (TargetOS<>'') then
      switches := switches + ' -T' + TargetOS;
    { Target CPU }
    if (Globals<>nil) and (Globals.TargetCPU<>'') then
      switches := switches + ' -P' + Globals.TargetCPU
    else if (TargetCPU<>'') then
      switches := switches + ' -P' + TargetCPU;
  end;

  { --------------- Linking Tab ------------------- }
  
  { Debugging }
  { Debug Info for GDB }
  if (GenerateDebugInfo) then
    switches := switches + ' -g';

  { Line Numbers in Run-time Error Backtraces - Use LineInfo Unit }
  if (UseLineInfoUnit) then
    switches := switches + ' -gl';

  { Generate dwarf debug information }
  if (GenerateDwarf) then
    switches := switches + ' -gw';

  { Use Heaptrc Unit }
  if (UseHeaptrc) and (not (ccloNoLinkerOpts in Flags)) then
    switches := switches + ' -gh';

  { Generate code for Valgrind }
  if (UseValgrind) and (not (ccloNoLinkerOpts in Flags)) then
    switches := switches + ' -gv';

  { Generate code gprof }
  if (GenGProfCode) then
    switches := switches + ' -pg';

  if (UseExternalDbgSyms) then
    switches := switches + ' -Xg';

  { Strip Symbols }
  if (StripSymbols) and (not (ccloNoLinkerOpts in Flags)) then
    switches := switches + ' -Xs';

  { Link Style
     -XD = Link with dynamic libraries
     -XS = Link with static libraries, default on non-win32 platforms
     -XX = Link smart
  }

  if (not (ccloNoLinkerOpts in Flags)) and LinkSmart then
    switches := switches + ' -XX';


  // additional Linker options
  if PassLinkerOptions and (not (ccloNoLinkerOpts in Flags))
  and (not (ccloNoMacroParams in Flags)) then begin
    CurLinkerOptions:=ParsedOpts.GetParsedValue(pcosLinkerOptions);
    if (CurLinkerOptions<>'') then
      switches := switches + ' ' + ConvertOptionsToCmdLine(' ','-k', CurLinkerOptions);

    // inherited Linker options
    InhLinkerOpts:=GetInheritedOption(icoLinkerOptions,
      not (ccloAbsolutePaths in Flags),coptParsed);
    if InhLinkerOpts<>'' then
      switches := switches + ' ' + ConvertOptionsToCmdLine(' ','-k', InhLinkerOpts);

    // add Linker options for widgetset
    LinkerAddition := LCLWidgetLinkerAddition[DirNameToLCLPlatform(GetEffectiveLCLWidgetType)];
    if LinkerAddition <> '' then
      switches := switches + ' ' + LinkerAddition;
  end;
  
  if Win32GraphicApp then
    switches := switches + ' -WG';

  { ---------------- Other Tab -------------------- }

  { Verbosity }
  { The following switches will not be needed by the IDE
      r = Rhide/GCC compatibility mode
  }
  tempsw := '';
    
  if (ShowErrors) then
    tempsw := tempsw + 'e';
  if (ShowWarn) then
    tempsw := tempsw + 'w';
  if (ShowNotes) then
    tempsw := tempsw + 'n';
  if (ShowHints) then
    tempsw := tempsw + 'h';
  if (ShowGenInfo) then
    tempsw := tempsw + 'i';
  if (ShowLineNum) then
    tempsw := tempsw + 'l';
  if (ShowAllProcsOnError) then
    tempsw := tempsw + 'b';
  if (ShowDebugInfo) then
    tempsw := tempsw + 'd';
  if (ShowUsedFiles) then
    tempsw := tempsw + 'u';
  if (ShowTriedFiles) then
    tempsw := tempsw + 't';
  if (ShowDefMacros) then
    tempsw := tempsw + 'm';
  if (ShowCompProc) then
    tempsw := tempsw + 'p';
  if (ShowCond) then
    tempsw := tempsw + 'c';
  if (ShowExecInfo) then
    tempsw := tempsw + 'x';

  if ShowNothing then
    tempsw := '0';

  if ShowAll or (ccloAddVerboseAll in Flags) then
    tempsw := 'a';

  if (tempsw <> '') then begin
    tempsw := '-v' + tempsw;
    switches := switches + ' ' + tempsw;
  end;

  if (StopAfterErrCount>1) then
    switches := switches + ' -Se'+IntToStr(StopAfterErrCount);


  { Write an FPC logo }
  if (WriteFPCLogo) then
    switches := switches + ' -l';

  { Ignore Config File }
  if DontUseConfigFile then
    switches := switches + ' -n';

  { Use Custom Config File     @ = yes and path }
  if not (ccloNoMacroParams in Flags)
  and (CustomConfigFile) and (ConfigFilePath<>'') then begin
    switches := switches + ' ' + PrepareCmdLineOption('@' + ConfigFilePath);
  end;

  { ------------- Search Paths ---------------- }

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

    // object path
    CurObjectPath:=GetObjectPath(not (ccloAbsolutePaths in Flags),
                                 coptParsed,false);
    if (CurObjectPath <> '') then
      switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fo', CurObjectPath);

    // unit path
    CurUnitPath:=GetUnitPath(not (ccloAbsolutePaths in Flags));
    //debugln('TBaseCompilerOptions.MakeOptionsString A ',dbgsName(Self),' CurUnitPath="',CurUnitPath,'"');
    // always add the current directory to the unit path, so that the compiler
    // checks for changed files in the directory
    CurUnitPath:=MergeSearchPaths(CurUnitPath,'.');
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fu', CurUnitPath);

    { CompilerPath - Nothing needs to be done with this one }

    { Unit output directory }
    if (UnitOutputDirectory<>'') then begin
      CurOutputDir:=ParsedOpts.GetParsedValue(pcosOutputDir);
      if not (ccloAbsolutePaths in Flags) then
        CurOutputDir:=CreateRelativePath(CurOutputDir,BaseDirectory,true);
    end else
      CurOutputDir:='';
    debugln(['TBaseCompilerOptions.MakeOptionsString UnitOutputDirectory="',UnitOutputDirectory,'" CurOutputDir="',CurOutputDir,'"']);
    if CurOutputDir<>'' then
      switches := switches + ' '+PrepareCmdLineOption('-FU'+CurOutputDir);
  end;

  t := GetIgnoredMsgsIndexes(CompilerMessages, ',');
  if t <> '' then
    switches := switches + ' ' + PrepareCmdLineOption('-vm'+t);
  if fUseMsgFile and FileExistsCached(MsgFileName)then
    switches := switches + ' ' + PrepareCmdLineOption('-Fr'+MsgFileName);


  { ----------------------------------------------- }

  { TODO: The following switches need to be implemented. They need to
          be added to the dialog. }
{
  -P = Use pipes instead of files when assembling
      

  -a = Delete generated assembler files
  -al = Include source code lines in assembler files as comments
  -ar = List register allocation in assembler files
  -at = List temporary allocations and deallocations in assembler files
  -Axxx = Assembler type
       o = unix coff object file using GNU assembler as
       nasmcoff = coff file using nasm assembler
       nasmonj = obj file using nasm assembler
       masm = obj file using Microsoft masm assembler
       tasm = obj file using Borland tasm assembler
       
  -B = Recompile all units even if they didn't change  ->  implemented by compiler.pp
  -b = Generate browser info
  -bl = Generate browser info, including local variables, types and procedures

  -dxxx = Define symbol name xxx (Used for conditional compiles)
  -uxxx = Undefine symbol name xxx
  
  -Ce        Compilation with emulated floating point opcodes
  -CR        verify object method call validity

  -s = Do not call assembler or linker. Write ppas.bat/ppas.sh script.
  -st        Generate script to link on target
  -sh        Generate script to link on host
  -V     write fpcdebug.txt file with lots of debugging info

  -Xc = Link with C library (LINUX only)
       
}
  // append -o Option if neccessary
  {   * -o to define the target file name.
      * -FE if the target file name is not in the project directory (where the lpi file is)
      * -FU if the unit output directory is not empty }
  //DebugLn(['TBaseCompilerOptions.MakeOptionsString ',DbgSName(Self),' ',ccloDoNotAppendOutFileOption in Flags,' TargetFilename="',TargetFilename,'" CurMainSrcFile="',CurMainSrcFile,'" CurOutputDir="',CurOutputDir,'"']);
  if (not (ccloDoNotAppendOutFileOption in Flags))
    and (not (ccloNoMacroParams in Flags))
    and ((TargetFilename<>'') or (CurMainSrcFile<>'') or (CurOutputDir<>'')) then
  begin
    NewTargetFilename := CreateTargetFilename(CurMainSrcFile);
    if (NewTargetFilename<>'') and
       ((CompareFileNames(NewTargetFilename,ChangeFileExt(CurMainSrcFile,''))<>0) or
       (CurOutputDir<>'')) then
    begin
      if not (ccloAbsolutePaths in Flags) then
        NewTargetFilename := CreateRelativePath(NewTargetFilename, BaseDirectory);
      NewTargetDirectory := ExtractFilePath(NewTargetFilename);
      if (NewTargetDirectory <> '')
      and (CompareFilenames(ChompPathDelim(NewTargetDirectory),ChompPathDelim(BaseDirectory))=0)
      then begin
        // if target file is in the base directory, do not use -FE switch
        // Without -FE and -FU switch the compiler puts .ppu files in the source
        // directories, which is Delphi compatible.
        // See bug http://bugs.freepascal.org/view.php?id=15535
        NewTargetDirectory:='';
      end;
      if NewTargetDirectory <> '' then
        switches := switches + ' '+PrepareCmdLineOption('-FE' + NewTargetDirectory);
      NewTargetFileName := ExtractFileName(NewTargetFilename);
      switches := switches + ' '+PrepareCmdLineOption('-o' + NewTargetFileName);
    end;
  end;

  // custom options
  if not (ccloNoMacroParams in Flags) then
  begin
    CurCustomOptions:=GetCustomOptions(coptParsed);
    if CurCustomOptions<>'' then
      switches := switches+' '+CurCustomOptions;
  end;

  Result := switches;
end;

function TBaseCompilerOptions.GetSyntaxOptionsString: string;
var
  tempsw: String;
begin
  { Syntax Options
   -S<x>  Syntax options:
      -Sc        Support operators like C (*=,+=,/= and -=)
      -Sa        Turn on assertions
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
      -Ss        Constructor name must be init (destructor must be done)
      -St        Allow static keyword in objects
      -Sx        Enable exception keywords (default in Delphi/ObjFPC modes)

   -M<x>  Set language mode to <x>
      -Mfpc      Free Pascal dialect (default)
      -Mobjfpc   FPC mode with Object Pascal support
      -Mdelphi   Delphi 7 compatibility mode
      -Mtp       TP/BP 7.0 compatibility mode
      -Mmacpas   Macintosh Pascal dialects compatibility mode

  }
  if SyntaxMode<>'' then
    Result:='-M'+SyntaxMode
  else
    Result:='';

  tempsw := '';

  if (CStyleOperators) then
    tempsw := tempsw + 'c';
  if (IncludeAssertionCode) then
    tempsw := tempsw + 'a';
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

  if (tempsw <> '') then begin
    if Result<>'' then
      Result:=Result+' ';
    Result := Result+'-S' + tempsw;
  end;
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
  fLCLWidgetType := '';
  
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
  FSmallerCode := false;
  fTargetProc := '';
  fTargetCPU := '';
  fVarsInReg := false;
  fUncertainOpt := false;
  fOptLevel := 1;
  fTargetOS := '';
    
  // linking
  fGenDebugInfo := false;
  fUseLineInfoUnit := true;
  FGenerateDwarf := false;
  fUseHeaptrc := false;
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
  fShowAllProcsOnError := false;
  fShowDebugInfo := false;
  fShowUsedFiles := false;
  fShowTriedFiles := false;
  fShowDefMacros := false;
  fShowCompProc := false;
  fShowCond := false;
  fShowExecInfo := false;
  fShowNothing := false;
  fShowSummary := false;
  fShowHintsForUnusedUnitsInMainSrc := false;
  fShowHintsForSenderNotUsed := false;
  fWriteFPCLogo := true;
  fStopAfterErrCount := 1;
  
  fUseCustomMessages := false;  
  fCompilerMessages.Clear; 
  fCompilerMessages.SetDefault; 

  // other
  fDontUseConfigFile := false;
  fCustomConfigFile := false;
  fConfigFilePath := 'extrafpc.cfg';
  CustomOptions := '';
  
  // inherited
  ClearInheritedOptions;

  // compilation
  CompilerPath := '$(CompPath)';
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
  TargetFilenameAppplyConventions := CompOpts.TargetFilenameAppplyConventions;

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
  fLCLWidgetType := CompOpts.fLCLWidgetType;

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
  fIOChecks := CompOpts.fIOChecks;
  fRangeChecks := CompOpts.fRangeChecks;
  fOverflowChecks := CompOpts.fOverflowChecks;
  fStackChecks := CompOpts.fStackChecks;
  FEmulatedFloatOpcodes := CompOpts.fEmulatedFloatOpcodes;
  fHeapSize := CompOpts.fHeapSize;
  fEmulatedFloatOpcodes := CompOpts.fEmulatedFloatOpcodes;
  FSmallerCode := CompOpts.FSmallerCode;
  fTargetProc := CompOpts.fTargetProc;
  fTargetCPU := CompOpts.fTargetCPU;
  fVarsInReg := CompOpts.fVarsInReg;
  fUncertainOpt := CompOpts.fUncertainOpt;
  fOptLevel := CompOpts.fOptLevel;
  fTargetOS := CompOpts.fTargetOS;

  // Linking
  fGenDebugInfo := CompOpts.fGenDebugInfo;
  fUseLineInfoUnit := CompOpts.fUseLineInfoUnit;
  FGenerateDwarf := CompOpts.FGenerateDwarf;
  fUseHeaptrc := CompOpts.fUseHeaptrc;
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
  fShowAllProcsOnError := CompOpts.fShowAllProcsOnError;
  fShowDebugInfo := CompOpts.fShowDebugInfo;
  fShowUsedFiles := CompOpts.fShowUsedFiles;
  fShowTriedFiles := CompOpts.fShowTriedFiles;
  fShowDefMacros := CompOpts.fShowDefMacros;
  fShowCompProc := CompOpts.fShowCompProc;
  fShowCond := CompOpts.fShowCond;
  fShowCond := CompOpts.fShowExecInfo;
  fShowNothing := CompOpts.fShowNothing;
  fShowSummary := CompOpts.FShowSummary;
  fShowHintsForUnusedUnitsInMainSrc := CompOpts.fShowHintsForUnusedUnitsInMainSrc;
  fShowHintsForSenderNotUsed := CompOpts.fShowHintsForSenderNotUsed;
  fWriteFPCLogo := CompOpts.fWriteFPCLogo;

  // Messages
  fUseCustomMessages := CompOpts.fUseCustomMessages;
  fUseMsgFile := CompOpts.fUseMsgFile;
  fMsgFileName := CompOpts.fMsgFileName;
  fCompilerMessages.Assign(CompOpts.fCompilerMessages);

  // Other
  fDontUseConfigFile := CompOpts.fDontUseConfigFile;
  fCustomConfigFile := CompOpts.fCustomConfigFile;
  fConfigFilePath := CompOpts.fConfigFilePath;
  fStopAfterErrCount := CompOpts.fStopAfterErrCount;
  CustomOptions := CompOpts.CustomOptions;

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
var
  i: Integer;
begin
  Result:=false;
  //if Tool<>nil then debugln(['TBaseCompilerOptions.CreateDiff ',DbgSName(Self)]);
  if Done(Tool.AddPathsDiff('StorePathDelim',PathDelimSwitchToDelim[FStorePathDelim],
                            PathDelimSwitchToDelim[CompOpts.FStorePathDelim])) then exit;

  // target
  if Done(Tool.AddDiff('TargetFilename',fTargetFilename,CompOpts.fTargetFilename)) then exit;
  if Done(Tool.AddDiff('TargetFilenameAppplyConventions',FTargetFilenameAppplyConventions,CompOpts.FTargetFilenameAppplyConventions)) then exit;

  // search paths
  if Tool<>nil then Tool.Path:='Paths';
  if Done(Tool.AddPathsDiff('IncludePaths',IncludePath,CompOpts.IncludePath)) then exit;
  if Done(Tool.AddPathsDiff('LibraryPaths',Libraries,CompOpts.Libraries)) then exit;
  if Done(Tool.AddPathsDiff('UnitPaths',OtherUnitFiles,CompOpts.OtherUnitFiles)) then exit;
  if Done(Tool.AddPathsDiff('UnitOutputDir',UnitOutputDirectory,CompOpts.UnitOutputDirectory)) then exit;
  if Done(Tool.AddPathsDiff('ObjectPath',ObjectPath,CompOpts.ObjectPath)) then exit;
  if Done(Tool.AddPathsDiff('SrcPath',SrcPath,CompOpts.SrcPath)) then exit;
  if Done(Tool.AddPathsDiff('DebugPath',DebugPath,CompOpts.DebugPath)) then exit;

  // conditionals
  if Done(Tool.AddPathsDiff('Conditionals',FConditionals,CompOpts.FConditionals)) then exit;
  if Tool<>nil then Tool.Path:='BuildModes';
  if Done(TIDEBuildMacros(fBuildMacros).CreateDiff(CompOpts.BuildMacros,Tool)) then exit;
  if Done(Tool.AddDiff('LCLWidgetType',fLCLWidgetType,CompOpts.fLCLWidgetType)) then exit;

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
  if Done(Tool.AddDiff('IOChecks',fIOChecks,CompOpts.fIOChecks)) then exit;
  if Done(Tool.AddDiff('RangeChecks',fRangeChecks,CompOpts.fRangeChecks)) then exit;
  if Done(Tool.AddDiff('OverflowChecks',fOverflowChecks,CompOpts.fOverflowChecks)) then exit;
  if Done(Tool.AddDiff('StackChecks',fStackChecks,CompOpts.fStackChecks)) then exit;
  if Done(Tool.AddDiff('EmulatedFloatOpcodes',FEmulatedFloatOpcodes,CompOpts.FEmulatedFloatOpcodes)) then exit;
  if Done(Tool.AddDiff('HeapSize',fHeapSize,CompOpts.fHeapSize)) then exit;
  if Done(Tool.AddDiff('EmulatedFloatOpcodes',fEmulatedFloatOpcodes,CompOpts.fEmulatedFloatOpcodes)) then exit;
  if Done(Tool.AddDiff('SmallerCode',FSmallerCode,CompOpts.FSmallerCode)) then exit;
  if Done(Tool.AddDiff('TargetProc',fTargetProc,CompOpts.fTargetProc)) then exit;
  if Done(Tool.AddDiff('TargetCPU',fTargetCPU,CompOpts.fTargetCPU)) then exit;
  if Done(Tool.AddDiff('VarsInReg',fVarsInReg,CompOpts.fVarsInReg)) then exit;
  if Done(Tool.AddDiff('UncertainOpt',fUncertainOpt,CompOpts.fUncertainOpt)) then exit;
  if Done(Tool.AddDiff('OptLevel',fOptLevel,CompOpts.fOptLevel)) then exit;
  if Done(Tool.AddDiff('TargetOS',fTargetOS,CompOpts.fTargetOS)) then exit;

  // linking
  if Tool<>nil then Tool.Path:='Linking';
  if Done(Tool.AddDiff('GenDebugInfo',fGenDebugInfo,CompOpts.fGenDebugInfo)) then exit;
  if Done(Tool.AddDiff('UseLineInfoUnit',fUseLineInfoUnit,CompOpts.fUseLineInfoUnit)) then exit;
  if Done(Tool.AddDiff('GenerateDwarf',FGenerateDwarf,CompOpts.FGenerateDwarf)) then exit;
  if Done(Tool.AddDiff('UseHeaptrc',fUseHeaptrc,CompOpts.fUseHeaptrc)) then exit;
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
  if Done(Tool.AddDiff('ShowAllProcsOnError',fShowAllProcsOnError,CompOpts.fShowAllProcsOnError)) then exit;
  if Done(Tool.AddDiff('ShowDebugInfo',fShowDebugInfo,CompOpts.fShowDebugInfo)) then exit;
  if Done(Tool.AddDiff('ShowUsedFiles',fShowUsedFiles,CompOpts.fShowUsedFiles)) then exit;
  if Done(Tool.AddDiff('ShowTriedFiles',fShowTriedFiles,CompOpts.fShowTriedFiles)) then exit;
  if Done(Tool.AddDiff('ShowDefMacros',fShowDefMacros,CompOpts.fShowDefMacros)) then exit;
  if Done(Tool.AddDiff('ShowCompProc',fShowCompProc,CompOpts.fShowCompProc)) then exit;
  if Done(Tool.AddDiff('ShowCond',fShowCond,CompOpts.fShowCond)) then exit;
  if Done(Tool.AddDiff('ShowExecInfo',fShowExecInfo,CompOpts.fShowExecInfo)) then exit;
  if Done(Tool.AddDiff('ShowNothing',fShowNothing,CompOpts.fShowNothing)) then exit;
  if Done(Tool.AddDiff('ShowSummary',fShowSummary,CompOpts.fShowSummary)) then exit;
  if Done(Tool.AddDiff('ShowHintsForUnusedUnitsInMainSrc',fShowHintsForUnusedUnitsInMainSrc,CompOpts.fShowHintsForUnusedUnitsInMainSrc)) then exit;
  if Done(Tool.AddDiff('ShowHintsForSenderNotUsed',fShowHintsForSenderNotUsed,CompOpts.fShowHintsForSenderNotUsed)) then exit;
  if Done(Tool.AddDiff('WriteFPCLogo',fWriteFPCLogo,CompOpts.fWriteFPCLogo)) then exit;

  //messages
  if Tool<>nil then Tool.Path:='Messages';
  if Done(Tool.AddDiff('UseCustomMessages',fUseCustomMessages,CompOpts.fUseCustomMessages)) then exit;
  if Done(Tool.AddDiff('UseMsgFile',fUseMsgFile,CompOpts.fUseMsgFile)) then exit;
  if Done(Tool.AddDiff('MsgFileName',fMsgFileName,CompOpts.fMsgFileName)) then exit;
  for i:=0 to fCompilerMessages.Count-1 do
    if fCompilerMessages.Msg[i].Ignored<>CompOpts.fCompilerMessages.Msg[i].Ignored
    then begin
      Result:=true;
      Tool.AddDiff('Ignored'+IntToStr(fCompilerMessages.Msg[i].MsgIndex),
        fCompilerMessages.Msg[i].Ignored,CompOpts.fCompilerMessages.Msg[i].Ignored);
    end;

  // other
  if Tool<>nil then Tool.Path:='Other';
  if Done(Tool.AddDiff('DontUseConfigFile',fDontUseConfigFile,CompOpts.fDontUseConfigFile)) then exit;
  if Done(Tool.AddDiff('CustomConfigFile',fCustomConfigFile,CompOpts.fCustomConfigFile)) then exit;
  if Done(Tool.AddDiff('ConfigFilePath',fConfigFilePath,CompOpts.fConfigFilePath)) then exit;
  if Done(Tool.AddDiff('StopAfterErrCount',fStopAfterErrCount,CompOpts.fStopAfterErrCount)) then exit;
  if Done(Tool.AddDiff('CustomOptions',CustomOptions,CompOpts.CustomOptions)) then exit;

  // compilation
  if Tool<>nil then Tool.Path:='Compilation';
  if Done(Tool.AddDiff('CompilerPath',CompilerPath,CompOpts.CompilerPath)) then exit;
  if Done(ExecuteBefore.CreateDiff(CompOpts.ExecuteBefore,Tool)) then exit;
  if Done(ExecuteAfter.CreateDiff(CompOpts.ExecuteAfter,Tool)) then exit;
  if Done(Tool.AddDiff('CreateMakefileOnBuild',fCreateMakefileOnBuild,CompOpts.fCreateMakefileOnBuild)) then exit;
  if Result then debugln(['TBaseCompilerOptions.CreateDiff END']);
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
  Result:=FParsedOpts.UnparsedValues[pcosUnitPath];
end;

function TAdditionalCompilerOptions.GetIncludePath: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosIncludePath];
end;

function TAdditionalCompilerOptions.GetBaseDirectory: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosBaseDir];
end;

function TAdditionalCompilerOptions.GetCustomOptions: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosCustomOptions];
end;

function TAdditionalCompilerOptions.GetLibraryPath: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosLibraryPath];
end;

function TAdditionalCompilerOptions.GetLinkerOptions: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosLinkerOptions];
end;

function TAdditionalCompilerOptions.GetObjectPath: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosObjectPath];
end;

function TAdditionalCompilerOptions.GetSrcPath: string;
begin
  Result:=FParsedOpts.UnparsedValues[pcosSrcPath];
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
  SrcPath:='';
  IncludePath:='';
  CustomOptions:='';
  LibraryPath:='';
  LinkerOptions:='';
  ObjectPath:='';
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
  case AnOption of
  icoNone: Result:='';
  icoUnitPath: Result:=UnitPath;
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

function TAdditionalCompilerOptions.
  GetBaseCompilerOptions: TBaseCompilerOptions;
begin
  Result:=nil;
end;

{ TParsedCompilerOptions }

procedure TParsedCompilerOptions.SetOutputDirectoryOverride(const AValue: string
  );
var
  NewDir: String;
begin
  NewDir:=AppendPathDelim(TrimFilename(AValue));
  if FOutputDirectoryOverride=NewDir then exit;
  FOutputDirectoryOverride:=NewDir;
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
  MacroValues:=TCTConfigScriptEngine.Create;
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
  Result:=(ParsedErrorStamp<>InvalidParseStamp)
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
begin
  Result:=UnparsedValues[Option];
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
    Result:=MergeSearchPaths(Result,SetDirSeparators(Vars[VarName]));
  pcosLinkerOptions:
    Result:=MergeLinkerOptions(Result,Vars[VarName]);
  pcosCustomOptions:
    Result:=MergeCustomOptions(Result,Vars[VarName]);
  pcosOutputDir:
    if Vars.IsDefined(PChar(VarName)) then Result:=SetDirSeparators(Vars[VarName]);
  pcosCompilerPath:
    if Vars.IsDefined(PChar(VarName)) then Result:=SetDirSeparators(Vars[VarName]);
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
  if ParsedStamp[Option]<>CompilerParseStamp then begin
    if Parsing[Option] then begin
      DebugLn('TParsedCompilerOptions.GetParsedValue Circle in Options: ',ParsedCompilerOptStringNames[Option],' Unparsed="',UnparsedValues[Option],'"');
      ParsedError(Option,'Circle in macros');
      exit('');
    end;
    Parsing[Option]:=true;
    try
      s:=DoParseOption(GetUnparsedWithConditionals(Option),Option,false);
      ParsedValues[Option]:=s;
      ParsedStamp[Option]:=CompilerParseStamp;
      //if Option=pcosCustomOptions then begin
      //  DebugLn('TParsedCompilerOptions.GetParsedValue PARSED ',dbgs(ParsedStamp[Option]),' ',dbgs(CompilerParseStamp),' new="',ParsedValues[Option],'"');
      //end;
    finally
      Parsing[Option]:=false;
    end;
  end;
  Result:=ParsedValues[Option];
end;

function TParsedCompilerOptions.GetParsedPIValue(
  Option: TParsedCompilerOptString): string;
var
  s: String;
begin
  if ParsedPIStamp[Option]<>CompilerParseStamp then begin
    if ParsingPI[Option] then begin
      DebugLn('TParsedCompilerOptions.GetParsedPIValue Circle in Options: ',ParsedCompilerOptStringNames[Option]);
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
  if NewValue=UnparsedValues[Option] then exit;
  if InvalidateParseOnChange then IncreaseCompilerParseStamp;
  if Option=pcosBaseDir then
    InvalidateFiles
  else begin
    ParsedStamp[Option]:=InvalidParseStamp;
    ParsedPIStamp[Option]:=InvalidParseStamp;
  end;
  UnparsedValues[Option]:=NewValue;
end;

function TParsedCompilerOptions.DoParseOption(const OptionText: string;
  Option: TParsedCompilerOptString; PlatformIndependent: boolean): string;

  function GetBaseDir: string;
  begin
    if PlatformIndependent then
      Result:=GetParsedPIValue(pcosBaseDir)
    else
      Result:=GetParsedValue(pcosBaseDir);
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
  BaseDirectory: String;
begin
  s:=OptionText;

  // parse locally (macros depending on owner, like pkgdir and build macros)
  //DebugLn(['TParsedCompilerOptions.DoParseOption local "',s,'" ...']);
  if Assigned(OnLocalSubstitute) then
    s:=OnLocalSubstitute(s,PlatformIndependent);
  // parse globally (general macros)
  //DebugLn(['TParsedCompilerOptions.DoParseOption global "',s,'" ...']);
  s:=ParseString(Self,s,PlatformIndependent);
  //DebugLn(['TParsedCompilerOptions.DoParseOption complete "',s,'" ...']);
  // improve
  if Option=pcosBaseDir then
    // base directory (append path)
    s:=AppendPathDelim(TrimFilename(s))
  else if Option in ParsedCompilerFilenames then begin
    // make filename absolute
    MakeFilenameAbsolute(s);
  end
  else if Option in ParsedCompilerDirectories then begin
    // make directory absolute
    s:=TrimFilename(s);
    if Option<>pcosBaseDir then
      MakeFilenameAbsolute(s);
    s:=AppendPathDelim(s);
  end
  else if Option in ParsedCompilerSearchPaths then begin
    // make search paths absolute
    BaseDirectory:=GetBaseDir;
    s:=TrimSearchPath(s,BaseDirectory);
  end else if Option=pcosCustomOptions then begin
    s:=SpecialCharsToSpaces(s,true);
  end;
  Result:=s;
end;

procedure TParsedCompilerOptions.Clear;
var
  Option: TParsedCompilerOptString;
begin
  InvalidateAll;
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
  begin
    ParsedValues[Option]:='';
    ParsedPIValues[Option]:='';
    UnparsedValues[Option]:='';
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
    ParsedStamp[Option]:=InvalidParseStamp;
    ParsedPIStamp[Option]:=InvalidParseStamp;
  end;
  InheritedMacroValuesStamp:=InvalidParseStamp;
  MacroValuesStamp:=InvalidParseStamp;
  ParsedErrorStamp:=InvalidParseStamp;
end;

procedure TParsedCompilerOptions.InvalidateFiles;
var
  Option: TParsedCompilerOptString;
begin
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
    if (Option in ParsedCompilerFiles) then begin
      ParsedStamp[Option]:=InvalidParseStamp;
      ParsedPIStamp[Option]:=InvalidParseStamp;
    end;
end;

procedure TParsedCompilerOptions.RenameMacro(const OldName, NewName: string;
  out Changed: TParsedCompilerOptStrings);
var
  o: TParsedCompilerOptString;
  s: String;
begin
  Changed:=[];
  for o:=Low(UnparsedValues) to High(UnparsedValues) do
  begin
    s:=UnparsedValues[o];
    RenameIDEMacroInString(s,OldName,NewName);
    if s<>UnparsedValues[o] then begin
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
  IncreaseChangeStamp;
end;

procedure TCompilationToolOptions.SetScanForFPCMessages(const AValue: boolean);
begin
  if FScanForFPCMessages=AValue then exit;
  FScanForFPCMessages:=AValue;
  IncreaseChangeStamp;
end;

procedure TCompilationToolOptions.SetScanForMakeMessages(const AValue: boolean
  );
begin
  if FScanForMakeMessages=AValue then exit;
  FScanForMakeMessages:=AValue;
  IncreaseChangeStamp;
end;

procedure TCompilationToolOptions.SetShowAllMessages(const AValue: boolean);
begin
  if FShowAllMessages=AValue then exit;
  FShowAllMessages:=AValue;
  IncreaseChangeStamp;
end;

procedure TCompilationToolOptions.Clear;
begin
  Command:='';
  ScanForFPCMessages:=false;
  ScanForMakeMessages:=false;
  ShowAllMessages:=false;
end;

procedure TCompilationToolOptions.Assign(Src: TCompilationToolOptions);
begin
  Command:=Src.Command;
  ScanForFPCMessages:=Src.ScanForFPCMessages;
  ScanForMakeMessages:=Src.ScanForMakeMessages;
  ShowAllMessages:=Src.ShowAllMessages;
end;

procedure TCompilationToolOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
begin
  //debugln(['TCompilationToolOptions.LoadFromXMLConfig ',Command,' Path=',Path]);
  Command:=SwitchPathDelims(XMLConfig.GetValue(Path+'Command/Value',''),
                            DoSwitchPathDelims);
  ScanForFPCMessages:=XMLConfig.GetValue(Path+'ScanForFPCMsgs/Value',false);
  ScanForMakeMessages:=XMLConfig.GetValue(Path+'ScanForMakeMsgs/Value',false);
  ShowAllMessages:=XMLConfig.GetValue(Path+'ShowAllMessages/Value',false);
end;

procedure TCompilationToolOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);
begin
  //debugln(['TCompilationToolOptions.SaveToXMLConfig ',Command,' Path=',Path]);
  XMLConfig.SetDeleteValue(Path+'Command/Value',
                           SwitchPathDelims(Command,UsePathDelim),'');
  XMLConfig.SetDeleteValue(Path+'ScanForFPCMsgs/Value',
                           ScanForFPCMessages,false);
  XMLConfig.SetDeleteValue(Path+'ScanForMakeMsgs/Value',
                           ScanForMakeMessages,false);
  XMLConfig.SetDeleteValue(Path+'ShowAllMessages/Value',
                           ShowAllMessages,false);
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
  if Done(Tool.AddDiff('ScanForFPCMessages',ScanForFPCMessages,CompOpts.ScanForFPCMessages)) then exit;
  if Done(Tool.AddDiff('ScanForMakeMessages',ScanForMakeMessages,CompOpts.ScanForMakeMessages)) then exit;
  if Done(Tool.AddDiff('ShowAllMessages',ShowAllMessages,CompOpts.ShowAllMessages)) then exit;
end;

function TCompilationToolOptions.Execute(const WorkingDir, ToolTitle: string
  ): TModalResult;
var
  ProgramFilename, Params: string;
  ExtTool: TIDEExternalToolOptions;
  Filename: String;
begin
  if Command='' then begin
    Result:=mrOk;
    exit;
  end;

  if SourceEditorManagerIntf<>nil then
    SourceEditorManagerIntf.ClearErrorLines;

  SplitCmdLine(Command,ProgramFilename,Params);
  if not FilenameIsAbsolute(ProgramFilename) then begin
    Filename:=FindProgram(ProgramFilename,WorkingDir,true);
    if Filename<>'' then ProgramFilename:=Filename;
  end;

  ExtTool:=TIDEExternalToolOptions.Create;
  try
    ExtTool.Filename:=ProgramFilename;
    ExtTool.ScanOutputForFPCMessages:=ScanForFPCMessages;
    ExtTool.ScanOutputForMakeMessages:=ScanForMakeMessages;
    ExtTool.ScanOutput:=true;
    ExtTool.ShowAllOutput:=ShowAllMessages;
    ExtTool.Title:=ToolTitle;
    ExtTool.WorkingDirectory:=WorkingDir;
    ExtTool.CmdLineParams:=Params;

    // run
    Result:=RunExternalTool(ExtTool);
  finally
    // clean up
    ExtTool.Free;
  end;
end;

procedure TCompilationToolOptions.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp64(FChangeStamp);
  if assigned(OnChanged) then OnChanged(Self);
end;

{ TGlobalCompilerOptions }

procedure TGlobalCompilerOptions.SetTargetCPU(const AValue: string);
begin
  if FTargetCPU=AValue then exit;
  FTargetCPU:=AValue;
end;

procedure TGlobalCompilerOptions.SetTargetOS(const AValue: string);
begin
  if FTargetOS=AValue then exit;
  FTargetOS:=AValue;
end;

{ TIDEBuildMacro }

procedure TIDEBuildMacro.SetIdentifier(const AValue: string);
begin
  if FIdentifier=AValue then exit;
  if (AValue='') or (not IsValidIdent(AValue)) then
    raise Exception.Create('TIDEBuildMacro.SetIdentifier invalid identifier: '+AValue);
  FIdentifier:=AValue;
  IncreaseChangeStamp;
  IncreaseBuildMacroChangeStamp;
end;

procedure TIDEBuildMacro.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  IncreaseChangeStamp;
end;

procedure TIDEBuildMacro.SetValueDescriptions(const AValue: TStrings);
begin
  if (FValueDescriptions=AValue) or FValueDescriptions.Equals(AValue) then exit;
  FValueDescriptions.Assign(AValue);
  IncreaseChangeStamp;
end;

procedure TIDEBuildMacro.SetValues(const AValue: TStrings);
begin
  if (FValues=AValue) or FValues.Equals(AValue) then exit;
  FValues.Assign(AValue);
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
    if (NewItem.Identifier<>'') and IsValidIdent(NewItem.Identifier) then
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

{ TCompilerMessagesList }

procedure TCompilerMessagesList.ClearHash; 
var
  i : integer; 
begin
  for i := 0 to length(fHash) - 1 do SetLength(fHash[i], 0); 
end;

procedure TCompilerMessagesList.AddHash(Msg: TCompilerMessageConfig); 
var
  idx : Integer; 
  sub : Integer; 
begin
  idx := Msg.MsgIndex div 1000;
  sub := Msg.MsgIndex mod 1000;
  while length(fHash) <= idx do 
    if length(FHash) = 0 
      then SetLength(fHash, 16)
      else SetLength(fHash, length(fHash)*2); 

  while length(fHash[idx]) <= sub do 
    if length(fHash[idx]) = 0 
      then SetLength(fHash[idx], 16)
      else SetLength(fHash[idx], length(fHash[idx])*2); 

  fHash[idx][sub] := Msg; 
end;

function TCompilerMessagesList.FindHash(AIndex: integer): TCompilerMessageConfig; 
var 
  idx : Integer; 
  sub : Integer; 
begin
  idx := AIndex div 1000;
  sub := AIndex mod 1000;
  Result := nil; 
  if (idx >= 0) and (idx < length(fHash)) then begin
    if (sub >= 0) and (sub < length(fHash[idx])) then
      Result := fHash[idx][sub]; 
  end; 
   
end;

function TCompilerMessagesList.GetMsgConfigByIndex(AIndex: Integer): TCompilerMessageConfig; 
begin
  Result := FindHash(Aindex);
end;

function TCompilerMessagesList.GetMsgConfig(i: Integer): TCompilerMessageConfig;
begin
  Result := TCompilerMessageConfig(fItems[i]);
end;

procedure TCompilerMessagesList.SetMsgIgnored(i: Integer; const AValue: Boolean);
begin
  msg[i].Ignored := AValue;
end;

function TCompilerMessagesList.GetMsgIgnored(i: Integer): Boolean;
begin
  Result := msg[i].Ignored;
end;

procedure TCompilerMessagesList.GetIgnoredArray(var b: array of Boolean); 
var
  i   : Integer; 
  idx : Integer; 
begin
  FillChar(b[0], length(b)*sizeof(boolean), false);
  for i := 0 to Count - 1 do begin 
    idx := msg[i].MsgIndex; 
    if (idx >= 0) and (idx < length(b)) then  
      b[idx] := msg[i].Ignored; 
  end; 
end;

procedure TCompilerMessagesList.SetIgnoredArray(const b: array of Boolean); 
var
  i   : Integer; 
  idx : Integer; 
begin
  for i := 0 to Count - 1 do begin 
    idx := msg[i].MsgIndex; 
    if (idx >= 0) and (idx < length(b)) then
      msg[i].Ignored := b[idx];
  end; 
end;

function TCompilerMessagesList.GetCount: Integer; 
begin
  Result := fItems.Count; 
end;

function TCompilerMessagesList.GetErrorNames(errtype: TFPCErrorType): string;
begin
  Result := FErrorNames[errtype];
end;

constructor TCompilerMessagesList.Create; 
begin
  inherited Create; 
  fItems := TFPList.Create; 
end;

destructor TCompilerMessagesList.Destroy;
begin
  Clear; 
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TCompilerMessagesList.Clear;
var
  i : integer;
  obj : TCompilerMessageConfig;
begin
  for i := 0 to fItems.Count - 1 do begin
    obj := TCompilerMessageConfig(fItems[i]);
    if Assigned(obj) then obj.Free;
  end;
  fItems.Clear; 
  ClearHash; 
end;

procedure TCompilerMessagesList.Assign(Src: TCompilerMessagesList); 
var
  i   : Integer; 
  m   : TCompilerMessageConfig; 
  err : TFPCErrorType;
begin
  BeginUpdate;
  try
    Clear;
    fUsedMsgFile := Src.fUsedMsgFile; 
    for i := 0 to Src.Count - 1 do begin
      with Src.Msg[i]do begin
        m := TCompilerMessageConfig.Create(Self);
        m.MsgIndex := MsgIndex;
        m.MsgText := MsgText;
        m.Ignored := Ignored;
        m.MsgType := MsgType;
        fItems.Add(m);
        AddHash(m);
      end;
    end; 
    for err := Low(err) to High(err) do  FErrorNames[err] := Src.FErrorNames[err];
  finally
    EndUpdate; 
  end;   
end; 

procedure TCompilerMessagesList.BeginUpdate; 
begin
  inc(fUpdating); 
end;

procedure TCompilerMessagesList.EndUpdate; 
begin
  dec(fUpdating); 
end;

function TCompilerMessagesList.LoadMsgFile(const FileName: string): Boolean;

  function IsMsgLine(const s: string; out msgIdx: Integer; out msgType, msgText: string;
    out isMultiLine: Boolean): Boolean;
  var
    i   : Integer; 
    p   : Integer; 
    err : Integer; 
    sub : string; 
  begin
    Result := (s <> '')  and not(s[1] in ['#',';','%']);
    if not Result then Exit;
        
    p := Pos('=', s);
    Result := p > 0;
    if not Result then Exit;
  
    sub := Copy(s, p+1, 5);
    Result := length(sub) = 5;
    if not Result then Exit;
     
    val( sub, msgIdx, err);
    Result := err = 0;
    if not Result then Exit; 
  
    inc(p, 6); 
    Result := s[p] = '_';
    if not Result then Exit;
    inc(p); 
    i := p; 
    while (p <= length(s)) and (s[p] <> '_') do inc(p); 
    msgType := Copy(s, i, p-i); 
    isMultiLine := msgType = '[';
    if isMultiLine then msgType := ''; 
  
    inc(p);
    msgText := Copy(s, p, length(s) - p + 1); 
    Result := true; 
  end;         
  
  function GetNextMultiLine(const s: string; var EndOfMultiLine: Boolean): string;
  begin
    EndOfMultiLine := s = ']';
    if EndOfMultiLine then Result := ''
    else Result := s; 
  end; 

  function StrToErrType(const msgtype: String): TFPCErrorType;
  begin
    if length(msgtype)<>1 then
      Result:=etNone
    else
      case UpperCase(msgtype)[1] of
        'W':Result:=etWarning;
        'H':Result:=etHint;
        'N':Result:=etNote;
        'F':Result:=etFatal;
        'E':Result:=etError;
      else
        Result:=etNone;
      end;
  end;

var
  temp    : TStringList; 
  isMln   : Boolean; 
  midx    : Integer;
  mtype   : string;
  mtext   : string; 
  i       : Integer; 
  lst     : Boolean; 
  b       : array of Boolean; 
  err     : TFPCErrorType;
const
  idxFatal   = 01012;
  idxError   = 01013;
  idxWarning = 01014;
  idxNote    = 01015;
  idxHint    = 01016;
begin
  BeginUpdate; 
  try
    SetLength(b, MaxMsgIndex);
    GetIgnoredArray(b); 
    
    SetDefault(false);
     
    temp := TStringList.Create;
    try 
      temp.LoadFromFile(FileName); 
      i := 0;
      while i < temp.Count do begin
        if IsMsgLine(temp[i], midx, mtype, mtext, isMln) then begin
          if isMln then begin
            lst := false; 
            while (i < temp.Count) and (not lst) do begin
              inc(i); 
              mtext:=mtext+#10+GetNextMultiLine(temp[i], lst);
            end; 
          end;

          Add(midx, StrToErrType(mtype), mtext, b[midx]);

          if (midx >= idxFatal) and (midx<= idxHint) then begin
            case midx of
              idxFatal: err := etFatal;
              idxError: err := etError;
              idxWarning: err := etWarning;
              idxNote: err := etNote;
              idxHint: err := etHint;
            else
              err := etNone;
            end;
            if err <> etNone then begin
              mtext := Trim(mtext);
              if (length(mtext)>1) and (mtext[length(mtext)]=':') then
                FErrorNames[err]:=Copy(mtext, 1, length(mtext)-1)
              else
                FErrorNames[err]:=mtext;
            end;
          end;

        end;
        inc(i);
      end; 
      Result := true; 
      fUsedMsgFile := FileName;    
    finally
      temp.Free; 
      SetIgnoredArray(b); 
      EndUpdate; 
    end;  
  except
    Result := false; 
  end; 
end;

function IntToStrLen(i:Integer; len: integer; FillCh: Char = '0'): string;
var
  s : string; 
  j : integer; 
begin
  if len <= 0 then begin
    Result := ''; 
    Exit;
  end; 
  s := IntToStr(i);
  if length(s)>= len then 
    Result := s
  else begin 
    SetLength(Result, len);
    FillChar(Result[1], len, FillCh); 
    j := (len - length(s)) + 1; 
    Move(s[1], Result[j], length(s)); 
  end; 
end; 

function TCompilerMessagesList.Add(AMsgIndex: Integer;
  AMsgType: TFPCErrorType; const AMsgText: string; AIgnored: Boolean): TCompilerMessageConfig;
var
  msgconf : TCompilerMessageConfig;
  prm   : array of string;
  cnt   : Integer;   
begin
  msgconf := FindHash(AMsgIndex); 
  if not Assigned(msgConf) then begin 
    msgconf := TCompilerMessageConfig.Create(Self);
    msgconf.MsgIndex := AMsgIndex;
    fItems.Add(msgconf);
    AddHash(msgconf); 
  end; 
  msgconf.MsgType := AMsgType;
  msgconf.MsgText := AMsgText;
  msgconf.Ignored := AIgnored;
  SetLength(prm, MaxMsgParams); 
  GetParams(AMsgIndex, prm, cnt); 
  Result := msgconf; 
end;

function GetNextNumber(const s: string; var index: Integer; var Num : Integer): Boolean; 
var
  i : integer; 
  err:Integer; 
begin
  i := index;
  while (i <= length(s)) and (s[i] in ['0'..'9']) do inc (i); 
  Result := i - index > 0;
  if Result then begin 
    Val(Copy(s, Index, i - Index), Num, err);
    if err=0 then ;
    index := i;
  end; 
end; 

function ReplaceParamsArray(const ACompilerMsg: string;
  const ReplaceParams: array of string): string;
var
  j   : Integer; 
  i   : Integer; 
  nm  : Integer; 
  p   : Integer; 
begin
  if length(ReplaceParams)=0 then begin
    Result:=ACompilerMsg;
    Exit;
  end;
  i := 1;
  p := 1;
  Result := '';
  while i <= length(ACompilerMsg) do begin 
    if ACompilerMsg[i] = '$' then begin
      j := i + 1; 
      nm := 0; 
      if GetNextNumber(ACompilerMsg, j, nm) then begin
        Result := Result + Copy(ACompilerMsg, p, i - p);
        if nm <= length(ReplaceParams) then
          Result := Result + ReplaceParams[nm-1]
        else
          Result:=Result+'$'+IntToStr(nm);
        p := j;
        i := p; 
      end else
        inc(i); 
    end else 
      inc(i); 
  end; 
  if p < length(ACompilerMsg) then
    Result := Result + Copy(ACompilerMsg, p, length(ACompilerMsg) - p + 1);
end;

procedure TCompilerMessagesList.SetDefault(KeepIgnored: Boolean);
var
  b   : array of Boolean; 
  err : TFPCErrorType;
begin
  if KeepIgnored then begin
    SetLength(b, MaxMsgIndex); 
    GetIgnoredArray(b) 
  end; 
  BeginUpdate;
  try 
    Clear; 
    for err := low(TFPCErrorType) to High(TFPCErrorType) do
      FErrorNames[err]:=FPCErrorTypeNames[err];

    Add(03005,etWarning,'Procedure type "$1" ignored');
    Add(03011,etWarning,'Relocatable DLL or executable $1 debug info does not work, disabled.');
    Add(03012,etWarning,'To allow debugging for win32 code you need to disable relocation with -WN option');
    Add(03018,etWarning,'Constructor should be public');
    Add(03019,etWarning,'Destructor should be public');
    Add(03020,etNote,'Class should have one destructor only');
    Add(03023,etNote,'The object "$1" has no VMT');
    Add(03031,etNote,'Values in enumeration types have to be ascending');
    Add(03036,etWarning,'range check error while evaluating constants');
    Add(03042,etWarning,'use extended syntax of NEW and DISPOSE for instances of objects');
    Add(03043,etWarning,'use of NEW or DISPOSE for untyped pointers is meaningless');
    Add(03057,etWarning,'An inherited method is hidden by "$1"');
    Add(03060,etWarning,'Stored property directive is not yet implemented');
    Add(03094,etWarning,'Unknown procedure directive had to be ignored: "$1"');
    Add(03100,etWarning,'Virtual methods are used without a constructor in "$1"');
    Add(03123,etWarning,'"$1" not yet supported inside inline procedure/function');
    Add(03124,etWarning,'Inlining disabled');
    Add(03126,etHint,'may be pointer dereference is missing');
    Add(03141,etWarning,'string "$1" is longer than "$2"');
    Add(03149,etWarning,'Don'#39't load OBJPAS unit manually, use \{\$mode objfpc\} or \{\$mode delphi\} instead');
    Add(03168,etWarning,'Procedure named "$1" not found that is suitable for implementing the $2.$3');
    Add(03175,etWarning,'Some fields coming before "$1" weren'#39't initialized');
    Add(03177,etWarning,'Some fields coming after "$1" weren'#39't initialized');
    Add(03182,etWarning,'Overriding calling convention "$1" with "$2"');
    Add(03186,etWarning,'Use of unsupported feature!');
    Add(03187,etHint,'C arrays are passed by reference');
    Add(03189,etHint,'Type "$1" redefinition');
    Add(03190,etWarning,'cdecl'#39'ared functions have no high parameter');
    Add(03191,etWarning,'cdecl'#39'ared functions do not support open strings');
    Add(03195,etWarning,'Calling convention directive ignored: "$1"');
    Add(03211,etWarning,'Implicit uses of Variants unit');
    Add(03218,etWarning,'Overridden methods must have a related return type. This code may crash, it depends on a Delphi parser bug ("$2" is overridden by "$1" which has another return type)');
    Add(03226,etWarning,'Don'#39't load LINEINFO unit manually, Use the -gl compiler switch instead');
    Add(03237,etWarning,'Register list is ignored for pure assembler routines');
  
    Add(04014,etWarning,'Automatic type conversion from floating type to COMP which is an integer type');
    Add(04015,etHint,'use DIV instead to get an integer result');
    Add(04022,etWarning,'lo/hi(dword/qword) returns the upper/lower word/dword');
    Add(04035,etWarning,'Mixing signed expressions and longwords gives a 64bit result');
    Add(04036,etWarning,'Mixing signed expressions and cardinals here may cause a range check error');
    Add(04040,etWarning,'Class types "$1" and "$2" are not related');
    Add(04043,etWarning,'String literal has more characters than short string length');
    Add(04044,etWarning,'Comparison is always false due to range of values');
    Add(04045,etWarning,'Comparison is always true due to range of values');
    Add(04046,etWarning,'Constructing a class "$1" with abstract method "$2"');
    Add(04047,etHint,'The left operand of the IN operator should be byte sized');
    Add(04048,etWarning,'Type size mismatch, possible loss of data / range check error');
    Add(04049,etHint,'Type size mismatch, possible loss of data / range check error');
    Add(04055,etHint,'Conversion between ordinals and pointers is not portable');
    Add(04056,etWarning,'Conversion between ordinals and pointers is not portable');
    Add(04059,etWarning,'Converting constant real value to double for C variable argument, add explicit typecast to prevent this.');
    Add(04066,etWarning,'Arithmetic "$1" on untyped pointer is unportable to {$T+}, suggest typecast');
    Add(04079,etHint,'Converting the operands to "$1" before doing the add could prevent overflow errors.');
    Add(04080,etHint,'Converting the operands to "$1" before doing the subtract could prevent overflow errors.');
    Add(04081,etHint,'Converting the operands to "$1" before doing the multiply could prevent overflow errors.');
    Add(04082,etWarning,'Converting pointers to signed integers may result in wrong comparison results and range errors, use an unsigned type instead.');
  
    Add(05003,etHint,'Identifier already defined in $1 at line $2');
    Add(05014,etWarning,'Label not defined "$1"');
    Add(05023,etHint,'Unit "$1" not used in $2');
    Add(05024,etHint,'Parameter "$1" not used');
    Add(05025,etNote,'Local variable "$1" not used');
    Add(05026,etHint,'Value parameter "$1" is assigned but never used');
    Add(05027,etNote,'Local variable "$1" is assigned but never used');
    Add(05028,etHint,'Local $1 "$2" is not used');
    Add(05029,etNote,'Private field "$1.$2" is never used');
    Add(05030,etNote,'Private field "$1.$2" is assigned but never used');
    Add(05031,etNote,'Private method "$1.$2" never used');
    Add(05033,etWarning,'Function result does not seem to be set');
    Add(05034,etWarning,'Type "$1" is not aligned correctly in current record for C');
    Add(05036,etWarning,'Local variable "$1" does not seem to be initialized');
    Add(05037,etWarning,'Variable "$1" does not seem to be initialized');
    Add(05039,etHint,'Found declaration: $1');
    Add(05043,etWarning,'Symbol "$1" is deprecated');
    Add(05044,etWarning,'Symbol "$1" is not portable');
    Add(05055,etWarning,'Symbol "$1" is not implemented');
    Add(05057,etHint,'Local variable "$1" does not seem to be initialized');
    Add(05058,etHint,'Variable "$1" does not seem to be initialized');
    Add(05059,etWarning,'Function result variable does not seem to initialized');
    Add(05060,etHint,'Function result variable does not seem to be initialized');
    Add(05061,etWarning,'Variable "$1" read but nowhere assigned');
    Add(05062,etHint,'Found abstract method: $1');
    Add(05063,etWarning,'Symbol "$1" is experimental');
    Add(05064,etWarning,'Forward declaration "$1" not resolved, assumed external');
  
    Add(06016,etWarning,'Possible illegal call of constructor or destructor');
    Add(06017,etNote,'Inefficient code');
    Add(06018,etWarning,'unreachable code');
    Add(06041,etWarning,'Parameters size exceeds limit for certain cpu'#39's');
    Add(06042,etWarning,'Local variable size exceed limit for certain cpu'#39's');
    Add(06048,etHint,'Inherited call to abstract method ignored');
  
    Add(07018,etWarning,'Possible error in object field handling');
    Add(07023,etWarning,'@CODE and @DATA not supported');
    Add(07029,etWarning,'Fwait can cause emulation problems with emu387');
    Add(07030,etWarning,'$1 without operand translated into $1P');
    Add(07031,etWarning,'ENTER instruction is not supported by Linux kernel');
    Add(07032,etWarning,'Calling an overload function in assembler');
    Add(07039,etHint,'$1 translated to $2');
    Add(07040,etWarning,'$1 is associated to an overloaded function');
    Add(07043,etWarning,'Procedures can'#39't return any value in asm code');
    Add(07046,etWarning,'Size suffix and destination or source size do not match');
    Add(07052,etWarning,'constant with symbol $1 for address which is not on a pointer');
    Add(07058,etWarning,'NEAR ignored');
    Add(07059,etWarning,'FAR ignored');
    Add(07066,etWarning,'Modulo not supported');
    Add(07072,etWarning,'Identifier $1 supposed external');
    Add(07079,etWarning,'32bit constant created for address');
    Add(07080,etNote,'.align is target specific, use .balign or .p2align');
    //Add(07086,etWarning,'"$1" without operand translated into "$1 %st,%st(1)"');
    //Add(07087,etWarning,'"$1 %st(n)" translated into "$1 %st,%st(n)"');
    //Add(07088,etWarning,'"$1 %st(n)" translated into "$1 %st(n),%st"');
    Add(07093,etWarning,'ALIGN not supported');
    Add(07098,etWarning,'No size specified and unable to determine the size of the operands, using DWORD as default');
    Add(07101,etWarning,'No size specified and unable to determine the size of the operands, using BYTE as default');
    Add(07102,etWarning,'Use of +offset(%ebp) for parameters invalid here');
    Add(07103,etWarning,'Use of +offset(%ebp) is not compatible with regcall convention');
    Add(07104,etWarning,'Use of -offset(%ebp) is not recommended for local variable access');
    Add(07105,etWarning,'Use of -offset(%esp), access may cause a crash or value may be lost');
  
    Add(09000,etWarning,'Source operating system redefined');
    Add(09011,etWarning,'Object $1 not found, Linking may fail !');
    Add(09012,etWarning,'Library $1 not found, Linking may fail !');
  finally
    EndUpdate; 
    if KeepIgnored then
      SetIgnoredArray(b);
  end; 
end;

function TCompilerMessagesList.GetParams(MsgIndex: Integer;
  var prms: array of string; out PrmCount: Integer): Integer;

  procedure SetParams(const Src: array of string);
  var
    i : integer;
  begin
    PrmCount := length(src);
    if PrmCount > length(prms) then Result := length(Prms)
    else Result := PrmCount;
    for i := 0 to PrmCount - 1 do
      Prms[i] := Src[i];
  end;

begin
  case MsgIndex of 
    3005: SetParams([symName]);
    3011: SetParams([symFile]);
    3023, 3057, 3094, 3100, 3123, 3175, 3177, 3189 : SetParams([symName]);
    3141, 3182: SetParams([symName, symName]); 
    3168: SetParams([symName, symName, symName]);
    3195: SetParams([symName]);
    3218: SetParams([symName, symName]);
    4040: SetParams([symClass, symClass]);
    4046: SetParams([symClass, symName]);
    4066, 4079,4080, 4081: SetParams([symName]);
    5003: SetParams([symName, symLineNo]);
    5014: SetParams([symName]);
    5023: SetParams([symName, symName]);
    5024,5025,5026,5027: SetParams([symName]);
    5028: SetParams([symItem, symName]);
    5029: SetParams([symClass, symName]);
    5030: SetParams([symClass, symName]);
    5031: SetParams([symClass, symName]);
    5034,5036,5037,5039,
    5043,5044,5055,5057,
    5058,5061,5062,5063,
    5064,7030: SetParams([symName]);
    7039: SetParams([symName, symName]);
    7040,9011: SetParams([symName]);
    9012: SetParams([symFile]);
  else
    PrmCount := 0;
    Result := 0;
  end;

end;

{ TCompilerMessageConfig }

constructor TCompilerMessageConfig.Create(AOwner: TCompilerMessagesList); 
begin
  fOwner:=AOwner; 
end;

function TCompilerMessageConfig.GetUserText(const ReplaceParams: array of string): string;
begin
  Result := ReplaceParamsArray(MsgText, ReplaceParams);
end;

function TCompilerMessageConfig.GetUserText: string; 
var
  prm : array of string; 
  cnt : Integer; 
begin
  if Assigned(fOwner) then begin
    SetLength(prm, MaxMsgParams);
    fOwner.GetParams(MsgIndex, prm, cnt);
    SetLength(prm, cnt);
    Result := GetUserText(prm); 
  end else
    Result := GetUserText([]); 
end;

initialization
  CompilerParseStamp:=1;
  CompilerParseStampIncreased:=nil;
  BuildMacroChangeStamp:=1;

end.

