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
  Controls, Laz_XMLCfg, ExprEval,
  // IDEIntf
  ProjectIntf, MacroIntf, IDEExternToolIntf, SrcEditorIntf,
  // IDE
  IDEProcs, LazConf, TransferMacros, CompOptsModes;

type

  { TIDEBuildMode }

  TIDEBuildMode = class(TLazBuildMode)
  protected
    procedure SetIdentifier(const AValue: string); override;
    procedure SetLocalizedName(const AValue: string); override;
    procedure SetLocalizedValues(const AValue: TStrings); override;
    procedure SetValues(const AValue: TStrings); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TLazBuildMode); override;
    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string; DoSwitchPathDelims: boolean);
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
  end;

  { TIDEBuildModes }

  TIDEBuildModes = class(TLazBuildModes)
  protected
    FItems: TFPList;// list of TIDEBuildMode
    function GetItems(Index: integer): TLazBuildMode; override;
  public
    function Add(Identifier: string): TLazBuildMode; override;
    procedure Clear; override;
    function Count: integer; override;
    constructor Create;
    procedure Delete(Index: integer); override;
    destructor Destroy; override;
    function IndexOfIdentifier(Identifier: string): integer; override;
    function ModeWithIdentifier(Identifier: string): TLazBuildMode; override;
    procedure Move(OldIndex, NewIndex: integer); override;
    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string; DoSwitchPathDelims: boolean);
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
  end;

  { TBuildModeSet }

  TBuildModeSet = class
  private
    FEvaluator: TExpressionEvaluator;
  public
    constructor Create;
    destructor Destroy; override;
    property Evaluator: TExpressionEvaluator read FEvaluator;
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
    pcosBaseDir,      // the base directory for the relative paths
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
    'pcosBaseDir',
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
  ParsedCompOptToConditional: array[TParsedCompilerOptString] of TCOCValueType = (
    cocvtNone,        // pcosNone
    cocvtNone,        // pcosBaseDir
    cocvtUnitPath,    // pcosUnitPath
    cocvtIncludePath, // pcosIncludePath
    cocvtObjectPath,  // pcosObjectPath
    cocvtLibraryPath, // pcosLibraryPath
    cocvtSrcPath,     // pcosSrcPath
    cocvtLinkerOptions, // pcosLinkerOptions
    cocvtCustomOptions, // pcosCustomOptions
    cocvtNone,        // pcosOutputDir
    cocvtNone,        // pcosCompilerPath
    cocvtDebugPath    // pcosDebugPath
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
    FConditionals: TCompOptConditionals;
    FGetWritableOutputDirectory: TGetWritableOutputDirectory;
    FInvalidateParseOnChange: boolean;
    FOnLocalSubstitute: TLocalSubstitutionEvent;
  public
    UnparsedValues: array[TParsedCompilerOptString] of string;
    // parsed
    ParsedValues: array[TParsedCompilerOptString] of string;
    ParsedStamp: array[TParsedCompilerOptString] of integer;
    Parsing: array[TParsedCompilerOptString] of boolean;
    // parsed except for platform macros
    ParsedPIValues: array[TParsedCompilerOptString] of string;
    ParsedPIStamp: array[TParsedCompilerOptString] of integer;
    ParsingPI: array[TParsedCompilerOptString] of boolean;
    constructor Create(TheConditionals: TCompOptConditionals);
    function GetParsedValue(Option: TParsedCompilerOptString): string;
    function GetParsedPIValue(Option: TParsedCompilerOptString): string;
    procedure SetUnparsedValue(Option: TParsedCompilerOptString;
                               const NewValue: string);
    function DoParseOption(const OptionText: string;
                           Option: TParsedCompilerOptString;
                           UseGetWritableOutputDirectory,
                           PlatformIndependent: boolean): string;
    procedure Clear;
    procedure InvalidateAll;
    procedure InvalidateFiles;
  public
    property OnLocalSubstitute: TLocalSubstitutionEvent read FOnLocalSubstitute
                                                       write FOnLocalSubstitute;
    property InvalidateParseOnChange: boolean read FInvalidateParseOnChange
                                              write FInvalidateParseOnChange;
    property GetWritableOutputDirectory: TGetWritableOutputDirectory
             read FGetWritableOutputDirectory write FGetWritableOutputDirectory;
    property Conditionals: TCompOptConditionals read FConditionals;
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
    cclAbsolutePaths
    );
  TCompilerCmdLineOptions = set of TCompilerCmdLineOption;
  
type

  { TCompilationToolOptions }

  TCompilationToolOptions = class
  public
    Command: string;
    ScanForFPCMessages: boolean;
    ScanForMakeMessages: boolean;
    ShowAllMessages: boolean;
    procedure Clear; virtual;
    function IsEqual(CompOpts: TCompilationToolOptions): boolean;
    procedure Assign(Src: TCompilationToolOptions); virtual;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string); virtual;
    procedure CreateDiff(CompOpts: TCompilationToolOptions;
                         Tool: TCompilerDiffTool); virtual;
    function Execute(const WorkingDir, ToolTitle: string): TModalResult;
  end;
  TCompilationToolClass = class of TCompilationToolOptions;

  TBaseCompilerOptionsClass = class of TBaseCompilerOptions;

  { TBaseCompilerOptions }

  TBaseCompilerOptions = class(TLazCompilerOptions)
  private
    FBaseDirectory: string;
    FDefaultMakeOptionsFlags: TCompilerCmdLineOptions;
    fInheritedOptions: TInheritedCompOptsParseTypesStrings;
    fInheritedOptParseStamps: integer;
    fLoaded: Boolean;
    fOptionsString: String;
    FParsedOpts: TParsedCompilerOptions;
    fTargetFilename: string;
    fXMLFile: String;
    FXMLConfig: TXMLConfig;

    // Compilation
    fCompilerPath: String;
    fExecuteBefore: TCompilationToolOptions;
    fExecuteAfter: TCompilationToolOptions;
    FCreateMakefileOnBuild: boolean;

    procedure SetTargetFilename(const AValue: String);
  protected
    procedure SetBaseDirectory(const AValue: string); override;
    procedure SetCompilerPath(const AValue: String); override;
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
    procedure SetModified(const AValue: boolean); override;
  protected
    procedure LoadTheCompilerOptions(const Path: string); virtual;
    procedure SaveTheCompilerOptions(const Path: string); virtual;
    procedure ClearInheritedOptions;
    procedure SetDefaultMakeOptionsFlags(const AValue: TCompilerCmdLineOptions);
  public
    constructor Create(const AOwner: TObject); override;
    constructor Create(const AOwner: TObject;
                  const AToolClass: TCompilationToolClass);
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
    
    procedure LoadCompilerOptions(UseExistingFile: Boolean);
    procedure SaveCompilerOptions(UseExistingFile: Boolean);
    procedure Assign(Source: TPersistent); override;
    function IsEqual(CompOpts: TBaseCompilerOptions): boolean;
    procedure CreateDiff(CompOpts: TBaseCompilerOptions; Diff: TStrings);
    procedure CreateDiff(CompOpts: TBaseCompilerOptions;
                         Tool: TCompilerDiffTool); virtual;

    function MakeOptionsString(Globals: TGlobalCompilerOptions;
                               Flags: TCompilerCmdLineOptions): String;
    function MakeOptionsString(const MainSourceFileName: string;
                               Globals: TGlobalCompilerOptions;
                               Flags: TCompilerCmdLineOptions): String; virtual;
    function GetSyntaxOptionsString: string; virtual;
    function GetXMLConfigPath: String; virtual;
    function CreateTargetFilename(const MainSourceFileName: string): string; virtual;
    function GetTargetFileExt: string; virtual;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList); virtual;
    function GetOwnerName: string; virtual;
    function GetInheritedOption(Option: TInheritedCompilerOption;
                                RelativeToBaseDir: boolean;
                                Parsed: TCompilerOptionsParseType = coptParsed
                                ): string; virtual;
    function GetDefaultMainSourceFileName: string; virtual;
    function NeedsLinkerOpts: boolean;
    function GetUnitPath(RelativeToBaseDir: boolean;
                         Parsed: TCompilerOptionsParseType = coptParsed;
                         WithProjDir: boolean = true): string;
    function GetIncludePath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed;
                            WithProjDir: boolean = true): string;
    function GetSrcPath(RelativeToBaseDir: boolean;
                        Parsed: TCompilerOptionsParseType = coptParsed;
                        WithProjDir: boolean = true): string;
    function GetDebugPath(RelativeToBaseDir: boolean;
                          Parsed: TCompilerOptionsParseType = coptParsed;
                          WithProjDir: boolean = true): string;
    function GetLibraryPath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed;
                            WithProjDir: boolean = true): string;
    function GetUnitOutPath(RelativeToBaseDir: boolean;
                            Parsed: TCompilerOptionsParseType = coptParsed): string;
    function GetObjectPath(RelativeToBaseDir: boolean;
                           Parsed: TCompilerOptionsParseType = coptParsed;
                           WithProjDir: boolean = true): string;
    function GetPath(Option: TParsedCompilerOptString;
                     InheritedOption: TInheritedCompilerOption;
                     RelativeToBaseDir: boolean;
                     Parsed: TCompilerOptionsParseType;
                     WithProjDir: boolean): string;
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
    function GetCustomOptions(Parsed: TCompilerOptionsParseType = coptParsed): string;
    function GetOptionsForCTDefines: string;
    function GetEffectiveLCLWidgetType: string;
  public
    // Properties
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property TargetFilename: String read fTargetFilename write SetTargetFilename;
    property DefaultMakeOptionsFlags: TCompilerCmdLineOptions
                 read FDefaultMakeOptionsFlags write SetDefaultMakeOptionsFlags;

    property XMLFile: String read fXMLFile write fXMLFile;
    property XMLConfigFile: TXMLConfig read FXMLConfig write FXMLConfig;
    property Loaded: Boolean read fLoaded write fLoaded;

    // compilation
    property CompilerPath: String read fCompilerPath write SetCompilerPath;
    property ExecuteBefore: TCompilationToolOptions read fExecuteBefore;
    property ExecuteAfter: TCompilationToolOptions read fExecuteAfter;
    property CreateMakefileOnBuild: boolean read FCreateMakefileOnBuild
                                            write FCreateMakefileOnBuild;
  end;
  
  
  { TAdditionalCompilerOptions
  
    Additional Compiler options are used by packages to define, what a project
    or a package or the IDE needs to use the package.
  }
  
  { TAdditionalCompilerOptions }

  TAdditionalCompilerOptions = class
  private
    FBaseDirectory: string;
    FConditionals: TCompOptConditionals;
    FCustomOptions: string;
    FIncludePath: string;
    FLibraryPath: string;
    FLinkerOptions: string;
    FObjectPath: string;
    fOwner: TObject;
    FParsedOpts: TParsedCompilerOptions;
    FSrcPath: string;
    FUnitPath: string;
  protected
    procedure SetBaseDirectory(const AValue: string); virtual;
    procedure SetCustomOptions(const AValue: string); virtual;
    procedure SetIncludePath(const AValue: string); virtual;
    procedure SetLibraryPath(const AValue: string); virtual;
    procedure SetLinkerOptions(const AValue: string); virtual;
    procedure SetObjectPath(const AValue: string); virtual;
    procedure SetUnitPath(const AValue: string); virtual;
    procedure SetSrcPath(const AValue: string); virtual;
  public
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                AdjustPathDelims: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function GetOwnerName: string; virtual;
    function GetOption(AnOption: TInheritedCompilerOption): string;
  public
    property Owner: TObject read fOwner;
    property UnitPath: string read FUnitPath write SetUnitPath;
    property IncludePath: string read FIncludePath write SetIncludePath;
    property SrcPath: string read FSrcPath write SetSrcPath;
    property ObjectPath: string read FObjectPath write SetObjectPath;
    property LibraryPath: string read FLibraryPath write SetLibraryPath;
    property LinkerOptions: string read FLinkerOptions write SetLinkerOptions;
    property CustomOptions: string read FCustomOptions write SetCustomOptions;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
    property Conditionals: TCompOptConditionals read FConditionals;
  end;


  { TCompilerOptions }

  TCompilerOptions = TBaseCompilerOptions;

var
  BuildModeSet: TBuildModeSet;

const
  CompileReasonNames: array[TCompileReason] of string = (
    'Compile',
    'Build',
    'Run'
    );
    
const LCLWidgetLinkerAddition: array[TLCLPlatform] of string = (
    '', // gtk
    '', // gtk2
    '', // win32
    '', // wince
{$IFDEF DARWIN}
    ' -k-framework -kCarbon -k-framework -kOpenGL -k''-dylib_file'' -k''/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib''', // carbon
{$ELSE}
    '', //carbon
{$ENDIF}
    '', // qt
    '', // fpGUI
    '',  // noGUI
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

function LoadXMLCompileReasons(const AConfig: TXMLConfig;
  const APath: String; const DefaultReasons: TCompileReasons): TCompileReasons;
procedure SaveXMLCompileReasons(const AConfig: TXMLConfig; const APath: String;
  const AFlags, DefaultFlags: TCompileReasons);

implementation

const
  CompilerOptionsVersion = 8;
  Config_Filename = 'compileroptions.xml';

function ParseString(Options: TParsedCompilerOptions;
  const UnparsedValue: string; PlatformIndependent: boolean): string;
begin
  Result:=OnParseString(Options,UnparsedValue,PlatformIndependent);
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
    Result := Result + ' ' +  SpecialCharsToSpaces(CurCustomOptions);
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
  FConditionals := TCompOptConditionals.Create(BuildModeSet.Evaluator);
  FParsedOpts := TParsedCompilerOptions.Create(TCompOptConditionals(FConditionals));
  FExecuteBefore := AToolClass.Create;
  FExecuteAfter := AToolClass.Create;
  fBuildModes := TIDEBuildModes.Create;

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
  FreeAndNil(fBuildModes);
  FreeThenNil(fExecuteBefore);
  FreeThenNil(fExecuteAfter);
  FreeThenNil(FParsedOpts);
  FreeThenNil(FConditionals); // free FConditionals before FParsedOpts
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  procedure TBaseCompilerOptions.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
    const Path: string);
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfigFile := AXMLConfig;
  LoadTheCompilerOptions(Path);
end;

{------------------------------------------------------------------------------
  procedure TBaseCompilerOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
    const Path: string);
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SaveToXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfigFile := AXMLConfig;
  SaveTheCompilerOptions(Path);
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions LoadCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadCompilerOptions(UseExistingFile: Boolean);
var
  confPath: String;
begin
  if (UseExistingFile and (XMLConfigFile <> nil)) then
  begin
    LoadTheCompilerOptions('CompilerOptions');
  end
  else
  begin
    confPath := GetXMLConfigPath;
    try
      XMLConfigFile := TXMLConfig.Create(SetDirSeparators(confPath));
      LoadTheCompilerOptions('CompilerOptions');
      XMLConfigFile.Free;
      XMLConfigFile := nil;
    except
      on E: Exception do begin
        DebugLn('TBaseCompilerOptions.LoadCompilerOptions '+Classname+' '+E.Message);
      end;
    end;
  end;
  fLoaded := true;
end;

{------------------------------------------------------------------------------
  procedure TBaseCompilerOptions.SetIncludePaths(const AValue: String);
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SetIncludePaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if NewValue<>AValue then
  if fIncludePaths=NewValue then exit;
  fIncludePaths:=NewValue;
  ParsedOpts.SetUnparsedValue(pcosIncludePath,fIncludePaths);
end;

procedure TBaseCompilerOptions.SetCompilerPath(const AValue: String);
begin
  if fCompilerPath=AValue then exit;
  fCompilerPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosCompilerPath,fCompilerPath);
end;

procedure TBaseCompilerOptions.SetDefaultMakeOptionsFlags(
  const AValue: TCompilerCmdLineOptions);
begin
  if FDefaultMakeOptionsFlags=AValue then exit;
  FDefaultMakeOptionsFlags:=AValue;
end;

procedure TBaseCompilerOptions.SetSrcPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if FSrcPath=NewValue then exit;
  FSrcPath:=NewValue;
  ParsedOpts.SetUnparsedValue(pcosSrcPath,FSrcPath);
end;

procedure TBaseCompilerOptions.SetDebugPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if fDebugPath=NewValue then exit;
  fDebugPath:=NewValue;
  ParsedOpts.SetUnparsedValue(pcosDebugPath,fDebugPath);
end;

procedure TBaseCompilerOptions.SetTargetCPU(const AValue: string);
begin
  if fTargetCPU=AValue then exit;
  fTargetCPU:=AValue;
  IncreaseCompilerParseStamp;
end;

procedure TBaseCompilerOptions.SetTargetProc(const AValue: string);
begin
  if fTargetProc=AValue then exit;
  fTargetProc:=AValue;
  IncreaseCompilerParseStamp;
end;

procedure TBaseCompilerOptions.SetTargetOS(const AValue: string);
begin
  if fTargetOS=AValue then exit;
  fTargetOS:=AValue;
  IncreaseCompilerParseStamp;
end;

procedure TBaseCompilerOptions.SetTargetFilename(const AValue: String);
begin
  if fTargetFilename=AValue then exit;
  fTargetFilename:=AValue;
  Modified:=true;
end;

procedure TBaseCompilerOptions.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  ParsedOpts.SetUnparsedValue(pcosBaseDir,FBaseDirectory);
end;

procedure TBaseCompilerOptions.SetCustomOptions(const AValue: string);
begin
  if fCustomOptions=AValue then exit;
  fCustomOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosCustomOptions,fCustomOptions);
end;

procedure TBaseCompilerOptions.SetLibraryPaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if fLibraryPaths=NewValue then exit;
  fLibraryPaths:=NewValue;
  ParsedOpts.SetUnparsedValue(pcosLibraryPath,fLibraryPaths);
end;

procedure TBaseCompilerOptions.SetLinkerOptions(const AValue: String);
begin
  if fLinkerOptions=AValue then exit;
  fLinkerOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLinkerOptions,fLinkerOptions);
end;

procedure TBaseCompilerOptions.SetUnitPaths(const AValue: String);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if fUnitPaths=NewValue then exit;
  fUnitPaths:=NewValue;
  ParsedOpts.SetUnparsedValue(pcosUnitPath,fUnitPaths);
end;

procedure TBaseCompilerOptions.SetUnitOutputDir(const AValue: string);
begin
  if fUnitOutputDir=AValue then exit;
  fUnitOutputDir:=AValue;
  ParsedOpts.SetUnparsedValue(pcosOutputDir,fUnitOutputDir);
end;

procedure TBaseCompilerOptions.SetObjectPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ShortenPath(AValue,false);
  if FObjectPath=NewValue then exit;
  FObjectPath:=NewValue;
  ParsedOpts.SetUnparsedValue(pcosObjectPath,FObjectPath);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions LoadTheCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadTheCompilerOptions(const Path: string);
var
  p: String;
  PathDelimChanged: boolean;
  FileVersion: Integer;
  i: LongInt;
  
  function f(const Filename: string): string;
  begin
    Result:=SwitchPathDelims(Filename,PathDelimChanged);
  end;
  
  function sp(const SearchPath: string): string;
  begin
    Result:=SwitchPathDelims(SearchPath,PathDelimChanged);
    Result:=MinimizeSearchPath(Result);
  end;

  procedure ReadSmaller;
  begin
    if FileVersion<2 then begin
      if XMLConfigFile.GetValue(p+'Generate/Value', 1)<>1 then
        SmallerCode:=true;
    end else if FileVersion<8 then begin
      if XMLConfigFile.GetValue(p+'Generate/Value','')='Smaller' then
        SmallerCode:=true;
    end else
      SmallerCode:=XMLConfigFile.GetValue(p+'SmallerCode/Value',false);
  end;
  
  procedure ReadSmartLinkUnit;
  begin
    if FileVersion<3 then
      SmartLinkUnit := XMLConfigFile.GetValue(p+'UnitStyle/Value', 1)=2
    else
      SmartLinkUnit := XMLConfigFile.GetValue(p+'SmartLinkUnit/Value', false);
  end;
  
  procedure ReadLinkSmart;
  begin
    if FileVersion<3 then
      LinkSmart := XMLConfigFile.GetValue(p+'LinkStyle/Value', 1)=3
    else
      LinkSmart := XMLConfigFile.GetValue(p+'LinkSmart/Value', false);
  end;

begin
  { Load the compiler options from the XML file }
  p:=Path;
  PathDelimChanged:=XMLConfigFile.GetValue(p+'PathDelim/Value', '/')<>PathDelim;
  FileVersion:=XMLConfigFile.GetValue(p+'Version/Value', 0);

  { Target }
  p:=Path+'Target/';
  TargetFilename := f(XMLConfigFile.GetValue(p+'Filename/Value', ''));

  { SearchPaths }
  p:=Path+'SearchPaths/';
  IncludePath := sp(XMLConfigFile.GetValue(p+'IncludeFiles/Value', ''));
  Libraries := sp(XMLConfigFile.GetValue(p+'Libraries/Value', ''));
  OtherUnitFiles := sp(XMLConfigFile.GetValue(p+'OtherUnitFiles/Value', ''));
  UnitOutputDirectory := sp(XMLConfigFile.GetValue(p+'UnitOutputDirectory/Value', ''));
  LCLWidgetType := XMLConfigFile.GetValue(p+'LCLWidgetType/Value', '');
  ObjectPath := sp(XMLConfigFile.GetValue(p+'ObjectPath/Value', ''));
  SrcPath := sp(XMLConfigFile.GetValue(p+'SrcPath/Value', ''));

  { Conditionals }
  TCompOptConditionals(FConditionals).LoadFromXMLConfig(XMLConfigFile,
                                         Path+'Conditionals/',PathDelimChanged);

  { Parsing }
  p:=Path+'Parsing/';
  AssemblerStyle := XMLConfigFile.GetValue(p+'Style/Value', 0);
  
  { Syntax Options }
  if FileVersion>=5 then
    p:=Path+'Parsing/SyntaxOptions/'
  else
    p:=Path+'SymantecChecking/';
  if FileVersion<6 then begin
    if XMLConfigFile.GetValue(p+'D2Extensions/Value', true) then
      FSyntaxMode:='ObjFPC';
    if XMLConfigFile.GetValue(p+'TPCompatible/Value', false) then
      FSyntaxMode:='TP';
    if XMLConfigFile.GetValue(p+'DelphiCompat/Value', false) then
      FSyntaxMode:='Delphi';
    if XMLConfigFile.GetValue(p+'GPCCompat/Value', false) then
      FSyntaxMode:='GPC';
  end else begin
    FSyntaxMode:=XMLConfigFile.GetValue(p+'SyntaxMode/Value', '');
    if FSyntaxMode='' then
      FSyntaxMode:='ObjFPC';
  end;
  CStyleOperators := XMLConfigFile.GetValue(p+'CStyleOperator/Value', true);
  IncludeAssertionCode := XMLConfigFile.GetValue(p+'IncludeAssertionCode/Value', false);
  AllowLabel := XMLConfigFile.GetValue(p+'AllowLabel/Value', true);
  CPPInline := XMLConfigFile.GetValue(p+'CPPInline/Value', true);
  CStyleMacros := XMLConfigFile.GetValue(p+'CStyleMacros/Value', false);
  InitConstructor := XMLConfigFile.GetValue(p+'InitConstructor/Value', false);
  StaticKeyword := XMLConfigFile.GetValue(p+'StaticKeyword/Value', false);
  UseAnsiStrings := XMLConfigFile.GetValue(p+'UseAnsiStrings/Value', false);

  { CodeGeneration }
  p:=Path+'CodeGeneration/';
  ReadSmartLinkUnit;
  IOChecks := XMLConfigFile.GetValue(p+'Checks/IOChecks/Value', false);
  RangeChecks := XMLConfigFile.GetValue(p+'Checks/RangeChecks/Value', false);
  OverflowChecks := XMLConfigFile.GetValue(p+'Checks/OverflowChecks/Value', false);
  StackChecks := XMLConfigFile.GetValue(p+'Checks/StackChecks/Value', false);
  EmulatedFloatOpcodes := XMLConfigFile.GetValue(p+'EmulateFloatingPointOpCodes/Value', false);
  HeapSize := XMLConfigFile.GetValue(p+'HeapSize/Value', 0);
  VerifyObjMethodCall := XMLConfigFile.GetValue(p+'VerifyObjMethodCallValidity/Value', false);
  ReadSmaller;
  if FileVersion<7 then begin
    i:=XMLConfigFile.GetValue(p+'TargetProcessor/Value', 0);
    case i of
    1: TargetProcessor:='PENTIUM';
    2: TargetProcessor:='PENTIUM2';
    3: TargetProcessor:='PENTIUM3';
    end;
  end else
    TargetProcessor := XMLConfigFile.GetValue(p+'TargetProcessor/Value', '');
  TargetCPU := XMLConfigFile.GetValue(p+'TargetCPU/Value', '');
  VariablesInRegisters := XMLConfigFile.GetValue(p+'Optimizations/VariablesInRegisters/Value', false);
  UncertainOptimizations := XMLConfigFile.GetValue(p+'Optimizations/UncertainOptimizations/Value', false);
  OptimizationLevel := XMLConfigFile.GetValue(p+'Optimizations/OptimizationLevel/Value', 1);
  TargetOS := XMLConfigFile.GetValue(p+'TargetOS/Value', '');

  { Linking }
  p:=Path+'Linking/';
  GenerateDebugInfo := XMLConfigFile.GetValue(p+'Debugging/GenerateDebugInfo/Value', false);
  UseLineInfoUnit := XMLConfigFile.GetValue(p+'Debugging/UseLineInfoUnit/Value', true);
  UseHeaptrc := XMLConfigFile.GetValue(p+'Debugging/UseHeaptrc/Value', false);
  UseValgrind := XMLConfigFile.GetValue(p+'Debugging/UseValgrind/Value', false);
  GenGProfCode := XMLConfigFile.GetValue(p+'Debugging/GenGProfCode/Value', false);
  StripSymbols := XMLConfigFile.GetValue(p+'Debugging/StripSymbols/Value', false);
  UseExternalDbgSyms := XMLConfigFile.GetValue(p+'Debugging/UseExternalDbgSyms/Value', false);
  ReadLinkSmart;
  PassLinkerOptions := XMLConfigFile.GetValue(p+'Options/PassLinkerOptions/Value', false);
  LinkerOptions := LineBreaksToSystemLineBreaks(
                f(XMLConfigFile.GetValue(p+'Options/LinkerOptions/Value', '')));
  Win32GraphicApp := XMLConfigFile.GetValue(p+'Options/Win32/GraphicApplication/Value', false);
  ExecutableType := CompilationExecutableTypeNameToType(
                    XMLConfigFile.GetValue(p+'Options/ExecutableType/Value',''));
  //DebugLn('TBaseCompilerOptions.LoadTheCompilerOptions ',CompilationExecutableTypeNames[ExecutableType]);

  { Messages }
  p:=Path+'Other/';
  ShowErrors := XMLConfigFile.GetValue(p+'Verbosity/ShowErrors/Value', true);
  ShowWarn := XMLConfigFile.GetValue(p+'Verbosity/ShowWarn/Value', true);
  ShowNotes := XMLConfigFile.GetValue(p+'Verbosity/ShowNotes/Value', true);
  ShowHints := XMLConfigFile.GetValue(p+'Verbosity/ShowHints/Value', true);
  ShowGenInfo := XMLConfigFile.GetValue(p+'Verbosity/ShowGenInfo/Value', true);
  ShowLineNum := XMLConfigFile.GetValue(p+'Verbosity/ShoLineNum/Value', false);
  ShowAll := XMLConfigFile.GetValue(p+'Verbosity/ShowAll/Value', false);
  ShowAllProcsOnError := XMLConfigFile.GetValue(p+'Verbosity/ShowAllProcsOnError/Value', false);
  ShowDebugInfo := XMLConfigFile.GetValue(p+'Verbosity/ShowDebugInfo/Value', false);
  ShowUsedFiles := XMLConfigFile.GetValue(p+'Verbosity/ShowUsedFiles/Value', false);
  ShowTriedFiles := XMLConfigFile.GetValue(p+'Verbosity/ShowTriedFiles/Value', false);
  ShowDefMacros := XMLConfigFile.GetValue(p+'Verbosity/ShowDefMacros/Value', false);
  ShowCompProc := XMLConfigFile.GetValue(p+'Verbosity/ShowCompProc/Value', false);
  ShowCond := XMLConfigFile.GetValue(p+'Verbosity/ShowCond/Value', false);
  ShowExecInfo := XMLConfigFile.GetValue(p+'Verbosity/ShowExecInfo/Value', false);
  ShowNothing := XMLConfigFile.GetValue(p+'Verbosity/ShowNothing/Value', false);
  ShowSummary := XMLConfigFile.GetValue(p+'Verbosity/ShowSummary/Value', false);
  ShowHintsForUnusedUnitsInMainSrc := XMLConfigFile.GetValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', false);
  ShowHintsForSenderNotUsed := XMLConfigFile.GetValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', false);
  WriteFPCLogo := XMLConfigFile.GetValue(p+'WriteFPCLogo/Value', true);
  StopAfterErrCount := XMLConfigFile.GetValue(p+'ConfigFile/StopAfterErrCount/Value', 1);

  { Other }
  p:=Path+'Other/';
  DontUseConfigFile := XMLConfigFile.GetValue(p+'ConfigFile/DontUseConfigFile/Value', false);
  if FileVersion<=3 then
    CustomConfigFile := XMLConfigFile.GetValue(p+'ConfigFile/AdditionalConfigFile/Value', false)
  else
    CustomConfigFile := XMLConfigFile.GetValue(p+'ConfigFile/CustomConfigFile/Value', false);
  ConfigFilePath := f(XMLConfigFile.GetValue(p+'ConfigFile/ConfigFilePath/Value', 'extrafpc.cfg'));
  CustomOptions := LineBreaksToSystemLineBreaks(XMLConfigFile.GetValue(p+'CustomOptions/Value', ''));

  { Compilation }
  CompilerPath := f(XMLConfigFile.GetValue(p+'CompilerPath/Value','$(CompPath)'));

  ExecuteBefore.LoadFromXMLConfig(XMLConfigFile,p+'ExecuteBefore/',PathDelimChanged);
  ExecuteAfter.LoadFromXMLConfig(XMLConfigFile,p+'ExecuteAfter/',PathDelimChanged);
  CreateMakefileOnBuild:=XMLConfigFile.GetValue(p+'CreateMakefileOnBuild/Value',false);
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SaveCompilerOptions                                     }
{------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SaveCompilerOptions(UseExistingFile: Boolean);
var
  confPath: String;
begin
  if ((UseExistingFile) and (XMLConfigFile <> nil)) then
  begin
    SaveTheCompilerOptions('CompilerOptions');
  end
  else
  begin
    confPath := GetXMLConfigPath;
    try
      XMLConfigFile := TXMLConfig.Create(SetDirSeparators(confPath));
      SaveTheCompilerOptions('CompilerOptions');
      XMLConfigFile.Free;
      XMLConfigFile := nil;
    except
      on E: Exception do begin
        DebugLn('TBaseCompilerOptions.LoadCompilerOptions '+Classname+' '+E.Message);
      end;
    end;
  end;
  fModified:=false;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SaveTheCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SaveTheCompilerOptions(const Path: string);
var
  P: string;
begin
  { Save the compiler options to the XML file }
  p:=Path;
  XMLConfigFile.SetValue(p+'Version/Value', CompilerOptionsVersion);
  XMLConfigFile.SetDeleteValue(p+'PathDelim/Value', PathDelim, '/');

  { Target }
  p:=Path+'Target/';
  XMLConfigFile.SetDeleteValue(p+'Filename/Value', TargetFilename,'');

  { SearchPaths }
  p:=Path+'SearchPaths/';
  XMLConfigFile.SetDeleteValue(p+'IncludeFiles/Value', IncludePath,'');
  XMLConfigFile.SetDeleteValue(p+'Libraries/Value', Libraries,'');
  XMLConfigFile.SetDeleteValue(p+'OtherUnitFiles/Value', OtherUnitFiles,'');
  XMLConfigFile.SetDeleteValue(p+'UnitOutputDirectory/Value', UnitOutputDirectory,'');
  XMLConfigFile.SetDeleteValue(p+'LCLWidgetType/Value', LCLWidgetType,'');
  XMLConfigFile.SetDeleteValue(p+'ObjectPath/Value', ObjectPath,'');
  XMLConfigFile.SetDeleteValue(p+'SrcPath/Value', SrcPath,'');

  { Conditionals }
  TCompOptConditionals(FConditionals).SaveToXMLConfig(XMLConfigFile,
                                                      Path+'Conditionals/');

  { Parsing }
  p:=Path+'Parsing/';
  XMLConfigFile.SetDeleteValue(p+'Style/Value', AssemblerStyle,0);
  
  { Syntax Options }
  p:=Path+'Parsing/SyntaxOptions/';
  XMLConfigFile.SetDeleteValue(p+'SyntaxMode/Value', SyntaxMode,'ObjFPC');
  XMLConfigFile.SetDeleteValue(p+'CStyleOperator/Value', CStyleOperators,true);
  XMLConfigFile.SetDeleteValue(p+'IncludeAssertionCode/Value', IncludeAssertionCode,false);
  XMLConfigFile.SetDeleteValue(p+'AllowLabel/Value', AllowLabel,true);
  XMLConfigFile.SetDeleteValue(p+'CPPInline/Value', CPPInline,true);
  XMLConfigFile.SetDeleteValue(p+'CStyleMacros/Value', CStyleMacros,false);
  XMLConfigFile.SetDeleteValue(p+'InitConstructor/Value', InitConstructor,false);
  XMLConfigFile.SetDeleteValue(p+'StaticKeyword/Value', StaticKeyword,false);
  XMLConfigFile.SetDeleteValue(p+'UseAnsiStrings/Value', UseAnsiStrings,false);

  { CodeGeneration }
  p:=Path+'CodeGeneration/';
  XMLConfigFile.SetDeleteValue(p+'SmartLinkUnit/Value', SmartLinkUnit,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/IOChecks/Value', IOChecks,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/RangeChecks/Value', RangeChecks,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/OverflowChecks/Value', OverflowChecks,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/StackChecks/Value', StackChecks,false);
  XMLConfigFile.SetDeleteValue(p+'EmulateFloatingPointOpCodes/Value', EmulatedFloatOpcodes,false);
  XMLConfigFile.SetDeleteValue(p+'HeapSize/Value', HeapSize,0);
  XMLConfigFile.SetDeleteValue(p+'VerifyObjMethodCallValidity/Value', VerifyObjMethodCall,false);
  XMLConfigFile.SetDeleteValue(p+'SmallerCode/Value', SmallerCode, false);
  XMLConfigFile.SetDeleteValue(p+'TargetProcessor/Value', TargetProcessor,'');
  XMLConfigFile.SetDeleteValue(p+'TargetCPU/Value', TargetCPU,'');
  XMLConfigFile.SetDeleteValue(p+'Optimizations/VariablesInRegisters/Value', VariablesInRegisters,false);
  XMLConfigFile.SetDeleteValue(p+'Optimizations/UncertainOptimizations/Value', UncertainOptimizations,false);
  XMLConfigFile.SetDeleteValue(p+'Optimizations/OptimizationLevel/Value', OptimizationLevel,1);
  XMLConfigFile.SetDeleteValue(p+'TargetOS/Value', TargetOS,'');

  { Linking }
  p:=Path+'Linking/';
  XMLConfigFile.SetDeleteValue(p+'Debugging/GenerateDebugInfo/Value', GenerateDebugInfo,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/UseLineInfoUnit/Value', UseLineInfoUnit,true);
  XMLConfigFile.SetDeleteValue(p+'Debugging/UseHeaptrc/Value', UseHeaptrc,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/UseValgrind/Value', UseValgrind,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/GenGProfCode/Value', GenGProfCode,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/StripSymbols/Value', StripSymbols,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/UseExternalDbgSyms/Value', UseExternalDbgSyms,false);
  XMLConfigFile.SetDeleteValue(p+'LinkSmart/Value', LinkSmart,false);
  XMLConfigFile.SetDeleteValue(p+'Options/PassLinkerOptions/Value', PassLinkerOptions,false);
  XMLConfigFile.SetDeleteValue(p+'Options/LinkerOptions/Value',
                               LineBreaksToSystemLineBreaks(LinkerOptions),'');
  XMLConfigFile.SetDeleteValue(p+'Options/Win32/GraphicApplication/Value', Win32GraphicApp,false);
  XMLConfigFile.SetDeleteValue(p+'Options/ExecutableType/Value',
                                 CompilationExecutableTypeNames[ExecutableType],
                                 CompilationExecutableTypeNames[cetProgram]);
  //DebugLn('TBaseCompilerOptions.SaveCompilerOptions ',CompilationExecutableTypeNames[ExecutableType]);

  { Messages }
  p:=Path+'Other/';
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowErrors/Value', ShowErrors,true);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowWarn/Value', ShowWarn,true);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowNotes/Value', ShowNotes,true);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowHints/Value', ShowHints,true);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowGenInfo/Value', ShowGenInfo,true);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShoLineNum/Value', ShowLineNum,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowAll/Value', ShowAll,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowAllProcsOnError/Value', ShowAllProcsOnError,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowDebugInfo/Value', ShowDebugInfo,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowUsedFiles/Value', ShowUsedFiles,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowTriedFiles/Value', ShowTriedFiles,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowDefMacros/Value', ShowDefMacros,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowCompProc/Value', ShowCompProc,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowCond/Value', ShowCond,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowExecInfo/Value', ShowExecInfo,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowNothing/Value', ShowNothing,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowSummary/Value', ShowSummary,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', ShowHintsForUnusedUnitsInMainSrc,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', ShowHintsForSenderNotUsed,false);
  XMLConfigFile.SetDeleteValue(p+'WriteFPCLogo/Value', WriteFPCLogo,true);
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/StopAfterErrCount/Value', StopAfterErrCount,1);

  { Other }
  p:=Path+'Other/';
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/DontUseConfigFile/Value', DontUseConfigFile,false);
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/CustomConfigFile/Value', CustomConfigFile,false);
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/ConfigFilePath/Value', ConfigFilePath,'extrafpc.cfg');
  XMLConfigFile.SetDeleteValue(p+'CustomOptions/Value',
                               LineBreaksToSystemLineBreaks(CustomOptions),'');

  { Compilation }
  XMLConfigFile.SetDeleteValue(p+'CompilerPath/Value', CompilerPath,'');
  ExecuteBefore.SaveToXMLConfig(XMLConfigFile,p+'ExecuteBefore/');
  ExecuteAfter.SaveToXMLConfig(XMLConfigFile,p+'ExecuteAfter/');
  XMLConfigFile.SetDeleteValue(p+'CreateMakefileOnBuild/Value',
                               CreateMakefileOnBuild,false);

  // write
  FModified := False;
end;

procedure TBaseCompilerOptions.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if Assigned(OnModified) then
    OnModified(Self);
end;

procedure TBaseCompilerOptions.ClearInheritedOptions;
var
  i: TInheritedCompilerOption;
  p: TCompilerOptionsParseType;
begin
  fInheritedOptParseStamps:=InvalidParseStamp;
  //QWE fInheritedOptGraphStamps:=InvalidParseStamp;
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
    // TargetFilename is relative to main source file
    Result:=AppendPathDelim(ExtractFilePath(MainSourceFileName))+Result;
  end else begin
    // calculate output directory
    UnitOutDir:=GetUnitOutPath(false);
    if UnitOutDir='' then
      UnitOutDir:=ExtractFilePath(MainSourceFileName);
    if Result<>'' then
      OutFilename:=Result
    else
      OutFilename:=ExtractFileNameOnly(MainSourceFileName);
    //debugln('TBaseCompilerOptions.CreateTargetFilename MainSourceFileName=',MainSourceFileName,' OutFilename=',OutFilename,' TargetFilename=',TargetFilename);

    Result:=AppendPathDelim(UnitOutDir)+OutFilename;
  end;
  Result:=TrimFilename(Result);
  AppendDefaultExt;
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
  //QWE or (fInheritedOptGraphStamps<>CompilerGraphStamp)
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
    //QWE fInheritedOptGraphStamps:=CompilerGraphStamp;
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

function TBaseCompilerOptions.NeedsLinkerOpts: boolean;
begin
  Result:=not (ccloNoLinkerOpts in fDefaultMakeOptionsFlags);
end;

function TBaseCompilerOptions.GetUnitPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
begin
  Result:=GetPath(pcosUnitPath,icoUnitPath,RelativeToBaseDir,Parsed,WithProjDir);
end;

function TBaseCompilerOptions.GetIncludePath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
begin
  Result:=GetPath(pcosIncludePath,icoIncludePath,RelativeToBaseDir,Parsed,
                  WithProjDir);
end;

function TBaseCompilerOptions.GetSrcPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
begin
  Result:=GetPath(pcosSrcPath,icoSrcPath,RelativeToBaseDir,Parsed,WithProjDir);
end;

function TBaseCompilerOptions.GetDebugPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
begin
  Result:=GetPath(pcosDebugPath,icoNone,RelativeToBaseDir,Parsed,WithProjDir);
end;

function TBaseCompilerOptions.GetLibraryPath(RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
begin
  Result:=GetPath(pcosLibraryPath,icoLibraryPath,RelativeToBaseDir,Parsed,
                  WithProjDir);
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
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
begin
  Result:=GetPath(pcosObjectPath,icoObjectPath,RelativeToBaseDir,Parsed,
                  WithProjDir);
end;

function TBaseCompilerOptions.GetPath(Option: TParsedCompilerOptString;
  InheritedOption: TInheritedCompilerOption; RelativeToBaseDir: boolean;
  Parsed: TCompilerOptionsParseType; WithProjDir: boolean): string;
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
  if WithProjDir then begin
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

function TBaseCompilerOptions.GetCustomOptions(Parsed: TCompilerOptionsParseType
  ): string;
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
  Result:=SpecialCharsToSpaces(Result);
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

function CompilerSupportMessages(CompilerVersion, CompilerRelease, CompilerPatch: Integer): Boolean;
begin
  Result := (CompilerVersion >= 2) and (CompilerRelease >= 3) and (CompilerPatch >=1);
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
  switches, tempsw: String;
  InhLinkerOpts: String;
  NewTargetFilename: String;
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


  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
  n = Omit linking stage
  sxxx = Set stack size to xxx
}

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
  { --------------- Linking Tab ------------------- }
  
  { Debugging }
  { Debug Info for GDB }
  if (GenerateDebugInfo) then
    switches := switches + ' -g';

  { Line Numbers in Run-time Error Backtraces - Use LineInfo Unit }
  if (UseLineInfoUnit) then
    switches := switches + ' -gl';

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
     -XD = Link with dynamic libraries, not implemented by FPC
     -XS = Link with static libraries, default on non-win32 platforms
     -XX = Link smart
  }

  if (not (ccloNoLinkerOpts in Flags)) and LinkSmart then
    switches := switches + ' -XX';

  // additional Linker options
  if PassLinkerOptions and (not (ccloNoLinkerOpts in Flags)) then begin
    CurLinkerOptions:=ParsedOpts.GetParsedValue(pcosLinkerOptions);
    if (CurLinkerOptions<>'') then
      switches := switches + ' ' + ConvertOptionsToCmdLine(' ','-k', CurLinkerOptions);
  end;

  // inherited Linker options
  if (not (ccloNoLinkerOpts in Flags)) then begin
    InhLinkerOpts:=GetInheritedOption(icoLinkerOptions,
      not (cclAbsolutePaths in Flags),coptParsed);
    if InhLinkerOpts<>'' then
      switches := switches + ' ' + ConvertOptionsToCmdLine(' ','-k', InhLinkerOpts);
  end;
  
  // add Linker options for widgetset
  if not (ccloNoLinkerOpts in Flags) then
  begin
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
    tempsw := tempsw + ' -Se'+IntToStr(StopAfterErrCount);


  { Write an FPC logo }
  if (WriteFPCLogo) then
    switches := switches + ' -l';

  { Ignore Config File }
  if DontUseConfigFile then
    switches := switches + ' -n';

  { Use Custom Config File     @ = yes and path }
  if (CustomConfigFile) and (ConfigFilePath<>'') then
    switches := switches + ' ' + PrepareCmdLineOption('@' + ConfigFilePath);


  { ------------- Search Paths ---------------- }
  
  // include path
  CurIncludePath:=GetIncludePath(not (cclAbsolutePaths in Flags),
                                 coptParsed,false);
  if (CurIncludePath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fi', CurIncludePath);

  // library path
  if (not (ccloNoLinkerOpts in Flags)) then begin
    CurLibraryPath:=GetLibraryPath(not (cclAbsolutePaths in Flags),
                                   coptParsed,false);
    if (CurLibraryPath <> '') then
      switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fl', CurLibraryPath);
  end;

  // object path
  CurObjectPath:=GetObjectPath(not (cclAbsolutePaths in Flags),
                               coptParsed,false);
  if (CurObjectPath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fo', CurObjectPath);

  // unit path
  CurUnitPath:=GetUnitPath(not (cclAbsolutePaths in Flags));
  //debugln('TBaseCompilerOptions.MakeOptionsString A ',dbgsName(Self),' CurUnitPath="',CurUnitPath,'"');
  // always add the current directory to the unit path, so that the compiler
  // checks for changed files in the directory
  CurUnitPath:=MergeSearchPaths(CurUnitPath,'.');
  switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fu', CurUnitPath);

  { CompilerPath - Nothing needs to be done with this one }
  
  { Unit output directory }
  if UnitOutputDirectory<>'' then begin
    CurOutputDir:=ParsedOpts.GetParsedValue(pcosOutputDir);
    if not (cclAbsolutePaths in Flags) then
      CurOutputDir:=CreateRelativePath(CurOutputDir,BaseDirectory,true);
  end else
    CurOutputDir:='';
  if CurOutputDir<>'' then
    switches := switches + ' '+PrepareCmdLineOption('-FU'+CurOutputDir);

  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
     exxx = Errors file
     Lxxx = Use xxx as dynamic linker (LINUX only)
     oxxx = Object files
     rxxx = Compiler messages file
}
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
  if not (ccloDoNotAppendOutFileOption in Flags)
  and ((TargetFilename<>'') or (CurMainSrcFile<>'') or (CurOutputDir<>'')) then
  begin
    NewTargetFilename:=CreateTargetFilename(CurMainSrcFile);
    if (NewTargetFilename<>'')
    and ((CompareFileNames(NewTargetFilename,ChangeFileExt(CurMainSrcFile,''))<>0)
     or (CurOutputDir<>'')) then
    begin
      if not (cclAbsolutePaths in Flags) then
        NewTargetFilename:=CreateRelativePath(NewTargetFilename,BaseDirectory);
      switches := switches + ' '+PrepareCmdLineOption('-o' + NewTargetFilename);
    end;
  end;

  // custom options
  CurCustomOptions:=GetCustomOptions;
  if CurCustomOptions<>'' then
    switches := switches+' '+CurCustomOptions;


  fOptionsString := switches;
  Result := fOptionsString;
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
  TBaseCompilerOptions GetXMLConfigPath
 ------------------------------------------------------------------------------}
function TBaseCompilerOptions.GetXMLConfigPath: String;
var
  fn: String;
begin
  // Setup the filename to write to
  fn := XMLFile;
  if (fn = '') then
    fn := Config_Filename;
  Result := GetPrimaryConfigPath + '/' + fn;
  CopySecondaryConfigFile(fn);
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions Clear
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.Clear;
begin
  fOptionsString := '';
  fLoaded := false;
  FModified := false;

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
  fUseAnsiStr := false;

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
  fOptionsString := CompOpts.fOptionsString;
  fLoaded := CompOpts.fLoaded;

  // Search Paths
  IncludePath := CompOpts.fIncludePaths;
  Libraries := CompOpts.fLibraryPaths;
  OtherUnitFiles := CompOpts.fUnitPaths;
  UnitOutputDirectory := CompOpts.fUnitOutputDir;
  ObjectPath := CompOpts.FObjectPath;
  SrcPath := CompOpts.SrcPath;
  DebugPath := CompOpts.DebugPath;

  // conditionals
  fLCLWidgetType := CompOpts.fLCLWidgetType;
  Conditionals.Assign(CompOpts.Conditionals);

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
  fUseHeaptrc := CompOpts.fUseHeaptrc;
  fUseValgrind := CompOpts.fUseValgrind;
  fGenGProfCode := CompOpts.fGenGProfCode;
  fStripSymbols := CompOpts.fStripSymbols;
  fLinkSmart := CompOpts.fLinkSmart;
  fPassLinkerOpt := CompOpts.fPassLinkerOpt;
  LinkerOptions := CompOpts.fLinkerOptions;
  Win32GraphicApp := CompOpts.Win32GraphicApp;
  ExecutableType := CompOpts.ExecutableType;

  // Messages
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
  fStopAfterErrCount := CompOpts.fStopAfterErrCount;

  // Other
  fDontUseConfigFile := CompOpts.fDontUseConfigFile;
  fCustomConfigFile := CompOpts.fCustomConfigFile;
  fConfigFilePath := CompOpts.fConfigFilePath;
  CustomOptions := CompOpts.fCustomOptions;

  // compilation
  CompilerPath := CompOpts.fCompilerPath;
  ExecuteBefore.Assign(CompOpts.ExecuteBefore);
  ExecuteAfter.Assign(CompOpts.ExecuteAfter);
  CreateMakefileOnBuild:=CompOpts.CreateMakefileOnBuild;
end;

function TBaseCompilerOptions.IsEqual(CompOpts: TBaseCompilerOptions): boolean;
var
  Tool: TCompilerDiffTool;
begin
  Tool:=TCompilerDiffTool.Create(nil);
  CreateDiff(CompOpts,Tool);
  Result:=not Tool.Differ;
  Tool.Free;
end;

procedure TBaseCompilerOptions.CreateDiff(CompOpts: TBaseCompilerOptions;
  Diff: TStrings);
var
  Tool: TCompilerDiffTool;
begin
  Tool:=TCompilerDiffTool.Create(Diff);
  CreateDiff(CompOpts,Tool);
  Tool.Free;
end;

procedure TBaseCompilerOptions.CreateDiff(CompOpts: TBaseCompilerOptions;
  Tool: TCompilerDiffTool);

  procedure AddDiff(const PropertyName: string;
    const Old, New: TCompilationExecutableType);
  begin
    if Old=New then exit;
    Tool.AddDiffItem(PropertyName,CompilationExecutableTypeNames[New]);
  end;

begin
  // search paths
  Tool.Path:='Paths';
  Tool.AddPathsDiff('IncludePaths',fIncludePaths,CompOpts.fIncludePaths);
  Tool.AddPathsDiff('LibraryPaths',fLibraryPaths,CompOpts.fLibraryPaths);
  Tool.AddPathsDiff('UnitPaths',fUnitPaths,CompOpts.fUnitPaths);
  Tool.AddPathsDiff('UnitOutputDir',fUnitOutputDir,CompOpts.fUnitOutputDir);
  Tool.AddPathsDiff('ObjectPath',FObjectPath,CompOpts.FObjectPath);
  Tool.AddPathsDiff('SrcPath',FSrcPath,CompOpts.FSrcPath);
  Tool.AddPathsDiff('DebugPath',fDebugPath,CompOpts.fDebugPath);

  // conditionals
  TCompOptConditionals(Conditionals).CreateDiff(CompOpts.Conditionals,Tool);
  Tool.AddDiff('LCLWidgetType',fLCLWidgetType,CompOpts.fLCLWidgetType);

  // parsing
  Tool.Path:='Parsing';
  Tool.AddDiff('SyntaxMode',FSyntaxMode,CompOpts.FSyntaxMode);
  Tool.AddDiff('AssemblerStyle',fAssemblerStyle,CompOpts.fAssemblerStyle);
  Tool.AddDiff('CStyleOp',fCStyleOp,CompOpts.fCStyleOp);
  Tool.AddDiff('IncludeAssertionCode',fIncludeAssertionCode,CompOpts.fIncludeAssertionCode);
  Tool.AddDiff('AllowLabel',fAllowLabel,CompOpts.fAllowLabel);
  Tool.AddDiff('CPPInline',fCPPInline,CompOpts.fCPPInline);
  Tool.AddDiff('CMacros',fCMacros,CompOpts.fCMacros);
  Tool.AddDiff('InitConst',fInitConst,CompOpts.fInitConst);
  Tool.AddDiff('StaticKeyword',fStaticKeyword,CompOpts.fStaticKeyword);
  Tool.AddDiff('UseAnsiStr',fUseAnsiStr,CompOpts.fUseAnsiStr);

  // code generation
  Tool.Path:='Code';
  Tool.AddDiff('SmartLinkUnit',fSmartLinkUnit,CompOpts.SmartLinkUnit);
  Tool.AddDiff('IOChecks',fIOChecks,CompOpts.fIOChecks);
  Tool.AddDiff('RangeChecks',fRangeChecks,CompOpts.fRangeChecks);
  Tool.AddDiff('OverflowChecks',fOverflowChecks,CompOpts.fOverflowChecks);
  Tool.AddDiff('StackChecks',fStackChecks,CompOpts.fStackChecks);
  Tool.AddDiff('EmulatedFloatOpcodes',FEmulatedFloatOpcodes,CompOpts.FEmulatedFloatOpcodes);
  Tool.AddDiff('HeapSize',fHeapSize,CompOpts.fHeapSize);
  Tool.AddDiff('EmulatedFloatOpcodes',fEmulatedFloatOpcodes,CompOpts.fEmulatedFloatOpcodes);
  Tool.AddDiff('SmallerCode',FSmallerCode,CompOpts.FSmallerCode);
  Tool.AddDiff('TargetProc',fTargetProc,CompOpts.fTargetProc);
  Tool.AddDiff('TargetCPU',fTargetCPU,CompOpts.fTargetCPU);
  Tool.AddDiff('VarsInReg',fVarsInReg,CompOpts.fVarsInReg);
  Tool.AddDiff('UncertainOpt',fUncertainOpt,CompOpts.fUncertainOpt);
  Tool.AddDiff('OptLevel',fOptLevel,CompOpts.fOptLevel);
  Tool.AddDiff('TargetOS',fTargetOS,CompOpts.fTargetOS);

  // linking
  Tool.Path:='Linking';
  Tool.AddDiff('GenDebugInfo',fGenDebugInfo,CompOpts.fGenDebugInfo);
  Tool.AddDiff('UseLineInfoUnit',fUseLineInfoUnit,CompOpts.fUseLineInfoUnit);
  Tool.AddDiff('UseHeaptrc',fUseHeaptrc,CompOpts.fUseHeaptrc);
  Tool.AddDiff('UseValgrind',fUseValgrind,CompOpts.fUseValgrind);
  Tool.AddDiff('GenGProfCode',fGenGProfCode,CompOpts.fGenGProfCode);
  Tool.AddDiff('StripSymbols',fStripSymbols,CompOpts.fStripSymbols);
  Tool.AddDiff('LinkSmart',fLinkSmart,CompOpts.fLinkSmart);
  Tool.AddDiff('PassLinkerOpt',fPassLinkerOpt,CompOpts.fPassLinkerOpt);
  Tool.AddDiff('LinkerOptions',fLinkerOptions,CompOpts.fLinkerOptions);
  Tool.AddDiff('Win32GraphicApp',FWin32GraphicApp,CompOpts.FWin32GraphicApp);
       AddDiff('ExecutableType',FExecutableType,CompOpts.FExecutableType);

  // messages
  Tool.Path:='Messages';
  Tool.AddDiff('ShowErrors',fShowErrors,CompOpts.fShowErrors);
  Tool.AddDiff('ShowWarn',fShowWarn,CompOpts.fShowWarn);
  Tool.AddDiff('ShowNotes',fShowNotes,CompOpts.fShowNotes);
  Tool.AddDiff('ShowHints',fShowHints,CompOpts.fShowHints);
  Tool.AddDiff('ShowGenInfo',fShowGenInfo,CompOpts.fShowGenInfo);
  Tool.AddDiff('ShowLineNum',fShowLineNum,CompOpts.fShowLineNum);
  Tool.AddDiff('ShowAll',fShowAll,CompOpts.fShowAll);
  Tool.AddDiff('ShowAllProcsOnError',fShowAllProcsOnError,CompOpts.fShowAllProcsOnError);
  Tool.AddDiff('ShowDebugInfo',fShowDebugInfo,CompOpts.fShowDebugInfo);
  Tool.AddDiff('ShowUsedFiles',fShowUsedFiles,CompOpts.fShowUsedFiles);
  Tool.AddDiff('ShowTriedFiles',fShowTriedFiles,CompOpts.fShowTriedFiles);
  Tool.AddDiff('ShowDefMacros',fShowDefMacros,CompOpts.fShowDefMacros);
  Tool.AddDiff('ShowCompProc',fShowCompProc,CompOpts.fShowCompProc);
  Tool.AddDiff('ShowCond',fShowCond,CompOpts.fShowCond);
  Tool.AddDiff('ShowExecInfo',fShowExecInfo,CompOpts.fShowExecInfo);
  Tool.AddDiff('ShowNothing',fShowNothing,CompOpts.fShowNothing);
  Tool.AddDiff('ShowSummary',fShowSummary,CompOpts.fShowSummary);
  Tool.AddDiff('ShowHintsForUnusedUnitsInMainSrc',fShowHintsForUnusedUnitsInMainSrc,CompOpts.fShowHintsForUnusedUnitsInMainSrc);
  Tool.AddDiff('ShowHintsForSenderNotUsed',fShowHintsForSenderNotUsed,CompOpts.fShowHintsForSenderNotUsed);
  Tool.AddDiff('WriteFPCLogo',fWriteFPCLogo,CompOpts.fWriteFPCLogo);

  // other
  Tool.Path:='Other';
  Tool.AddDiff('DontUseConfigFile',fDontUseConfigFile,CompOpts.fDontUseConfigFile);
  Tool.AddDiff('CustomConfigFile',fCustomConfigFile,CompOpts.fCustomConfigFile);
  Tool.AddDiff('ConfigFilePath',fConfigFilePath,CompOpts.fConfigFilePath);
  Tool.AddDiff('StopAfterErrCount',fStopAfterErrCount,CompOpts.fStopAfterErrCount);
  Tool.AddDiff('CustomOptions',fCustomOptions,CompOpts.fCustomOptions);

  // compilation
  Tool.Path:='Compilation';
  Tool.AddDiff('CompilerPath',fCompilerPath,CompOpts.fCompilerPath);
  ExecuteBefore.CreateDiff(CompOpts.ExecuteBefore,Tool);
  ExecuteAfter.CreateDiff(CompOpts.ExecuteAfter,Tool);
  Tool.AddDiff('CreateMakefileOnBuild',fCreateMakefileOnBuild,CompOpts.fCreateMakefileOnBuild);
end;


{ TAdditionalCompilerOptions }

procedure TAdditionalCompilerOptions.SetCustomOptions(const AValue: string);
begin
  if FCustomOptions=AValue then exit;
  FCustomOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosCustomOptions,fCustomOptions);
end;

procedure TAdditionalCompilerOptions.SetSrcPath(const AValue: string);
begin
  if FSrcPath=AValue then exit;
  FSrcPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosSrcPath,FSrcPath);
end;

procedure TAdditionalCompilerOptions.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  ParsedOpts.SetUnparsedValue(pcosBaseDir,FBaseDirectory);
end;

procedure TAdditionalCompilerOptions.SetIncludePath(const AValue: string);
begin
  if FIncludePath=AValue then exit;
  FIncludePath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosIncludePath,FIncludePath);
end;

procedure TAdditionalCompilerOptions.SetLibraryPath(const AValue: string);
begin
  if FLibraryPath=AValue then exit;
  FLibraryPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLibraryPath,FLibraryPath);
end;

procedure TAdditionalCompilerOptions.SetLinkerOptions(const AValue: string);
begin
  if FLinkerOptions=AValue then exit;
  FLinkerOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLinkerOptions,fLinkerOptions);
end;

procedure TAdditionalCompilerOptions.SetObjectPath(const AValue: string);
begin
  if FObjectPath=AValue then exit;
  FObjectPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosObjectPath,FObjectPath);
end;

procedure TAdditionalCompilerOptions.SetUnitPath(const AValue: string);
begin
  if FUnitPath=AValue then exit;
  FUnitPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosUnitPath,FUnitPath);
end;

constructor TAdditionalCompilerOptions.Create(TheOwner: TObject);
begin
  fOwner:=TheOwner;
  FConditionals:=TCompOptConditionals.Create(BuildModeSet.Evaluator);
  FParsedOpts:=TParsedCompilerOptions.Create(FConditionals);
  Clear;
end;

destructor TAdditionalCompilerOptions.Destroy;
begin
  FreeThenNil(FParsedOpts);
  FreeThenNil(FConditionals);// free conditionals before FParsedOpts
  inherited Destroy;
end;

procedure TAdditionalCompilerOptions.Clear;
begin
  FCustomOptions:='';
  FIncludePath:='';
  FLibraryPath:='';
  FLinkerOptions:='';
  FObjectPath:='';
  FUnitPath:='';
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
  FConditionals.LoadFromXMLConfig(XMLConfig,Path+'Conditionals/',AdjustPathDelims);
end;

procedure TAdditionalCompilerOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'CustomOptions/Value',fCustomOptions,'');
  XMLConfig.SetDeleteValue(Path+'IncludePath/Value',FIncludePath,'');
  XMLConfig.SetDeleteValue(Path+'LibraryPath/Value',FLibraryPath,'');
  XMLConfig.SetDeleteValue(Path+'LinkerOptions/Value',fLinkerOptions,'');
  XMLConfig.SetDeleteValue(Path+'ObjectPath/Value',FObjectPath,'');
  XMLConfig.SetDeleteValue(Path+'UnitPath/Value',FUnitPath,'');
  FConditionals.SaveToXMLConfig(XMLConfig,Path+'Conditionals/');
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

{ TParsedCompilerOptions }

constructor TParsedCompilerOptions.Create(TheConditionals: TCompOptConditionals);
begin
  FConditionals:=TheConditionals;
  Clear;
end;

function TParsedCompilerOptions.GetParsedValue(Option: TParsedCompilerOptString
  ): string;
var
  s: String;
begin
  if ParsedStamp[Option]<>CompilerParseStamp then begin
    if Parsing[Option] then begin
      DebugLn('TParsedCompilerOptions.GetParsedValue Circle in Options: ',ParsedCompilerOptStringNames[Option]);
      exit('');
    end;
    Parsing[Option]:=true;
    try
      s:=DoParseOption(UnparsedValues[Option],Option,true,false);
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
      s:=DoParseOption(UnparsedValues[Option],Option,false,true);
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
  Option: TParsedCompilerOptString; UseGetWritableOutputDirectory,
  PlatformIndependent: boolean): string;
var
  s: String;
  BaseDirectory: String;
  cocOption: TCOCValueType;
  h: string;
begin
  s:=OptionText;
  // add conditional additions
  if Conditionals<>nil then begin
    cocOption:=ParsedCompOptToConditional[Option];
    case Option of
    pcosUnitPath,pcosSrcPath,pcosIncludePath,pcosObjectPath,pcosLibraryPath,
    pcosDebugPath:
      // add search path
      s:=MergeSearchPaths(s,FConditionals.Values[cocOption]);
    pcosLinkerOptions,pcosCustomOptions:
      begin
        // add command line option
        h:=FConditionals.Values[cocOption];
        if (h<>'') then begin
          if s<>'' then s:=s+' ';
          s:=s+h;
        end;
      end;
    end;
  end;
  // parse locally
  if Assigned(OnLocalSubstitute) then
    s:=OnLocalSubstitute(s,PlatformIndependent);
  // parse globally
  s:=ParseString(Self,s,PlatformIndependent);
  // improve
  if Option=pcosBaseDir then
    // base directory (append path)
    s:=AppendPathDelim(TrimFilename(s))
  else if Option in ParsedCompilerFilenames then begin
    // make filename absolute
    s:=TrimFilename(s);
    if (s<>'') and (not FilenameIsAbsolute(s)) then begin
      if PlatformIndependent then
        BaseDirectory:=GetParsedPIValue(pcosBaseDir)
      else
        BaseDirectory:=GetParsedValue(pcosBaseDir);
      if (BaseDirectory<>'') then s:=TrimFilename(BaseDirectory+s);
    end;
  end
  else if Option in ParsedCompilerDirectories then begin
    // make directory absolute
    s:=TrimFilename(s);
    if ((s='') or (not FilenameIsAbsolute(s)))
    and (Option<>pcosBaseDir) then begin
      if PlatformIndependent then
        BaseDirectory:=GetParsedPIValue(pcosBaseDir)
      else
        BaseDirectory:=GetParsedValue(pcosBaseDir);
      if (BaseDirectory<>'') then s:=TrimFilename(BaseDirectory+s);
      if (Option in ParsedCompilerOutDirectories)
      and UseGetWritableOutputDirectory
      and Assigned(GetWritableOutputDirectory) then begin
        GetWritableOutputDirectory(s);
      end;
    end;
    s:=AppendPathDelim(s);
  end
  else if Option in ParsedCompilerSearchPaths then begin
    // make search paths absolute
    if PlatformIndependent then
      BaseDirectory:=GetParsedPIValue(pcosBaseDir)
    else
      BaseDirectory:=GetParsedValue(pcosBaseDir);
    s:=TrimSearchPath(s,BaseDirectory);
  end else if Option=pcosCustomOptions then begin
    s:=SpecialCharsToSpaces(s);
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

{ TCompilationToolOptions }

procedure TCompilationToolOptions.Clear;
begin
  Command:='';
  ScanForFPCMessages:=false;
  ScanForMakeMessages:=false;
  ShowAllMessages:=false;
end;

function TCompilationToolOptions.IsEqual(CompOpts: TCompilationToolOptions
  ): boolean;
var
  Tool: TCompilerDiffTool;
begin
  Tool:=TCompilerDiffTool.Create(nil);
  CreateDiff(CompOpts,Tool);
  Result:=Tool.Differ;
  Tool.Free;
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
  Command:=SwitchPathDelims(XMLConfig.GetValue(Path+'Command/Value',''),
                            DoSwitchPathDelims);
  ScanForFPCMessages:=XMLConfig.GetValue(Path+'ScanForFPCMsgs/Value',false);
  ScanForMakeMessages:=XMLConfig.GetValue(Path+'ScanForMakeMsgs/Value',false);
  ShowAllMessages:=XMLConfig.GetValue(Path+'ShowAllMessages/Value',false);
end;

procedure TCompilationToolOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Command/Value',Command,'');
  XMLConfig.SetDeleteValue(Path+'ScanForFPCMsgs/Value',
                           ScanForFPCMessages,false);
  XMLConfig.SetDeleteValue(Path+'ScanForMakeMsgs/Value',
                           ScanForMakeMessages,false);
  XMLConfig.SetDeleteValue(Path+'ShowAllMessages/Value',
                           ShowAllMessages,false);
end;

procedure TCompilationToolOptions.CreateDiff(CompOpts: TCompilationToolOptions;
  Tool: TCompilerDiffTool);
begin
  Tool.AddDiff('Command',Command,CompOpts.Command);
  Tool.AddDiff('ScanForFPCMessages',ScanForFPCMessages,CompOpts.ScanForFPCMessages);
  Tool.AddDiff('ScanForMakeMessages',ScanForMakeMessages,CompOpts.ScanForMakeMessages);
  Tool.AddDiff('ShowAllMessages',ShowAllMessages,CompOpts.ShowAllMessages);
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

  if SourceEditorWindow<>nil then
    SourceEditorWindow.ClearErrorLines;

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

{ TBuildModeSet }

constructor TBuildModeSet.Create;
begin
  FEvaluator:=TExpressionEvaluator.Create;
end;

destructor TBuildModeSet.Destroy;
begin
  FreeAndNil(FEvaluator);
  inherited Destroy;
end;

{ TIDEBuildMode }

procedure TIDEBuildMode.SetIdentifier(const AValue: string);
begin
  if FIdentifier=AValue then exit;
  if (FIdentifier='') or (not IsValidIdent(FIdentifier)) then
    raise Exception.Create('TIDEBuildMode.SetIdentifier invalid identifier: '+FIdentifier);
  FIdentifier:=AValue;
end;

procedure TIDEBuildMode.SetLocalizedName(const AValue: string);
begin
  if FLocalizedName=AValue then exit;
  FLocalizedName:=AValue;
end;

procedure TIDEBuildMode.SetLocalizedValues(const AValue: TStrings);
begin
  if FLocalizedValues=AValue then exit;
  FLocalizedValues.Assign(AValue);
end;

procedure TIDEBuildMode.SetValues(const AValue: TStrings);
begin
  if FValues=AValue then exit;
  FValues.Assign(AValue);
end;

constructor TIDEBuildMode.Create;
begin
  FValues:=TStringList.Create;
  FLocalizedValues:=TStringList.Create;
  FDefaultValue:=TCompOptConditionals.Create(BuildModeSet.Evaluator);
end;

destructor TIDEBuildMode.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FLocalizedValues);
  FreeAndNil(FDefaultValue);
  inherited Destroy;
end;

procedure TIDEBuildMode.Assign(Source: TLazBuildMode);
begin
  FIdentifier:=Source.Identifier;
  FDefaultValue.Assign(Source.DefaultValue);
  FLocalizedName:=Source.LocalizedName;
  FLocalizedValues.Assign(Source.LocalizedValues);
  FValues.Assign(Source.Values);
end;

procedure TIDEBuildMode.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
begin
  FIdentifier:=AXMLConfig.GetValue(Path+'Identifier/Value','');
  TCompOptConditionals(FDefaultValue).LoadFromXMLConfig(AXMLConfig,Path+'DefaultValue',
                                                        DoSwitchPathDelims);
  LoadStringList(AXMLConfig,FValues,Path+'Values/');
end;

procedure TIDEBuildMode.SaveToXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
begin
  AXMLConfig.SetDeleteValue(Path+'Identifier/Value',FIdentifier,'');
  TCompOptConditionals(FDefaultValue).SaveToXMLConfig(AXMLConfig,Path+'DefaultValue');
  SaveStringList(AXMLConfig,FValues,Path+'Values/');
end;

{ TIDEBuildModes }

function TIDEBuildModes.GetItems(Index: integer): TLazBuildMode;
begin
  Result:=TLazBuildMode(FItems[Index]);
end;

function TIDEBuildModes.Add(Identifier: string): TLazBuildMode;
begin
  if IndexOfIdentifier(Identifier)>=0 then
    raise Exception.Create('TIDEBuildModes.Add identifier already exists');
  Result:=TIDEBuildMode.Create;
  Result.Identifier:=Identifier;
  FItems.Add(Result);
end;

procedure TIDEBuildModes.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TIDEBuildModes.Count: integer;
begin
  Result:=FItems.Count;
end;

constructor TIDEBuildModes.Create;
begin
  FItems:=TFPList.Create;
end;

procedure TIDEBuildModes.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

destructor TIDEBuildModes.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TIDEBuildModes.IndexOfIdentifier(Identifier: string): integer;
begin
  Result:=FItems.Count-1;
  while (Result>=0) and (SysUtils.CompareText(Identifier,Items[Result].Identifier)<>0) do
    dec(Result);
end;

function TIDEBuildModes.ModeWithIdentifier(Identifier: string): TLazBuildMode;
var
  i: LongInt;
begin
  i:=IndexOfIdentifier(Identifier);
  if i<0 then
    Result:=nil
  else
    Result:=Items[i];
end;

procedure TIDEBuildModes.Move(OldIndex, NewIndex: integer);
begin
  FItems.Move(OldIndex,NewIndex);
end;

procedure TIDEBuildModes.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
var
  NewItem: TIDEBuildMode;
  NewCount: LongInt;
  i: Integer;
begin
  Clear;
  NewCount:=AXMLConfig.GetValue(Path+'Count/Value',0);
  for i:=0 to NewCount-1 do begin
    NewItem:=TIDEBuildMode.Create;
    NewItem.LoadFromXMLConfig(AXMLConfig,Path+'Item'+IntToStr(i+1)+'/',DoSwitchPathDelims);
    if (NewItem.Identifier<>'') and IsValidIdent(NewItem.Identifier) then
      FItems.Add(NewItem);
  end;
end;

procedure TIDEBuildModes.SaveToXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
begin
  AXMLConfig.SetDeleteValue(Path+'Count/Value',Count,0);
  for i:=0 to Count-1 do
    TIDEBuildMode(Items[i]).SaveToXMLConfig(AXMLConfig,Path+'Item'+IntToStr(i+1)+'/');
end;

initialization
  CompilerParseStamp:=1;
  CompilerParseStampIncreased:=nil;

end.

