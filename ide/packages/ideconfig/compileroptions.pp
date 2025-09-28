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
  Classes, SysUtils, AVL_Tree, System.UITypes,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, Laz2_XMLCfg, Laz2_DOM, LazUtilities, LazTracer,
  LazStringUtils, FPCAdds,
  // CodeTools
  FileProcs, DefineTemplates, CodeToolsCfgScript, CodeToolManager,
  KeywordFuncLists, BasicCodeTools, LinkScanner, DirectoryCacher,
  // BuildIntf
  ProjectIntf, MacroIntf, IDEExternToolIntf, CompOptsIntf, IDEOptionsIntf,
  // IdeConfig
  LazConf, EnvironmentOpts, SearchPathProcs, IdeXmlConfigProcs, TransferMacros,
  IDEProcs, ModeMatrixOpts, CompOptsModes, etFPCMsgFilePool, ParsedCompilerOpts;

const
  DefaultCompilerPath = '$(CompPath)';

type
  TCheckCompileOptionsMsgLvl = (
    ccomlHints,
    ccomlWarning,
    ccomlErrors,
    ccomlNone
    );

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
    - every package and project has this list of build macros (editable via GUI)
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

const
  CompilerOptionMacroNormal = 0;
  CompilerOptionMacroPlatformIndependent = 1;

type
  //TLocalSubstitutionEvent = function(s: string;
  //                              PlatformIndependent: boolean): string of object;

  TInheritedCompOptsParseTypesStrings =
    array[TCompilerOptionsParseType] of TInheritedCompOptsStrings;


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

const
  CompilerCmdLineOptionNames: array[TCompilerCmdLineOption] of string = (
    'NoLinkerOpts',
    'AddVerboseAll',
    'DoNotAppendOutFileOption',
    'AbsolutePaths',
    'NoMacroParams',
    'AddCompilerPath'
  );

type

  { TCompilationToolOptions }

  TCompilationToolOptions = class(TLazCompilationToolOptions)
  private
    FParsedCommandStamp: integer;
    FParsedCommand: string;
  protected
    procedure DoClearErrorLines; virtual;
    procedure SetCommand(AValue: string); override;
    procedure SubstituteMacros(var s: string); virtual;
  public
    function CreateDiff(CompOpts: TCompilationToolOptions;
                        Tool: TCompilerDiffTool = nil): boolean; virtual;
    procedure Assign(Src: TLazCompilationToolOptions); override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch); virtual;
    function Execute(const WorkingDir, ToolTitle, CompileHint: string): TModalResult;
    function CreateExtTool(const WorkingDir, ToolTitle, CompileHint: string): TAbstractExternalTool;
    function GetParsedCommand: string; // resolved macros
    function HasCommands: boolean; // true if there is something to execute
  end;

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
    function Count: SizeInt;
    property ChangeStamp: int64 read FChangeStamp;
  end;

type
  { TBaseCompilerOptions }

  TBaseCompilerOptions = class(TLazCompilerOptions)
  private
    FDefaultMakeOptionsFlags: TCompilerCmdLineOptions;
    fInheritedOptions: TInheritedCompOptsParseTypesStrings;
    fInheritedOptParseStamps: integer;
    FOutputDirectoryOverride: string;
    FParsedOpts: TParsedCompilerOptions;
    FStorePathDelim: TPathDelimSwitch;
    FOtherDefines: TStrings; // list of user selectable defines for custom options
    FFPCMsgFile: TFPCMsgFilePoolItem;
    FCreateMakefileOnBuild: boolean;
    procedure AppendDefaultExt(var aFilename: string);
    function GetExecuteAfter: TCompilationToolOptions;
    function GetExecuteBefore: TCompilationToolOptions;
    function GetOutputDirOverride: string;
    procedure PrependDefaultType(var AFilename: string);
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
    function GetWriteConfigFilePath: String; override;
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
    procedure SetOutputDirectoryOverride(const AValue: string); virtual;
    procedure SetSrcPath(const AValue: string); override;
    procedure SetDebugPath(const AValue: string); override;
    procedure SetTargetCPU(const AValue: string); override;
    procedure SetTargetProc(const AValue: string); override;
    procedure SetTargetOS(const AValue: string); override;
    procedure SetTargetFileExt(const AValue: String); override;
    procedure SetTargetFilename(const AValue: String); override;
    procedure SetWriteConfigFilePath(AValue: String); override;
  protected
    function GetModified: boolean; override;
    procedure SetModified(const AValue: boolean); override;
    procedure ClearInheritedOptions;
    procedure SetDefaultMakeOptionsFlags(const AValue: TCompilerCmdLineOptions);
  public
    constructor Create(const AOwner: TObject); override;
    constructor Create(const AOwner: TObject; const AToolClass: TLazCompilationToolClass);
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

    function MakeCompilerParams(Flags: TCompilerCmdLineOptions): TStringListUTF8Fast;
    procedure GetSyntaxOptions(Kind: TPascalCompiler; Params: TStrings); virtual;
    function CreatePPUFilename(const SourceFileName: string): string; override;
    function CreateTargetFilename: string; override;
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
    function GetDefaultWriteConfigFilePath: string; virtual; abstract;
    function CanBeDefaultForProject: boolean; virtual;
    function NeedsLinkerOpts: boolean;
    function HasCommands: boolean; // true if there is at least one commad to execute
    function HasCompilerCommand: boolean; virtual;
    function GetEffectiveTargetOS: string; override;
    function GetEffectiveTargetCPU: string; override;
    function GetEffectiveLCLWidgetType: string; override;
    // parsed CompilerFilename: use ParsedOpts.GetParsedValue(pcosCompilerPath)
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
    function ShortenPath(const SearchPath: string): string;
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
    property OutputDirectoryOverride: string read FOutputDirectoryOverride write SetOutputDirectoryOverride;
    // stored properties
    property StorePathDelim: TPathDelimSwitch read FStorePathDelim write FStorePathDelim;
    property OtherDefines: TStrings read FOtherDefines;
    // compilation
    property ExecuteBefore: TCompilationToolOptions read GetExecuteBefore;
    property ExecuteAfter: TCompilationToolOptions read GetExecuteAfter;
    property CreateMakefileOnBuild: boolean read FCreateMakefileOnBuild
                                            write SetCreateMakefileOnBuild;
  end;

  TBaseCompilerOptionsClass = class of TBaseCompilerOptions;


  { TCompilerOptions }

  TCompilerOptions = TBaseCompilerOptions;

const
  CompileReasonNames: array[TCompileReason] of string = (
    'Compile',
    'Build',
    'Run'
    );

function GetMakefileMacroValue(const MacroName: string): string;
function TargetNeedsFPCOptionCG(TargetOS, TargetCPU: string): boolean;

procedure GatherInheritedOptions(AddOptionsList: TFPList;
  Parsed: TCompilerOptionsParseType;
  var InheritedOptionStrings: TInheritedCompOptsStrings);
function InheritedOptionsToCompilerParameters(
  var InheritedOptionStrings: TInheritedCompOptsStrings;
  Flags: TCompilerCmdLineOptions): string;
procedure ConvertSearchPathToCmdParams(const Switch, Paths, BasePath: String; Params: TStrings);
procedure ConvertOptionsToCmdParams(const Switch, OptionStr: string; Params: TStrings);

function LoadXMLCompileReasons(const AConfig: TXMLConfig;
  const APath: String; const DefaultReasons: TCompileReasons): TCompileReasons;
procedure SaveXMLCompileReasons(const AConfig: TXMLConfig; const APath: String;
  const AFlags, DefaultFlags: TCompileReasons);

function EnumToStr(Flag: TCompilerFlagValue): string; overload;
function CompareCompMsgIdFlag(Data1, Data2: Pointer): integer;

function dbgs(const Opts: TCompilerCmdLineOptions): string; overload;

var
  TestCompilerOptions: TNotifyEvent = nil;

implementation

const
  CompilerOptionsVersion = 11;
  // 11 Debugging/DebugInfoType/Value
  // 7  TargetProcessor/Value
  // 6  SyntaxMode/Value

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
      or (TargetOS='openbsd') or (TargetOS='dragonfly') or (TargetOS='solaris'));
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
              RaiseGDBException('GatherInheritedOptions'){%H-};
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
  Params: TStringListUTF8Fast;
begin
  Result:='';
  Params:=TStringListUTF8Fast.Create;
  try

    // inherited Linker options
    if (not (ccloNoLinkerOpts in Flags)) then begin
      CurLinkerOpts:=InheritedOptionStrings[icoLinkerOptions];
      if CurLinkerOpts<>'' then
        ConvertOptionsToCmdParams('-k', CurLinkerOpts, Params);
    end;

    // include path
    CurIncludePath:=InheritedOptionStrings[icoIncludePath];
    if (CurIncludePath <> '') then
      ConvertSearchPathToCmdParams('-Fi', CurIncludePath, '', Params);

    // library path
    if (not (ccloNoLinkerOpts in Flags)) then begin
      CurLibraryPath:=InheritedOptionStrings[icoLibraryPath];
      if (CurLibraryPath <> '') then
        ConvertSearchPathToCmdParams('-Fl', CurLibraryPath, '', Params);
    end;

    // namespaces
    CurNamespaces:=InheritedOptionStrings[icoNamespaces];
    if CurNamespaces <> '' then
      Params.Add('-FN'+CurNamespaces);

    // object path
    CurObjectPath:=InheritedOptionStrings[icoObjectPath];
    if (CurObjectPath <> '') then
      ConvertSearchPathToCmdParams('-Fo', CurObjectPath, '', Params);

    // unit path
    CurUnitPath:=InheritedOptionStrings[icoUnitPath];
    // always add the current directory to the unit path, so that the compiler
    // checks for changed files in the directory
    CurUnitPath:=CurUnitPath+';.';
    ConvertSearchPathToCmdParams('-Fu', CurUnitPath, '', Params);

    // custom options
    CurCustomOptions:=InheritedOptionStrings[icoCustomOptions];
    if CurCustomOptions<>'' then
      ConvertOptionsToCmdParams('',SpecialCharsToSpaces(CurCustomOptions,true),Params);

    Result:=MergeCmdLineParams(Params);
  finally
    Params.Free;
  end;
end;

procedure ConvertSearchPathToCmdParams(const Switch, Paths, BasePath: String;
  Params: TStrings);
var
  StartPos: Integer;
  l, p, i: Integer;
  EndPos: LongInt;
  CurPath, Dir: String;
  Kind: TCTStarDirectoryKind;
  Cache: TCTStarDirectoryCache;
  SubDirs: TStringListUTF8Fast;
  IsRelative: Boolean;
begin
  if Switch='' then
    RaiseGDBException('ConvertSearchPathToCmdLine no Switch');
  if (Paths = '') then exit;
  l:=length(Paths);
  StartPos:=1;
  while StartPos<=l do
  begin
    while (StartPos<=l) and (Paths[StartPos]=' ') do inc(StartPos);
    EndPos:=StartPos;
    while (EndPos<=l) and (Paths[EndPos]<>';') do inc(EndPos);
    if StartPos<EndPos then
    begin
      CurPath:=copy(Paths,StartPos,EndPos-StartPos);
      StartPos:=EndPos+1;
      Kind:=IsCTStarDirectory(CurPath,p);
      if Kind in [ctsdStar,ctsdStarStar] then
      begin
        Delete(CurPath,p+1,length(CurPath));
        IsRelative:=not FilenameIsAbsolute(CurPath);
        if IsRelative then
        begin
          if not FilenameIsAbsolute(BasePath) then continue;
          CurPath:=BasePath+CurPath;
        end;
        Cache:=CodeToolBoss.DirectoryCachePool.GetStarCache(CurPath,Kind);
        if Cache<>nil then
        begin
          Cache.UpdateListing;
          SubDirs:=Cache.Listing.SubDirs;
          for i:=0 to SubDirs.Count-1 do
          begin
            Dir:=CurPath + SubDirs[i];
            if IsRelative then
              Dir:=CreateRelativePath(Dir,BasePath,true);
            Params.Add(Switch + Dir);
          end;
        end;
      end else
        Params.Add(Switch + CurPath);
    end;
  end;
end;

procedure ConvertOptionsToCmdParams(const Switch, OptionStr: string; Params: TStrings);
var
  Startpos, EndPos: integer;
  p: Integer;
begin
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
      Params.Add(Switch+copy(OptionStr,StartPos,EndPos-StartPos));
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

function dbgs(const Opts: TCompilerCmdLineOptions): string;
var
  o: TCompilerCmdLineOption;
begin
  Result:='';
  for o in Opts do
  begin
    if Result<>'' then Result+=',';
    Result+=CompilerCmdLineOptionNames[o];
  end;
  Result:='['+Result+']';
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

function TCompilerMsgIDFlagsEnumerator.GetEnumerator: TCompilerMsgIDFlagsEnumerator;
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
  const AToolClass: TLazCompilationToolClass);
begin
  inherited Create(AOwner);
  FParsedOpts := TParsedCompilerOptions.Create(Self);
  ParsedOpts.OnGetOutputDirectoryOverride:=@GetOutputDirOverride;
  FOtherDefines := TStringList.Create;
  FExecuteBefore := AToolClass.Create(Self);
  FExecuteAfter := AToolClass.Create(Self);
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
  NewValue:=ShortenPath(AValue);
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
  NewValue:=ShortenPath(AValue);
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
  NewValue:=ShortenPath(AValue);
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

procedure TBaseCompilerOptions.SetWriteConfigFilePath(AValue: String);
begin
  if WriteConfigFilePath=AValue then exit;
  ParsedOpts.SetUnparsedValue(pcosWriteConfigFilePath,AValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetWriteConfigFilePath ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

function TBaseCompilerOptions.GetModified: boolean;
begin
  Result:=(inherited GetModified) or MessageFlags.Modified;
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

function TBaseCompilerOptions.GetWriteConfigFilePath: String;
begin
  Result:=ParsedOpts.Values[pcosWriteConfigFilePath].UnparsedValue;
end;

function TBaseCompilerOptions.GetExecuteAfter: TCompilationToolOptions;
begin
  Result:=TCompilationToolOptions(fExecuteAfter);
end;

function TBaseCompilerOptions.GetExecuteBefore: TCompilationToolOptions;
begin
  Result:=TCompilationToolOptions(fExecuteBefore);
end;

function TBaseCompilerOptions.GetOutputDirOverride: string;
begin
  Result:=OutputDirectoryOverride;
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
  NewValue:=ShortenPath(AValue);
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
  NewValue:=ShortenPath(AValue);
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
  NewValue:=ShortenPath(AValue);
  if ObjectPath=NewValue then exit;
  ParsedOpts.SetUnparsedValue(pcosObjectPath,NewValue);
  {$IFDEF VerboseIDEModified}
  debugln(['TBaseCompilerOptions.SetObjectPath ',AValue]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TBaseCompilerOptions.SetOutputDirectoryOverride(const AValue: string);
begin
  if FOutputDirectoryOverride=AValue then Exit;
  FOutputDirectoryOverride:=AValue;
  if ParsedOpts.InvalidateParseOnChange then
    IncreaseCompilerParseStamp;// the output dir is used by other packages
  //if FOutputDirectoryOverride<>'' then
  //  DebugLn(['TBaseCompilerOptions.SetOutputDirectoryOverride New=',FOutputDirectoryOverride])
  //else
  //  DebugLn(['TBaseCompilerOptions.SetOutputDirectoryOverride using default']);
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
  FConditionals:=LineBreaksToSystemLineBreaks(UTF8Trim(
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
  UseAnsiStrings := aXMLConfig.GetValue(p+'UseAnsiStrings/Value', FileVersion>=9);
  CPPInline := aXMLConfig.GetValue(p+'CPPInline/Value', true);
  CStyleMacros := aXMLConfig.GetValue(p+'CStyleMacros/Value', false);
  InitConstructor := aXMLConfig.GetValue(p+'InitConstructor/Value', false);
  TypedAddress := aXMLConfig.GetValue(p+'TypedAddress/Value', false);

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
  Subtarget := aXMLConfig.GetValue(p+'Subtarget/Value', '');
  OptimizationLevel := aXMLConfig.GetValue(p+'Optimizations/OptimizationLevel/Value', 1);
  VariablesInRegisters := aXMLConfig.GetValue(p+'Optimizations/VariablesInRegisters/Value', false);
  UncertainOptimizations := aXMLConfig.GetValue(p+'Optimizations/UncertainOptimizations/Value', false);
  ReadSmaller;

  { Linking }
  p:=Path+'Linking/';
  GenerateDebugInfo := aXMLConfig.GetValue(p+'Debugging/GenerateDebugInfo/Value', FileVersion >= 11); // Default = True, since version 11 (was False before)
  RunWithoutDebug := aXMLConfig.GetValue(p+'Debugging/RunWithoutDebug/Value', False);
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
  ShowWarn := aXMLConfig.GetValue(p+'Verbosity/ShowWarn/Value', true);
  ShowNotes := aXMLConfig.GetValue(p+'Verbosity/ShowNotes/Value', true);
  ShowHints := aXMLConfig.GetValue(p+'Verbosity/ShowHints/Value', true);
  if aXMLConfig.HasPath(p+'Verbosity/ShoLineNum/Value', True) then begin
    // Support an old typo. This can be deleted after Lazarus 4.0 release.
    DebugLn(['TBaseCompilerOptions.LoadFromXMLConfig: Reading "',p+'Verbosity/ShoLineNum/Value"']);
    ShowLineNum := aXMLConfig.GetValue(p+'Verbosity/ShoLineNum/Value', false)
  end
  else
    ShowLineNum := aXMLConfig.GetValue(p+'Verbosity/ShowLineNum/Value', false);
  ShowAll := aXMLConfig.GetValue(p+'Verbosity/ShowAll/Value', false);
  ShowDebugInfo := aXMLConfig.GetValue(p+'Verbosity/ShowDebugInfo/Value', false);
  ShowUsedFiles := aXMLConfig.GetValue(p+'Verbosity/ShowUsedFiles/Value', false);
  ShowTriedFiles := aXMLConfig.GetValue(p+'Verbosity/ShowTriedFiles/Value', false);
  ShowCompProc := aXMLConfig.GetValue(p+'Verbosity/ShowCompProc/Value', false);
  ShowCond := aXMLConfig.GetValue(p+'Verbosity/ShowCond/Value', false);
  ShowExecInfo := aXMLConfig.GetValue(p+'Verbosity/ShowExecInfo/Value', false);
  ShowHintsForUnusedUnitsInMainSrc := aXMLConfig.GetValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', false);
  ShowHintsForSenderNotUsed := aXMLConfig.GetValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', false);
  WriteFPCLogo := aXMLConfig.GetValue(p+'WriteFPCLogo/Value', true);
  StopAfterErrCount := aXMLConfig.GetValue(p+'ConfigFile/StopAfterErrCount/Value', 1);

  ReadListOfMessageFlags(p+'CompilerMessages/IgnoredMessages',cfvHide);
  ReadListOfMessageFlags(p+'CompilerMessages/NonIgnoredMessages',cfvShow);

  { Other }
  p:=Path+'Other/';
  DontUseConfigFile := aXMLConfig.GetValue(p+'ConfigFile/DontUseConfigFile/Value', false);
  WriteConfigFile := aXMLConfig.GetValue(p+'ConfigFile/WriteConfigFile/Value', false);
  WriteConfigFilePath := f(aXMLConfig.GetValue(p+'ConfigFile/WriteConfigFilePath/Value',''));
  if WriteConfigFilePath='' then
    WriteConfigFilePath:=GetDefaultWriteConfigFilePath; // GetDefaultWriteConfigFilePath can contain pathdelims, which might differ between lpi/lpk and current platform
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
  aXMLConfig.SetDeleteValue(p+'UseAnsiStrings/Value', UseAnsiStrings,true);
  aXMLConfig.SetDeleteValue(p+'CPPInline/Value', CPPInline,true);
  aXMLConfig.SetDeleteValue(p+'CStyleMacros/Value', CStyleMacros,false);
  aXMLConfig.SetDeleteValue(p+'InitConstructor/Value', InitConstructor,false);
  aXMLConfig.SetDeleteValue(p+'TypedAddress/Value', TypedAddress,false);

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
  aXMLConfig.SetDeleteValue(p+'Subtarget/Value', Subtarget,'');
  aXMLConfig.SetDeleteValue(p+'Optimizations/OptimizationLevel/Value', OptimizationLevel,1);
  aXMLConfig.SetDeleteValue(p+'Optimizations/VariablesInRegisters/Value', VariablesInRegisters,false);
  aXMLConfig.SetDeleteValue(p+'Optimizations/UncertainOptimizations/Value', UncertainOptimizations,false);
  aXMLConfig.SetDeleteValue(p+'SmallerCode/Value', SmallerCode, false);

  { Linking }
  p:=Path+'Linking/';
  aXMLConfig.SetDeleteValue(p+'Debugging/GenerateDebugInfo/Value', GenerateDebugInfo, True); // Default = True, since version 11 (was False before)
  aXMLConfig.SetDeleteValue(p+'Debugging/RunWithoutDebug/Value', RunWithoutDebug, False);
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
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowWarn/Value', ShowWarn,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowNotes/Value', ShowNotes,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHints/Value', ShowHints,true);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowLineNum/Value', ShowLineNum,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowAll/Value', ShowAll,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowDebugInfo/Value', ShowDebugInfo,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowUsedFiles/Value', ShowUsedFiles,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowTriedFiles/Value', ShowTriedFiles,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowCompProc/Value', ShowCompProc,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowCond/Value', ShowCond,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowExecInfo/Value', ShowExecInfo,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHintsForUnusedUnitsInMainSrc/Value', ShowHintsForUnusedUnitsInMainSrc,false);
  aXMLConfig.SetDeleteValue(p+'Verbosity/ShowHintsForSenderNotUsed/Value', ShowHintsForSenderNotUsed,false);
  aXMLConfig.SetDeleteValue(p+'WriteFPCLogo/Value', WriteFPCLogo,true);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/StopAfterErrCount/Value', StopAfterErrCount,1);

  WriteListOfMessageFlags(p+'CompilerMessages/IgnoredMessages',cfvHide);
  WriteListOfMessageFlags(p+'CompilerMessages/NonIgnoredMessages',cfvShow);

  { Other }
  p:=Path+'Other/';
  aXMLConfig.SetDeleteValue(p+'ConfigFile/DontUseConfigFile/Value', DontUseConfigFile,false);
  aXMLConfig.SetDeleteValue(p+'ConfigFile/WriteConfigFile/Value', WriteConfigFile,false);
  s:=WriteConfigFilePath; // GetDefaultWriteConfigFilePath can contain pathdelims, which might differ between lpi/lpk and current platform
  if CompareFilenames(s,GetDefaultWriteConfigFilePath)=0 then
    s:='';
  aXMLConfig.SetDeleteValue(p+'ConfigFile/WriteConfigFilePath/Value', f(s),'');
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

procedure TBaseCompilerOptions.AppendDefaultExt(var aFilename: string);
var
  Ext: String;
begin
  if ExtractFileName(aFilename)='' then exit;
  Ext:=GetTargetFileExt;
  if (Ext<>'') and not FilenameExtIs(aFilename,Ext) then
    aFilename:=aFilename+Ext;
  //DebugLn('Filename is ',AFilename,' in AppendDefaultExt');
end;

procedure TBaseCompilerOptions.PrependDefaultType(var AFilename: string);
var
  Prefix, FileName, PathName, CurTargetOS, aSrcOS: String;
begin
  //DebugLn('Filename AFilename is ',AFilename, ' in PrependDefaultType');
  if (ExtractFileName(AFilename)='')
  or (CompareText(copy(ExtractFileName(AFilename),1,3), 'lib') = 0) then exit;
  Prefix:=GetTargetFilePrefix;
  if Prefix<>'' then
  begin
    FileName := ExtractFileName(AFilename);
    PathName := ExtractFilePath(AFilename);
    //debugln ( 'Filename is ',FileName, ' in PrependDefaultType' );
    CurTargetOS:=TargetOS;
    if CurTargetOS='' then CurTargetOS:=FPCAdds.GetCompiledTargetOS;
    aSrcOS:=GetDefaultSrcOSForTargetOS(CurTargetOS);
    if CompareText(aSrcOS, 'unix') = 0 then
      AFilename:=PathName+Prefix+UTF8LowerCase(FileName)
    else
      AFilename:=PathName+Prefix+FileName;
    //DebugLn('AFilename is ',AFilename, ' in PrependDefaultType');
    exit;
  end;
end;

function TBaseCompilerOptions.CreateTargetFilename: string;
var
  UnitOutDir, OutFilename, Dir: String;
begin
  Result:=TargetFilename;
  if Assigned(ParsedOpts.OnLocalSubstitute) then
    Result:=ParsedOpts.OnLocalSubstitute(Result,false)
  else
    Result:=ParseString(ParsedOpts,Result,false);
  if (Result<>'') and FilenameIsAbsolute(Result) then begin
    // fully specified target filename
  end else if Result<>'' then begin
    //debugln(['TBaseCompilerOptions.CreateTargetFilename OutputDirectoryOverride=',OutputDirectoryOverride]);
    if OutputDirectoryOverride<>'' then
    begin
      // the program/package is put into the output directory
      UnitOutDir:=GetUnitOutPath(false);
      if UnitOutDir='' then
        UnitOutDir:=BaseDirectory;
      Result:=AppendPathDelim(UnitOutDir)+ExtractFileName(Result);
    end else if BaseDirectory<>'' then begin
      // the program/package is put relative to the base directory
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
    RaiseGDBException(''){%H-};
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

function TBaseCompilerOptions.CanBeDefaultForProject: boolean;
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
    Result:=FPCAdds.GetCompiledTargetOS;
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
    Result:=FPCAdds.GetCompiledTargetCPU;
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
    RaiseGDBException(''){%H-};
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
    begin
      Result:=GetParsedPath(Option,InheritedOption,RelativeToBaseDir,WithBaseDir);
      exit;
    end;
  coptParsedPlatformIndependent:
    Result:=GetParsedPIPath(Option,InheritedOption,RelativeToBaseDir);
  else
    RaiseGDBException(''){%H-};
  end;
  if WithBaseDir then begin
    if RelativeToBaseDir then
      AddPath:='.'
    else
      AddPath:=BaseDirectory;
    if AddPath<>'' then
      Result:=MergeSearchPaths(AddPath,Result);
  end;
end;

function TBaseCompilerOptions.GetParsedPath(Option: TParsedCompilerOptString;
  InheritedOption: TInheritedCompilerOption;
  RelativeToBaseDir: boolean; AddBaseDir: boolean = false): string;
var
  CurrentPath: String;
  InheritedPath: String;
begin
  // the first path is searched first

  Result:='';
  if AddBaseDir then
    Result:=ParsedOpts.GetParsedValue(pcosBaseDir);

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

    Result:=MergeSearchPaths(Result,CurrentPath);
  end;

  // inherited path
  if InheritedOption<>icoNone then begin
    InheritedPath:=GetInheritedOption(InheritedOption,RelativeToBaseDir,coptParsed);
    {$IFDEF VerbosePkgUnitPath}
    if Option=pcosUnitPath then
      debugln('TBaseCompilerOptions.GetParsedPath Inherited ',dbgsName(Self),' InheritedPath="',InheritedPath,'"');
    {$ENDIF}

    Result:=MergeSearchPaths(Result,InheritedPath);
    {$IFDEF VerbosePkgUnitPath}
    if Option=pcosUnitPath then
      debugln('TBaseCompilerOptions.GetParsedPath Total ',dbgsName(Self),' Result="',Result,'"');
    {$ENDIF}
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
    RaiseGDBException(''){%H-};
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
var
  Params: TStringListUTF8Fast;
  h: String;
begin
  Params:=MakeCompilerParams([ccloNoMacroParams]);
  try
    Result:=MergeCmdLineParams(Params);
    h:=GetCustomOptions(coptParsed);
    if h<>'' then
    begin
      if Result<>'' then
        Result:=Result+' '+h
      else
        Result:=h;
    end;
  finally
    Params.Free;
  end;
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

function TBaseCompilerOptions.ShortenPath(const SearchPath: string): string;
begin
  Result:=ShortenSearchPath(TrimSearchPath(SearchPath,''),BaseDirectory,BaseDirectory);
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.MakeCompilerParams(
    const MainSourceFilename: string;
    Flags: TCompilerCmdLineOptions): String;

  Get all the options and create a string that can be passed to the compiler
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MakeCompilerParams(Flags: TCompilerCmdLineOptions
  ): TStringListUTF8Fast;
var
  tempsw, quietsw, t: String;
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
  CurSubtarget: String;
  CurSrcOS: String;
  dit: TCompilerDbgSymbolType;
  CompilerFilename: String;
  DefaultTargetOS: string;
  DefaultTargetCPU: string;
  RealCompilerFilename: String;
  CurNamespaces, BasePath: string;
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

  function FixExeExtForEmbeddedCompiler(exename: string): string;
  begin
    if SameStr(TargetOS, 'embedded') then
      Result := ChangeFileExt(exename, '')
    else
      Result := exename;
  end;

begin
  Result:=TStringListUTF8Fast.Create;

  Quiet:=ConsoleVerbosity<=-2; // if "-q" is specified more than once

  CompilerFilename:=ParsedOpts.GetParsedValue(pcosCompilerPath);
  if CompilerFilename<>'' then begin
    RealCompilerFilename:=CompilerFilename;
    Kind:=CodeToolBoss.GetPascalCompilerForDirectory(BaseDirectory);
  end
  else begin
    // use default compiler
    RealCompilerFilename:=EnvironmentOptions.GetParsedCompilerFilename;
    Kind:=pcFPC;
  end;

  if ccloAddCompilerPath in Flags then
    Result.Add(RealCompilerFilename);

  CurTargetOS:='';
  CurTargetCPU:='';
  CurSubtarget:='';
  if not (ccloNoMacroParams in Flags) then
  begin
    Vars:=GetBuildMacroValues(Self,true);
    if Vars<>nil then
    begin
      CurTargetOS:=GetFPCTargetOS(Vars.Values['TargetOS']);
      CurTargetCPU:=GetFPCTargetCPU(Vars.Values['TargetCPU']);
      CurSubtarget:=Vars.Values['Subtarget'];
    end;
  end;
  CurSrcOS:=GetDefaultSrcOSForTargetOS(CurTargetOS);

  CodeToolBoss.CompilerDefinesCache.ConfigCaches.GetDefaultCompilerTarget(
    RealCompilerFilename,'',DefaultTargetOS,DefaultTargetCPU);

  { ------------------ Target --------------------- }

  { Target OS }
  if (CurTargetOS<>'')
  and ((TargetOS<>'') or (CurTargetOS<>DefaultTargetOS)) then
    Result.Add('-T' + CurTargetOS);
  { Target CPU }
  if (CurTargetCPU<>'')
  and ((TargetCPU<>'') or (CurTargetCPU<>DefaultTargetCPU)) then
    Result.Add('-P' + CurTargetCPU);
  { Subtarget }
  if CurSubtarget<>'' then
    Result.Add('-t'+CurSubtarget);
  { TargetProcessor }
  if TargetProcessor<>'' then
    Result.Add('-Cp'+UpperCase(TargetProcessor));

  { --------------- Parsing Tab ------------------- }

  { Assembler reading style  -Ratt = AT&T    -Rintel = Intel  -Rdefault (or no option) = default }
  if IsCPUX86(CurTargetCPU) then
    case AssemblerStyle of
      1: Result.Add('-Rintel');
      2: Result.Add('-Ratt');
    end;

  // Syntax Options
  GetSyntaxOptions(Kind,Result);

  { ----------- Code Generation Tab --------------- }

  { UnitStyle   '' = Static     'D' = Dynamic (not implemented)   'X' = smart linked }
  if SmartLinkUnit then
    Result.Add('-CX');
  if RelocatableUnit and (CurSrcOS='win') then
    Result.Add('-WR');
  if (not (ccloNoMacroParams in Flags))
  and TargetNeedsFPCOptionCG(CurTargetOS,CurTargetCPU) then
    Result.Add('-Cg'); // see bug 17412

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
    Result.Add('-C' + tempsw);

  { Heap Size }
  if (HeapSize > 0) then
    Result.Add('-Ch' + IntToStr(HeapSize));

  { Stack Size }
  if (StackSize > 0) then
    Result.Add('-Cs' + IntToStr(StackSize));

  { Optimizations }
  OptimizeSwitches:='';
  if SmallerCode then
    OptimizeSwitches := OptimizeSwitches + 's';
  { OptimizationLevel     1 = Level 1    2 = Level 2    3 = Level 3 }
  if OptimizationLevel>0 then
    OptimizeSwitches := OptimizeSwitches + IntToStr(OptimizationLevel);
  if OptimizeSwitches<>'' then
    Result.Add('-O'+OptimizeSwitches);

  // uncertain
  if UncertainOptimizations then
    Result.Add('-OoUNCERTAIN');

  // registers
  if VariablesInRegisters then
    Result.Add('-OoREGVAR');

  { --------------- Linking Tab ------------------- }

  { Debugging }
  { Debug Info for GDB }
  if GenerateDebugInfo then begin

    dit := DebugInfoType;
    case dit of
      dsAuto:
        if Kind=pcFPC then begin
          if (not (ccloNoMacroParams in Flags)) and (CurTargetOS='darwin') then
            Result.Add('-gw')
          else
            Result.Add('-g');
        end;
      dsStabs:     Result.Add('-gs');
      dsDwarf2:    Result.Add('-gw2');
      dsDwarf2Set: begin Result.Add('-gw2'); Result.Add('-godwarfsets'); end;
      dsDwarf3:    Result.Add('-gw3');
    end;

    { Line Numbers in Run-time Error Backtraces - Use LineInfo Unit }
    if UseLineInfoUnit then
      Result.Add('-gl');

    { Use Heaptrc Unit }
    if UseHeaptrc and (not (ccloNoLinkerOpts in Flags)) then
      Result.Add('-gh');

    { Generate code for Valgrind }
    if UseValgrind and (not (ccloNoLinkerOpts in Flags)) then
      Result.Add('-gv');

    if UseExternalDbgSyms then
      Result.Add('-Xg');

  end
  else begin
    // no debug info wanted

    { Use Heaptrc Unit }
    if (UseHeaptrc) and (not (ccloNoLinkerOpts in Flags)) then
      Result.Add('-g-h'); // heaptrc, without -g
  end;

  { Trash variables }
  if TrashVariables then
    Result.Add('-gt');

  { Generate code gprof }
  if GenGProfCode then
    Result.Add('-pg');

  { Strip Symbols }
  if StripSymbols and (not (ccloNoLinkerOpts in Flags)) then
    Result.Add('-Xs');

  { Link Style
     -XD = Link with dynamic libraries
     -XS = Link with static libraries, default on non-win32 platforms
     -XX = Link smart
  }

  if (not (ccloNoLinkerOpts in Flags)) and LinkSmart then
    Result.Add('-XX');

  // additional Linker options
  if (not (ccloNoLinkerOpts in Flags))
  and (not (ccloNoMacroParams in Flags)) then
  begin
    if PassLinkerOptions then
    begin
      CurLinkerOptions:=ParsedOpts.GetParsedValue(pcosLinkerOptions);
      if (CurLinkerOptions<>'') then
        ConvertOptionsToCmdParams('-k', CurLinkerOptions, Result);
    end;

    // inherited Linker options
    InhLinkerOpts:=GetInheritedOption(icoLinkerOptions,
                                   not (ccloAbsolutePaths in Flags),coptParsed);
    //debugln(['TBaseCompilerOptions.MakeOptionsString InhLinkerOpts="',InhLinkerOpts,'"']);
    if InhLinkerOpts<>'' then
      ConvertOptionsToCmdParams('-k', InhLinkerOpts, Result);
  end;

  if Win32GraphicApp
  and ((CurSrcOS='win') or (CurTargetOS='macos') or (CurTargetOS='os2')) then
    Result.Add('-WG');

  { ---------------- Other Tab -------------------- }

  { Verbosity }
  if Quiet then
    Result.Add('-l-' )
  else if WriteFPCLogo then
    Result.Add('-l');

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
    Result.Add('-v' + tempsw);
  if (quietsw <> '') then
    Result.Add('-v' + quietsw);

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
      Result.Add('-vm'+t);
    t := IDEMessageFlags.GetMsgIdList(',',cfvShow,FFPCMsgFile);
    if t <> '' then
      Result.Add('-vm-'+t);
  end;

  if (StopAfterErrCount>1) then
    Result.Add('-Se'+IntToStr(StopAfterErrCount));

  { Ignore Config File }
  if DontUseConfigFile then
    Result.Add('-n');

  { Use Custom Config File     @ = yes and path }
  if not (ccloNoMacroParams in Flags)
  and (CustomConfigFile) and (ConfigFilePath<>'') then
    Result.Add('@' + ConfigFilePath);

  { ------------- Search Paths ---------------- }
  CurOutputDir:='';
  if not (ccloNoMacroParams in Flags) then
  begin
    BasePath:=AppendPathDelim(BaseDirectory);

    // include path
    CurIncludePath:=GetIncludePath(not (ccloAbsolutePaths in Flags),
                                   coptParsed,false);
    if (CurIncludePath <> '') then
      ConvertSearchPathToCmdParams('-Fi', CurIncludePath, BasePath, Result);

    // library path
    if (not (ccloNoLinkerOpts in Flags)) then begin
      CurLibraryPath:=GetLibraryPath(not (ccloAbsolutePaths in Flags),
                                     coptParsed,false);
      if (CurLibraryPath <> '') then
        ConvertSearchPathToCmdParams('-Fl', CurLibraryPath, BasePath, Result);
    end;

    // namespaces
    CurNamespaces:=GetNamespacesParsed(coptParsed);
    if CurNamespaces<>'' then
      Result.Add('-FN'+CurNamespaces);

    // object path
    CurObjectPath:=GetObjectPath(not (ccloAbsolutePaths in Flags),
                                 coptParsed,false);
    if (CurObjectPath <> '') then
      ConvertSearchPathToCmdParams('-Fo', CurObjectPath, BasePath, Result);

    // unit path
    CurUnitPath:=GetUnitPath(not (ccloAbsolutePaths in Flags));
    ConvertSearchPathToCmdParams('-Fu', CurUnitPath, BasePath, Result);

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
  //DebugLn(['TBaseCompilerOptions.MakeOptionsString ',DbgSName(Self),' ',dbgs(Flags),' TargetFilename="',TargetFilename,'" CurMainSrcFile="',CurMainSrcFile,'" CurOutputDir="',CurOutputDir,'"']);
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
    Result.Add('-FU' + CurOutputDir);
  end;
  if CurTargetDirectory <> '' then begin
    if not (ccloAbsolutePaths in Flags) then
      CurTargetDirectory:=CreateRelativePath(CurTargetDirectory,BaseDirectory,true);
    Result.Add('-FE' + CurTargetDirectory);
  end;
  if (CurTargetFilename<>'') then begin
    if not (ccloAbsolutePaths in Flags) then
      CurTargetFilename := CreateRelativePath(CurTargetFilename, BaseDirectory);
    if CurTargetFilename<>'' then
      Result.Add('-o' +
        FixExeExtForEmbeddedCompiler(CurTargetFilename));
  end;

  // append custom options as last, so they can override
  if not (ccloNoMacroParams in Flags) then
  begin
    CurCustomOptions:=GetCustomOptions(coptParsed);
    if CurCustomOptions<>'' then
      ConvertOptionsToCmdParams('', CurCustomOptions, Result);
  end;
end;

procedure TBaseCompilerOptions.GetSyntaxOptions(Kind: TPascalCompiler;
  Params: TStrings);
var
  tempsw: String;
begin
  if SyntaxMode<>'' then
    Params.Add('-M'+SyntaxMode);  // -M<x>  Set language mode to <x>

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
    if (TypedAddress) then
      tempsw := tempsw + 'y';
  end;

  if (tempsw <> '') then begin
    Params.Add('-S' + tempsw);
  end;
end;

function TBaseCompilerOptions.CreatePPUFilename(const SourceFileName: string): string;
var
  UnitOutDir: String;
begin
  Result:=SourceFileName;
  IDEMacros.SubstituteMacros(Result);
  if Result='' then exit;
  if FilenameIsAbsolute(Result) then begin
    // fully specified target filename
  end else if (UnitOutputDirectory='')
  and (OutputDirectoryOverride='')
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
  fUseAnsiStr := true;
  fCPPInline := true;
  fCMacros := false;
  fInitConst := false;
  fTypedAddress := false;

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
  fShowWarn := true;
  fShowNotes := true;
  fShowHints := true;
  fShowLineNum := false;
  fShowAll := false;
  fShowDebugInfo := false;
  fShowUsedFiles := false;
  fShowTriedFiles := false;
  fShowCompProc := false;
  fShowCond := false;
  fShowExecInfo := false;
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
  fRunWithoutDebug := False;

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
  fUseAnsiStr := CompOpts.fUseAnsiStr;
  fCPPInline := CompOpts.fCPPInline;
  fCMacros := CompOpts.fCMacros;
  fInitConst := CompOpts.fInitConst;
  fTypedAddress := CompOpts.fTypedAddress;

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
  FSubtarget := CompOpts.FSubtarget;
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
  fShowWarn := CompOpts.fShowWarn;
  fShowNotes := CompOpts.fShowNotes;
  fShowHints := CompOpts.fShowHints;
  fShowLineNum := CompOpts.fShowLineNum;
  fShowAll := CompOpts.fShowAll;
  fShowDebugInfo := CompOpts.fShowDebugInfo;
  fShowUsedFiles := CompOpts.fShowUsedFiles;
  fShowTriedFiles := CompOpts.fShowTriedFiles;
  fShowCompProc := CompOpts.fShowCompProc;
  fShowCond := CompOpts.fShowCond;
  fShowExecInfo := CompOpts.fShowExecInfo;
  fShowHintsForUnusedUnitsInMainSrc := CompOpts.fShowHintsForUnusedUnitsInMainSrc;
  fShowHintsForSenderNotUsed := CompOpts.fShowHintsForSenderNotUsed;
  fWriteFPCLogo := CompOpts.fWriteFPCLogo;

  // Messages
  fMessageFlags.Assign(CompOpts.fMessageFlags);

  // Other
  fDontUseConfigFile := CompOpts.fDontUseConfigFile;
  FWriteConfigFile := CompOpts.FWriteConfigFile;
  FWriteConfigFilePath := CompOpts.FWriteConfigFilePath;
  fCustomConfigFile := CompOpts.fCustomConfigFile;
  fConfigFilePath := CompOpts.fConfigFilePath;
  fStopAfterErrCount := CompOpts.fStopAfterErrCount;
  CustomOptions := CompOpts.CustomOptions;
  fRunWithoutDebug := CompOpts.fRunWithoutDebug;

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
  if Done(Tool.AddDiff('TargetFilenameApplyConventions',FTargetFilenameApplyConventions,CompOpts.FTargetFilenameApplyConventions)) then exit;

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
  if Done(Tool.AddDiff('UseAnsiStr',fUseAnsiStr,CompOpts.fUseAnsiStr)) then exit;
  if Done(Tool.AddDiff('CPPInline',fCPPInline,CompOpts.fCPPInline)) then exit;
  if Done(Tool.AddDiff('CMacros',fCMacros,CompOpts.fCMacros)) then exit;
  if Done(Tool.AddDiff('InitConst',fInitConst,CompOpts.fInitConst)) then exit;
  if Done(Tool.AddDiff('TypedAddress',fTypedAddress,CompOpts.fTypedAddress)) then exit;

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
  if Done(Tool.AddDiff('Subtarget',FSubtarget,CompOpts.FSubtarget)) then exit;
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
  if Done(Tool.AddDiff('ShowWarn',fShowWarn,CompOpts.fShowWarn)) then exit;
  if Done(Tool.AddDiff('ShowNotes',fShowNotes,CompOpts.fShowNotes)) then exit;
  if Done(Tool.AddDiff('ShowHints',fShowHints,CompOpts.fShowHints)) then exit;
  if Done(Tool.AddDiff('ShowLineNum',fShowLineNum,CompOpts.fShowLineNum)) then exit;
  if Done(Tool.AddDiff('ShowAll',fShowAll,CompOpts.fShowAll)) then exit;
  if Done(Tool.AddDiff('ShowDebugInfo',fShowDebugInfo,CompOpts.fShowDebugInfo)) then exit;
  if Done(Tool.AddDiff('ShowUsedFiles',fShowUsedFiles,CompOpts.fShowUsedFiles)) then exit;
  if Done(Tool.AddDiff('ShowTriedFiles',fShowTriedFiles,CompOpts.fShowTriedFiles)) then exit;
  if Done(Tool.AddDiff('ShowCompProc',fShowCompProc,CompOpts.fShowCompProc)) then exit;
  if Done(Tool.AddDiff('ShowCond',fShowCond,CompOpts.fShowCond)) then exit;
  if Done(Tool.AddDiff('ShowExecInfo',fShowExecInfo,CompOpts.fShowExecInfo)) then exit;
  if Done(Tool.AddDiff('ShowHintsForUnusedUnitsInMainSrc',fShowHintsForUnusedUnitsInMainSrc,CompOpts.fShowHintsForUnusedUnitsInMainSrc)) then exit;
  if Done(Tool.AddDiff('ShowHintsForSenderNotUsed',fShowHintsForSenderNotUsed,CompOpts.fShowHintsForSenderNotUsed)) then exit;
  if Done(Tool.AddDiff('WriteFPCLogo',fWriteFPCLogo,CompOpts.fWriteFPCLogo)) then exit;

  // messages
  if Tool<>nil then Tool.Path:='Messages';
  if Done(IDEMessageFlags.CreateDiff(Tool,CompOpts.IDEMessageFlags)) then exit;

  // other
  if Tool<>nil then Tool.Path:='Other';
  if Done(Tool.AddDiff('DontUseConfigFile',fDontUseConfigFile,CompOpts.fDontUseConfigFile)) then exit;
  if Done(Tool.AddDiff('WriteConfigFile',FWriteConfigFile,CompOpts.FWriteConfigFile)) then exit;
  if Done(Tool.AddDiff('WriteConfigFilePath',FWriteConfigFilePath,CompOpts.FWriteConfigFilePath)) then exit;
  if Done(Tool.AddDiff('CustomConfigFile',fCustomConfigFile,CompOpts.fCustomConfigFile)) then exit;
  if Done(Tool.AddDiff('ConfigFilePath',fConfigFilePath,CompOpts.fConfigFilePath)) then exit;
  if Done(Tool.AddDiff('StopAfterErrCount',fStopAfterErrCount,CompOpts.fStopAfterErrCount)) then exit;
  if Done(Tool.AddDiff('CustomOptions',CustomOptions,CompOpts.CustomOptions)) then exit;
  if Done(Tool.AddDiff('OtherDefines',OtherDefines.Text,CompOpts.OtherDefines.Text)) then exit;
  if Done(Tool.AddDiff('RunWithoutDebug',fRunWithoutDebug,CompOpts.fRunWithoutDebug)) then exit;

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


{ TCompilationToolOptions }

procedure TCompilationToolOptions.DoClearErrorLines;
begin
  ; // Do nothing.
end;

procedure TCompilationToolOptions.SetCommand(AValue: string);
begin
  inherited SetCommand(AValue);
  FParsedCommandStamp:=CTInvalidChangeStamp;
end;

procedure TCompilationToolOptions.SubstituteMacros(var s: string);
begin
  IDEMacros.SubstituteMacros(s);
end;

procedure TCompilationToolOptions.Assign(Src: TLazCompilationToolOptions);
begin
  inherited Assign(Src);
  if Src is TCompilationToolOptions then
    Parsers.Assign(TCompilationToolOptions(Src).Parsers);
end;

procedure TCompilationToolOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
var
  Params: TStrings;
  param, cmd: String;
  p, p2, i, j: Integer;
begin
  //debugln(['TCompilationToolOptions.LoadFromXMLConfig ',Command,' Path=',Path,' DoSwitchPathDelims=',DoSwitchPathDelims]);
  Command:=XMLConfig.GetValue(Path+'Command/Value','');
  if DoSwitchPathDelims then begin
    if (Command<>'')
    and (PathDelim='\') then {%H-}begin
      // specialhandling on windows to not switch path delimiters in options
      Params:=TStringList.Create;
      try
        SplitCmdLineParams(Command,Params);
        cmd:=SwitchPathDelims(Params[0],True);
        for i:=1 to Params.Count-1 do begin
          param:=Params[i];
          p:=-1;
          p2:=-1;
          for j:=1 to length(param) do
            if p>1 then
              break
            else if param[j]='/' then
              p:=j
            else if param[j]=':' then
              p2:=j;
          if p=1 then
            // param is option (the only / is at pos 1)
            if p2<>-1 then
              // potential filename after colon in option
              cmd+=' '+copy(param,1,p2)+SwitchPathDelims(Copy(param,p2+1,length(param)-p2),True)
            else
              cmd+=' '+param
          else
            cmd+=' '+SwitchPathDelims(param,True);
        end;
        Command:=cmd;
      finally
        Params.Free;
      end;
    end else begin
      Command:=SwitchPathDelims(Command,DoSwitchPathDelims);
    end;
  end;
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
    XMLConfig.SetDeleteValue(Path+'ScanForFPCMsgs/Value', HasParser[SubToolFPC],false);
    XMLConfig.SetDeleteValue(Path+'ScanForMakeMsgs/Value',HasParser[SubToolMake],false);
    XMLConfig.SetDeleteValue(Path+'ShowAllMessages/Value',HasParser[SubToolDefault],false);
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
  DoClearErrorLines;
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
  FDescription:=LineBreaksToSystemLineBreaks(AXMLConfig.GetValue(Path+'Description/Value',''));
  LoadStringList(AXMLConfig,FValues,Path+'Values/');
  LoadStringList(AXMLConfig,FValueDescriptions,Path+'ValueDescriptions/');
  FDefaultValue:=LineBreaksToSystemLineBreaks(AXMLConfig.GetValue(Path+'Default/Value',''));

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
  BuildMacroChangeStamp:=1;

end.

