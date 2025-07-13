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
}
unit ParsedCompilerOpts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // CodeTools
  FileProcs, DefineTemplates, CodeToolsCfgScript,
  KeywordFuncLists, BasicCodeTools, LinkScanner, DirectoryCacher,
  // LazUtils
  FPCAdds, LazVersion, LazUTF8, LazStringUtils, LazFileUtils, FileUtil,
  LazUtilities, LazTracer, Laz2_XMLCfg,
  // BuildIntf
  CompOptsIntf, MacroIntf,
  // IdeConfig
  EnvironmentOpts, TransferMacros, SearchPathProcs, ModeMatrixOpts, IdeConfStrConsts;

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
    pcosMsgFile,       // fpc message file (errore.msg)
    pcosCustomConfigFilePath, // additional custom config file
    pcosWriteConfigFilePath // auto generated cfg file
    );
  TParsedCompilerOptStrings = set of TParsedCompilerOptString;

const
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

  ParsedCompilerSearchPaths = [pcosUnitPath,pcosIncludePath,pcosObjectPath,
                               pcosLibraryPath,pcosSrcPath,pcosDebugPath];
  ParsedCompilerExecutables = [pcosCompilerPath];
  ParsedCompilerFilenames = ParsedCompilerExecutables+[pcosMsgFile,
                              pcosCustomConfigFilePath,pcosWriteConfigFilePath];
  ParsedCompilerDirectories = [pcosOutputDir];
  ParsedCompilerOutDirectories = [pcosOutputDir];
  ParsedCompilerFiles =
    ParsedCompilerSearchPaths+ParsedCompilerFilenames+ParsedCompilerDirectories;

  ParsedCompilerOptsVars: array[TParsedCompilerOptString] of string = (
    '', // pcosNone
    '', // pcosBaseDir
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
    'MsgFile',
    'CustomConfigFile',
    'WriteCfgFile'
    );
  ParsedCompilerOptsUsageVars: array[TParsedCompilerOptString] of string = (
    '', // pcosNone
    '', // pcosBaseDir
    'UsageUnitPath',
    'UsageNamespaces',
    'UsageIncPath',
    'UsageObjectPath',
    'UsageLibraryPath',
    'UsageSrcPath',
    'UsageLinkerOptions',
    'UsageCustomOptions',
    '', // pcosOutputDir
    '', // pcosCompilerPath
    'UsageDebugPath', // pcosDebugPath
    '', // pcosMsgFile
    '',
    ''  // pcosWriteConfigFilePath
    );

type
  TLocalSubstitutionEvent = function(s: string;
                                PlatformIndependent: boolean): string of object;
  TPCOGetOverride = function: string of object;

  { TParsedCompilerOptions }

  TParsedCompilerOptions = class
  private
    FInvalidateParseOnChange: boolean;
    FOnGetOutputDirectoryOverride: TPCOGetOverride;
    FOnLocalSubstitute: TLocalSubstitutionEvent;
    FOwner: TObject;
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
    function DoParseOption(OptionText: string;
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
    property OnGetOutputDirectoryOverride: TPCOGetOverride read FOnGetOutputDirectoryOverride
                                                           write FOnGetOutputDirectoryOverride;
  end;

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
    function GetLazCompilerOptions: TLazCompilerOptions; virtual;
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


  TGetBuildMacroValues = function(Options: TLazCompilerOptions;
             IncludeSelf: boolean): TCTCfgScriptVariables of object;
  TParseStringEvent = function(Options: TParsedCompilerOptions;
    const UnparsedValue: string; PlatformIndependent: boolean): string of object;
  TOnAppendCustomOptions = procedure(Sender: TObject;
    var CustomOptions: string; Types: TBuildMatrixGroupTypes) of object;
  TOnGetMatrixOutputDirectoryOverride = procedure(Sender: TObject;
             var OutDir: string; Types: TBuildMatrixGroupTypes) of object;

var
  OnAppendCustomOption: TOnAppendCustomOptions = nil; // set by MainBuildBoss
  OnGetMatrixOutputDirectoryOverride: TOnGetMatrixOutputDirectoryOverride = nil; // set by MainBuildBoss
  OnParseString: TParseStringEvent = nil;
  GetBuildMacroValues: TGetBuildMacroValues = nil; // set by TPkgManager, do not change or free the variables

function EnumToStr(opt: TParsedCompilerOptString): string; overload;
function MergeCustomOptions(const OldOptions, AddOptions: string): string;
function MergeLinkerOptions(const OldOptions, AddOptions: string): string;
function ParseString(Options: TParsedCompilerOptions;
              const UnparsedValue: string; PlatformIndependent: boolean): string;


implementation

function EnumToStr(opt: TParsedCompilerOptString): string;
begin
  Result:='';
  WriteStr(Result, opt);
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

function MergeLinkerOptions(const OldOptions, AddOptions: string): string;
begin
  Result:=MergeCustomOptions(OldOptions,AddOptions);
end;

function ParseString(Options: TParsedCompilerOptions;
  const UnparsedValue: string; PlatformIndependent: boolean): string;
begin
  Result:=OnParseString(Options,UnparsedValue,PlatformIndependent);
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
        SetCTCSVariableAsString(Value,FPCAdds.GetCompiledTargetOS)
      else if CompareIdentifiers(PChar(VarName),'CPU')=0 then
        SetCTCSVariableAsString(Value,FPCAdds.GetCompiledTargetCPU)
      else if CompareIdentifiers(PChar(VarName),'SrcOS')=0 then
        SetCTCSVariableAsString(Value,GetDefaultSrcOSForTargetOS(FPCAdds.GetCompiledTargetOS))
      else if CompareIdentifiers(PChar(VarName),'SrcOS2')=0 then
        SetCTCSVariableAsString(Value,GetDefaultSrcOS2ForTargetOS(FPCAdds.GetCompiledTargetOS))
      else if CompareIdentifiers(PChar(VarName),'LCLWidgetType')=0 then
        SetCTCSVariableAsString(Value,GetLCLWidgetTypeName)
      else if CompareIdentifiers(PChar(VarName),'LAZ_FULLVERSION')=0 then
        SetCTCSVariableAsNumber(Value,laz_fullversion)
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


{ TParsedCompilerOptions }

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
  Opts: TLazCompilerOptions;
  VarName: String;
  Vars: TCTCfgScriptVariables;
  MoreOptions: String;
begin
  Result:=Values[Option].UnparsedValue;
  Opts:=nil;
  VarName:='';
  if (Owner is TLazCompilerOptions) then
  begin
    Opts:=TLazCompilerOptions(Owner);
    VarName:=ParsedCompilerOptsVars[Option];
  end else if (Owner is TAdditionalCompilerOptions) then
  begin
    Opts:=TAdditionalCompilerOptions(Owner).GetLazCompilerOptions;
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
      if (Owner is TLazCompilerOptions) and Assigned(OnAppendCustomOption) then
      begin
        MoreOptions:='';
        OnAppendCustomOption(Opts,MoreOptions,bmgtAll);
        if Assigned(OnLocalSubstitute) then
          MoreOptions:=OnLocalSubstitute(MoreOptions,false);
        MoreOptions:=SpecialCharsToSpaces(MoreOptions,true);
        Result:=MergeCustomOptions(Result,MoreOptions);
      end;
    end;
  pcosOutputDir,pcosCompilerPath,pcosWriteConfigFilePath:
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
    if (Option=pcosOutputDir) and Assigned(OnGetOutputDirectoryOverride) then
    begin
      s:=OnGetOutputDirectoryOverride();
      if s<>'' then
        exit(s);
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

function TParsedCompilerOptions.DoParseOption(OptionText: string;
  Option: TParsedCompilerOptString; PlatformIndependent: boolean): string;
// Don't use "const" for OptionText parameter.

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
      if (BaseDirectory<>'') then
        aFilename:=TrimFilename(BaseDirectory+aFilename);
    end;
  end;

var
  BaseDirectory, h: String;
begin
  Result:=OptionText;

  // apply overrides
  if not PlatformIndependent then begin
    if Option=pcosOutputDir then begin
      if Assigned(OnGetMatrixOutputDirectoryOverride) then
        OnGetMatrixOutputDirectoryOverride(Self,Result,bmgtAll);
    end;
  end;

  // parse locally (macros depending on owner, like pkgdir and build macros)
  if Assigned(OnLocalSubstitute) then
  begin
    //DebugLn(['TParsedCompilerOptions.DoParseOption local "',Result,'" ...']);
    Result:=OnLocalSubstitute(Result,PlatformIndependent)
  end else
  begin
    //DebugLn(['TParsedCompilerOptions.DoParseOption global "',Result,'" ...']);
    Result:=ParseString(Self,Result,PlatformIndependent);
  end;
  //DebugLn(['TParsedCompilerOptions.DoParseOption complete "',Result,'" ...']);
  // improve
  if Option=pcosBaseDir then
    // base directory
    Result:=AppendPathDelim(TrimFilename(Result))
  else if Option in ParsedCompilerFilenames then
  begin
    // make filename absolute
    //debugln(['TParsedCompilerOptions.DoParseOption ',ParsedCompilerOptsVars[Option],' Result="',Result,'"']);
    if (Option in ParsedCompilerExecutables) and (ExtractFilePath(Result)='') then
    begin
      h:=FileUtil.FindDefaultExecutablePath(Result,GetBaseDir);
      if h<>'' then
        Result:=h;
    end;
    MakeFilenameAbsolute(Result);
  end
  else if Option in ParsedCompilerDirectories then
  begin
    // make directory absolute
    Result:=TrimFilename(Result);
    if Option<>pcosBaseDir then
      MakeFilenameAbsolute(Result);
    Result:=AppendPathDelim(Result);
  end
  else if Option in ParsedCompilerSearchPaths then
  begin
    // make search paths absolute
    BaseDirectory:=GetBaseDir;
    Result:=TrimSearchPath(Result,BaseDirectory);
  end else if Option=pcosCustomOptions then begin
    Result:=SpecialCharsToSpaces(Result,true);
  end;
end;

procedure TParsedCompilerOptions.Assign(Src: TParsedCompilerOptions);
begin
  FInvalidateParseOnChange := Src.FInvalidateParseOnChange;
//  FOnLocalSubstitute := Src.FOnLocalSubstitute;
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
    RaiseGDBException(''){%H-}; // inconsistency detected
  end;
end;

function TAdditionalCompilerOptions.GetLazCompilerOptions: TLazCompilerOptions;
begin
  Result:=nil;
end;


end.

