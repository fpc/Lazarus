{ /***************************************************************************
                      compileroptions.pp  -  Lazarus IDE unit
                      ---------------------------------------
                   Compiler options form sets the switches for the project
                   file for the PPC386 compiler.


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
  Forms, Classes, SysUtils, ComCtrls, Buttons, StdCtrls, ExtCtrls, Graphics,
  LResources, Laz_XMLCfg, FileCtrl, Dialogs, Controls,
  PathEditorDlg, IDEProcs, LazConf, IDEOptionDefs, LazarusIDEStrConsts,
  TransferMacros;

type
  TInheritedCompilerOption = (
    icoUnitPath,
    icoIncludePath,
    icoObjectPath,
    icoLibraryPath,
    icoLinkerOptions,
    icoCustomOptions
    );
  TInheritedCompilerOptions = set of TInheritedCompilerOption;
  
const
  icoAllSearchPaths = [icoUnitPath,icoIncludePath,icoObjectPath,icoLibraryPath];
  

type
  { TParsedCompilerOptions }
  
  TParsedCompilerOptString = (
    pcosBaseDir,      // the base directory for the relative paths
    pcosUnitPath,     // search path for pascal units
    pcosIncludePath,  // search path for pascal include files
    pcosObjectPath,   // search path for .o files
    pcosLibraryPath,  // search path for libraries
    pcosLinkerOptions,// additional linker options
    pcosCustomOptions,// additional options
    pcosOutputDir,    // the output directory
    pcosCompilerPath  // the filename of the compiler
    );
  TParsedCompilerOptStrings = set of TParsedCompilerOptString;
  
const
  ParsedCompilerSearchPaths = [pcosUnitPath,pcosIncludePath,
                               pcosObjectPath,pcosLibraryPath];
  ParsedCompilerFilenames = [pcosCompilerPath];
  ParsedCompilerDirectories = [pcosOutputDir];
  ParsedCompilerFiles =
    ParsedCompilerSearchPaths+ParsedCompilerFilenames+ParsedCompilerDirectories;

type
  TLocalSubstitutionEvent = function(const s: string): string of object;

  TParsedCompilerOptions = class
  private
    FInvalidateGraphOnChange: boolean;
    FOnLocalSubstitute: TLocalSubstitutionEvent;
  public
    UnparsedValues: array[TParsedCompilerOptString] of string;
    ParsedValues: array[TParsedCompilerOptString] of string;
    ParsedStamp: array[TParsedCompilerOptString] of integer;
    constructor Create;
    function GetParsedValue(Option: TParsedCompilerOptString): string;
    procedure SetUnparsedValue(Option: TParsedCompilerOptString;
                               const NewValue: string);
    procedure Clear;
    procedure InvalidateAll;
    procedure InvalidateFiles;
  public
    property OnLocalSubstitute: TLocalSubstitutionEvent read FOnLocalSubstitute
                                                       write FOnLocalSubstitute;
    property InvalidateGraphOnChange: boolean read FInvalidateGraphOnChange
                                              write FInvalidateGraphOnChange;
  end;

  TParseStringEvent =
    function(Options: TParsedCompilerOptions;
             const UnparsedValue: string): string of object;


  { TBaseCompilerOptions }

  TBaseCompilerOptions = class
  private
    FBaseDirectory: string;
    fInheritedOptions: array[TInheritedCompilerOption] of string;
    fInheritedOptParseStamps: integer;
    fInheritedOptGraphStamps: integer;
    fLoaded: Boolean;
    FModified: boolean;
    FOnModified: TNotifyEvent;
    fOptionsString: String;
    fOwner: TObject;
    FParsedOpts: TParsedCompilerOptions;
    fTargetFilename: string;
    fXMLFile: String;
    xmlconfig: TXMLConfig;

    // Search Paths:
    fIncludeFiles: String;
    fLibraries: String;
    fOtherUnitFiles: String;
    fCompilerPath: String;
    fUnitOutputDir: string;
    fLCLWidgetType: string;
    FObjectPath: string;

    // Parsing:
    // style
    fStyle: Integer;
    // symantec checking
    fD2Ext: Boolean;
    fCStyleOp: Boolean;
    fIncludeAssertionCode: Boolean;
    fDelphiCompat: Boolean;
    fAllowLabel: Boolean;
    fUseAnsiStr: Boolean;
    fCPPInline: Boolean;
    fCMacros: Boolean;
    fTPCompat: Boolean;
    fGPCCompat: Boolean;
    fInitConst: Boolean;
    fStaticKwd: Boolean;    

    // Code generation:
    fUnitStyle: Integer;
    fIOChecks: Boolean;
    fRangeChecks: Boolean;
    fOverflowChecks: Boolean;
    fStackChecks: Boolean;
    FEmulatedFloatOpcodes: boolean;
    fHeapSize: LongInt;
    fVerifyObjMethodCall: boolean;
    fGenerate: Integer;
    fTargetProc: Integer;
    fVarsInReg: Boolean;
    fUncertainOpt: Boolean;
    fOptLevel: Integer;
    fTargetOS: String;
    
    // Linking:
    fGenDebugInfo: Boolean;
    fGenDebugDBX: Boolean;
    fUseLineInfoUnit: Boolean;
    fUseHeaptrc: Boolean;
    fGenGProfCode: Boolean;
    fStripSymbols: Boolean;
    fLinkStyle: Integer;
    fPassLinkerOpt: Boolean;
    fLinkerOptions: String;
    
    // Messages:
    fShowErrors: Boolean;
    fShowWarn: Boolean;
    fShowNotes: Boolean;
    fShowHints: Boolean;
    fShowGenInfo: Boolean;
    fShowLineNum: Boolean;
    fShowAll: Boolean;
    fShowAllProcsOnError: Boolean;
    fShowDebugInfo: Boolean;
    fShowUsedFiles: Boolean;
    fShowTriedFiles: Boolean;
    fShowDefMacros: Boolean;
    fShowCompProc: Boolean;
    fShowCond: Boolean;
    fShowNothing: Boolean;
    fShowHintsForUnusedProjectUnits: Boolean;
    fWriteFPCLogo: Boolean;
    fStopAfterErrCount: integer;

    // Other:
    fDontUseConfigFile: Boolean;
    fAdditionalConfigFile: Boolean;
    fConfigFilePath: String;
    fCustomOptions: string;
  protected
    procedure SetBaseDirectory(const AValue: string); virtual;
    procedure SetCompilerPath(const AValue: String); virtual;
    procedure SetCustomOptions(const AValue: string); virtual;
    procedure SetIncludeFiles(const AValue: String); virtual;
    procedure SetLibraries(const AValue: String); virtual;
    procedure SetLinkerOptions(const AValue: String); virtual;
    procedure SetOtherUnitFiles(const AValue: String); virtual;
    procedure SetUnitOutputDir(const AValue: string); virtual;
    procedure SetObjectPath(const AValue: string); virtual;
  protected
    procedure LoadTheCompilerOptions(const Path: string); virtual;
    procedure SaveTheCompilerOptions(const Path: string); virtual;
    procedure SetModified(const AValue: boolean); virtual;
    procedure ClearInheritedOptions;
  public
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
    
    procedure LoadCompilerOptions(UseExistingFile: Boolean);
    procedure SaveCompilerOptions(UseExistingFile: Boolean);
    procedure Assign(CompOpts: TBaseCompilerOptions); virtual;
    function IsEqual(CompOpts: TBaseCompilerOptions): boolean; virtual;
    
    function MakeOptionsString: String;
    function MakeOptionsString(const MainSourceFileName: string): String; virtual;
    function CustomOptionsAsString: string;
    function ConvertSearchPathToCmdLine(const switch, paths: String): String;
    function ConvertOptionsToCmdLine(const Delim, Switch, OptionStr: string): string;
    function GetXMLConfigPath: String; virtual;
    function CreateTargetFilename(const MainSourceFileName: string): string; virtual;
    procedure GetInheritedCompilerOptions(var OptionsList: TList); virtual;
    function GetOwnerName: string; virtual;
    function GetInheritedOption(Option: TInheritedCompilerOption;
                                RelativeToBaseDir: boolean): string; virtual;
    function MergeLinkerOptions(const OldOptions, AddOptions: string): string;
    function MergeCustomOptions(const OldOptions, AddOptions: string): string;
    function GetDefaultMainSourceFileName: string; virtual;
  public
    { Properties }
    property Owner: TObject read fOwner write fOwner;
    property Modified: boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property TargetFilename: String read fTargetFilename write fTargetFilename;

    property XMLFile: String read fXMLFile write fXMLFile;
    property XMLConfigFile: TXMLConfig read xmlconfig write xmlconfig;
    property Loaded: Boolean read fLoaded write fLoaded;

    // search paths:
    property IncludeFiles: String read fIncludeFiles write SetIncludeFiles;
    property Libraries: String read fLibraries write SetLibraries;
    property OtherUnitFiles: String read fOtherUnitFiles write SetOtherUnitFiles;
    property CompilerPath: String read fCompilerPath write SetCompilerPath;
    property UnitOutputDirectory: string read fUnitOutputDir write SetUnitOutputDir;
    property LCLWidgetType: string read fLCLWidgetType write fLCLWidgetType;
    property ObjectPath: string read FObjectPath write SetObjectPath;

    // parsing:
    property Style: Integer read fStyle write fStyle;
    property D2Extensions: Boolean read fD2Ext write fD2Ext;
    property CStyleOperators: Boolean read fCStyleOp write fCStyleOp;
    property IncludeAssertionCode: Boolean 
      read fIncludeAssertionCode write fIncludeAssertionCode;
    property DelphiCompat: Boolean read fDelphiCompat write fDelphiCompat;
    property AllowLabel: Boolean read fAllowLabel write fAllowLabel;
    property UseAnsiStrings: Boolean read fUseAnsiStr write fUseAnsiStr;
    property CPPInline: Boolean read fCPPInline write fCPPInline;
    property CStyleMacros: Boolean read fCMacros write fCMacros;
    property TPCompatible: Boolean read fTPCompat write fTPCompat;
    property GPCCompat: Boolean read fGPCCompat write fGPCCompat;
    property InitConstructor: Boolean read fInitConst write fInitConst;
    property StaticKeyword: Boolean read fStaticKwd write fStaticKwd;

    // code generation:
    property UnitStyle: Integer read fUnitStyle write fUnitStyle;
    property IOChecks: Boolean read fIOChecks write fIOChecks;
    property RangeChecks: Boolean read fRangeChecks write fRangeChecks;
    property OverflowChecks: Boolean read fOverflowChecks write fOverflowChecks;
    property StackChecks: Boolean read fStackChecks write fStackChecks;
    property EmulatedFloatOpcodes: boolean read FEmulatedFloatOpcodes write FEmulatedFloatOpcodes;
    property HeapSize: Integer read fHeapSize write fHeapSize;
    property VerifyObjMethodCall: boolean read FEmulatedFloatOpcodes write FEmulatedFloatOpcodes;
    property Generate: Integer read fGenerate write fGenerate;
    property TargetProcessor: Integer read fTargetProc write fTargetProc;
    property VariablesInRegisters: Boolean read fVarsInReg write fVarsInReg;
    property UncertainOptimizations: Boolean read fUncertainOpt write fUncertainOpt;
    property OptimizationLevel: Integer read fOptLevel write fOptLevel;
    property TargetOS: string read fTargetOS write fTargetOS;
    
    // linking:
    property GenerateDebugInfo: Boolean read fGenDebugInfo write fGenDebugInfo;
    property GenerateDebugDBX: Boolean read fGenDebugDBX write fGenDebugDBX;
    property UseLineInfoUnit: Boolean read fUseLineInfoUnit write fUseLineInfoUnit;
    property UseHeaptrc: Boolean read fUseHeaptrc write fUseHeaptrc;
    property GenGProfCode: Boolean read fGenGProfCode write fGenGProfCode;
    property StripSymbols: Boolean read fStripSymbols write fStripSymbols;
    property LinkStyle: Integer read fLinkStyle write fLinkStyle;
    property PassLinkerOptions: Boolean read fPassLinkerOpt write fPassLinkerOpt;
    property LinkerOptions: String read fLinkerOptions write SetLinkerOptions;
    
    // messages:
    property ShowErrors: Boolean read fShowErrors write fShowErrors;
    property ShowWarn: Boolean read fShowWarn write fShowWarn;
    property ShowNotes: Boolean read fShowNotes write fShowNotes;
    property ShowHints: Boolean read fShowHints write fShowHints;
    property ShowGenInfo: Boolean read fShowGenInfo write fShowGenInfo;
    property ShowLineNum: Boolean read fShowLineNum write fShowLineNum;
    property ShowAll: Boolean read fShowAll write fShowAll;
    property ShowAllProcsOnError: Boolean
      read fShowAllProcsOnError write fShowAllProcsOnError;
    property ShowDebugInfo: Boolean read fShowDebugInfo write fShowDebugInfo;
    property ShowUsedFiles: Boolean read fShowUsedFiles write fShowUsedFiles;
    property ShowTriedFiles: Boolean read fShowTriedFiles write fShowTriedFiles;
    property ShowDefMacros: Boolean read fShowDefMacros write fShowDefMacros;
    property ShowCompProc: Boolean read fShowCompProc write fShowCompProc;
    property ShowCond: Boolean read fShowCond write fShowCond;
    property ShowNothing: Boolean read fShowNothing write fShowNothing;
    property ShowHintsForUnusedProjectUnits: Boolean 
      read fShowHintsForUnusedProjectUnits write fShowHintsForUnusedProjectUnits;
    property WriteFPCLogo: Boolean read fWriteFPCLogo write fWriteFPCLogo;
    property StopAfterErrCount: integer
      read fStopAfterErrCount write fStopAfterErrCount;

    // other
    property DontUseConfigFile: Boolean read fDontUseConfigFile write fDontUseConfigFile;
    property AdditionalConfigFile: Boolean read fAdditionalConfigFile write fAdditionalConfigFile;
    property ConfigFilePath: String read fConfigFilePath write fConfigFilePath;
    property CustomOptions: string read fCustomOptions write SetCustomOptions;
  end;
  
  
  { TAdditionalCompilerOptions
  
    Additional Compiler options are used by packages to define, what a project
    or a package or the IDE needs to use the package.
  }
  
  TAdditionalCompilerOptions = class
  private
    FBaseDirectory: string;
    FCustomOptions: string;
    FIncludePath: string;
    FLibraryPath: string;
    FLinkerOptions: string;
    FObjectPath: string;
    fOwner: TObject;
    FParsedOpts: TParsedCompilerOptions;
    FUnitPath: string;
  protected
    procedure SetBaseDirectory(const AValue: string); virtual;
    procedure SetCustomOptions(const AValue: string); virtual;
    procedure SetIncludePath(const AValue: string); virtual;
    procedure SetLibraryPath(const AValue: string); virtual;
    procedure SetLinkerOptions(const AValue: string); virtual;
    procedure SetObjectPath(const AValue: string); virtual;
    procedure SetUnitPath(const AValue: string); virtual;
  public
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function GetOwnerName: string; virtual;
  public
    property Owner: TObject read fOwner;
    property UnitPath: string read FUnitPath write SetUnitPath;
    property IncludePath: string read FIncludePath write SetIncludePath;
    property ObjectPath: string read FObjectPath write SetObjectPath;
    property LibraryPath: string read FLibraryPath write SetLibraryPath;
    property LinkerOptions: string read FLinkerOptions write SetLinkerOptions;
    property CustomOptions: string read FCustomOptions write SetCustomOptions;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property ParsedOpts: TParsedCompilerOptions read FParsedOpts;
  end;


  { TCompilerOptions }

  TCompilerOptions = class(TBaseCompilerOptions)
  public
    procedure Clear; override;
  end;
  

  { Compiler options form }
  
  TfrmCompilerOptions = class(TForm)
    nbMain: TNotebook;
    ImageList: TImageList;

    { Search Paths Controls }
    PathPage: TPage;
    grpOtherUnits: TGroupBox;
    edtOtherUnits: TEdit;
    OtherUnitsPathEditBtn: TPathEditorButton;

    grpIncludeFiles: TGroupBox;
    edtIncludeFiles: TEdit;
    IncludeFilesPathEditBtn: TPathEditorButton;

    grpOtherSources: TGroupBox;
    edtOtherSources: TEdit;
    OtherSourcesPathEditBtn: TPathEditorButton;

    grpLibraries: TGroupBox;
    edtLibraries: TEdit;
    LibrariesPathEditBtn: TPathEditorButton;

    grpCompiler: TGroupBox;
    edtCompiler: TEdit;

    grpUnitOutputDir: TGroupBox;
    edtUnitOutputDir: TEdit;

    LCLWidgetTypeRadioGroup: TRadioGroup;

    { Parsing Controls }
    ParsingPage: TPage;
    grpStyle: TGroupBox;
    radStyleIntel: TRadioButton;
    radStyleATT: TRadioButton;
    radStyleAsIs: TRadioButton;

    grpSymantecChk: TGroupBox;
    chkSymD2Ext: TCheckBox;
    chkSymCOper: TCheckBox;
    chkSymIncludeAssertions: TCheckBox;
    chkSymAllowLab: TCheckBox;
    chkSymUseAnsiStrings: TCheckBox;
    chkSymCPPInline: TCheckBox;
    chkSymCMacros: TCheckBox;
    chkSymDelphiCompat: TCheckBox;
    chkSymTP7Compat: TCheckBox;
    chkSymGPCCompat: TCheckBox;
    chkSymConstInit: TCheckBox;
    chkSymStaticKwd: TCheckBox;

    { Code Generation Controls }
    CodeGenPage: TPage;
    grpUnitStyle: TRadioGroup;

    grpChecks: TGroupBox;
    chkChecksIO: TCheckBox;
    chkChecksRange: TCheckBox;
    chkChecksOverflow: TCheckBox;
    chkChecksStack: TCheckBox;

    grpHeapSize: TGroupBox;
    edtHeapSize: TEdit;

    grpGenerate: TGroupBox;
    radGenFaster: TRadioButton;
    radGenSmaller: TRadioButton;

    grpTargetProc: TGroupBox;
    radTarget386: TRadioButton;
    radTargetPent: TRadioButton;
    radTargetPentPro: TRadioButton;

    grpOptimizations: TGroupBox;
    chkOptVarsInReg: TCheckBox;
    chkOptUncertain: TCheckBox;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;

    TargetOSRadioGroup: TRadioGroup;

    { Linking Controls }
    LinkingPage: TPage;
    grpDebugging: TGroupBox;
    chkDebugGDB: TCheckBox;
    chkDebugDBX: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkSymbolsStrip: TCheckBox;

    grpLinkLibraries: TGroupBox;
    radLibsLinkDynamic: TRadioButton;
    radLibsLinkStatic: TRadioButton;
    radLibsLinkSmart: TRadioButton;

    grpOptions: TGroupBox;
    chkOptionsLinkOpt: TCheckBox;
    edtOptionsLinkOpt: TEdit;

    { Messages Controls }
    MsgPage: TPage;
    grpVerbosity: TGroupBox;
    chkErrors: TCheckBox;
    chkWarnings: TCheckBox;
    chkNotes: TCheckBox;
    chkHints: TCheckBox;
    chkGeneralInfo: TCheckBox;
    chkLineNumbers: TCheckBox;
    chkEverything: TCheckBox;
    chkAllProcsOnError: TCheckBox;
    chkDebugInfo: TCheckBox;
    chkUsedFiles: TCheckBox;
    chkTriedFiles: TCheckBox;
    chkDefinedMacros: TCheckBox;
    chkCompiledProc: TCheckBox;
    chkConditionals: TCheckBox;
    chkNothing: TCheckBox;
    chkHintsForUnusedProjectUnits: TCheckBox;
    chkFPCLogo: TCheckBox;

    grpErrorCnt: TGroupBox;
    edtErrorCnt: TEdit;

    { 'Other' Controls }
    OtherPage: TPage;
    grpConfigFile: TGroupBox;
    chkConfigFile: TCheckBox;
    chkAdditionalConfigFile: TCheckBox;
    edtConfigPath: TEdit;
    grpCustomOptions: TGroupBox;
    memCustomOptions: TMemo;
    
    { Inherited Options }
    InheritedPage: TPage;
    InhNoteLabel: TLabel;
    InhTreeView: TTreeView;
    InhItemMemo: TMemo;

    { Buttons }
    btnTest: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;

    { Procedures }
    procedure ButtonOKClicked(Sender: TObject);
    procedure ButtonCancelClicked(Sender: TObject);
    procedure ButtonApplyClicked(Sender: TObject);
    procedure ButtonTestClicked(Sender: TObject);
    procedure InhTreeViewSelectionChanged(Sender: TObject);
    procedure InheritedPageResize(Sender: TObject);
    procedure chkAdditionalConfigFileClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure frmCompilerOptionsClose(Sender: TObject; var Action: TCloseAction
      );
    procedure frmCompilerOptionsResize(Sender: TObject);
  private
    procedure SetupSearchPathsTab(Page: integer);
    procedure SetupParsingTab(Page: integer);
    procedure SetupCodeGenerationTab(Page: integer);
    procedure SetupLinkingTab(Page: integer);
    procedure SetupMessagesTab(Page: integer);
    procedure SetupOtherTab(Page: integer);
    procedure SetupInheritedTab(Page: integer);
    procedure SetupButtonBar;
  private
    FReadOnly: boolean;
    ImageIndexPackage: integer;
    ImageIndexRequired: integer;
    ImageIndexInherited: integer;
    InheritedChildDatas: TList; // list of PInheritedNodeData
    function GetOtherSourcePath: string;
    procedure SetOtherSourcePath(const AValue: string);
    procedure SetReadOnly(const AValue: boolean);
    procedure UpdateInheritedTab;
    procedure ClearInheritedTree;
  public
    CompilerOpts: TBaseCompilerOptions;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetCompilerOptions;
    procedure PutCompilerOptions;
  public
    property OtherSourcePath: string
      read GetOtherSourcePath write SetOtherSourcePath;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
  end;


var
  frmCompilerOptions: TfrmCompilerOptions;
  CompilerParseStamp: integer;
  CompilerGraphStamp: integer;
  OnParseString: TParseStringEvent;
  
procedure IncreaseCompilerParseStamp;
procedure IncreaseCompilerGraphStamp;
function ParseString(Options: TParsedCompilerOptions;
                     const UnparsedValue: string): string;

implementation

const
  Config_Filename = 'compileroptions.xml';
  MaxParseStamp = $7fffffff;
  MinParseStamp = -$7fffffff;
  InvalidParseStamp = MinParseStamp-1;
  
type
  TInheritedNodeData = record
    FullText: string;
    Option: TInheritedCompilerOption;
  end;
  PInheritedNodeData = ^TInheritedNodeData;

procedure IncreaseCompilerParseStamp;
begin
  if CompilerParseStamp<MaxParseStamp then
    inc(CompilerParseStamp)
  else
    CompilerParseStamp:=MinParseStamp;
end;

procedure IncreaseCompilerGraphStamp;
begin
  if CompilerGraphStamp<MaxParseStamp then
    inc(CompilerGraphStamp)
  else
    CompilerGraphStamp:=MinParseStamp;
end;

function ParseString(Options: TParsedCompilerOptions;
                     const UnparsedValue: string): string;
begin
  Result:=OnParseString(Options,UnparsedValue);
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions Constructor
------------------------------------------------------------------------------}
constructor TBaseCompilerOptions.Create(TheOwner: TObject);
begin
  inherited Create;
  fOwner:=TheOwner;
  FParsedOpts:=TParsedCompilerOptions.Create;
  Clear;
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions Destructor
------------------------------------------------------------------------------}
destructor TBaseCompilerOptions.Destroy;
begin
  FreeThenNil(FParsedOpts);
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
  TfrmCompilerOptions LoadCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadCompilerOptions(UseExistingFile: Boolean);
var
  confPath: String;
begin
  if (UseExistingFile and (XMLConfigFile <> nil)) then
  begin
    LoadTheCompilerOptions('');
  end
  else
  begin
    confPath := GetXMLConfigPath;
    try
      XMLConfigFile := TXMLConfig.Create(SetDirSeparators(confPath));
      LoadTheCompilerOptions('');
      XMLConfigFile.Free;
      XMLConfigFile := nil;
    except
      on E: Exception do begin
        writeln('TBaseCompilerOptions.LoadCompilerOptions '+Classname+' '+E.Message);
      end;
    end;
  end;
  fLoaded := true;
end;

{------------------------------------------------------------------------------
  procedure TBaseCompilerOptions.SetIncludeFiles(const AValue: String);
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SetIncludeFiles(const AValue: String);
begin
  if fIncludeFiles=AValue then exit;
  fIncludeFiles:=AValue;
  ParsedOpts.SetUnparsedValue(pcosIncludePath,fIncludeFiles);
end;

procedure TBaseCompilerOptions.SetCompilerPath(const AValue: String);
begin
  if fCompilerPath=AValue then exit;
  fCompilerPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosCompilerPath,fCompilerPath);
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

procedure TBaseCompilerOptions.SetLibraries(const AValue: String);
begin
  if fLibraries=AValue then exit;
  fLibraries:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLibraryPath,fLibraries);
end;

procedure TBaseCompilerOptions.SetLinkerOptions(const AValue: String);
begin
  if fLinkerOptions=AValue then exit;
  fLinkerOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosLinkerOptions,fLinkerOptions);
end;

procedure TBaseCompilerOptions.SetOtherUnitFiles(const AValue: String);
begin
  if fOtherUnitFiles=AValue then exit;
  fOtherUnitFiles:=AValue;
  ParsedOpts.SetUnparsedValue(pcosUnitPath,fOtherUnitFiles);
end;

procedure TBaseCompilerOptions.SetUnitOutputDir(const AValue: string);
begin
  if fUnitOutputDir=AValue then exit;
  fUnitOutputDir:=AValue;
  ParsedOpts.SetUnparsedValue(pcosOutputDir,fUnitOutputDir);
end;

procedure TBaseCompilerOptions.SetObjectPath(const AValue: string);
begin
  if FObjectPath=AValue then exit;
  FObjectPath:=AValue;
  ParsedOpts.SetUnparsedValue(pcosObjectPath,FObjectPath);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions LoadTheCompilerOptions
------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.LoadTheCompilerOptions(const Path: string);
var
  p: String;
begin
  { Load the compiler options from the XML file }
  if Path='' then
    p:='CompilerOptions/Target/'
  else
    p:=Path;

  { Target }
  TargetFilename := XMLConfigFile.GetValue(p+'Filename/Value', '');

  { SearchPaths }
  p:='CompilerOptions/SearchPaths/';
  IncludeFiles := XMLConfigFile.GetValue(p+'IncludeFiles/Value', '');
  Libraries := XMLConfigFile.GetValue(p+'Libraries/Value', '');
  OtherUnitFiles := XMLConfigFile.GetValue(p+'OtherUnitFiles/Value', '');
  CompilerPath := XMLConfigFile.GetValue(p+'CompilerPath/Value', '$(CompPath)');
  UnitOutputDirectory := XMLConfigFile.GetValue(p+'UnitOutputDirectory/Value', '');
  LCLWidgetType := XMLConfigFile.GetValue(p+'LCLWidgetType/Value', 'gtk');
  ObjectPath := XMLConfigFile.GetValue(p+'ObjectPath/Value', '');

  { Parsing }
  p:='CompilerOptions/Parsing/';
  Style := XMLConfigFile.GetValue(p+'Style/Value', 1);
  D2Extensions := XMLConfigFile.GetValue(p+'SymantecChecking/D2Extensions/Value', true);
  CStyleOperators := XMLConfigFile.GetValue(p+'SymantecChecking/CStyleOperator/Value', true);
  IncludeAssertionCode := XMLConfigFile.GetValue(p+'SymantecChecking/IncludeAssertionCode/Value', false);
  AllowLabel := XMLConfigFile.GetValue(p+'SymantecChecking/AllowLabel/Value', true);
  CPPInline := XMLConfigFile.GetValue(p+'SymantecChecking/CPPInline/Value', true);
  CStyleMacros := XMLConfigFile.GetValue(p+'SymantecChecking/CStyleMacros/Value', false);
  TPCompatible := XMLConfigFile.GetValue(p+'SymantecChecking/TPCompatible/Value', false);
  InitConstructor := XMLConfigFile.GetValue(p+'SymantecChecking/InitConstructor/Value', false);
  StaticKeyword := XMLConfigFile.GetValue(p+'SymantecChecking/StaticKeyword/Value', false);
  DelphiCompat := XMLConfigFile.GetValue(p+'SymantecChecking/DelphiCompat/Value', false);
  UseAnsiStrings := XMLConfigFile.GetValue(p+'SymantecChecking/UseAnsiStrings/Value', false);
  GPCCompat := XMLConfigFile.GetValue(p+'SymantecChecking/GPCCompat/Value', false);

  { CodeGeneration }
  p:='CompilerOptions/CodeGeneration/';
  UnitStyle := XMLConfigFile.GetValue(p+'UnitStyle/Value', 1);
  IOChecks := XMLConfigFile.GetValue(p+'Checks/IOChecks/Value', false);
  RangeChecks := XMLConfigFile.GetValue(p+'Checks/RangeChecks/Value', false);
  OverflowChecks := XMLConfigFile.GetValue(p+'Checks/OverflowChecks/Value', false);
  StackChecks := XMLConfigFile.GetValue(p+'Checks/StackChecks/Value', false);
  EmulatedFloatOpcodes := XMLConfigFile.GetValue(p+'EmulateFloatingPointOpCodes/Value', false);
  HeapSize := XMLConfigFile.GetValue(p+'HeapSize/Value', 8000000);
  VerifyObjMethodCall := XMLConfigFile.GetValue(p+'VerifyObjMethodCallValidity/Value', false);
  Generate := XMLConfigFile.GetValue(p+'Generate/Value', 1);
  TargetProcessor := XMLConfigFile.GetValue(p+'TargetProcessor/Value', 1);
  VariablesInRegisters := XMLConfigFile.GetValue(p+'Optimizations/VariablesInRegisters/Value', false);
  UncertainOptimizations := XMLConfigFile.GetValue(p+'Optimizations/UncertainOptimizations/Value', false);
  OptimizationLevel := XMLConfigFile.GetValue(p+'Optimizations/OptimizationLevel/Value', 1);
  TargetOS := XMLConfigFile.GetValue(p+'TargetOS/Value', 'linux');

  { Linking }
  p:='CompilerOptions/Linking/';
  GenerateDebugInfo := XMLConfigFile.GetValue(p+'Debugging/GenerateDebugInfo/Value', false);
  GenerateDebugDBX := XMLConfigFile.GetValue(p+'Debugging/GenerateDebugDBX/Value', false);
  UseLineInfoUnit := XMLConfigFile.GetValue(p+'Debugging/UseLineInfoUnit/Value', true);
  UseHeaptrc := XMLConfigFile.GetValue(p+'Debugging/UseHeaptrc/Value', false);
  GenGProfCode := XMLConfigFile.GetValue(p+'Debugging/GenGProfCode/Value', false);
  StripSymbols := XMLConfigFile.GetValue(p+'Debugging/StripSymbols/Value', false);
  LinkStyle := XMLConfigFile.GetValue(p+'LinkStyle/Value', 1);
  PassLinkerOptions := XMLConfigFile.GetValue(p+'Options/PassLinkerOptions/Value', false);
  LinkerOptions := XMLConfigFile.GetValue(p+'Options/LinkerOptions/Value', '');
    
  { Messages }
  p:='CompilerOptions/Other/';
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
  ShowNothing := XMLConfigFile.GetValue(p+'Verbosity/ShowNothing/Value', false);
  ShowHintsForUnusedProjectUnits := XMLConfigFile.GetValue(p+'Verbosity/ShowHintsForUnusedProjectUnits/Value', false);
  WriteFPCLogo := XMLConfigFile.GetValue(p+'WriteFPCLogo/Value', true);
  StopAfterErrCount := XMLConfigFile.GetValue(p+'ConfigFile/StopAfterErrCount/Value', 1);

  { Other }
  p:='CompilerOptions/Other/';
  DontUseConfigFile := XMLConfigFile.GetValue(p+'ConfigFile/DontUseConfigFile/Value', false);
  AdditionalConfigFile := XMLConfigFile.GetValue(p+'ConfigFile/AdditionalConfigFile/Value', false);
  ConfigFilePath := XMLConfigFile.GetValue(p+'ConfigFile/ConfigFilePath/Value', './fpc.cfg');
  CustomOptions := XMLConfigFile.GetValue(p+'CustomOptions/Value', '');
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
    SaveTheCompilerOptions('');
  end
  else
  begin
    confPath := GetXMLConfigPath;
    try
      XMLConfigFile := TXMLConfig.Create(SetDirSeparators(confPath));
      SaveTheCompilerOptions('');
      XMLConfigFile.Free;
      XMLConfigFile := nil;
    except
      on E: Exception do begin
        writeln('TBaseCompilerOptions.LoadCompilerOptions '+Classname+' '+E.Message);
      end;
    end;
  end;
  fModified:=false;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SaveTheCompilerOptions                                  }
{------------------------------------------------------------------------------}
procedure TBaseCompilerOptions.SaveTheCompilerOptions(const Path: string);
var
  P: string;
begin
  { Save the compiler options to the XML file }
  if Path='' then
    p:='CompilerOptions/Target/'
  else
    p:=Path;

  { Target }
  XMLConfigFile.SetDeleteValue(p+'Filename/Value', TargetFilename,'');

  { SearchPaths }
  p:='CompilerOptions/SearchPaths/';
  XMLConfigFile.SetDeleteValue(p+'IncludeFiles/Value', IncludeFiles,'');
  XMLConfigFile.SetDeleteValue(p+'Libraries/Value', Libraries,'');
  XMLConfigFile.SetDeleteValue(p+'OtherUnitFiles/Value', OtherUnitFiles,'');
  XMLConfigFile.SetDeleteValue(p+'CompilerPath/Value', CompilerPath,'');
  XMLConfigFile.SetDeleteValue(p+'UnitOutputDirectory/Value', UnitOutputDirectory,'');
  XMLConfigFile.SetDeleteValue(p+'LCLWidgetType/Value', LCLWidgetType,'');
  XMLConfigFile.SetDeleteValue(p+'ObjectPath/Value', ObjectPath,'');

  { Parsing }
  p:='CompilerOptions/Parsing/';
  XMLConfigFile.SetDeleteValue(p+'Style/Value', Style,1);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/D2Extensions/Value', D2Extensions,true);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/CStyleOperator/Value', CStyleOperators,true);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/IncludeAssertionCode/Value', IncludeAssertionCode,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/AllowLabel/Value', AllowLabel,true);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/CPPInline/Value', CPPInline,true);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/CStyleMacros/Value', CStyleMacros,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/TPCompatible/Value', TPCompatible,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/InitConstructor/Value', InitConstructor,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/StaticKeyword/Value', StaticKeyword,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/DelphiCompat/Value', DelphiCompat,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/UseAnsiStrings/Value', UseAnsiStrings,false);
  XMLConfigFile.SetDeleteValue(p+'SymantecChecking/GPCCompat/Value', GPCCompat,false);
  
  { CodeGeneration }
  p:='CompilerOptions/CodeGeneration/';
  XMLConfigFile.SetDeleteValue(p+'UnitStyle/Value', UnitStyle,1);
  XMLConfigFile.SetDeleteValue(p+'Checks/IOChecks/Value', IOChecks,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/RangeChecks/Value', RangeChecks,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/OverflowChecks/Value', OverflowChecks,false);
  XMLConfigFile.SetDeleteValue(p+'Checks/StackChecks/Value', StackChecks,false);
  XMLConfigFile.SetDeleteValue(p+'EmulateFloatingPointOpCodes/Value', EmulatedFloatOpcodes,false);
  XMLConfigFile.SetDeleteValue(p+'HeapSize/Value', HeapSize,8000000);
  XMLConfigFile.SetDeleteValue(p+'VerifyObjMethodCallValidity/Value', VerifyObjMethodCall,false);
  XMLConfigFile.SetDeleteValue(p+'Generate/Value', Generate,1);
  XMLConfigFile.SetDeleteValue(p+'TargetProcessor/Value', TargetProcessor,1);
  XMLConfigFile.SetDeleteValue(p+'Optimizations/VariablesInRegisters/Value', VariablesInRegisters,false);
  XMLConfigFile.SetDeleteValue(p+'Optimizations/UncertainOptimizations/Value', UncertainOptimizations,false);
  XMLConfigFile.SetDeleteValue(p+'Optimizations/OptimizationLevel/Value', OptimizationLevel,1);
  XMLConfigFile.SetDeleteValue(p+'TargetOS/Value', TargetOS,'linux');
  XMLConfigFile.SetDeleteValue(p+'LinkStyle/Value', LinkStyle,1);

  { Linking }
  p:='CompilerOptions/Linking/';
  XMLConfigFile.SetDeleteValue(p+'Debugging/GenerateDebugInfo/Value', GenerateDebugInfo,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/GenerateDebugDBX/Value', GenerateDebugDBX,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/UseLineInfoUnit/Value', UseLineInfoUnit,true);
  XMLConfigFile.SetDeleteValue(p+'Debugging/UseHeaptrc/Value', UseHeaptrc,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/GenGProfCode/Value', GenGProfCode,false);
  XMLConfigFile.SetDeleteValue(p+'Debugging/StripSymbols/Value', StripSymbols,false);
  XMLConfigFile.SetDeleteValue(p+'Options/PassLinkerOptions/Value', PassLinkerOptions,false);
  XMLConfigFile.SetDeleteValue(p+'Options/LinkerOptions/Value', LinkerOptions,'');
    
  { Messages }
  p:='CompilerOptions/Other/';
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
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowNothing/Value', ShowNothing,false);
  XMLConfigFile.SetDeleteValue(p+'Verbosity/ShowHintsForUnusedProjectUnits/Value', ShowHintsForUnusedProjectUnits,false);
  XMLConfigFile.SetDeleteValue(p+'WriteFPCLogo/Value', WriteFPCLogo,true);
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/StopAfterErrCount/Value', StopAfterErrCount,1);

  { Other }
  p:='CompilerOptions/Other/';
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/DontUseConfigFile/Value', DontUseConfigFile,false);
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/AdditionalConfigFile/Value', AdditionalConfigFile,false);
  XMLConfigFile.SetDeleteValue(p+'ConfigFile/ConfigFilePath/Value', ConfigFilePath,'./fpc.cfg');
  XMLConfigFile.SetDeleteValue(p+'CustomOptions/Value', CustomOptions,'');

  XMLConfigFile.Flush;
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
begin
  fInheritedOptParseStamps:=InvalidParseStamp;
  fInheritedOptGraphStamps:=InvalidParseStamp;
  for i:=Low(TInheritedCompilerOption) to High(TInheritedCompilerOption) do
    fInheritedOptions[i]:='';
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions CreateTargetFilename
------------------------------------------------------------------------------}
function TBaseCompilerOptions.CreateTargetFilename(
  const MainSourceFileName: string): string;
begin
  if (TargetFilename <> '') then begin
    Result:=ExtractFilePath(MainSourceFileName)+TargetFilename;
  end else begin
    // fpc creates lowercase executables as default
    Result:=lowercase(ExtractFileNameOnly(MainSourceFileName));
    if Result<>'' then begin
      Result:=ExtractFilePath(MainSourceFileName)+Result;
      if fTargetOS = 'win32' then
        Result:=Result+'.exe';
    end else
      Result:='';
  end;
end;

procedure TBaseCompilerOptions.GetInheritedCompilerOptions(
  var OptionsList: TList);
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
    Option: TInheritedCompilerOption; RelativeToBaseDir: boolean): string;
------------------------------------------------------------------------------}
function TBaseCompilerOptions.GetInheritedOption(
  Option: TInheritedCompilerOption; RelativeToBaseDir: boolean): string;
var
  OptionsList: TList;
  i: Integer;
  AddOptions: TAdditionalCompilerOptions;
begin
  if (fInheritedOptParseStamps<>CompilerParseStamp)
  or (fInheritedOptGraphStamps<>CompilerGraphStamp)
  then begin
    // update inherited options
    ClearInheritedOptions;
    OptionsList:=nil;
    GetInheritedCompilerOptions(OptionsList);
    if OptionsList<>nil then begin
      for i:=0 to OptionsList.Count-1 do begin
        AddOptions:=TAdditionalCompilerOptions(OptionsList[i]);
        if (not (AddOptions is TAdditionalCompilerOptions)) then continue;

        // unit search path
        fInheritedOptions[icoUnitPath]:=
          MergeSearchPaths(fInheritedOptions[icoUnitPath],
                       AddOptions.ParsedOpts.GetParsedValue(pcosUnitPath));
        // include search path
        fInheritedOptions[icoIncludePath]:=
          MergeSearchPaths(fInheritedOptions[icoIncludePath],
                       AddOptions.ParsedOpts.GetParsedValue(pcosIncludePath));
        // object search path
        fInheritedOptions[icoObjectPath]:=
          MergeSearchPaths(fInheritedOptions[icoObjectPath],
                       AddOptions.ParsedOpts.GetParsedValue(pcosObjectPath));
        // library search path
        fInheritedOptions[icoLibraryPath]:=
          MergeSearchPaths(fInheritedOptions[icoLibraryPath],
                       AddOptions.ParsedOpts.GetParsedValue(pcosLibraryPath));
        // linker options
        fInheritedOptions[icoLinkerOptions]:=
          MergeLinkerOptions(fInheritedOptions[icoLinkerOptions],
                       AddOptions.ParsedOpts.GetParsedValue(pcosLinkerOptions));
        // custom options
        fInheritedOptions[icoCustomOptions]:=
          MergeCustomOptions(fInheritedOptions[icoCustomOptions],
                       AddOptions.ParsedOpts.GetParsedValue(pcosCustomOptions));
      end;
    end;
    fInheritedOptParseStamps:=CompilerParseStamp;
    fInheritedOptGraphStamps:=CompilerGraphStamp;
  end;
  Result:=fInheritedOptions[Option];
  if RelativeToBaseDir then begin
    if Option in [icoUnitPath,icoIncludePath,icoObjectPath,icoLibraryPath] then
      Result:=CreateRelativeSearchPath(Result,BaseDirectory);
  end;
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.MergeLinkerOptions(const OldOptions,
    AddOptions: string): string;
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MergeLinkerOptions(const OldOptions,
  AddOptions: string): string;
begin
  Result:=OldOptions;
  if AddOptions='' then exit;
  if (OldOptions<>'') and (OldOptions[length(OldOptions)]<>' ')
  and (AddOptions[1]<>' ') then
    Result:=Result+' '+AddOptions
  else
    Result:=Result+AddOptions;
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.MergeCustomOptions(const OldOptions,
    AddOptions: string): string;
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MergeCustomOptions(const OldOptions,
  AddOptions: string): string;
begin
  Result:=OldOptions;
  if AddOptions='' then exit;
  if (OldOptions<>'') and (OldOptions[length(OldOptions)]<>' ')
  and (AddOptions[1]<>' ') then
    Result:=Result+' '+AddOptions
  else
    Result:=Result+AddOptions;
end;

function TBaseCompilerOptions.GetDefaultMainSourceFileName: string;
begin
  Result:='';
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions MakeOptionsString
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MakeOptionsString: String;
begin
  Result:=MakeOptionsString(GetDefaultMainSourceFileName)
end;

{------------------------------------------------------------------------------
  function TBaseCompilerOptions.MakeOptionsString(
    const MainSourceFilename: string): String;
------------------------------------------------------------------------------}
function TBaseCompilerOptions.MakeOptionsString(
  const MainSourceFilename: string): String;
var
  switches, tempsw: String;
  InhLinkerOpts: String;
  InhIncludePath: String;
  InhLibraryPath: String;
  InhUnitPath: String;
  InhCustomOptions: String;
  NewTargetFilename: String;
  CurIncludePath: String;
  CurLibraryPath: String;
  CurUnitPath: String;
  CurOutputDir: String;
  CurCustomOptions: String;
  CurLinkerOptions: String;
  InhObjectPath: String;
  CurObjectPath: String;
  CurMainSrcFile: String;
begin
  if MainSourceFileName='' then
    CurMainSrcFile:=GetDefaultMainSourceFileName
  else
    CurMainSrcFile:=MainSourceFileName;

  switches := '';

  { Get all the options and create a string that can be passed to the compiler }
  
  { options of ppc386 1.1 :

  put + after a boolean switch option to enable it, - to disable it
  -a     the compiler doesn't delete the generated assembler file
      -al        list sourcecode lines in assembler file
      -ar        list register allocation/release info in assembler file
      -at        list temp allocation/release info in assembler file
  -b     generate browser info
      -bl        generate local symbol info
  -B     build all modules
  -C<x>  code generation options:
      -CD        create also dynamic library (not supported)
      -Ce        Compilation with emulated floating point opcodes
      -Ch<n>     <n> bytes heap (between 1023 and 67107840)
      -Ci        IO-checking
      -Cn        omit linking stage
      -Co        check overflow of integer operations
      -Cr        range checking
      -CR        verify object method call validity
      -Cs<n>     set stack size to <n>
      -Ct        stack checking
      -CX        create also smartlinked library
  -d<x>  defines the symbol <x>
  -e<x>  set path to executable
  -E     same as -Cn
  -F<x>  set file names and paths:
      -FD<x>     sets the directory where to search for compiler utilities
      -Fe<x>     redirect error output to <x>
      -FE<x>     set exe/unit output path to <x>
      -Fi<x>     adds <x> to include path
      -Fl<x>     adds <x> to library path
      -FL<x>     uses <x> as dynamic linker
      -Fo<x>     adds <x> to object path
      -Fr<x>     load error message file <x>
      -Fu<x>     adds <x> to unit path
      -FU<x>     set unit output path to <x>, overrides -FE
  -g     generate debugger information:
      -gg        use gsym
      -gd        use dbx
      -gh        use heap trace unit (for memory leak debugging)
      -gl        use line info unit to show more info for backtraces
      -gc        generate checks for pointers
  -i     information
      -iD        return compiler date
      -iV        return compiler version
      -iSO       return compiler OS
      -iSP       return compiler processor
      -iTO       return target OS
      -iTP       return target processor
  -I<x>  adds <x> to include path
  -k<x>  Pass <x> to the linker
  -l     write logo
  -n     don't read the default config file
  -o<x>  change the name of the executable produced to <x>
  -pg    generate profile code for gprof (defines FPC_PROFILE)
  -P     use pipes instead of creating temporary assembler files
  -S<x>  syntax options:
      -S2        switch some Delphi 2 extensions on
      -Sc        supports operators like C (*=,+=,/= and -=)
      -Sa        include assertion code.
      -Sd        tries to be Delphi compatible
      -Se<x>     compiler stops after the <x> errors (default is 1)
      -Sg        allow LABEL and GOTO
      -Sh        Use ansistrings
      -Si        support C++ styled INLINE
      -Sm        support macros like C (global)
      -So        tries to be TP/BP 7.0 compatible
      -Sp        tries to be gpc compatible
      -Ss        constructor name must be init (destructor must be done)
      -St        allow static keyword in objects
  -s     don't call assembler and linker (only with -a)
      -st        Generate script to link on target
      -sh        Generate script to link on host
  -u<x>  undefines the symbol <x>
  -U     unit options:
      -Un        don't check the unit name
      -Ur        generate release unit files
      -Us        compile a system unit
  -v<x>  Be verbose. <x> is a combination of the following letters:
      e : Show errors (default)       d : Show debug info
      w : Show warnings               u : Show unit info
      n : Show notes                  t : Show tried/used files
      h : Show hints                  m : Show defined macros
      i : Show general info           p : Show compiled procedures
      l : Show linenumbers            c : Show conditionals
      a : Show everything             0 : Show nothing (except errors)
      b : Show all procedure          r : Rhide/GCC compatibility mode
          declarations if an error    x : Executable info (Win32 only)
          occurs
  -V     write fpcdebug.txt file with lots of debugging info
  -X     executable options:
      -Xc        link with the c library
      -Xs        strip all symbols from executable
      -XD        try to link dynamic          (defines FPC_LINK_DYNAMIC)
      -XS        try to link static (default) (defines FPC_LINK_STATIC)
      -XX        try to link smart            (defines FPC_LINK_SMART)

Processor specific options:
  -A<x>  output format:
      -Aas       assemble using GNU AS
      -Anasmcoff coff (Go32v2) file using Nasm
      -Anasmelf  elf32 (Linux) file using Nasm
      -Anasmobj  obj file using Nasm
      -Amasm     obj file using Masm (Microsoft)
      -Atasm     obj file using Tasm (Borland)
      -Acoff     coff (Go32v2) using internal writer
      -Apecoff   pecoff (Win32) using internal writer
  -R<x>  assembler reading style:
      -Ratt      read AT&T style assembler
      -Rintel    read Intel style assembler
      -Rdirect   copy assembler text directly to assembler file
  -O<x>  optimizations:
      -Og        generate smaller code
      -OG        generate faster code (default)
      -Or        keep certain variables in registers
      -Ou        enable uncertain optimizations (see docs)
      -O1        level 1 optimizations (quick optimizations)
      -O2        level 2 optimizations (-O1 + slower optimizations)
      -O3        level 3 optimizations (-O2 repeatedly, max 5 times)
      -Op<x>     target processor:
         -Op1  set target processor to 386/486
         -Op2  set target processor to Pentium/PentiumMMX (tm)
         -Op3  set target processor to PPro/PII/c6x86/K6 (tm)
  -T<x>  Target operating system:
      -TGO32V2   version 2 of DJ Delorie DOS extender
      -          3*2TWDOSX DOS 32 Bit Extender
      -TLINUX    Linux
      -Tnetware  Novell Netware Module (experimental)
      -TOS2      OS/2 2.x
      -TSUNOS    SunOS/Solaris
      -TWin32    Windows 32 Bit
  -W<x>  Win32 target options
      -WB<x>     Set Image base to Hexadecimal <x> value
      -WC        Specify console type application
      -WD        Use DEFFILE to export functions of DLL or EXE
      -WF        Specify full-screen type application (OS/2 only)
      -WG        Specify graphic type application
      -WN        Do not generate relocation code (necessary for debugging)
      -WR        Generate relocation code
  }
  
  

  { --------------- Parsing Tab ------------------- }

  { Style }
  { assembler reading style  -Ratt = AT&T    -Rintel = Intel  -Rdirect = As-is }
  switches := switches + ' -R';
  case (Style) of
    1: switches := switches + 'intel';
    2: switches := switches + 'att';
    3: switches := switches + 'direct';
  end;
  
  { Symantec Checking
  
    -S<x>  syntax options:
      -S2        switch some Delphi 2 extensions on
      -Sc        supports operators like C (*=,+=,/= and -=)
      -sa        include assertion code.
      -Sd        tries to be Delphi compatible
      -Se<x>     compiler stops after the <x> errors (default is 1)
      -Sg        allow LABEL and GOTO
      -Sh        Use ansistrings
      -Si        support C++ styled INLINE
      -Sm        support macros like C (global)
      -So        tries to be TP/BP 7.0 compatible
      -Sp        tries to be gpc compatible
      -Ss        constructor name must be init (destructor must be done)
      -St        allow static keyword in objects

  }
  tempsw := '';

  if (D2Extensions) then
    tempsw := tempsw + '2';
  if (CStyleOperators) then
    tempsw := tempsw + 'c';
  if (IncludeAssertionCode) then
    tempsw := tempsw + 'a';
  if (DelphiCompat) then
    tempsw := tempsw + 'd';
  if (AllowLabel) then
    tempsw := tempsw + 'g';
  if (UseAnsiStrings) then
    tempsw := tempsw + 'h';
  if (CPPInline) then
    tempsw := tempsw + 'i';
  if (CStyleMacros) then
    tempsw := tempsw + 'm';
  if (TPCompatible) then
    tempsw := tempsw + 'o';
  if (GPCCompat) then
    tempsw := tempsw + 'p';
  if (InitConstructor) then
    tempsw := tempsw + 's';
  if (StaticKeyword) then
    tempsw := tempsw + 't';

  if (tempsw <> '') then begin
    tempsw := '-S' + tempsw;
    switches := switches + ' ' + tempsw;
  end;

  if (StopAfterErrCount>1) then
    tempsw := tempsw + ' -Se'+IntToStr(StopAfterErrCount);

  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
  -Un = Do not check the unit name
  -Us = Compile a system unit
}

  { ----------- Code Generation Tab --------------- }

  { UnitStyle   '' = Static     'D' = Dynamic   'X' = smart linked }
  case (UnitStyle) of
    0: ;
    1: switches := switches + ' -CD';
    2: switches := switches + ' -CX';
  end;

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
  if (HeapSize >= 0) then
    switches := switches + ' ' + '-Ch' + IntToStr(HeapSize);


  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
  n = Omit linking stage
  sxxx = Set stack size to xxx
}

  switches := switches + ' -O';

  { Generate    G = faster g = smaller  }
  case (Generate) of
    1:  switches := switches + 'G';
    2:  switches := switches + 'g';
  end;

  { OptimizationLevel     1 = Level 1    2 = Level 2    3 = Level 3 }
  case (OptimizationLevel) of
    1:  switches := switches + '1';
    2:  switches := switches + '2';
    3:  switches := switches + '3';
  end;

  if (VariablesInRegisters) then
    switches := switches + 'r';
  if (UncertainOptimizations) then
    switches := switches + 'u';

  { TargetProcessor  p1 = 386/486   p2 = Pentium/Pentium MMX  p3 = PentiumPro/PII/K6 }
  case (TargetProcessor) of
    1:  switches := switches + 'p1';
    2:  switches := switches + 'p2';
    3:  switches := switches + 'p3';
  end;

  { Target OS
       GO32V1 = DOS and version 1 of the DJ DELORIE extender (no longer maintained).
       GO32V2 = DOS and version 2 of the DJ DELORIE extender.
       LINUX = LINUX.
       OS2 = OS/2 (2.x) using the EMX extender.
       WIN32 = Windows 32 bit.
       ... }
  { Only linux and win32 are in the dialog at this moment}
  if TargetOS<>'' then
    switches := switches + ' -T' + TargetOS;
  { --------------- Linking Tab ------------------- }
  
  { Debugging }
  { Debug Info for GDB }
  if (GenerateDebugInfo) then
    switches := switches + ' -g';

  { Debug Info for DBX }
  if (GenerateDebugDBX) then
    switches := switches + ' -gd';

  { Line Numbers in Run-time Error Backtraces - Use LineInfo Unit }
  if (UseLineInfoUnit) then
    switches := switches + ' -gl';

  { Use Heaptrc Unit }
  if (UseHeaptrc) then
    switches := switches + ' -gh';

  { Generate code gprof }
  if (GenGProfCode) then
    switches := switches + ' -pg';

  { Strip Symbols }
  if (StripSymbols) then
    switches := switches + ' -Xs';

  { Link Style
     -XD = Link with dynamic libraries
     -XS = Link with static libraries 
     -XX = Link smart
  }
  case (LinkStyle) of
    1:  switches := switches + ' -XD';
    2:  switches := switches + ' -XS';
    3:  switches := switches + ' -XX';
  end;

  // additional Linker options
  if PassLinkerOptions then begin
    CurLinkerOptions:=ParsedOpts.GetParsedValue(pcosLinkerOptions);
    if (CurLinkerOptions<>'') then
      switches := switches + ' ' + ConvertOptionsToCmdLine(' ','-k', CurLinkerOptions);
  end;

  // inherited Linker options
  InhLinkerOpts:=GetInheritedOption(icoLinkerOptions,true);
  if InhLinkerOpts<>'' then
    switches := switches + ' ' + ConvertOptionsToCmdLine(' ','-k', InhLinkerOpts);

  { ---------------- Other Tab -------------------- }

  { Verbosity }
  { The following switches will not be needed by the IDE
      x = Output some executable info (Win32 only)
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

  if (ShowAll) then
    tempsw := 'a';

  if (ShowNothing) then
    tempsw := '0';

  if (tempsw <> '') then begin
    tempsw := '-v' + tempsw;
    switches := switches + ' ' + tempsw;
  end;


  { Write an FPC logo }
  if (WriteFPCLogo) then
    switches := switches + ' -l';

  { Ignore Config File }
  if DontUseConfigFile then
    switches := switches + ' -n';

  { Use Additional Config File     @ = yes and path }
  if (AdditionalConfigFile) and (ConfigFilePath<>'') then
    switches := switches + ' ' + PrepareCmdLineOption('@' + ConfigFilePath);


  { ------------- Search Paths ---------------- }
  
  // include path
  CurIncludePath:=ParsedOpts.GetParsedValue(pcosIncludePath);
  if (CurIncludePath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fi', CurIncludePath);
    
  // inherited include path
  InhIncludePath:=GetInheritedOption(icoIncludePath,true);
  if (InhIncludePath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fi', InhIncludePath);

  // library path
  CurLibraryPath:=ParsedOpts.GetParsedValue(pcosLibraryPath);
  if (CurLibraryPath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fl', CurLibraryPath);
    
  // inherited library path
  InhLibraryPath:=GetInheritedOption(icoLibraryPath,true);
  if (InhLibraryPath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fl', InhLibraryPath);

  // object path
  CurObjectPath:=ParsedOpts.GetParsedValue(pcosObjectPath);
  if (CurObjectPath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fo', CurObjectPath);

  // inherited object path
  InhObjectPath:=GetInheritedOption(icoObjectPath,true);
  if (InhObjectPath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fo', InhObjectPath);

  // unit path
  CurUnitPath:=ParsedOpts.GetParsedValue(pcosUnitPath);
  // always add the current directory to the unit path, so that the compiler
  // checks for changed files in the directory
  CurUnitPath:=CurUnitPath+';.';
  switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fu', CurUnitPath);

  // inherited unit path
  InhUnitPath:=GetInheritedOption(icoUnitPath,true);
  if (InhUnitPath <> '') then
    switches := switches + ' ' + ConvertSearchPathToCmdLine('-Fu', InhUnitPath);


  { CompilerPath - Nothing needs to be done with this one }
  
  { Unit output directory }
  if UnitOutputDirectory<>'' then
    CurOutputDir:=ParsedOpts.GetParsedValue(pcosOutputDir)
  else
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
  if (TargetFilename<>'') or (CurMainSrcFile<>'') or (CurOutputDir<>'') then
  begin
    NewTargetFilename:=CreateTargetFilename(CurMainSrcFile);
    if (NewTargetFilename<>'')
    and ((CompareFileNames(NewTargetFilename,ChangeFileExt(CurMainSrcFile,''))<>0)
     or (CurOutputDir<>'')) then
      switches := switches + ' '+PrepareCmdLineOption('-o' + NewTargetFilename);
  end;

  // custom options
  CurCustomOptions:=ParsedOpts.GetParsedValue(pcosCustomOptions);
  if CurCustomOptions<>'' then
    Switches:=Switches+' '+CurCustomOptions;
    
  // inherited custom options
  InhCustomOptions:=GetInheritedOption(icoCustomOptions,true);
  if InhCustomOptions<>'' then
    Switches:=Switches+' '+InhCustomOptions;


  fOptionsString := switches;
  Result := fOptionsString;
end;

function TBaseCompilerOptions.CustomOptionsAsString: string;
var
  i: Integer;
begin
  Result:=CustomOptions;
  if Result='' then exit;
  for i:=1 to length(Result) do
    if Result[i]<' ' then Result[i]:=' ';
  if Result='' then exit;
  Result:=Trim(Result);
end;

{------------------------------------------------------------------------------}
{  TBaseCompilerOptions ConvertSearchPathToCmdLine                                           }
{------------------------------------------------------------------------------}
function TBaseCompilerOptions.ConvertSearchPathToCmdLine(
  const switch, paths: String): String;
var
  tempsw, SS, Delim: String;
  M: Integer;
begin
  Delim := ';';
  
  if (switch = '') or (paths = '') then
  begin
    Result := '';
    Exit;
  end;
    
  tempsw := '';
  SS := paths;

  repeat
    M := Pos (Delim, SS);

    if (M = 0) then
    begin
      if (tempsw <> '') then
        tempsw := tempsw + ' ';
      tempsw := tempsw + PrepareCmdLineOption(switch + SS);
      Break;
    end
    else if (M = 1) then
    begin
      SS := Copy (SS, M + 1, Length(SS));
      Continue;
    end
    else
    begin
      if (tempsw <> '') then
        tempsw := tempsw + ' ';
      tempsw := tempsw + PrepareCmdLineOption(switch + Copy (SS, 1, M - 1));
      SS := Copy (SS, M + 1, Length(SS));
    end;
  until (SS = '') or (M = 0);
  
  Result := tempsw;
end;

{------------------------------------------------------------------------------
  TBaseCompilerOptions ConvertOptionsToCmdLine
 ------------------------------------------------------------------------------}
function TBaseCompilerOptions.ConvertOptionsToCmdLine(const Delim, Switch,
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
  IncludeFiles := '';
  Libraries := '';
  OtherUnitFiles := '';
  CompilerPath := '$(CompPath)';
  UnitOutputDirectory := '';
  ObjectPath:='';
  fLCLWidgetType := 'gtk';
  
  // parsing
  fStyle := 1;
  fD2Ext := true;
  fCStyleOp := true;
  fIncludeAssertionCode := false;
  fAllowLabel := true;
  fCPPInline := true;
  fCMacros := false;
  fTPCompat := false;
  fInitConst := false;
  fStaticKwd := false;
  fDelphiCompat := false;
  fUseAnsiStr := false;
  fGPCCompat := false;
    
  // code generation
  fUnitStyle := 1;
  fIOChecks := false;
  fRangeChecks := false;
  fOverflowChecks := false;
  fStackChecks := false;
  fHeapSize := 8000000;
  fGenerate := 1;
  fTargetProc := 1;
  fVarsInReg := false;
  fUncertainOpt := false;
  fOptLevel := 1;
  fTargetOS := 'linux';
    
  // linking
  fGenDebugInfo := false;
  fGenDebugDBX := false;
  fUseLineInfoUnit := true;
  fUseHeaptrc := false;
  fGenGProfCode := false;
  fStripSymbols := false;
  fLinkStyle := 1;
  fPassLinkerOpt := false;
  LinkerOptions := '';
    
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
  fShowNothing := false;
  fShowHintsForUnusedProjectUnits := false;
  fWriteFPCLogo := true;
  fStopAfterErrCount := 1;

  // other
  fDontUseConfigFile := false;
  fAdditionalConfigFile := false;
  fConfigFilePath := './fpc.cfg';
  CustomOptions := '';
  
  // inherited
  ClearInheritedOptions;
end;

procedure TBaseCompilerOptions.Assign(CompOpts: TBaseCompilerOptions);
begin
  fOptionsString := CompOpts.fOptionsString;
  fLoaded := CompOpts.fLoaded;

  // Search Paths
  IncludeFiles := CompOpts.fIncludeFiles;
  Libraries := CompOpts.fLibraries;
  OtherUnitFiles := CompOpts.fOtherUnitFiles;
  CompilerPath := CompOpts.fCompilerPath;
  UnitOutputDirectory := CompOpts.fUnitOutputDir;
  fLCLWidgetType := CompOpts.fLCLWidgetType;
  ObjectPath := CompOpts.FObjectPath;

  // Parsing
  fStyle := CompOpts.fStyle;
  fD2Ext := CompOpts.fD2Ext;
  fCStyleOp := CompOpts.fCStyleOp;
  fIncludeAssertionCode := CompOpts.fIncludeAssertionCode;
  fAllowLabel := CompOpts.fAllowLabel;
  fCPPInline := CompOpts.fCPPInline;
  fCMacros := CompOpts.fCMacros;
  fTPCompat := CompOpts.fTPCompat;
  fInitConst := CompOpts.fInitConst;
  fStaticKwd := CompOpts.fStaticKwd;
  fDelphiCompat := CompOpts.fDelphiCompat;
  fUseAnsiStr := CompOpts.fUseAnsiStr;
  fGPCCompat := CompOpts.fGPCCompat;

  // Code Generation
  fUnitStyle := CompOpts.fUnitStyle;
  fIOChecks := CompOpts.fIOChecks;
  fRangeChecks := CompOpts.fRangeChecks;
  fOverflowChecks := CompOpts.fOverflowChecks;
  fStackChecks := CompOpts.fStackChecks;
  FEmulatedFloatOpcodes := CompOpts.fEmulatedFloatOpcodes;
  fHeapSize := CompOpts.fHeapSize;
  fVerifyObjMethodCall := CompOpts.fVerifyObjMethodCall;
  fGenerate := CompOpts.fGenerate;
  fTargetProc := CompOpts.fTargetProc;
  fVarsInReg := CompOpts.fVarsInReg;
  fUncertainOpt := CompOpts.fUncertainOpt;
  fOptLevel := CompOpts.fOptLevel;
  fTargetOS := CompOpts.fTargetOS;

  // Linking
  fGenDebugInfo := CompOpts.fGenDebugInfo;
  fGenDebugDBX := CompOpts.fGenDebugDBX;
  fUseLineInfoUnit := CompOpts.fUseLineInfoUnit;
  fUseHeaptrc := CompOpts.fUseHeaptrc;
  fGenGProfCode := CompOpts.fGenGProfCode;
  fStripSymbols := CompOpts.fStripSymbols;
  fLinkStyle := CompOpts.fLinkStyle;
  fPassLinkerOpt := CompOpts.fPassLinkerOpt;
  LinkerOptions := CompOpts.fLinkerOptions;

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
  fShowNothing := CompOpts.fShowNothing;
  fShowHintsForUnusedProjectUnits := CompOpts.fShowHintsForUnusedProjectUnits;
  fWriteFPCLogo := CompOpts.fWriteFPCLogo;
  fStopAfterErrCount := CompOpts.fStopAfterErrCount;

  // Other
  fDontUseConfigFile := CompOpts.fDontUseConfigFile;
  fAdditionalConfigFile := CompOpts.fAdditionalConfigFile;
  fConfigFilePath := CompOpts.fConfigFilePath;
  CustomOptions := CompOpts.fCustomOptions;
end;

function TBaseCompilerOptions.IsEqual(CompOpts: TBaseCompilerOptions): boolean;
begin
  Result:=
    // search paths
        (fIncludeFiles = CompOpts.fIncludeFiles)
    and (fLibraries = CompOpts.fLibraries)
    and (fOtherUnitFiles = CompOpts.fOtherUnitFiles)
    and (fCompilerPath = CompOpts.fCompilerPath)
    and (fUnitOutputDir = CompOpts.fUnitOutputDir)
    and (FObjectPath = CompOpts.FObjectPath)

    and (fLCLWidgetType = CompOpts.fLCLWidgetType)

    // parsing
    and (fStyle = CompOpts.fStyle)
    and (fD2Ext = CompOpts.fD2Ext)
    and (fCStyleOp = CompOpts.fCStyleOp)
    and (fIncludeAssertionCode = CompOpts.fIncludeAssertionCode)
    and (fAllowLabel = CompOpts.fAllowLabel)
    and (fCPPInline = CompOpts.fCPPInline)
    and (fCMacros = CompOpts.fCMacros)
    and (fTPCompat = CompOpts.fTPCompat)
    and (fInitConst = CompOpts.fInitConst)
    and (fStaticKwd = CompOpts.fStaticKwd)
    and (fDelphiCompat = CompOpts.fDelphiCompat)
    and (fUseAnsiStr = CompOpts.fUseAnsiStr)
    and (fGPCCompat = CompOpts.fGPCCompat)

    // code generation
    and (fUnitStyle = CompOpts.fUnitStyle)
    and (fIOChecks = CompOpts.fIOChecks)
    and (fRangeChecks = CompOpts.fRangeChecks)
    and (fOverflowChecks = CompOpts.fOverflowChecks)
    and (fStackChecks = CompOpts.fStackChecks)
    and (FEmulatedFloatOpcodes = CompOpts.FEmulatedFloatOpcodes)
    and (fHeapSize = CompOpts.fHeapSize)
    and (fVerifyObjMethodCall = CompOpts.fVerifyObjMethodCall)
    and (fGenerate = CompOpts.fGenerate)
    and (fTargetProc = CompOpts.fTargetProc)
    and (fVarsInReg = CompOpts.fVarsInReg)
    and (fUncertainOpt = CompOpts.fUncertainOpt)
    and (fOptLevel = CompOpts.fOptLevel)
    and (fTargetOS = CompOpts.fTargetOS)

    // linking
    and (fGenDebugInfo = CompOpts.fGenDebugInfo)
    and (fGenDebugDBX = CompOpts.fGenDebugDBX)
    and (fUseLineInfoUnit = CompOpts.fUseLineInfoUnit)
    and (fUseHeaptrc = CompOpts.fUseHeaptrc)
    and (fGenGProfCode = CompOpts.fGenGProfCode)
    and (fStripSymbols = CompOpts.fStripSymbols)
    and (fLinkStyle = CompOpts.fLinkStyle)
    and (fPassLinkerOpt = CompOpts.fPassLinkerOpt)
    and (fLinkerOptions = CompOpts.fLinkerOptions)

    // messages
    and (fShowErrors = CompOpts.fShowErrors)
    and (fShowWarn = CompOpts.fShowWarn)
    and (fShowNotes = CompOpts.fShowNotes)
    and (fShowHints = CompOpts.fShowHints)
    and (fShowGenInfo = CompOpts.fShowGenInfo)
    and (fShowLineNum = CompOpts.fShowLineNum)
    and (fShowAll = CompOpts.fShowAll)
    and (fShowAllProcsOnError = CompOpts.fShowAllProcsOnError)
    and (fShowDebugInfo = CompOpts.fShowDebugInfo)
    and (fShowUsedFiles = CompOpts.fShowUsedFiles)
    and (fShowTriedFiles = CompOpts.fShowTriedFiles)
    and (fShowDefMacros = CompOpts.fShowDefMacros)
    and (fShowCompProc = CompOpts.fShowCompProc)
    and (fShowCond = CompOpts.fShowCond)
    and (fShowNothing = CompOpts.fShowNothing)
    and (fShowHintsForUnusedProjectUnits = CompOpts.fShowHintsForUnusedProjectUnits)
    and (fWriteFPCLogo = CompOpts.fWriteFPCLogo)
    
    // other
    and (fDontUseConfigFile = CompOpts.fDontUseConfigFile)
    and (fAdditionalConfigFile = CompOpts.fAdditionalConfigFile)
    and (fConfigFilePath = CompOpts.fConfigFilePath)
    and (fStopAfterErrCount = CompOpts.fStopAfterErrCount)
    and (fCustomOptions = CompOpts.fCustomOptions)
    ;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Constructor                                             }
{------------------------------------------------------------------------------}
constructor TfrmCompilerOptions.Create(TheOwner: TComponent);

  procedure AddResImg(const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ImageList.Add(Pixmap,nil)
  end;

var Page: integer;
begin
  inherited Create(TheOwner);
  Name:='CompilerOptionsDlg';
  Caption := dlgCompilerOptions;

  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,550,450);
  
  ImageList:=TImageList.Create(Self);
  with ImageList do begin
    Width:=17;
    Height:=17;
    Name:='ImageList';
    ImageIndexPackage:=Count;
    AddResImg('pkg_package');
    ImageIndexRequired:=Count;
    AddResImg('pkg_required');
    ImageIndexInherited:=Count;
    AddResImg('pkg_inherited');
  end;

  nbMain := TNotebook.Create(Self);
  nbMain.Parent := Self;
  nbMain.Height := Height - 50;
  nbMain.Width := Width - 4;
  nbMain.Top := 0;
  nbMain.Left := 0;

  // Add the pages 
  with nbMain.Pages do begin
    Add(dlgSearchPaths);
    Add(dlgCOParsing);
    Add(dlgCodeGeneration);
    Add(dlgCOLinking);
    Add(dlgCOMessages);
    Add(dlgCOOther);
    Add(dlgCOInherited);
  end;
  nbMain.PageIndex:=0;

  Page:=0;
  
  { Search Paths Tab }
  SetupSearchPathsTab(Page);
  inc(Page);
  
  { Parsing Tab }
  SetupParsingTab(Page);
  inc(Page);

  { Code Generation Tab }
  SetupCodeGenerationTab(Page);
  inc(Page);

  { Linking Tab }
  SetupLinkingTab(Page);
  inc(Page);

  { Messages Tab }
  SetupMessagesTab(Page);
  inc(Page);

  { Other Tab }
  SetupOtherTab(Page);
  inc(Page);

  { Inherited Tab }
  SetupInheritedTab(Page);
  inc(Page);

  { Bottom Buttons }
  SetupButtonBar;
  
  OnResize:=@frmCompilerOptionsResize;
  OnResize(Self);
  OnClose:=@frmCompilerOptionsClose;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Destructor                                              }
{------------------------------------------------------------------------------}
destructor TfrmCompilerOptions.Destroy;
begin
  ClearInheritedTree;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonOKClicked                                         }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonOKClicked(Sender: TObject);
begin
  // Accept any changes
  Assert(False, 'Trace:Accept compiler options changes');

  { Save the options and hide the dialog }
  PutCompilerOptions;
  ModalResult:=mrOk;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonCancelClicked                                     }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonCancelClicked(Sender: TObject);
begin
  // Cancel any changes
  Assert(False, 'Trace:Cancel compiler options changes');

  ModalResult:=mrCancel;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonApplyClicked                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonApplyClicked(Sender: TObject);
begin
  // Apply any changes
  PutCompilerOptions;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonTestClicked                                       }
{     This function is for testing the MakeOptionsString function only. Remove }
{     this function and its button when the function is working correctly.     }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonTestClicked(Sender: TObject);
var
  teststr: String;
  i, LineLen: integer;
begin
  // Test MakeOptionsString function
  Assert(False, 'Trace:Test MakeOptionsString function');

  PutCompilerOptions;
  teststr := CompilerOpts.MakeOptionsString;
  WriteLn('CompilerOpts.MakeOptionsString: ' + teststr);
  i:=1;
  LineLen:=0;
  while (i<=length(TestStr)) do begin
    inc(LineLen);
    if (LineLen>60) and (TestStr[i]=' ') then begin
      TestStr[i]:=#13;
      LineLen:=0;
    end;
    inc(i);
  end;
  MessageDlg(dlgShowCompilerOptions,dlgCOOpts+#13+TestStr,mtInformation,
    [mbOk],0);
end;

procedure TfrmCompilerOptions.InhTreeViewSelectionChanged(Sender: TObject);
var
  ANode: TTreeNode;
  ChildData: PInheritedNodeData;
  sl: TStringList;
begin
  ANode:=InhTreeView.Selected;
  if (ANode=nil) or (ANode.Data=nil) then begin
    InhItemMemo.Lines.Text:='Select a node';
  end else begin
    ChildData:=PInheritedNodeData(ANode.Data);
    if ChildData^.Option in icoAllSearchPaths then begin
      sl:=SplitString(ChildData^.FullText,';');
      InhItemMemo.Lines.Assign(sl);
      sl.Free;
    end else
      InhItemMemo.Lines.Text:=ChildData^.FullText;
  end;
end;

{------------------------------------------------------------------------------
  procedure TfrmCompilerOptions.InheritedPageResize(Sender: TObject);
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.InheritedPageResize(Sender: TObject);
var
  y: Integer;
begin
  InhNoteLabel.SetBounds(3,3,InheritedPage.ClientWidth-6,20);
  InhTreeView.SetBounds(0,25,
                      InheritedPage.ClientWidth,InheritedPage.ClientHeight-100);
  y:=InhTreeView.Top+InhTreeView.Height;
  InhItemMemo.SetBounds(0,y,
                        InheritedPage.ClientWidth,InheritedPage.ClientHeight-y);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions GetCompilerOptions
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.GetCompilerOptions;
var i: integer;
begin
  { Get the compiler options and apply them to the dialog }
  case CompilerOpts.Style of
      1: radStyleIntel.Checked := true;
      2: radStyleATT.Checked := true;
      3: radStyleATT.Checked := true;
  end;

  chkSymD2Ext.Checked := CompilerOpts.D2Extensions;
  chkSymCOper.Checked := CompilerOpts.CStyleOperators;
  chkSymIncludeAssertions.Checked := CompilerOpts.IncludeAssertionCode;
  chkSymAllowLab.Checked := CompilerOpts.AllowLabel;
  chkSymCPPInline.Checked := CompilerOpts.CPPInline;
  chkSymCMacros.Checked := CompilerOpts.CStyleMacros;
  chkSymTP7Compat.Checked := CompilerOpts.TPCompatible;
  chkSymConstInit.Checked := CompilerOpts.InitConstructor;
  chkSymStaticKwd.Checked := CompilerOpts.StaticKeyword;
  chkSymDelphiCompat.Checked := CompilerOpts.DelphiCompat;
  chkSymUseAnsiStrings.Checked := CompilerOpts.UseAnsiStrings;
  chkSymGPCCompat.Checked := CompilerOpts.GPCCompat;

  grpUnitStyle.ItemIndex:=CompilerOpts.UnitStyle;

  chkChecksIO.Checked := CompilerOpts.IOChecks;
  chkChecksRange.Checked := CompilerOpts.RangeChecks;
  chkChecksOverflow.Checked := CompilerOpts.OverflowChecks;
  chkChecksStack.Checked := CompilerOpts.StackChecks;

  edtHeapSize.Text := IntToStr(CompilerOpts.HeapSize);

  case CompilerOpts.Generate of
      1: radGenFaster.Checked := true;
      2: radGenSmaller.Checked := true;
  end;

  case CompilerOpts.TargetProcessor of
    1: radTarget386.Checked := true;
    2: radTargetPent.Checked := true;
    3: radTargetPentPro.Checked := true;
  end;

  chkOptVarsInReg.Checked := CompilerOpts.VariablesInRegisters;
  chkOptUncertain.Checked := CompilerOpts.UncertainOptimizations;

  case CompilerOpts.OptimizationLevel of
    1: radOptLevel1.Checked := true;
    2: radOptLevel2.Checked := true;
    3: radOptLevel3.Checked := true;
  end;

  chkDebugGDB.Checked := CompilerOpts.GenerateDebugInfo;
  chkDebugDBX.Checked := CompilerOpts.GenerateDebugDBX;
  chkUseLineInfoUnit.Checked := CompilerOpts.UseLineInfoUnit;
  chkUseHeaptrc.Checked := CompilerOpts.UseHeaptrc;
  chkGenGProfCode.Checked := CompilerOpts.GenGProfCode;
  chkSymbolsStrip.Checked := CompilerOpts.StripSymbols;

  case CompilerOpts.LinkStyle of
    1: radLibsLinkDynamic.Checked := true;
    2: radLibsLinkStatic.Checked := true;
    3: radLibsLinkSmart.Checked := true;
  end;

  chkOptionsLinkOpt.Checked := CompilerOpts.PassLinkerOptions;
  edtOptionsLinkOpt.Text := CompilerOpts.LinkerOptions;
  
  chkErrors.Checked := CompilerOpts.ShowErrors;
  chkWarnings.Checked := CompilerOpts.ShowWarn;
  chkNotes.Checked := CompilerOpts.ShowNotes;
  chkHints.Checked := CompilerOpts.ShowHints;
  chkGeneralInfo.Checked := CompilerOpts.ShowGenInfo;
  chkLineNumbers.Checked := CompilerOpts.ShowLineNum;
  chkEverything.Checked := CompilerOpts.ShowAll;
  chkAllProcsOnError.Checked := CompilerOpts.ShowAllProcsOnError;
  chkDebugInfo.Checked := CompilerOpts.ShowDebugInfo;
  chkUsedFiles.Checked := CompilerOpts.ShowUsedFiles;
  chkTriedFiles.Checked := CompilerOpts.ShowTriedFiles;
  chkDefinedMacros.Checked := CompilerOpts.ShowDefMacros;
  chkCompiledProc.Checked := CompilerOpts.ShowCompProc;
  chkConditionals.Checked := CompilerOpts.ShowCond;
  chkNothing.Checked := CompilerOpts.ShowNothing;
  chkHintsForUnusedProjectUnits.Checked :=
    CompilerOpts.ShowHintsForUnusedProjectUnits;

  chkFPCLogo.Checked := CompilerOpts.WriteFPCLogo;

  chkConfigFile.Checked := not CompilerOpts.DontUseConfigFile;
  chkAdditionalConfigFile.Checked := CompilerOpts.AdditionalConfigFile;
  edtConfigPath.Enabled := chkAdditionalConfigFile.Checked;
  edtConfigPath.Text := CompilerOpts.ConfigFilePath;
  memCustomOptions.Text := CompilerOpts.CustomOptions;
  
  edtErrorCnt.Text := IntToStr(CompilerOpts.StopAfterErrCount);
    
  edtOtherUnits.Text := CompilerOpts.OtherUnitFiles;
  edtIncludeFiles.Text := CompilerOpts.IncludeFiles;
  edtLibraries.Text := CompilerOpts.Libraries;
  edtCompiler.Text := CompilerOpts.CompilerPath;
  edtUnitOutputDir.Text := CompilerOpts.UnitOutputDirectory;
  
  i:=LCLWidgetTypeRadioGroup.Items.IndexOf(CompilerOpts.LCLWidgetType);
  if i<0 then i:=0;
  LCLWidgetTypeRadioGroup.ItemIndex:=i;
  i:=TargetOSRadioGroup.Items.IndexOf(CompilerOpts.TargetOS);
  if i<0 then i:=0;
  TargetOSRadioGroup.ItemIndex:=i;
  
  // inherited tab
  UpdateInheritedTab;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions PutCompilerOptions                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.PutCompilerOptions;
var
  code: LongInt;
  hs: LongInt;
  i: integer;
  OldCompOpts: TBaseCompilerOptions;
begin
  { Put the compiler options into the TCompilerOptions class to be saved }
  if ReadOnly then exit;
  
  OldCompOpts:=TBaseCompilerOptions.Create(nil);
  OldCompOpts.Assign(CompilerOpts);

  if (radStyleIntel.Checked) then
      CompilerOpts.Style := 1
  else if (radStyleATT.Checked) then
      CompilerOpts.Style := 2
  else if (radStyleAsIs.Checked) then
      CompilerOpts.Style := 3
  else
      CompilerOpts.Style := 1;

  CompilerOpts.D2Extensions := chkSymD2Ext.Checked;
  CompilerOpts.CStyleOperators := chkSymCOper.Checked;
  CompilerOpts.IncludeAssertionCode := chkSymIncludeAssertions.Checked;
  CompilerOpts.AllowLabel := chkSymAllowLab.Checked;
  CompilerOpts.CPPInline := chkSymCPPInline.Checked;
  CompilerOpts.CStyleMacros := chkSymCMacros.Checked;
  CompilerOpts.TPCompatible := chkSymTP7Compat.Checked;
  CompilerOpts.InitConstructor := chkSymConstInit.Checked;
  CompilerOpts.StaticKeyword := chkSymStaticKwd.Checked;
  CompilerOpts.DelphiCompat := chkSymDelphiCompat.Checked;
  CompilerOpts.UseAnsiStrings := chkSymUseAnsiStrings.Checked;
  CompilerOpts.GPCCompat := chkSymGPCCompat.Checked;

  
  CompilerOpts.UnitStyle := grpUnitStyle.ItemIndex;

  CompilerOpts.IOChecks := chkChecksIO.Checked;
  CompilerOpts.RangeChecks := chkChecksRange.Checked;
  CompilerOpts.OverflowChecks := chkChecksOverflow.Checked;
  CompilerOpts.StackChecks := chkChecksStack.Checked;

  Val(edtHeapSize.Text, hs, code);
  if (code <> 0) then
    CompilerOpts.HeapSize := 8000000
  else
    CompilerOpts.HeapSize := hs;

  if (radGenFaster.Checked) then
      CompilerOpts.Generate := 1
  else if (radGenSmaller.Checked) then
      CompilerOpts.Generate := 2
  else
      CompilerOpts.Generate := 1;

  if (radTarget386.Checked) then
      CompilerOpts.TargetProcessor := 1
  else if (radTargetPent.Checked) then
      CompilerOpts.TargetProcessor := 2
  else if (radTargetPentPro.Checked) then
      CompilerOpts.TargetProcessor := 3
  else
      CompilerOpts.TargetProcessor := 1;

  CompilerOpts.VariablesInRegisters := chkOptVarsInReg.Checked;
  CompilerOpts.UncertainOptimizations := chkOptUncertain.Checked;

  if (radOptLevel1.Checked) then
      CompilerOpts.OptimizationLevel := 1
  else if (radOptLevel2.Checked) then
      CompilerOpts.OptimizationLevel := 2
  else if (radOptLevel3.Checked) then
      CompilerOpts.OptimizationLevel := 3
  else
      CompilerOpts.OptimizationLevel := 1;

  CompilerOpts.GenerateDebugInfo := chkDebugGDB.Checked;
  CompilerOpts.GenerateDebugDBX := chkDebugDBX.Checked;
  CompilerOpts.UseLineInfoUnit := chkUseLineInfoUnit.Checked;
  CompilerOpts.UseHeaptrc := chkUseHeaptrc.Checked;
  CompilerOpts.GenGProfCode := chkGenGProfCode.Checked;
  CompilerOpts.StripSymbols := chkSymbolsStrip.Checked;

  CompilerOpts.PassLinkerOptions := chkOptionsLinkOpt.Checked;
  CompilerOpts.LinkerOptions := edtOptionsLinkOpt.Text;

  if (radLibsLinkDynamic.Checked) then
    CompilerOpts.LinkStyle := 1
  else if (radLibsLinkStatic.Checked) then
    CompilerOpts.LinkStyle := 2
  else if (radLibsLinkSmart.Checked) then
    CompilerOpts.LinkStyle := 3
  else
    CompilerOpts.LinkStyle := 1;
  
  CompilerOpts.ShowErrors := chkErrors.Checked;
  CompilerOpts.ShowWarn := chkWarnings.Checked;
  CompilerOpts.ShowNotes := chkNotes.Checked;
  CompilerOpts.ShowHints := chkHints.Checked;
  CompilerOpts.ShowGenInfo := chkGeneralInfo.Checked;
  CompilerOpts.ShowLineNum := chkLineNumbers.Checked;
  CompilerOpts.ShowAll := chkEverything.Checked;
  CompilerOpts.ShowAllProcsOnError := chkAllProcsOnError.Checked;
  CompilerOpts.ShowDebugInfo := chkDebugInfo.Checked;
  CompilerOpts.ShowUsedFiles := chkUsedFiles.Checked;
  CompilerOpts.ShowTriedFiles := chkTriedFiles.Checked;
  CompilerOpts.ShowDefMacros := chkDefinedMacros.Checked;
  CompilerOpts.ShowCompProc := chkCompiledProc.Checked;
  CompilerOpts.ShowCond := chkConditionals.Checked;
  CompilerOpts.ShowNothing := chkNothing.Checked;
  CompilerOpts.ShowHintsForUnusedProjectUnits := chkHintsForUnusedProjectUnits.Checked;

  CompilerOpts.WriteFPCLogo := chkFPCLogo.Checked;

  CompilerOpts.DontUseConfigFile := not chkConfigFile.Checked;
  CompilerOpts.AdditionalConfigFile := chkAdditionalConfigFile.Checked;
  CompilerOpts.ConfigFilePath := edtConfigPath.Text;
  CompilerOpts.CustomOptions := memCustomOptions.Text;
  
  CompilerOpts.StopAfterErrCount := StrToIntDef(edtErrorCnt.Text,1);
    
  CompilerOpts.IncludeFiles := edtIncludeFiles.Text;
  CompilerOpts.Libraries := edtLibraries.Text;
  CompilerOpts.OtherUnitFiles := edtOtherUnits.Text;
  CompilerOpts.CompilerPath := edtCompiler.Text;
  CompilerOpts.UnitOutputDirectory := edtUnitOutputDir.Text;
  
  i:=LCLWidgetTypeRadioGroup.Itemindex;
  if i<0 then i:=0;
  CompilerOpts.LCLWidgetType:= LCLWidgetTypeRadioGroup.Items[i];

  i:=TargetOSRadioGroup.Itemindex;
  if i<0 then i:=0;
  CompilerOpts.TargetOS:= TargetOSRadioGroup.Items[i];
  
  if not OldCompOpts.IsEqual(CompilerOpts) then
    CompilerOpts.Modified:=true;
  OldCompOpts.Free;
end;

procedure TfrmCompilerOptions.UpdateInheritedTab;
var
  OptionsList: TList;
  i: Integer;
  AncestorOptions: TAdditionalCompilerOptions;
  AncestorNode: TTreeNode;
  
  procedure AddChildNode(const NewNodeName, Value: string;
    Option: TInheritedCompilerOption);
  var
    VisibleValue: String;
    ChildNode: TTreeNode;
    ChildData: PInheritedNodeData;
  begin
    if Value='' then exit;
    New(ChildData);
    ChildData^.FullText:=Value;
    ChildData^.Option:=Option;
    if InheritedChildDatas=nil then InheritedChildDatas:=TList.Create;
    InheritedChildDatas.Add(ChildData);

    if length(Value)>100 then
      VisibleValue:=copy(Value,1,100)+'[...]'
    else
      VisibleValue:=Value;
    ChildNode:=InhTreeView.Items.AddChildObject(AncestorNode,
                                 NewNodeName+' = "'+VisibleValue+'"',ChildData);
    ChildNode.ImageIndex:=ImageIndexRequired;
    ChildNode.SelectedIndex:=ChildNode.ImageIndex;
  end;
  
begin
  CompilerOpts.GetInheritedCompilerOptions(OptionsList);
  InhTreeView.BeginUpdate;
  ClearInheritedTree;
  if OptionsList<>nil then begin
    // add All node
    AncestorNode:=InhTreeView.Items.Add(nil,'All inherited options');
    AncestorNode.ImageIndex:=ImageIndexInherited;
    AncestorNode.SelectedIndex:=AncestorNode.ImageIndex;
    with CompilerOpts do begin
      AddChildNode('unit path',
        GetInheritedOption(icoUnitPath,true),icoUnitPath);
      AddChildNode('include path',
        GetInheritedOption(icoIncludePath,true),icoIncludePath);
      AddChildNode('object path',
        GetInheritedOption(icoObjectPath,true),icoObjectPath);
      AddChildNode('library path',
        GetInheritedOption(icoLibraryPath,true),icoLibraryPath);
      AddChildNode('linker options',GetInheritedOption(icoLinkerOptions,true),
        icoLinkerOptions);
      AddChildNode('custom options',GetInheritedOption(icoCustomOptions,true),
        icoCustomOptions);
    end;
    AncestorNode.Expanded:=true;
    // add detail nodes
    for i:=0 to OptionsList.Count-1 do begin
      AncestorOptions:=TAdditionalCompilerOptions(OptionsList[i]);
      AncestorNode:=InhTreeView.Items.Add(nil,'');
      AncestorNode.Text:=AncestorOptions.GetOwnerName;
      AncestorNode.ImageIndex:=ImageIndexPackage;
      AncestorNode.SelectedIndex:=AncestorNode.ImageIndex;
      with AncestorOptions.ParsedOpts do begin
        AddChildNode('unit path',
          CreateRelativeSearchPath(GetParsedValue(pcosUnitPath),
          CompilerOpts.BaseDirectory),icoUnitPath);
        AddChildNode('include path',
          CreateRelativeSearchPath(GetParsedValue(pcosIncludePath),
          CompilerOpts.BaseDirectory),icoIncludePath);
        AddChildNode('object path',
          CreateRelativeSearchPath(GetParsedValue(pcosObjectPath),
          CompilerOpts.BaseDirectory),icoObjectPath);
        AddChildNode('library path',
          CreateRelativeSearchPath(GetParsedValue(pcosLibraryPath),
          CompilerOpts.BaseDirectory),icoLibraryPath);
        AddChildNode('linker options',GetParsedValue(pcosLinkerOptions),
          icoLinkerOptions);
        AddChildNode('custom options',GetParsedValue(pcosCustomOptions),
          icoCustomOptions);
      end;
      AncestorNode.Expanded:=true;
    end;
  end else begin
    InhTreeView.Items.Add(nil,'No compiler options inherited.');
  end;
  InhTreeView.EndUpdate;
end;

procedure TfrmCompilerOptions.ClearInheritedTree;
var
  i: Integer;
  ChildData: PInheritedNodeData;
begin
  InhTreeView.BeginUpdate;
  // dispose all child data
  if InheritedChildDatas<>nil then begin
    for i:=0 to InheritedChildDatas.Count-1 do begin
      ChildData:=PInheritedNodeData(InheritedChildDatas[i]);
      Dispose(ChildData);
    end;
    InheritedChildDatas.Free;
  end;
  InhTreeView.Items.Clear;
  InhTreeView.EndUpdate;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupParsingTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupParsingTab(Page: integer);
var
  y: Integer;
  yDiff: Integer;
begin
  // Setup the Parsing Tab
  ParsingPage:=nbMain.Page[Page];

  grpStyle := TGroupBox.Create(Self);
  with grpStyle do
  begin
    Parent := ParsingPage;
    Top := 5;
    Left := 5;
    Height := 42;
    Width := 300;
    Caption := dlgCOStyle;
  end;

  radStyleIntel := TRadioButton.Create(grpStyle);
  with radStyleIntel do
  begin
    Parent := grpStyle;
    Top := 0;
    Left := 5;
    Width := 50;
    Caption := 'Intel';//Really we should localize this? :)
  end;

  radStyleATT := TRadioButton.Create(grpStyle);
  with radStyleATT do
  begin
    Parent := grpStyle;
    Top := 0;
    Left := 80;
    Width := 70;
    Caption := 'AT&T';
  end;

  radStyleAsIs := TRadioButton.Create(grpStyle);
  with radStyleAsIs do
  begin
    Parent := grpStyle;
    Top := 0;
    Left := 170;
    Width := 100;
    Caption := dlgCOAsIs ;
  end;

  yDiff:=22;

  grpSymantecChk := TGroupBox.Create(Self);
  with grpSymantecChk do
  begin
    Parent := ParsingPage;
    Top := grpStyle.Top+grpStyle.Height+5;
    Left := grpStyle.Left;
    Height := 25+12*yDiff;
    Width := Self.ClientWidth-28;
    Caption := dlgSymantecChecking ;
  end;

  y:=2;

  chkSymD2Ext := TCheckBox.Create(grpSymantecChk);
  with chkSymD2Ext do
  begin
    Parent := grpSymantecChk;
    Caption := dlgDelphi2Ext ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymCOper := TCheckBox.Create(grpSymantecChk);
  with chkSymCOper do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCOCOps;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymIncludeAssertions := TCheckBox.Create(grpSymantecChk);
  with chkSymIncludeAssertions do
  begin
    Parent := grpSymantecChk;
    Caption := dlgAssertCode ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;
  
  inc(y,yDiff);
  chkSymAllowLab := TCheckBox.Create(grpSymantecChk);
  with chkSymAllowLab do
  begin
    Parent := grpSymantecChk;
    Caption := dlgLabelGoto ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymCPPInline := TCheckBox.Create(grpSymantecChk);
  with chkSymCPPInline do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCppInline ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymCMacros := TCheckBox.Create(grpSymantecChk);
  with chkSymCMacros do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCMacro;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymTP7Compat := TCheckBox.Create(grpSymantecChk);
  with chkSymTP7Compat do
  begin
    Parent := grpSymantecChk;
    Caption := dlgBP7Cptb ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymConstInit := TCheckBox.Create(grpSymantecChk);
  with chkSymConstInit do
  begin
    Parent := grpSymantecChk;
    Caption := dlgInitDoneOnly ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymStaticKwd := TCheckBox.Create(grpSymantecChk);
  with chkSymStaticKwd do
  begin
    Parent := grpSymantecChk;
    Caption := dlgStaticKeyword ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;
  
  inc(y,yDiff);
  chkSymDelphiCompat := TCheckBox.Create(grpSymantecChk);
  with chkSymDelphiCompat do
  begin
    Parent := grpSymantecChk;
    Caption := dlgDeplhiComp;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymUseAnsiStrings := TCheckBox.Create(grpSymantecChk);
  with chkSymUseAnsiStrings do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCOAnsiStr ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymGPCCompat := TCheckBox.Create(grpSymantecChk);
  with chkSymGPCCompat do
  begin
    Parent := grpSymantecChk;
    Caption := dlgGPCComp ;
    Top := y;
    Left := 5;
    Height := 16;
    Width := Parent.ClientWidth-20;
  end;
end;


{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupCodeGenerationTab                                  }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupCodeGenerationTab(Page: integer);
begin
  // Setup the Code Generation Tab
  CodeGenPage:=nbMain.Page[Page];
  Assert(False, 'Trace:Setting up compiler options code generation tab');

  grpUnitStyle := TRadioGroup.Create(Self);
  with grpUnitStyle do
  begin
    Parent := CodeGenPage;
    Top := 5;
    Left := 5;
    Height := 70;
    Width := 120;
    Caption := dlgCOUnitStyle ;
    with Items do begin
      Add(dlgStatic );
      Add(dlgDynamic );
      Add(dlgCOSmart );
    end;
  end;

  {------------------------------------------------------------}
  grpChecks := TGroupBox.Create(Self);
  with grpChecks do
  begin
    Parent := CodeGenPage;
    Top := 5;
    Left := grpUnitStyle.Left + grpUnitStyle.Width + 10;
    Height := 70;
    Width := 175;
    Caption := dlgCOChecks ;
  end;

  chkChecksIO := TCheckBox.Create(grpChecks);
  with chkChecksIO do
  begin
    Parent := grpChecks;
    Caption := 'I/O';
    Top := 2;
    Left := 5;
    Height := 16;
    Width := 70;
  end;

  chkChecksRange := TCheckBox.Create(grpChecks);
  with chkChecksRange do
  begin
    Parent := grpChecks;
    Caption := dlgCORange ;
    Top := 2;
    Left := 85;
    Height := 16;
    Width := 90;
  end;

  chkChecksOverflow := TCheckBox.Create(grpChecks);
  with chkChecksOverflow do
  begin
    Parent := grpChecks;
    Caption := dlgCOOverflow ;
    Top := 27;
    Left := 5;
    Height := 16;
    Width := 80;
  end;

  chkChecksStack := TCheckBox.Create(grpChecks);
  with chkChecksStack do
  begin
    Parent := grpChecks;
    Caption := dlgCOStack ;
    Top := 27;
    Left := 85;
    Height := 16;
    Width := 70;
  end;

  {------------------------------------------------------------}

  grpHeapSize := TGroupBox.Create(Self);
  with grpHeapSize do
  begin
    Parent := CodeGenPage;
    Top := 10;
    Left := grpChecks.Left + grpChecks.Width + 10;
    Height := 55;
    Width := 80;
    Caption := dlgHeapSize +':';
  end;

  edtHeapSize := TEdit.Create(grpHeapSize);
  with edtHeapSize do
  begin
    Parent := grpHeapSize;
    Caption := dlgHeapSize ;
    Top := 8;
    Left := 5;
    Height := 23;
    Width := 65;
    Text := '';
  end;

  {------------------------------------------------------------}

  grpGenerate := TGroupBox.Create(Self);
  with grpGenerate do
  begin
    Parent := CodeGenPage;
    Top := grpUnitStyle.Top + grpUnitStyle.Height + 6;
    Left := 10;
    Height := 70;
    Width := 115;
    Caption := dlgCOGenerate ;
  end;

  radGenFaster := TRadioButton.Create(grpGenerate);
  with radGenFaster do
  begin
    Parent := grpGenerate;
    Top := 8;
    Left := 5;
    Height := 16;
    Width := 100;
    Caption := dlgCOFast;
  end;

  radGenSmaller := TRadioButton.Create(grpGenerate);
  with radGenSmaller do
  begin
    Parent := grpGenerate;
    Top := 29;
    Left := 5;
    Height := 16;
    Width := 100;
    Caption := dlgCOSmaller ;
  end;


  {------------------------------------------------------------}

  grpTargetProc := TGroupBox.Create(Self);
  with grpTargetProc do
  begin
    Parent := CodeGenPage;
    Top := grpGenerate.Top;
    Left := grpGenerate.Left + grpGenerate.Width + 10;
    Height := 90;
    Width := 270;
    Caption := dlgTargetProc ;
  end;

  radTarget386 := TRadioButton.Create(grpTargetProc);
  with radTarget386 do
  begin
    Parent := grpTargetProc;
    Caption := '386/486';
    Top := 8;
    Left := 5;
    Height := 16;
    Width := 220;
  end;

  radTargetPent := TRadioButton.Create(grpTargetProc);
  with radTargetPent do
  begin
    Parent := grpTargetProc;
    Caption := 'Pentium/Pentium MMX';
    Top := 29;
    Left := 5;
    Height := 16;
    Width := 220;
  end;

  radTargetPentPro := TRadioButton.Create(grpTargetProc);
  with radTargetPentPro do
  begin
    Parent := grpTargetProc;
    Caption := 'Pentium Pro/Pentium II/C6x86/K6';
    Top := 50;
    Left := 5;
    Height := 16;
    Width := 220;
  end;


  {------------------------------------------------------------}

  grpOptimizations := TGroupBox.Create(Self);
  with grpOptimizations do
  begin
    Parent := CodeGenPage;
    Top := grpTargetProc.Top + grpTargetProc.Height + 6;
    Left := 10;
    Height := 132;
    Width := 395;
    Caption :=   dlgOptimiz ;
  end;

  chkOptVarsInReg := TCheckBox.Create(grpOptimizations);
  with chkOptVarsInReg do
  begin
    Parent := grpOptimizations;
    Caption := dlgCOKeepVarsReg ;
    Top := 5;
    Left := 5;
    Height := 16;
    Width := 330;
  end;

  chkOptUncertain := TCheckBox.Create(grpOptimizations);
  with chkOptUncertain do
  begin
    Parent := grpOptimizations;
    Caption := dlgUncertOpt ;
    Top := 26;
    Left := 5;
    Height := 16;
    Width := 330;
  end;

  radOptLevel1 := TRadioButton.Create(grpOptimizations);
  with radOptLevel1 do
  begin
    Parent := grpOptimizations;
    Caption :=  dlgLevel1Opt ;
    Top := 52;
    Left := 5;
    Height := 16;
    Width := 330;
  end;

  radOptLevel2 := TRadioButton.Create(grpOptimizations);
  with radOptLevel2 do
  begin
    Parent := grpOptimizations;
    Caption := dlgLevel2Opt;
    Top := 73;
    Left := 5;
    Height := 16;
    Width := 330;
  end;

  radOptLevel3 := TRadioButton.Create(grpOptimizations);
  with radOptLevel3 do
  begin
    Parent := grpOptimizations;
    Caption := dlgLevel3Opt ;
    Top := 94;
    Left := 5;
    Height := 16;
    Width := 330;
  end;

  TargetOSRadioGroup:=TRadioGroup.Create(Self);
  with TargetOSRadioGroup do begin
    Name:='TargetOSRadioGroup';
    Parent := CodeGenPage;
    Left := grpOtherUnits.Left;
    Top:=grpOptimizations.Top+grpOptimizations.Height+5;
    Width:=150;
    Height:=45;
    Caption:=dlgTargetOS;
    with Items do begin
      Add('linux');
      Add('win32');
    end;
    Columns:=2;
    ItemIndex:=0;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupLinkingTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupLinkingTab(Page: integer);
begin
  // Setup the Linking Tab
  LinkingPage:=nbMain.Page[Page];
  Assert(False, 'Trace:Setting up compiler options linking tab');

  grpDebugging := TGroupBox.Create(Self);
  with grpDebugging do
  begin
    Parent := LinkingPage;
    Top := 10;
    Left := 10;
    Height := 151;
    Width := Self.ClientWidth-28;
    Caption := dlgCODebugging ;
  end;

  chkDebugGDB := TCheckBox.Create(grpDebugging);
  with chkDebugGDB do
  begin
    Parent := grpDebugging;
    Caption := dlgCOGDB ;
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  chkDebugDBX := TCheckBox.Create(grpDebugging);
  with chkDebugDBX do
  begin
    Parent := grpDebugging;
    Caption := dlgCODBX;
    Top := 27;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  chkUseLineInfoUnit := TCheckBox.Create(grpDebugging);
  with chkUseLineInfoUnit do
  begin
    Parent := grpDebugging;
    Caption := dlgLNumsBct;
    Top := 48;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  chkUseHeaptrc := TCheckBox.Create(grpDebugging);
  with chkUseHeaptrc do
  begin
    Parent := grpDebugging;
    Caption := dlgCOHeaptrc ;
    Top := 69;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  chkGenGProfCode := TCheckBox.Create(grpDebugging);
  with chkGenGProfCode do
  begin
    Parent := grpDebugging;
    Caption := dlgGPROF ;
    Top := 90;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  chkSymbolsStrip := TCheckBox.Create(grpDebugging);
  with chkSymbolsStrip do
  begin
    Parent := grpDebugging;
    Caption := dlgCOStrip;
    Top := 111;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  {------------------------------------------------------------}

  grpLinkLibraries := TGroupBox.Create(Self);
  with grpLinkLibraries do
  begin
    Parent := LinkingPage;
    Top := grpDebugging.Top + grpDebugging.Height + 10;
    Left := 10;
    Height := 91;
    Width := Self.ClientWidth-28;
    Caption := dlgLinkLibraries ;
  end;

  radLibsLinkDynamic := TRadioButton.Create(grpLinkLibraries);
  with radLibsLinkDynamic do
  begin
    Parent := grpLinkLibraries;
    Caption := dlgLinkDinLibs;
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 360;
  end;

  radLibsLinkStatic := TRadioButton.Create(grpLinkLibraries);
  with radLibsLinkStatic do
  begin
    Parent := grpLinkLibraries;
    Caption := dlgLinkStatLibs ;
    Top := 27;
    Left := 8;
    Height := 16;
    Width := 330;
  end;

  radLibsLinkSmart := TRadioButton.Create(grpLinkLibraries);
  with radLibsLinkSmart do
  begin
    Parent := grpLinkLibraries;
    Caption := dlgLinkSmart ;
    Top := 48;
    Left := 8;
    Height := 16;
    Width := 330;
  end;

  {------------------------------------------------------------}

  grpOptions := TGroupBox.Create(Self);
  with grpOptions do
  begin
    Parent := LinkingPage;
    Top := grpLinkLibraries.Top + grpLinkLibraries.Height + 10;
    Left := 10;
    Height := 75;
    Width := Self.ClientWidth-28;
    Caption := dlgCOOpts;
  end;

  chkOptionsLinkOpt := TCheckBox.Create(grpOptions);
  with chkOptionsLinkOpt do
  begin
    Parent := grpOptions;
    Caption := dlgPassOptsLinker ;
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 330;
  end;

  edtOptionsLinkOpt := TEdit.Create(grpOptions);
  with edtOptionsLinkOpt do
  begin
    Parent := grpOptions;
    Top := 27;
    Left := 8;
    Height := 23;
    Width := Parent.ClientWidth-20;
    Text := '';
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupMessagesTab                                           }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupMessagesTab(Page: integer);
begin
  // Setup the Messages Tab
  MsgPage:=nbMain.Page[Page];
  
  grpVerbosity := TGroupBox.Create(Self);
  with grpVerbosity do
  begin
    Parent := MsgPage;
    Top := 10;
    Left := 10;
    Height := 212;
    Width := Self.ClientWidth-28;
    Caption := dlgVerbosity;
  end;

  chkErrors := TCheckBox.Create(grpVerbosity);
  with chkErrors do
  begin
    Parent := grpVerbosity;
    Caption := dlgCOShowErr ;
    Top := 6;
    Left := 8;
    Height := 16;
    Width := (grpVerbosity.ClientWidth div 2)-12;
  end;

  chkWarnings := TCheckBox.Create(grpVerbosity);
  with chkWarnings do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowWarnings ;
    Top := 27;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkNotes := TCheckBox.Create(grpVerbosity);
  with chkNotes do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowNotes ;
    Top := 48;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkHints := TCheckBox.Create(grpVerbosity);
  with chkHints do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowHint ;
    Top := 69;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkGeneralInfo := TCheckBox.Create(grpVerbosity);
  with chkGeneralInfo do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowGeneralInfo ;
    Top := 90;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkLineNumbers := TCheckBox.Create(grpVerbosity);
  with chkLineNumbers do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowLineNumbers ;
    Top := 111;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkAllProcsOnError := TCheckBox.Create(grpVerbosity);
  with chkAllProcsOnError do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowProcsError ;
    Top := 132;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkEverything := TCheckBox.Create(grpVerbosity);
  with chkEverything do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowEverything ;
    Top := 153;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkDebugInfo := TCheckBox.Create(grpVerbosity);
  with chkDebugInfo do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowDebugInfo ;
    Top := 6;
    Left := (grpVerbosity.ClientWidth div 2)+4;
    Height := 16;
    Width := (grpVerbosity.ClientWidth div 2)-12;
  end;

  chkUsedFiles := TCheckBox.Create(grpVerbosity);
  with chkUsedFiles do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowUsedFiles ;
    Top := 27;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkTriedFiles := TCheckBox.Create(grpVerbosity);
  with chkTriedFiles do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowTriedFiles ;
    Top := 48;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkDefinedMacros := TCheckBox.Create(grpVerbosity);
  with chkDefinedMacros do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowDefinedMacros ;
    Top := 69;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkCompiledProc := TCheckBox.Create(grpVerbosity);
  with chkCompiledProc do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowCompiledProcedures ;
    Top := 90;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkConditionals := TCheckBox.Create(grpVerbosity);
  with chkConditionals do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowConditionals ;
    Top := 111;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkNothing := TCheckBox.Create(grpVerbosity);
  with chkNothing do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowNothing ;
    Top := 132;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkFPCLogo := TCheckBox.Create(Self);
  with chkFPCLogo do
  begin
    Parent := grpVerbosity;
    Caption := dlgWriteFPCLogo ;
    Top := 153;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkHintsForUnusedProjectUnits := TCheckBox.Create(grpVerbosity);
  with chkHintsForUnusedProjectUnits do
  begin
    Parent := grpVerbosity;
    Caption := dlgHintsUnused ;
    Top := 174;
    Left := ChkErrors.Left;
    Height := ChkErrors.Height;
    Width := 250;
  end;

  {------------------------------------------------------------}
  grpErrorCnt := TGroupBox.Create(Self);
  with grpErrorCnt do
  begin
    Parent := MsgPage;
    Top := grpVerbosity.Top + grpVerbosity.Height + 10;
    Left := 10;
    Height := 50;
    Width := 200;
    Caption := dlgStopAfterNrErr ;
  end;

  edtErrorCnt := TEdit.Create(grpErrorCnt);
  with edtErrorCnt do
  begin
    Parent := grpErrorCnt;
    Top := 6;
    Left := 8;
    Height := 23;
    Width := grpErrorCnt.ClientWidth-2*Left-4;
    Text := '';
  end;
end;

procedure TfrmCompilerOptions.SetupOtherTab(Page: integer);
begin
  {------------------------------------------------------------}
  OtherPage:=nbMain.Page[Page];

  grpConfigFile := TGroupBox.Create(Self);
  with grpConfigFile do
  begin
    Parent := OtherPage;
    Top := 10;
    Left := 10;
    Height := 95;
    Width := Self.ClientWidth-28;
    Caption := dlgConfigFiles ;
  end;

  chkConfigFile := TCheckBox.Create(grpConfigFile);
  with chkConfigFile do
  begin
    Parent := grpConfigFile;
    Caption := dlgUseFpcCfg ;
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 330;
  end;

  chkAdditionalConfigFile := TCheckBox.Create(grpConfigFile);
  with chkAdditionalConfigFile do
  begin
    Parent := grpConfigFile;
    Caption := dlgUseAdditionalConfig ;
    Top := 27;
    Left := 8;
    Height := 16;
    Width := 330;
    OnClick:=@chkAdditionalConfigFileClick;
  end;

  edtConfigPath := TEdit.Create(grpConfigFile);
  with edtConfigPath do
  begin
    Parent := grpConfigFile;
    Top := 48;
    Left := 8;
    Height := 23;
    Width := 330;
    Text := '';
  end;
  
  grpCustomOptions := TGroupBox.Create(Self);
  with grpCustomOptions do begin
    Name:='grpCustomOptions';
    Parent := OtherPage;
    Left:=grpConfigFile.Left;
    Top:=grpConfigFile.Top+grpConfigFile.Height+10;
    Width:=grpConfigFile.Width;
    Height:=200;
    Caption:='Custom options';
  end;
  
  memCustomOptions := TMemo.Create(Self);
  with memCustomOptions do begin
    Name:='memCustomOptions';
    Parent:=grpCustomOptions;
    Align:=alClient;
  end;
end;

{------------------------------------------------------------------------------
  procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
begin
  InheritedPage:=nbMain.Page[Page];
  InheritedPage.OnResize:=@InheritedPageResize;
  
  InhNoteLabel:=TLabel.Create(Self);
  with InhNoteLabel do begin
    Name:='InhNoteLabel';
    Parent:=InheritedPage;
    Caption:='Additional compiler options inherited from packages';
  end;
  
  InhTreeView:=TTreeView.Create(Self);
  with InhTreeView do begin
    Name:='InhTreeView';
    Parent:=InheritedPage;
    Options:=Options+[tvoReadOnly, tvoRightClickSelect, tvoShowRoot,
                      tvoKeepCollapsedNodes];
    Images:=ImageList;
    OnSelectionChanged:=@InhTreeViewSelectionChanged;
  end;
  
  InhItemMemo:=TMemo.Create(Self);
  with InhItemMemo do begin
    Name:='InhItemMemo';
    Parent:=InheritedPage;
    ReadOnly:=true;
    WordWrap:=true;
    ScrollBars:=ssAutoVertical;
    Text:='Select a node';
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupSearchPathsTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupSearchPathsTab(Page: integer);
var
  y: Integer;
begin
  // Setup the Search Paths Tab
  PathPage:=nbMain.Page[Page];

  y:=5;
  
  grpOtherUnits := TGroupBox.Create(Self);
  with grpOtherUnits do
  begin
    Parent := PathPage;
    Left := 10;
    Top := y;
    Width := Self.ClientWidth-28;
    Height := 45;
    Caption := dlgOtherUnitFiles ;
  end;

  edtOtherUnits := TEdit.Create(grpOtherUnits);
  with edtOtherUnits do
  begin
    Parent := grpOtherUnits;
    Left := 8;
    Top := 0;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;
  
  OtherUnitsPathEditBtn:=TPathEditorButton.Create(Self);
  with OtherUnitsPathEditBtn do begin
    Name:='OtherUnitsPathEditBtn';
    Parent:=grpOtherUnits;
    Left:=edtOtherUnits.Left+edtOtherUnits.Width+3;
    Top:=edtOtherUnits.Top;
    Width:=25;
    Height:=edtOtherUnits.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;
  
  {------------------------------------------------------------}
  
  grpIncludeFiles := TGroupBox.Create(Self);
  with grpIncludeFiles do
  begin
    Parent := PathPage;
    Left := grpOtherUnits.Left;
    Top := grpOtherUnits.Top+grpOtherUnits.Height+5;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOIncFiles ;
  end;

  edtIncludeFiles := TEdit.Create(grpIncludeFiles);
  with edtIncludeFiles do
  begin
    Parent := grpIncludeFiles;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  IncludeFilesPathEditBtn:=TPathEditorButton.Create(Self);
  with IncludeFilesPathEditBtn do begin
    Name:='IncludeFilesPathEditBtn';
    Parent:=grpIncludeFiles;
    Left:=edtIncludeFiles.Left+edtIncludeFiles.Width+3;
    Top:=edtIncludeFiles.Top;
    Width:=25;
    Height:=edtIncludeFiles.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  {------------------------------------------------------------}

  grpOtherSources := TGroupBox.Create(Self);
  with grpOtherSources do
  begin
    Parent := PathPage;
    Top := grpIncludeFiles.Top+grpIncludeFiles.Height+5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOSources ;
  end;

  edtOtherSources := TEdit.Create(grpIncludeFiles);
  with edtOtherSources do
  begin
    Parent := grpOtherSources;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  OtherSourcesPathEditBtn:=TPathEditorButton.Create(Self);
  with OtherSourcesPathEditBtn do begin
    Name:='OtherSourcesPathEditBtn';
    Parent:=grpOtherSources;
    Left:=edtOtherSources.Left+edtOtherSources.Width+3;
    Top:=edtOtherSources.Top;
    Width:=25;
    Height:=edtOtherSources.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  {------------------------------------------------------------}

  grpLibraries := TGroupBox.Create(Self);
  with grpLibraries do
  begin
    Parent := PathPage;
    Top := grpOtherSources.Top + grpOtherSources.Height + 5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOLibraries ;
  end;

  edtLibraries := TEdit.Create(grpLibraries);
  with edtLibraries do
  begin
    Parent := grpLibraries;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  LibrariesPathEditBtn:=TPathEditorButton.Create(Self);
  with LibrariesPathEditBtn do begin
    Name:='LibrariesPathEditBtn';
    Parent:=grpLibraries;
    Left:=edtLibraries.Left+edtLibraries.Width+3;
    Top:=edtLibraries.Top;
    Width:=25;
    Height:=edtLibraries.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  {------------------------------------------------------------}

  grpCompiler := TGroupBox.Create(Self);
  with grpCompiler do
  begin
    Parent := PathPage;
    Top := grpLibraries.Top + grpLibraries.Height + 5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgToFPCPath ;
  end;

  edtCompiler := TEdit.Create(grpCompiler);
  with edtCompiler do
  begin
    Parent := grpCompiler;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-2*Left;
    Text := '';
  end;
  
  {------------------------------------------------------------}

  grpUnitOutputDir := TGroupBox.Create(Self);
  with grpUnitOutputDir do
  begin
    Parent := PathPage;
    Top := grpCompiler.Top + grpCompiler.Height + 5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgUnitOutp ;
  end;

  edtUnitOutputDir := TEdit.Create(grpCompiler);
  with edtUnitOutputDir do
  begin
    Parent := grpUnitOutputDir;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-2*Left;
    Text := '';
  end;
  
  {------------------------------------------------------------}
  
  LCLWidgetTypeRadioGroup:=TRadioGroup.Create(Self);
  with LCLWidgetTypeRadioGroup do begin
    Name:='LCLWidgetTypeRadioGroup';
    Parent := PathPage;
    Left := grpOtherUnits.Left;
    Top:=grpUnitOutputDir.Top+grpUnitOutputDir.Height+5;
    Width:=300;
    Height:=45;
    Caption:=dlgLCLWidgetType ;
    with Items do begin
      Add('gnome');
      Add('gtk');
      Add('gtk2');
      Add('win32');
    end;
    Columns:=Items.Count;
    ItemIndex:=1;
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupButtonBar                                          }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupButtonBar;
begin
  // Setup the Button Bar
  Assert(False, 'Trace:Setting up compiler options button bar');

  btnApply := TButton.Create(Self);
  with btnApply do
  begin
    Parent := Self;
    Width := 70;
    Height := 23; 
    Top := Self.Height - btnApply.Height - 15;
    Left := Self.Width - btnApply.Width - 10;
    Caption := dlgButApply;
    OnClick := @ButtonApplyClicked;
  end;

  btnCancel := TButton.Create(Self);
  with btnCancel do
  begin
    Parent := Self;
    Width := 70;
    Height := 23; 
    Top := Self.Height - btnCancel.Height - 15;
    Left := btnApply.Left - btnCancel.Width - 5;
    Caption := dlgCancel ;
    OnClick := @ButtonCancelClicked;
  end;

  btnOK := TButton.Create(Self);
  with btnOK do
  begin
    Parent := Self;
    Width := 70;
    Height := 23; 
    Top := Self.Height - btnOK.Height - 15;
    Left := btnCancel.Left - btnOK.Width - 5;
    Caption := 'OK';
    OnClick := @ButtonOKClicked;
  end;

  btnTest := TButton.Create(Self);
  with btnTest do
  begin
    Parent := Self;
    Width := 110;
    Height := 23; 
    Top := Self.Height - btnTest.Height - 15;
    Left := btnOK.Left - btnTest.Width - 5;
    Caption := dlgCOShowOptions;
    OnClick := @ButtonTestClicked;
  end;
end;

procedure TfrmCompilerOptions.chkAdditionalConfigFileClick(Sender: TObject);
begin
  edtConfigPath.Enabled:=chkAdditionalConfigFile.Checked;
end;

procedure TfrmCompilerOptions.PathEditBtnClick(Sender: TObject);
var AButton: TPathEditorButton;
  OldPath, Templates: string;
begin
  if Sender is TPathEditorButton then begin
    AButton:=TPathEditorButton(Sender);
    if AButton=OtherUnitsPathEditBtn then begin
      OldPath:=edtOtherUnits.Text;
      Templates:=
            '$(LazarusDir)/lcl/units'
          +';$(LazarusDir)/lcl/units/$(LCLWidgetType)'
          +';$(LazarusDir)/components/units'
          +';$(LazarusDir)/components/custom';
    end else
    if AButton=IncludeFilesPathEditBtn then begin
      OldPath:=edtIncludeFiles.Text;
      Templates:='include';
    end else
    if AButton=OtherSourcesPathEditBtn then begin
      OldPath:=edtOtherSources.Text;
      Templates:=SetDirSeparators(
            '$(LazarusDir)/lcl'
          +';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)'
          +';$(LazarusDir)/components/synedit'
          +';$(LazarusDir)/components/codetools');
    end else
    if AButton=LibrariesPathEditBtn then begin
      OldPath:=edtLibraries.Text;
      Templates:='';
    end;
    AButton.CurrentPathEditor.Path:=OldPath;
    AButton.CurrentPathEditor.Templates:=SetDirSeparators(Templates);
  end;
end;

procedure TfrmCompilerOptions.PathEditBtnExecuted(Sender: TObject);
var AButton: TPathEditorButton;
  NewPath: string;
begin
  if Sender is TPathEditorButton then begin
    AButton:=TPathEditorButton(Sender);
    if AButton.CurrentPathEditor.ModalResult<>mrOk then exit;
    NewPath:=AButton.CurrentPathEditor.Path;
    if AButton=OtherUnitsPathEditBtn then begin
      edtOtherUnits.Text:=NewPath;
    end else
    if AButton=IncludeFilesPathEditBtn then begin
      edtIncludeFiles.Text:=NewPath;
    end else
    if AButton=OtherSourcesPathEditBtn then begin
      edtOtherSources.Text:=NewPath;
    end else
    if AButton=LibrariesPathEditBtn then begin
      edtLibraries.Text:=NewPath;
    end;
  end;
end;

procedure TfrmCompilerOptions.frmCompilerOptionsClose(Sender: TObject;
  var Action: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TfrmCompilerOptions.frmCompilerOptionsResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
begin
  with nbMain do
    SetBounds(0,0,Parent.ClientWidth,Parent.ClientHeight-45);

  x:=Width - 10;
  y:=Height - btnApply.Height - 15;
  with btnApply do
    SetBounds(x-70,y,70,Height);
  dec(x,btnApply.Width+10);
  
  with btnCancel do
    SetBounds(x-70,y,70,Height);
  dec(x,btnCancel.Width+10);

  with btnOK do
    SetBounds(x-70,y,70,Height);
  dec(x,btnOk.Width+10);

  with btnTest do
    SetBounds(x-120,y,120,Height);
end;

function TfrmCompilerOptions.GetOtherSourcePath: string;
begin
  Result:=edtOtherSources.Text;
end;

procedure TfrmCompilerOptions.SetOtherSourcePath(const AValue: string);
begin
  edtOtherSources.Text:=AValue;
end;

procedure TfrmCompilerOptions.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  btnOk.Enabled:=not FReadOnly;
  btnApply.Enabled:=not FReadOnly;
end;

  
{ TAdditionalCompilerOptions }

procedure TAdditionalCompilerOptions.SetCustomOptions(const AValue: string);
begin
  if FCustomOptions=AValue then exit;
  FCustomOptions:=AValue;
  ParsedOpts.SetUnparsedValue(pcosCustomOptions,fCustomOptions);
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
  FParsedOpts:=TParsedCompilerOptions.Create;
  Clear;
end;

destructor TAdditionalCompilerOptions.Destroy;
begin
  FreeThenNil(FParsedOpts);
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
  const Path: string);
begin
  Clear;
  CustomOptions:=XMLConfig.GetValue(Path+'CustomOptions/Value','');
  IncludePath:=XMLConfig.GetValue(Path+'IncludePath/Value','');
  LibraryPath:=XMLConfig.GetValue(Path+'LibraryPath/Value','');
  LinkerOptions:=XMLConfig.GetValue(Path+'LinkerOptions/Value','');
  ObjectPath:=XMLConfig.GetValue(Path+'ObjectPath/Value','');
  UnitPath:=XMLConfig.GetValue(Path+'UnitPath/Value','');
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
end;

function TAdditionalCompilerOptions.GetOwnerName: string;
begin
  if fOwner<>nil then
    Result:=fOwner.Classname
  else
    Result:='Has no owner';
end;

{ TParsedCompilerOptions }

constructor TParsedCompilerOptions.Create;
begin
  Clear;
end;

function TParsedCompilerOptions.GetParsedValue(Option: TParsedCompilerOptString
  ): string;
var
  BaseDirectory: String;
  s: String;
begin
  if ParsedStamp[Option]<>CompilerParseStamp then begin
    // parse locally
    if Assigned(OnLocalSubstitute) then
      s:=OnLocalSubstitute(UnparsedValues[Option])
    else
      s:=UnparsedValues[Option];
    // parse globally
    s:=ParseString(Self,s);
    // improve
    if Option=pcosBaseDir then
      // base directory (append path)
      s:=AppendPathDelim(TrimFilename(s))
    else if Option in ParsedCompilerFilenames then begin
      // make filename absolute
      s:=TrimFilename(s);
      BaseDirectory:=GetParsedValue(pcosBaseDir);
      if (BaseDirectory<>'') and (not FilenameIsAbsolute(s)) then
        s:=BaseDirectory+s;
    end
    else if Option in ParsedCompilerDirectories then begin
      // make directory absolute
      s:=TrimFilename(s);
      BaseDirectory:=GetParsedValue(pcosBaseDir);
      if (BaseDirectory<>'') and (not FilenameIsAbsolute(s)) then
        s:=BaseDirectory+s;
      s:=AppendPathDelim(s);
    end
    else if Option in ParsedCompilerSearchPaths then begin
      // make search paths absolute
      BaseDirectory:=GetParsedValue(pcosBaseDir);
      s:=TrimSearchPath(s,BaseDirectory);
    end;
    ParsedValues[Option]:=s;
    ParsedStamp[Option]:=CompilerParseStamp;
  end;
  Result:=ParsedValues[Option];
end;

procedure TParsedCompilerOptions.SetUnparsedValue(
  Option: TParsedCompilerOptString; const NewValue: string);
begin
  if NewValue=UnparsedValues[Option] then exit;
  if InvalidateGraphOnChange then IncreaseCompilerGraphStamp;
  if Option=pcosBaseDir then
    InvalidateFiles
  else
    ParsedStamp[Option]:=InvalidParseStamp;
  UnparsedValues[Option]:=NewValue;
end;

procedure TParsedCompilerOptions.Clear;
var
  Option: TParsedCompilerOptString;
begin
  InvalidateAll;
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
  begin
    ParsedValues[Option]:='';
    UnparsedValues[Option]:='';
  end;
end;

procedure TParsedCompilerOptions.InvalidateAll;
var
  Option: TParsedCompilerOptString;
begin
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
    ParsedStamp[Option]:=InvalidParseStamp;
end;

procedure TParsedCompilerOptions.InvalidateFiles;
var
  Option: TParsedCompilerOptString;
begin
  for Option:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
    if (Option in ParsedCompilerFiles) then
      ParsedStamp[Option]:=InvalidParseStamp;
end;

{ TCompilerOptions }

procedure TCompilerOptions.Clear;
begin
  inherited Clear;
end;

initialization
  CompilerParseStamp:=1;
  CompilerGraphStamp:=1;

end.

