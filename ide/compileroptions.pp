{ /***************************************************************************
                          compiler.pp  -  Lazarus IDE unit
                          --------------------------------
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
  Forms, Classes, SysUtils, ComCtrls, Buttons, StdCtrls, ExtCtrls, LazConf,
  Laz_XMLCfg, FileCtrl, Dialogs, Controls, PathEditorDlg, IDEProcs,
  LazarusIDEStrConsts;

type
  { Compiler Options object used to hold the compiler options }
  TCompilerOptions = class(TObject)
  private
    fOptionsString: String;
    xmlconfig: TXMLConfig;

    fProjectFile: String;
    fTargetFilename: string;
    fLoaded: Boolean;

    fStyle: Integer;
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

    fUnitStyle: Integer;
    fIOChecks: Boolean;
    fRangeChecks: Boolean;
    fOverflowChecks: Boolean;
    fStackChecks: Boolean;
    fHeapSize: LongInt;
    fGenerate: Integer;
    fTargetProc: Integer;
    fVarsInReg: Boolean;
    fUncertainOpt: Boolean;
    fOptLevel: Integer;
    fTargetOS: String;
    
    fGenDebugInfo: Boolean;
    fGenDebugDBX: Boolean;
    fUseLineInfoUnit: Boolean;
    fUseHeaptrc: Boolean;
    fGenGProfCode: Boolean;
    fStripSymbols: Boolean;
    fLinkStyle: Integer;
    fPassLinkerOpt: Boolean;
    fLinkerOptions: String;
    
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
    fDontUseConfigFile: Boolean;
    fAdditionalConfigFile: Boolean;
    fConfigFilePath: String;
    fStopAfterErrCount: integer;
    
    fIncludeFiles: String;
    fLibraries: String;
    fOtherUnitFiles: String;
    fCompilerPath: String;
    fUnitOutputDir: string;
    fLCLWidgetType: string;

    procedure LoadTheCompilerOptions;
    procedure SaveTheCompilerOptions;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadCompilerOptions(UseExistingFile: Boolean);
    procedure SaveCompilerOptions(UseExistingFile: Boolean);
    procedure Assign(CompOpts: TCompilerOptions);
    function MakeOptionsString: String;
    function MakeOptionsString(const MainSourceFileName: string): String;
    function ParseSearchPaths(const switch, paths: String): String;
    function ParseOptions(const Delim, Switch, OptionStr: string): string;
    function GetXMLConfigPath: String;
    procedure Clear;
    function CreateTargetFilename(const MainSourceFileName: string): string;

    property ProjectFile: String read fProjectFile write fProjectFile;
    property TargetFilename: String read fTargetFilename write fTargetFilename;
    property XMLConfigFile: TXMLConfig read xmlconfig write xmlconfig;
    property Loaded: Boolean read fLoaded write fLoaded;
    
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

    property UnitStyle: Integer read fUnitStyle write fUnitStyle;
    property IOChecks: Boolean read fIOChecks write fIOChecks;
    property RangeChecks: Boolean read fRangeChecks write fRangeChecks;
    property OverflowChecks: Boolean read fOverflowChecks write fOverflowChecks;
    property StackChecks: Boolean read fStackChecks write fStackChecks;
    property HeapSize: Integer read fHeapSize write fHeapSize;
    property Generate: Integer read fGenerate write fGenerate;
    property TargetProcessor: Integer read fTargetProc write fTargetProc;
    property VariablesInRegisters: Boolean read fVarsInReg write fVarsInReg;
    property UncertainOptimizations: Boolean read fUncertainOpt write fUncertainOpt;
    property OptimizationLevel: Integer read fOptLevel write fOptLevel;
    property TargetOS: string read fTargetOS write fTargetOS;
    
    property GenerateDebugInfo: Boolean read fGenDebugInfo write fGenDebugInfo;
    property GenerateDebugDBX: Boolean read fGenDebugDBX write fGenDebugDBX;
    property UseLineInfoUnit: Boolean read fUseLineInfoUnit write fUseLineInfoUnit;
    property UseHeaptrc: Boolean read fUseHeaptrc write fUseHeaptrc;
    property GenGProfCode: Boolean read fGenGProfCode write fGenGProfCode;
    property StripSymbols: Boolean read fStripSymbols write fStripSymbols;
    property LinkStyle: Integer read fLinkStyle write fLinkStyle;
    property PassLinkerOptions: Boolean read fPassLinkerOpt write fPassLinkerOpt;
    property LinkerOptions: String read fLinkerOptions write fLinkerOptions;
    
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
    property DontUseConfigFile: Boolean read fDontUseConfigFile write fDontUseConfigFile;
    property AdditionalConfigFile: Boolean read fAdditionalConfigFile write fAdditionalConfigFile;
    property ConfigFilePath: String read fConfigFilePath write fConfigFilePath;
    property StopAfterErrCount: integer 
      read fStopAfterErrCount write fStopAfterErrCount;
    
    property IncludeFiles: String read fIncludeFiles write fIncludeFiles;
    property Libraries: String read fLibraries write fLibraries;
    property OtherUnitFiles: String read fOtherUnitFiles write fOtherUnitFiles;
    property CompilerPath: String read fCompilerPath write fCompilerPath;
    property UnitOutputDirectory: string read fUnitOutputDir write fUnitOutputDir;
    property LCLWidgetType: string read fLCLWidgetType write fLCLWidgetType;
  end;


  { Compiler options form }
  
  TfrmCompilerOptions = class(TForm)
    nbMain: TNotebook;
    //bvlButtonBar: TBevel;

    { Search Paths Controls }
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
    //bvlOptSepLine: TBevel;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;

    TargetOSRadioGroup: TRadioGroup;

    { Linking Controls }
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

    grpOptions: TGroupBox;
    chkOptionsLinkOpt: TCheckBox;
    edtOptionsLinkOpt: TEdit;

    { Other Controls }
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

    grpConfigFile: TGroupBox;
    chkConfigFile: TCheckBox;
    chkAdditionalConfigFile: TCheckBox;
    edtConfigPath: TEdit;
    
    grpErrorCnt: TGroupBox;
    edtErrorCnt: TEdit;

    { Buttons }
    btnTest: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;

    { Other variables }
//    fPath: String;

    { Procedures }
    procedure chkAdditionalConfigFileClick(Sender: TObject);
    procedure CreateForm(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
  private
    procedure SetupSearchPathsTab(Page: integer);
    procedure SetupParsingTab(Page: integer);
    procedure SetupCodeGenerationTab(Page: integer);
    procedure SetupLinkingTab(Page: integer);
    procedure SetupOtherTab(Page: integer);
    procedure SetupButtonBar;
  private
    function GetOtherSourcePath: string;
    procedure SetOtherSourcePath(const AValue: string);
  public
    CompilerOpts: TCompilerOptions;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ButtonOKClicked(Sender: TObject);
    procedure ButtonCancelClicked(Sender: TObject);
    procedure ButtonApplyClicked(Sender: TObject);
    procedure ButtonTestClicked(Sender: TObject);

    procedure GetCompilerOptions;
    procedure PutCompilerOptions;
    
    property OtherSourcePath: string
      read GetOtherSourcePath write SetOtherSourcePath;
  end;

var
  frmCompilerOptions: TfrmCompilerOptions;

implementation

const
  Config_Filename = 'compileroptions.xml';

{------------------------------------------------------------------------------}
{  TCompilerOptions Constructor                                                }
{------------------------------------------------------------------------------}
constructor TCompilerOptions.Create;
begin
  inherited Create;
  Assert(False, 'Trace:Compiler Options Class Created');

  Clear;
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCompilerOptions.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions LoadCompilerOptions                                     }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.LoadCompilerOptions(UseExistingFile: Boolean);
var
  confPath: String;
begin
  if (UseExistingFile and (XMLConfigFile <> nil)) then
  begin
    LoadTheCompilerOptions;
  end
  else
  begin
    confPath := GetXMLConfigPath;
    XMLConfigFile := TXMLConfig.Create(SetDirSeparators(confPath));
    LoadTheCompilerOptions;
    XMLConfigFile.Free;
    XMLConfigFile := nil;
  end;
  fLoaded := true;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions LoadTheCompilerOptions                                  }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.LoadTheCompilerOptions;
begin
  { Load the compiler options from the XML file }

  { Target }
  TargetFilename := XMLConfigFile.GetValue('CompilerOptions/Target/Filename/Value', '');

  { Parsing }
  Style := XMLConfigFile.GetValue('CompilerOptions/Parsing/Style/Value', 1);

  D2Extensions := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/D2Extensions/Value', true);
  CStyleOperators := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/CStyleOperator/Value', true);
  IncludeAssertionCode := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/IncludeAssertionCode/Value', true);
  AllowLabel := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/AllowLabel/Value', true);
  CPPInline := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/CPPInline/Value', true);
  CStyleMacros := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/CStyleMacros/Value', false);
  TPCompatible := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/TPCompatible/Value', false);
  InitConstructor := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/InitConstructor/Value', false);
  StaticKeyword := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/StaticKeyword/Value', false);
  DelphiCompat := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/DelphiCompat/Value', false);
  UseAnsiStrings := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/UseAnsiStrings/Value', false);
  GPCCompat := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/GPCCompat/Value', false);

  { CodeGeneration }
  UnitStyle := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/UnitStyle/Value', 1);
  IOChecks := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Checks/IOChecks/Value', false);
  RangeChecks := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Checks/RangeChecks/Value', false);
  OverflowChecks := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Checks/OverflowChecks/Value', false);
  StackChecks := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Checks/StackChecks/Value', false);
  HeapSize := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/HeapSize/Value', 8000000);
  Generate := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Generate/Value', 1);
  TargetProcessor := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/TargetProcessor/Value', 1);
  VariablesInRegisters := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Optimizations/VariablesInRegisters/Value', false);
  UncertainOptimizations := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Optimizations/UncertainOptimizations/Value', false);
  OptimizationLevel := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/Optimizations/OptimizationLevel/Value', 1);
  TargetOS := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/TargetOS/Value', 'linux');

  { Linking }
  GenerateDebugInfo := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/GenerateDebugInfo/Value', false);
  GenerateDebugDBX := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/GenerateDebugDBX/Value', false);
  UseLineInfoUnit := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/UseLineInfoUnit/Value', true);
  UseHeaptrc := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/UseHeaptrc/Value', false);
  GenGProfCode := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/GenGProfCode/Value', false);
  StripSymbols := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/StripSymbols/Value', false);
  LinkStyle := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/LinkStyle/Value', 1);
  PassLinkerOptions := XMLConfigFile.GetValue('CompilerOptions/Linking/Options/PassLinkerOptions/Value', false);
  LinkerOptions := XMLConfigFile.GetValue('CompilerOptions/Linking/Options/LinkerOptions/Value', '');
    
  { Other }
  ShowErrors := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowErrors/Value', true);
  ShowWarn := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowWarn/Value', true);
  ShowNotes := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowNotes/Value', true);
  ShowHints := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowHints/Value', true);
  ShowGenInfo := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowGenInfo/Value', true);
  ShowLineNum := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShoLineNum/Value', false);
  ShowAll := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowAll/Value', false);
  ShowAllProcsOnError := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowAllProcsOnError/Value', false);
  ShowDebugInfo := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowDebugInfo/Value', false);
  ShowUsedFiles := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowUsedFiles/Value', false);
  ShowTriedFiles := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowTriedFiles/Value', false);
  ShowDefMacros := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowDefMacros/Value', false);
  ShowCompProc := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowCompProc/Value', false);
  ShowCond := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowCond/Value', false);
  ShowNothing := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowNothing/Value', false);
  ShowHintsForUnusedProjectUnits := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowHintsForUnusedProjectUnits/Value', false);
  WriteFPCLogo := XMLConfigFile.GetValue('CompilerOptions/Other/WriteFPCLogo/Value', true);
  DontUseConfigFile := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/DontUseConfigFile/Value', false);
  AdditionalConfigFile := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/AdditionalConfigFile/Value', false);
  ConfigFilePath := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/ConfigFilePath/Value', './fpc.cfg');
  StopAfterErrCount := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/StopAfterErrCount/Value', 1);

  { SearchPaths }
  IncludeFiles := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/IncludeFiles/Value', '');
  Libraries := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/Libraries/Value', '');
  OtherUnitFiles := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', '');
  CompilerPath := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/CompilerPath/Value', '/opt/fpc/ppc386');
  UnitOutputDirectory := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', '');
  LCLWidgetType := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/LCLWidgetType/Value', 'gtk');
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SaveCompilerOptions                                     }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.SaveCompilerOptions(UseExistingFile: Boolean);
var
  confPath: String;
begin
  if ((UseExistingFile) and (XMLConfigFile <> nil)) then
  begin
    SaveTheCompilerOptions;
  end
  else
  begin
    confPath := GetXMLConfigPath;
    XMLConfigFile := TXMLConfig.Create(SetDirSeparators(confPath));
    SaveTheCompilerOptions;
    XMLConfigFile.Free;
    XMLConfigFile := nil;
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SaveTheCompilerOptions                                  }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.SaveTheCompilerOptions;
begin
  { Save the compiler options to the XML file }
  
  { Target }
  XMLConfigFile.SetValue('CompilerOptions/Target/Filename/Value', TargetFilename);

  { Parsing }
  XMLConfigFile.SetValue('CompilerOptions/Parsing/Style/Value', Style);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/D2Extensions/Value', D2Extensions);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/CStyleOperator/Value', CStyleOperators);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/IncludeAssertionCode/Value', IncludeAssertionCode);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/AllowLabel/Value', AllowLabel);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/CPPInline/Value', CPPInline);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/CStyleMacros/Value', CStyleMacros);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/TPCompatible/Value', TPCompatible);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/InitConstructor/Value', InitConstructor);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/StaticKeyword/Value', StaticKeyword);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/DelphiCompat/Value', DelphiCompat);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/UseAnsiStrings/Value', UseAnsiStrings);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/GPCCompat/Value', GPCCompat);
  
  { CodeGeneration }
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/UnitStyle/Value', UnitStyle);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Checks/IOChecks/Value', IOChecks);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Checks/RangeChecks/Value', RangeChecks);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Checks/OverflowChecks/Value', OverflowChecks);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Checks/StackChecks/Value', StackChecks);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/HeapSize/Value', HeapSize);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Generate/Value', Generate);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/TargetProcessor/Value', TargetProcessor);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Optimizations/VariablesInRegisters/Value', VariablesInRegisters);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Optimizations/UncertainOptimizations/Value', UncertainOptimizations);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/Optimizations/OptimizationLevel/Value', OptimizationLevel);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/TargetOS/Value', TargetOS);

  { Linking }
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/GenerateDebugInfo/Value', GenerateDebugInfo);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/GenerateDebugDBX/Value', GenerateDebugDBX);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/UseLineInfoUnit/Value', UseLineInfoUnit);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/UseHeaptrc/Value', UseHeaptrc);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/GenGProfCode/Value', GenGProfCode);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/StripSymbols/Value', StripSymbols);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/LinkStyle/Value', LinkStyle);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Options/PassLinkerOptions/Value', PassLinkerOptions);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Options/LinkerOptions/Value', LinkerOptions);
    
  { Other }
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowErrors/Value', ShowErrors);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowWarn/Value', ShowWarn);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowNotes/Value', ShowNotes);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowHints/Value', ShowHints);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowGenInfo/Value', ShowGenInfo);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShoLineNum/Value', ShowLineNum);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowAll/Value', ShowAll);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowAllProcsOnError/Value', ShowAllProcsOnError);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowDebugInfo/Value', ShowDebugInfo);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowUsedFiles/Value', ShowUsedFiles);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowTriedFiles/Value', ShowTriedFiles);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowDefMacros/Value', ShowDefMacros);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowCompProc/Value', ShowCompProc);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowCond/Value', ShowCond);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowNothing/Value', ShowNothing);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowHintsForUnusedProjectUnits/Value', ShowHintsForUnusedProjectUnits);
  XMLConfigFile.SetValue('CompilerOptions/Other/WriteFPCLogo/Value', WriteFPCLogo);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/DontUseConfigFile/Value', DontUseConfigFile);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/AdditionalConfigFile/Value', AdditionalConfigFile);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/ConfigFilePath/Value', ConfigFilePath);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/StopAfterErrCount/Value', StopAfterErrCount);

  { SearchPaths }
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/IncludeFiles/Value', IncludeFiles);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/Libraries/Value', Libraries);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', OtherUnitFiles);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/CompilerPath/Value', CompilerPath);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', UnitOutputDirectory);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/LCLWidgetType/Value', LCLWidgetType);

  XMLConfigFile.Flush;
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions CreateTargetFilename                                       }
{------------------------------------------------------------------------------}
function TCompilerOptions.CreateTargetFilename(
  const MainSourceFileName: string): string;
var Ext: string;
begin
  if (TargetFilename <> '') then begin
    Result:=ExtractFilePath(MainSourceFileName)+TargetFilename;
  end else begin
    if MainSourceFileName<>'' then begin
      Result:=ExtractFileName(MainSourceFileName);
      Ext:=ExtractFileExt(Result);
      Result:=copy(Result,1,length(Result)-length(Ext));
      Result:=lowercase(Result);
      if fTargetOS = 'win32'
         then Result:=Result+'.exe';
      Result:=ExtractFilePath(MainSourceFileName)+Result;
    end else
      Result:='';
  end;
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions MakeOptionsString                                          }
{------------------------------------------------------------------------------}
function TCompilerOptions.MakeOptionsString: String;
begin
  Result:=MakeOptionsString('')
end;

function TCompilerOptions.MakeOptionsString(
  const MainSourceFilename: string): String;
var
  switches, tempsw: String;
begin
  switches := '';

  { Get all the options and create a string that can be passed to the compiler }
  
  { options of ppc386 1.0.5 :
  
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
      -Ch<n>     <n> bytes heap (between 1023 and 67107840)
      -Ci        IO-checking
      -Cn        omit linking stage
      -Co        check overflow of integer operations
      -Cr        range checking
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
  -s     don't call assembler and linker (only with -a)
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
  -X     executable options:
      -Xc        link with the c library
      -Xs        strip all symbols from executable
      -XD        try to link dynamic          (defines FPC_LINK_DYNAMIC)
      -XS        try to link static (default) (defines FPC_LINK_STATIC)
      -XX        try to link smart            (defines FPC_LINK_SMART)

  Processor specific options:
  -A<x>  output format:
      -Aas       assemble using GNU AS
      -Aasaout   assemble using GNU AS for aout (Go32v1)
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
      -O3        level 3 optimizations (same as -O2u)
      -Op<x>     target processor:
         -Op1  set target processor to 386/486
         -Op2  set target processor to Pentium/PentiumMMX (tm)
         -Op3  set target processor to PPro/PII/c6x86/K6 (tm)
  -T<x>  Target operating system:
      -TGO32V1   version 1 of DJ Delorie DOS extender
      -TGO32V2   version 2 of DJ Delorie DOS extender
      -TLINUX    Linux
      -TOS2      OS/2 2.x
      -TSUNOS    SunOS/Solaris
      -TWin32    Windows 32 Bit
      -TBeOS     BeOS
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

  { Unit Style }
  { UnitStyle   '' = Static     'D' = Dynamic   'X' = smart linked }
  case (UnitStyle) of
    1: ;
    2: switches := switches + ' -CD';
    3: switches := switches + ' -CX';
  end;

  { Checks }
  tempsw := '';

  if (IOChecks) then
    tempsw := tempsw + 'i';
  if (RangeChecks) then
    tempsw := tempsw + 'r';
  if (OverflowChecks) then
    tempsw := tempsw + 'o';
  if (StackChecks) then
    tempsw := tempsw + 't';

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

  { Generate }
  { Generate    G = faster g = smaller  }
  case (Generate) of
    1:  switches := switches + 'G';
    2:  switches := switches + 'g';
  end;

  { Optimizations }
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

  { Target Processor }
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
       WIN32 = Windows 32 bit. }
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


  if PassLinkerOptions and (LinkerOptions<>'') then
    switches := switches + ' ' + ParseOptions(' ','-k', LinkerOptions);

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

  { ------------- Search Paths Tab ---------------- }
  if (IncludeFiles <> '') then
    switches := switches + ' ' + ParseSearchPaths('-Fi', IncludeFiles);

  if (Libraries <> '') then
    switches := switches + ' ' + ParseSearchPaths('-Fl', Libraries);

  if (OtherUnitFiles <> '') then
    switches := switches + ' ' + ParseSearchPaths('-Fu', OtherUnitFiles);

  { CompilerPath - Nothing needs to be done with this one }
  
  { Unit output directory }
  if UnitOutputDirectory<>'' then
    switches := switches + ' '+PrepareCmdLineOption('-FU'+UnitOutputDirectory);

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
  
  -s = Do not call assembler or linker. Write ppas.bat/ppas.sh script.

  -Xc = Link with C library (LINUX only)
       
}
  if (TargetFilename<>'') or (MainSourceFilename<>'') 
  or (UnitOutputDirectory<>'') then begin
    tempsw:=CreateTargetFilename(MainSourceFilename);
    if (tempsw <> ChangeFileExt(MainSourceFilename,''))
    or (UnitOutputDirectory<>'') then
      switches := switches + ' '+PrepareCmdLineOption('-o' + tempsw);
  end;

  fOptionsString := switches;
  Result := fOptionsString;
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions ParseSearchPaths                                           }
{------------------------------------------------------------------------------}
function TCompilerOptions.ParseSearchPaths(const switch, paths: String): String;
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
  TCompilerOptions ParseOptions
 ------------------------------------------------------------------------------}
function TCompilerOptions.ParseOptions(const Delim, Switch, 
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
  TCompilerOptions GetXMLConfigPath
 ------------------------------------------------------------------------------}
function TCompilerOptions.GetXMLConfigPath: String;
var
  fn: String;
begin
  // Setup the filename to write to
  if (ProjectFile <> '') then
    fn := ProjectFile
  else
    fn := Config_Filename;
  Result := GetPrimaryConfigPath + '/' + fn;
  CopySecondaryConfigFile(fn);
end;


{------------------------------------------------------------------------------}
{  TCompilerOptions Clear                                                      }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.Clear;
begin
  fOptionsString := '';
  fLoaded := false;

  { Set Defaults }
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
    
  fGenDebugInfo := false;
  fGenDebugDBX := false;
  fUseLineInfoUnit := true;
  fUseHeaptrc := false;
  fGenGProfCode := false;
  fStripSymbols := false;
  fLinkStyle := 1;
  fPassLinkerOpt := false;
  fLinkerOptions := '';
    
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
  fDontUseConfigFile := false;
  fAdditionalConfigFile := false;
  fConfigFilePath := './fpc.cfg';
  fStopAfterErrCount := 1;
    
  fIncludeFiles := '';
  fLibraries := '';
  fOtherUnitFiles := '';
  fCompilerPath := '/opt/fpc/ppc386';
  fUnitOutputDir := '';
  fLCLWidgetType := 'gtk';
end;

procedure TCompilerOptions.Assign(CompOpts: TCompilerOptions);
begin
  fOptionsString := CompOpts.fOptionsString;
  fLoaded := CompOpts.fLoaded;

  { Set Defaults }
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

  fUnitStyle := CompOpts.fUnitStyle;
  fIOChecks := CompOpts.fIOChecks;
  fRangeChecks := CompOpts.fRangeChecks;
  fOverflowChecks := CompOpts.fOverflowChecks;
  fStackChecks := CompOpts.fStackChecks;
  fHeapSize := CompOpts.fHeapSize;
  fGenerate := CompOpts.fGenerate;
  fTargetProc := CompOpts.fTargetProc;
  fVarsInReg := CompOpts.fVarsInReg;
  fUncertainOpt := CompOpts.fUncertainOpt;
  fOptLevel := CompOpts.fOptLevel;
  fTargetOS := CompOpts.fTargetOS;

  fGenDebugInfo := CompOpts.fGenDebugInfo;
  fGenDebugDBX := CompOpts.fGenDebugDBX;
  fUseLineInfoUnit := CompOpts.fUseLineInfoUnit;
  fUseHeaptrc := CompOpts.fUseHeaptrc;
  fGenGProfCode := CompOpts.fGenGProfCode;
  fStripSymbols := CompOpts.fStripSymbols;
  fLinkStyle := CompOpts.fLinkStyle;
  fPassLinkerOpt := CompOpts.fPassLinkerOpt;
  fLinkerOptions := CompOpts.fLinkerOptions;

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
  fDontUseConfigFile := CompOpts.fDontUseConfigFile;
  fAdditionalConfigFile := CompOpts.fAdditionalConfigFile;
  fConfigFilePath := CompOpts.fConfigFilePath;
  fStopAfterErrCount := CompOpts.fStopAfterErrCount;

  fIncludeFiles := CompOpts.fIncludeFiles;
  fLibraries := CompOpts.fLibraries;
  fOtherUnitFiles := CompOpts.fOtherUnitFiles;
  fCompilerPath := CompOpts.fCompilerPath;
  fUnitOutputDir := CompOpts.fUnitOutputDir;
  
  fLCLWidgetType := CompOpts.fLCLWidgetType;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Constructor                                             }
{------------------------------------------------------------------------------}
constructor TfrmCompilerOptions.Create(AOwner: TComponent);
var Page: integer;
begin
  inherited Create(AOwner);

  Assert(False, 'Trace:Compiler Options Form Created');
  SetBounds((Screen.Width-440) div 2,(Screen.Height-500) div 2,435,480);
  Caption := dlgCompilerOptions ;
  OnShow := @CreateForm;
  
  nbMain := TNotebook.Create(Self);
  nbMain.Parent := Self;
  nbMain.Height := Height - 50;
  nbMain.Width := Width - 4;
  nbmain.Top := 0;
  nbmain.Left := 0;

  // Add the pages 
  if nbMain.PageCount>0 then
    nbMain.Pages[0] := dlgSearchPaths 
  else
    nbMain.Pages.Add(dlgSearchPaths );
  nbMain.Pages.Add(dlgCOParsing );
  nbMain.Pages.Add(dlgCodeGeneration );
  nbMain.Pages.Add(dlgCOLinking );
  nbMain.Pages.Add(dlgCOOther );

{
  bvlButtonBar := TBevel.Create(Self);
  bvlButtonBar.Parent := Self;
  bvlButtonBar.Height := 2;
  bvlButtonBar.Width := Width;
  bvlButtonBar.Top := Height - 30;
  bvlButtonBar.Left := 0;
}

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

  { Other Tab }
  SetupOtherTab(Page);
  inc(Page);

  { Bottom Buttons }
  SetupButtonBar;

  { Show everything }
  nbMain.Show;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Destructor                                              }
{------------------------------------------------------------------------------}
destructor TfrmCompilerOptions.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions FormOnShow Event                                        }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.CreateForm(Sender: TObject);
begin

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
  Assert(False, 'Trace:Apply compiler options changes');

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

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions GetCompilerOptions                                      }
{------------------------------------------------------------------------------}
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
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions PutCompilerOptions                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.PutCompilerOptions;
var
  code: LongInt;
  hs: LongInt;
  i: integer;
begin
  { Put the compiler options into the TCompilerOptions class to be saved }

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
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupParsingTab                                         }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupParsingTab(Page: integer);
begin
  // Setup the Parsing Tab
  Assert(False, 'Trace:Setting up compiler options parsing tab');

  grpStyle := TGroupBox.Create(Self);
  with grpStyle do
  begin
    Parent := nbMain.Page[Page];
    Top := 10;
    Left := 10;
    Height := 45;
    Width := 300;
    Caption := dlgCOStyle;
    Visible := True;
  end;

  radStyleIntel := TRadioButton.Create(grpStyle);
  with radStyleIntel do
  begin
    Parent := grpStyle;
    Top := 5;
    Left := 10;
    Height := 16;
    Width := 50;
    Caption := 'Intel';//Really we should localize this? :)
    Visible := True;
  end;

  radStyleATT := TRadioButton.Create(grpStyle);
  with radStyleATT do
  begin
    Parent := grpStyle;
    Top := 5;
    Left := 80;
    Height := 16;
    Width := 70;
    Caption := 'AT&T';
    Visible := True;
  end;

  radStyleAsIs := TRadioButton.Create(grpStyle);
  with radStyleAsIs do
  begin
    Parent := grpStyle;
    Top := 5;
    Left := 170;
    Height := 16;
    Width := 100;
    Caption := dlgCOAsIs ;
    Visible := True;
  end;

  grpSymantecChk := TGroupBox.Create(Self);
  with grpSymantecChk do
  begin
    Parent := nbMain.Page[Page];
    Top := 65;
    Left := 10;
    Height := 316;
    Width := Self.ClientWidth-28;
    Caption := dlgSymantecChecking ;
    Visible := True;
  end;

  chkSymD2Ext := TCheckBox.Create(grpSymantecChk);
  with chkSymD2Ext do
  begin
    Parent := grpSymantecChk;
    Caption := dlgDelphi2Ext ;
    Top := 10;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymCOper := TCheckBox.Create(grpSymantecChk);
  with chkSymCOper do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCOCOps;
    Top := 34;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymIncludeAssertions := TCheckBox.Create(grpSymantecChk);
  with chkSymIncludeAssertions do
  begin
    Parent := grpSymantecChk;
    Caption := dlgAssertCode ;
    Top := 58;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;
  
  chkSymAllowLab := TCheckBox.Create(grpSymantecChk);
  with chkSymAllowLab do
  begin
    Parent := grpSymantecChk;
    Caption := dlgLabelGoto ;
    Top := 82;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymCPPInline := TCheckBox.Create(grpSymantecChk);
  with chkSymCPPInline do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCppInline ;
    Top := 106;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymCMacros := TCheckBox.Create(grpSymantecChk);
  with chkSymCMacros do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCMacro;
    Top := 130;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymTP7Compat := TCheckBox.Create(grpSymantecChk);
  with chkSymTP7Compat do
  begin
    Parent := grpSymantecChk;
    Caption := dlgBP7Cptb ;
    Top := 154;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymConstInit := TCheckBox.Create(grpSymantecChk);
  with chkSymConstInit do
  begin
    Parent := grpSymantecChk;
    Caption := dlgInitDoneOnly ;
    Top := 178;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymStaticKwd := TCheckBox.Create(grpSymantecChk);
  with chkSymStaticKwd do
  begin
    Parent := grpSymantecChk;
    Caption := dlgStaticKeyword ;
    Top := 202;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;
  
  chkSymDelphiCompat := TCheckBox.Create(grpSymantecChk);
  with chkSymDelphiCompat do
  begin
    Parent := grpSymantecChk;
    Caption := dlgDeplhiComp;
    Top := 226;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymUseAnsiStrings := TCheckBox.Create(grpSymantecChk);
  with chkSymUseAnsiStrings do
  begin
    Parent := grpSymantecChk;
    Caption := dlgCOAnsiStr ;
    Top := 250;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymGPCCompat := TCheckBox.Create(grpSymantecChk);
  with chkSymGPCCompat do
  begin
    Parent := grpSymantecChk;
    Caption := dlgGPCComp ;
    Top := 274;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;
end;


{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupCodeGenerationTab                                  }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupCodeGenerationTab(Page: integer);
begin
  // Setup the Code Generation Tab
  Assert(False, 'Trace:Setting up compiler options code generation tab');

  grpUnitStyle := TRadioGroup.Create(Self);
  with grpUnitStyle do
  begin
    Parent := nbMain.Page[Page];
    Top := 10;
    Left := 10;
    Height := 70;
    Width := 120;
    Caption := dlgCOUnitStyle ;
    with Items do begin
      Add(dlgStatic );
      Add(dlgDynamic );
      Add(dlgCOSmart );
    end;
    Visible := True;
  end;

  {------------------------------------------------------------}
  grpChecks := TGroupBox.Create(Self);
  with grpChecks do
  begin
    Parent := nbMain.Page[Page];
    Top := 10;
    Left := grpUnitStyle.Left + grpUnitStyle.Width + 10;
    Height := 70;
    Width := 175;
    Caption := dlgCOChecks ;
    Visible := True;
  end;

  chkChecksIO := TCheckBox.Create(grpChecks);
  with chkChecksIO do
  begin
    Parent := grpChecks;
    Caption := 'I/O';
    Top := 8;
    Left := 5;
    Height := 16;
    Width := 70;
    Visible := True;
  end;

  chkChecksRange := TCheckBox.Create(grpChecks);
  with chkChecksRange do
  begin
    Parent := grpChecks;
    Caption := dlgCORange ;
    Top := 8;
    Left := 85;
    Height := 16;
    Width := 90;
    Visible := True;
  end;

  chkChecksOverflow := TCheckBox.Create(grpChecks);
  with chkChecksOverflow do
  begin
    Parent := grpChecks;
    Caption := dlgCOOverflow ;
    Top := 29;
    Left := 5;
    Height := 16;
    Width := 80;
    Visible := True;
  end;

  chkChecksStack := TCheckBox.Create(grpChecks);
  with chkChecksStack do
  begin
    Parent := grpChecks;
    Caption := dlgCOStack ;
    Top := 29;
    Left := 85;
    Height := 16;
    Width := 70;
    Visible := True;
  end;

  {------------------------------------------------------------}

  grpHeapSize := TGroupBox.Create(Self);
  with grpHeapSize do
  begin
    Parent := nbMain.Page[Page];
    Top := 10;
    Left := grpChecks.Left + grpChecks.Width + 10;
    Height := 55;
    Width := 80;
    Caption := dlgHeapSize +':';
    Visible := True;
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
    Visible := True;
  end;

  {------------------------------------------------------------}

  grpGenerate := TGroupBox.Create(Self);
  with grpGenerate do
  begin
    Parent := nbMain.Page[Page];
    Top := grpUnitStyle.Top + grpUnitStyle.Height + 6;
    Left := 10;
    Height := 70;
    Width := 115;
    Caption := dlgCOGenerate ;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;


  {------------------------------------------------------------}

  grpTargetProc := TGroupBox.Create(Self);
  with grpTargetProc do
  begin
    Parent := nbMain.Page[Page];
    Top := grpGenerate.Top;
    Left := grpGenerate.Left + grpGenerate.Width + 10;
    Height := 90;
    Width := 270;
    Caption := dlgTargetProc ;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;


  {------------------------------------------------------------}

  grpOptimizations := TGroupBox.Create(Self);
  with grpOptimizations do
  begin
    Parent := nbMain.Page[Page];
    Top := grpTargetProc.Top + grpTargetProc.Height + 6;
    Left := 10;
    Height := 132;
    Width := 395;
    Caption :=   dlgOptimiz ;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;

{   Enable this as soon as TBevel works
  bvlOptSepLine := TBevel.Create(grpOptimizations);
  with bvlOptSepLine do
  begin
    Parent := grpOptimizations;
    Top := 47;
    Left := 10;
    Height := 10;
    Width := 325;
    Visible := True;
  end;
}  

  radOptLevel1 := TRadioButton.Create(grpOptimizations);
  with radOptLevel1 do
  begin
    Parent := grpOptimizations;
    Caption :=  dlgLevel1Opt ;
    Top := 52;
    Left := 5;
    Height := 16;
    Width := 330;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;

  TargetOSRadioGroup:=TRadioGroup.Create(Self);
  with TargetOSRadioGroup do begin
    Name:='TargetOSRadioGroup';
    Parent:=nbMain.Page[Page];
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
    Visible:=true;
  end;

end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupLinkingTab                                         }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupLinkingTab(Page: integer);
begin
  // Setup the Linking Tab
  Assert(False, 'Trace:Setting up compiler options linking tab');

  grpDebugging := TGroupBox.Create(Self);
  with grpDebugging do
  begin
    Parent := nbMain.Page[Page];
    Top := 10;
    Left := 10;
    Height := 151;
    Width := Self.ClientWidth-28;
    Caption := dlgCODebugging ;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;

  {------------------------------------------------------------}

  grpLinkLibraries := TGroupBox.Create(Self);
  with grpLinkLibraries do
  begin
    Parent := nbMain.Page[Page];
    Top := grpDebugging.Top + grpDebugging.Height + 10;
    Left := 10;
    Height := 70;
    Width := Self.ClientWidth-28;
    Caption := dlgLinkLibraries ;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;

  {------------------------------------------------------------}

  grpOptions := TGroupBox.Create(Self);
  with grpOptions do
  begin
    Parent := nbMain.Page[Page];
    Top := grpLinkLibraries.Top + grpLinkLibraries.Height + 10;
    Left := 10;
    Height := 75;
    Width := Self.ClientWidth-28;
    Caption := dlgCOOpts;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupOtherTab                                           }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupOtherTab(Page: integer);
begin
  // Setup the Other Tab
  Assert(False, 'Trace:Setting up compiler options other tab');

  grpVerbosity := TGroupBox.Create(Self);
  with grpVerbosity do
  begin
    Parent := nbMain.Page[Page];
    Top := 10;
    Left := 10;
    Height := 212;
    Width := Self.ClientWidth-28;
    Caption := dlgVerbosity;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;
  
  {------------------------------------------------------------}

  grpConfigFile := TGroupBox.Create(Self);
  with grpConfigFile do
  begin
    Parent := nbMain.Page[Page];
    Top := grpVerbosity.Top + grpVerbosity.Height + 10;
    Left := 10;
    Height := 95;
    Width := Self.ClientWidth-28;
    Caption := dlgConfigFiles ;
    Visible := True;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;
  
  {------------------------------------------------------------}
  grpErrorCnt := TGroupBox.Create(Self);
  with grpErrorCnt do
  begin
    Parent := nbMain.Page[Page];
    Top := grpConfigFile.Top + grpConfigFile.Height + 10;
    Left := 10;
    Height := 50;
    Width := 200;
    Caption := dlgStopAfterNrErr ;
    Visible := True;
  end;

  edtErrorCnt := TEdit.Create(grpConfigFile);
  with edtErrorCnt do
  begin
    Parent := grpErrorCnt;
    Top := 6;
    Left := 8;
    Height := 23;
    Width := grpErrorCnt.ClientWidth-2*Left-4;
    Text := '';
    Visible := True;
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupSearchPathsTab                                     }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupSearchPathsTab(Page: integer);
begin
  // Setup the Search Paths Tab

  grpOtherUnits := TGroupBox.Create(Self);
  with grpOtherUnits do
  begin
    Parent := nbMain.Page[Page];
    Left := 10;
    Top := 7;
    Width := Self.ClientWidth-28;
    Height := 50;
    Caption := dlgOtherUnitFiles ;
    Visible := True;
  end;

  edtOtherUnits := TEdit.Create(grpOtherUnits);
  with edtOtherUnits do
  begin
    Parent := grpOtherUnits;
    Left := 8;
    Top := 5;
    Width := Parent.ClientWidth-Left-37;
    Height := 23;
    Text := '';
    Visible := True;
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
    Visible:=true;
  end;

  {------------------------------------------------------------}
  
  grpIncludeFiles := TGroupBox.Create(Self);
  with grpIncludeFiles do
  begin
    Parent := nbMain.Page[Page];
    Left := grpOtherUnits.Left;
    Top := grpOtherUnits.Top+grpOtherUnits.Height+5;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOIncFiles ;
    Visible := True;
  end;

  edtIncludeFiles := TEdit.Create(grpIncludeFiles);
  with edtIncludeFiles do
  begin
    Parent := grpIncludeFiles;
    Top := 5;
    Left := 8;
    Width := Parent.ClientWidth-Left-37;
    Height := 23;
    Text := '';
    Visible := True;
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
    Visible:=true;
  end;

  {------------------------------------------------------------}

  grpOtherSources := TGroupBox.Create(Self);
  with grpOtherSources do
  begin
    Parent := nbMain.Page[Page];
    Top := grpIncludeFiles.Top+grpIncludeFiles.Height+5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOSources ;
    Visible := True;
  end;

  edtOtherSources := TEdit.Create(grpIncludeFiles);
  with edtOtherSources do
  begin
    Parent := grpOtherSources;
    Top := 5;
    Left := 8;
    Height := 23;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
    Visible := True;
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
    Visible:=true;
  end;

  {------------------------------------------------------------}

  grpLibraries := TGroupBox.Create(Self);
  with grpLibraries do
  begin
    Parent := nbMain.Page[Page];
    Top := grpOtherSources.Top + grpOtherSources.Height + 5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOLibraries ;
    Visible := True;
  end;

  edtLibraries := TEdit.Create(grpLibraries);
  with edtLibraries do
  begin
    Parent := grpLibraries;
    Top := 5;
    Left := 8;
    Height := 23;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
    Visible := True;
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
    Visible:=true;
  end;

  {------------------------------------------------------------}

  grpCompiler := TGroupBox.Create(Self);
  with grpCompiler do
  begin
    Parent := nbMain.Page[Page];
    Top := grpLibraries.Top + grpLibraries.Height + 5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgToFPCPath ;
    Visible := True;
  end;

  edtCompiler := TEdit.Create(grpCompiler);
  with edtCompiler do
  begin
    Parent := grpCompiler;
    Top := 5;
    Left := 8;
    Height := 23;
    Width := Parent.ClientWidth-2*Left;
    Text := '';
    Visible := True;
  end;
  
  {------------------------------------------------------------}

  grpUnitOutputDir := TGroupBox.Create(Self);
  with grpUnitOutputDir do
  begin
    Parent := nbMain.Page[Page];
    Top := grpCompiler.Top + grpCompiler.Height + 5;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgUnitOutp ;
    Visible := True;
  end;

  edtUnitOutputDir := TEdit.Create(grpCompiler);
  with edtUnitOutputDir do
  begin
    Parent := grpUnitOutputDir;
    Top := 5;
    Left := 8;
    Height := 23;
    Width := Parent.ClientWidth-2*Left;
    Text := '';
    Visible := True;
  end;  
  
  {------------------------------------------------------------}
  
  LCLWidgetTypeRadioGroup:=TRadioGroup.Create(Self);
  with LCLWidgetTypeRadioGroup do begin
    Name:='LCLWidgetTypeRadioGroup';
    Parent:=nbMain.Page[Page];
    Left := grpOtherUnits.Left;
    Top:=grpUnitOutputDir.Top+grpUnitOutputDir.Height+5;
    Width:=300;
    Height:=45;
    Caption:=dlgLCLWidgetType ;
    with Items do begin
      Add('gnome');
      Add('gtk');
      Add('win32');
    end;
    Columns:=3;
    ItemIndex:=1;
    Visible:=true;
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
    Visible := True;
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
    Visible := True;
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
    Visible := True;
  end;

  btnTest := TButton.Create(Self);
  with btnTest do
  begin
    Parent := Self;
    Width := 110;
    Height := 23; 
    Top := Self.Height - btnTest.Height - 15;
    Left := btnOK.Left - btnTest.Width - 5;
    Caption := dlgCOShowOptions ;
    OnClick := @ButtonTestClicked;
    Visible := True;
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
      Templates:=SetDirSeparators(
            '$(LazarusDir)/lcl/units'
          +';$(LazarusDir)/lcl/units/$(LCLWidgetType)'
          +';$(LazarusDir)/components/units'
          +';$(LazarusDir)/components/custom'
          );
    end else
    if AButton=IncludeFilesPathEditBtn then begin
      OldPath:=edtIncludeFiles.Text;
      Templates:='include';
    end else
    if AButton=OtherSourcesPathEditBtn then begin
      OldPath:=edtOtherSources.Text;
      Templates:=SetDirSeparators(
            '$(LazarusDir)/lcl'
          +';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)');
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

function TfrmCompilerOptions.GetOtherSourcePath: string;
begin
  Result:=edtOtherSources.Text;
end;

procedure TfrmCompilerOptions.SetOtherSourcePath(const AValue: string);
begin
  edtOtherSources.Text:=AValue;
end;

  
end.

