{ /***************************************************************************
                          compiler.pp  -  Main application unit
                             -------------------
                   Compiler options form sets the switches for the project
                   file for the PPC386 compiler.


                   Initial Revision  : Sat May 10 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit compileroptions;

{$mode objfpc}
{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

uses
  forms, classes, SysUtils, comctrls, buttons, stdctrls, extctrls, lazconf, xmlcfg,
  filectrl;

type
  { Compiler Options object used to hold the compiler options }
  TCompilerOptions = class(TObject)
  private
    fOptionsString: String;
    xmlcfg: TXMLConfig;

    fProjectFile: String;

    fStyle: Integer;
    fD2Ext: Boolean;
    fCStyleOp: Boolean;
    fAllowLabel: Boolean;
    fCPPInline: Boolean;
    fCMacros: Boolean;
    fTPCompat: Boolean;
    fInitConst: Boolean;
    fStaticKwd: Boolean;
    fDelphiCompat: Boolean;
    fUseAnsiStr: Boolean;
    fGPCCompat: Boolean;
    
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
    
    fGenDebugInfo: Boolean;
    fGenDebugDBX: Boolean;
    fUseHeaptrc: Boolean;
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
    fShowDebugInfo: Boolean;
    fShowUsedFiles: Boolean;
    fShowTriedFiles: Boolean;
    fShowDefMacros: Boolean;
    fShowCompProc: Boolean;
    fShowCond: Boolean;
    fShowNothing: Boolean;
    fWriteFPCLogo: Boolean;
    fUseConfigFile: Boolean;
    fAdditionalConfigFile: Boolean;
    fConfigFilePath: String;
    
    fIncludeFiles: String;
    fLibraries: String;
    fOtherUnitFiles: String;
    fCompilerPath: String;

    procedure LoadTheCompilerOptions;
    procedure SaveTheCompilerOptions;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadCompilerOptions(UseExistingFile: Boolean);
    procedure SaveCompilerOptions(UseExistingFile: Boolean);
    function MakeOptionsString: String;
    function ParseSearchPaths(switch, paths: String): String;
    function GetXMLConfigPath: String;

    property ProjectFile: String read fProjectFile write fProjectFile;
    property XMLConfigFile: TXMLConfig read xmlcfg write xmlcfg;
    
    property Style: Integer read fStyle write fStyle;
    property D2Extensions: Boolean read fD2Ext write fD2Ext;
    property CStyleOperators: Boolean read fCStyleOp write fCStyleOp;
    property AllowLabel: Boolean read fAllowLabel write fAllowLabel;
    property CPPInline: Boolean read fCPPInline write fCPPInline;
    property CStyleMacros: Boolean read fCMacros write fCMacros;
    property TPCompatible: Boolean read fTPCompat write fTPCompat;
    property InitConstructor: Boolean read fInitConst write fInitConst;
    property StaticKeyword: Boolean read fStaticKwd write fStaticKwd;
    property DelphiCompat: Boolean read fDelphiCompat write fDelphiCompat;
    property UseAnsiStrings: Boolean read fUseAnsiStr write fUseAnsiStr;
    property GPCCompat: Boolean read fGPCCompat write fGPCCompat;
    
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
    
    property GenerateDebugInfo: Boolean read fGenDebugInfo write fGenDebugInfo;
    property GenerateDebugDBX: Boolean read fGenDebugDBX write fGenDebugDBX;
    property UseHeaptrc: Boolean read fUseHeaptrc write fUseHeaptrc;
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
    property ShowDebugInfo: Boolean read fShowDebugInfo write fShowDebugInfo;
    property ShowUsedFiles: Boolean read fShowUsedFiles write fShowUsedFiles;
    property ShowTriedFiles: Boolean read fShowTriedFiles write fShowTriedFiles;
    property ShowDefMacros: Boolean read fShowDefMacros write fShowDefMacros;
    property ShowCompProc: Boolean read fShowCompProc write fShowCompProc;
    property ShowCond: Boolean read fShowCond write fShowCond;
    property ShowNothing: Boolean read fShowNothing write fShowNothing;
    property WriteFPCLogo: Boolean read fWriteFPCLogo write fWriteFPCLogo;
    property UseConfigFile: Boolean read fUseConfigFile write fUseConfigFile;
    property AdditionalConfigFile: Boolean read fAdditionalConfigFile write fAdditionalConfigFile;
    property ConfigFilePath: String read fConfigFilePath write fConfigFilePath;
    
    property IncludeFiles: String read fIncludeFiles write fIncludeFiles;
    property Libraries: String read fLibraries write fLibraries;
    property OtherUnitFiles: String read fOtherUnitFiles write fOtherUnitFiles;
    property CompilerPath: String read fCompilerPath write fCompilerPath;
  end;

  { Compiler options form }
  TfrmCompilerOptions = class(TForm)
  private
    nbMain: TNotebook;
    //bvlButtonBar: TBevel;

    { Parsing Controls }
    grpStyle: TGroupBox;
    radStyleIntel: TRadioButton;
    radStyleATT: TRadioButton;
    radStyleAsIs: TRadioButton;

    grpSymantecChk: TGroupBox;
    chkSymD2Ext: TCheckBox;
    chkSymCOper: TCheckBox;
    chkSymAllowLab: TCheckBox;
    chkSymCPPInline: TCheckBox;
    chkSymCMacros: TCheckBox;
    chkSymTP7Compat: TCheckBox;
    chkSymConstInit: TCheckBox;
    chkSymStaticKwd: TCheckBox;
    chkSymDelphiCompat: TCheckBox;
    chkSymUseAnsiStrings: TCheckBox;
    chkSymGPCCompat: TCheckBox;

    { Code Generation Controls }
    grpUnitStyle: TGroupBox;
    radUnitStyleStatic: TRadioButton;
    radUnitStyleDynamic: TRadioButton;

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

    { Linking Controls }
    grpDebugging: TGroupBox;
    chkDebugGDB: TCheckBox;
    chkDebugDBX: TCheckBox;
    chkUseHeaptrc: TCheckBox;
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
    chkDebugInfo: TCheckBox;
    chkUsedFiles: TCheckBox;
    chkTriedFiles: TCheckBox;
    chkDefinedMacros: TCheckBox;
    chkCompiledProc: TCheckBox;
    chkConditionals: TCheckBox;
    chkNothing: TCheckBox;

    chkFPCLogo: TCheckBox;

    grpConfigFile: TGroupBox;
    chkConfigFile: TCheckBox;
    chkAdditionalConfigFile: TCheckBox;
    edtConfigPath: TEdit;

    { Search Paths Controls }
    grpIncludeFiles: TGroupBox;
    edtIncludeFiles: TEdit;

    grpLibraries: TGroupBox;
    edtLibraries: TEdit;

    grpOtherUnits: TGroupBox;
    edtOtherUnits: TEdit;

    grpCompiler: TGroupBox;
    edtCompiler: TEdit;

    { Buttons }
    btnTest: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    
    { Other variables }
//    fPath: String;

    { Procedures }
    procedure CreateForm(Sender: TObject);
    procedure SetupParsingTab(Sender: TObject);
    procedure SetupCodeGenerationTab(Sender: TObject);
    procedure SetupLinkingTab(Sender: TObject);
    procedure SetupOtherTab(Sender: TObject);
    procedure SetupSearchPathsTab(Sender: TObject);
    procedure SetupButtonBar(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ButtonOKClicked(Sender: TObject);
    procedure ButtonCancelClicked(Sender: TObject);
    procedure ButtonApplyClicked(Sender: TObject);
    procedure ButtonTestClicked(Sender: TObject);

    procedure GetCompilerOptions;
    procedure PutCompilerOptions;
  end;

var
frmCompilerOptions: TfrmCompilerOptions;
compilerOpts: TCompilerOptions;

implementation

uses
  main, project;

const
  CONFIG_FILENAME = 'compileroptions.xml';

{------------------------------------------------------------------------------}
{  TCompilerOptions Constructor                                                }
{------------------------------------------------------------------------------}
constructor TCompilerOptions.Create;
begin
  inherited Create;
  Assert(False, 'Trace:Compiler Options Class Created');

  fOptionsString := '';

  { Set Defaults }
{ fStyle := 1;
  fD2Ext := true;
  fCStyleOp := true;
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
    
  fGenDebugInfo := false;
  fGenDebugDBX := false;
  fUseHeaptrc := false;
  fStripSymbols := false;
  fLinkStyle := 1;
  fPassLinkerOpt := false;
  fLinkerOptions := '';
    
  fShowErrors := false;
  fShowWarn := true;
  fShowNotes := true;
  fShowHints := true;
  fShowGenInfo := true;
  fShowLineNum := false;
  fShowAll := false;
  fShowDebugInfo := false;
  fShowUsedFiles := false;
  fShowTriedFiles := false;
  fShowDefMacros := false;
  fShowCompProc := false;
  fShowCond := false;
  fShowNothing := false;
  fWriteFPCLogo := true;
  fUseConfigFile := false;
  fAdditionalConfigFile := false;
  fConfigFilePath := './ppc386.cfg';
    
  fIncludeFiles := '';
  fLibraries := '';
  fOtherUnitFiles := '';
  fCompilerPath := '/opt/fpc/ppc386';}
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCompilerOptions.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions LoadCompilerOptions                                      }
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
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions LoadTheCompilerOptions                                      }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.LoadTheCompilerOptions;
begin
  { Load the compiler options from the XML file }

  { Parsing }
  Style := XMLConfigFile.GetValue('CompilerOptions/Parsing/Style/Value', 1);

  D2Extensions := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/D2Extensions/Value', true);
  CStyleOperators := XMLConfigFile.GetValue('CompilerOptions/Parsing/SymantecChecking/CStyleOperator/Value', true);
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

  { Linking }
  GenerateDebugInfo := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/GenerateDebugInfo/Value', false);
  GenerateDebugDBX := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/GenerateDebugDBX/Value', false);
  UseHeaptrc := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/UseHeaptrc/Value', false);
  StripSymbols := XMLConfigFile.GetValue('CompilerOptions/Linking/Debugging/StripSymbols/Value', false);
  LinkStyle := XMLConfigFile.GetValue('CompilerOptions/CodeGeneration/LinkStyle/Value', 1);
  PassLinkerOptions := XMLConfigFile.GetValue('CompilerOptions/Linking/Options/PassLinkerOptions/Value', false);
  LinkerOptions := XMLConfigFile.GetValue('CompilerOptions/Linking/Options/LinkerOptions/Value', '');
    
  { Other }
  ShowErrors := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowErrors/Value', false);
  ShowWarn := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowWarn/Value', true);
  ShowNotes := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowNotes/Value', true);
  ShowHints := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowHints/Value', true);
  ShowGenInfo := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowGenInfo/Value', true);
  ShowLineNum := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShoLineNum/Value', false);
  ShowAll := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowAll/Value', false);
  ShowDebugInfo := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowDebugInfo/Value', false);
  ShowUsedFiles := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowUsedFiles/Value', false);
  ShowTriedFiles := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowTriedFiles/Value', false);
  ShowDefMacros := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowDefMacros/Value', false);
  ShowCompProc := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowCompProc/Value', false);
  ShowCond := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowCond/Value', false);
  ShowNothing := XMLConfigFile.GetValue('CompilerOptions/Other/Verbosity/ShowNothing/Value', false);
  WriteFPCLogo := XMLConfigFile.GetValue('CompilerOptions/Other/WriteFPCLogo/Value', true);
  UseConfigFile := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/UseConfigFile/Value', false);
  AdditionalConfigFile := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/AdditionalConfigFile/Value', false);
  ConfigFilePath := XMLConfigFile.GetValue('CompilerOptions/Other/ConfigFile/ConfigFilePath/Value', './ppc386.cfg');

  { SearchPaths }
  IncludeFiles := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/IncludeFiles/Value', '');
  Libraries := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/Libraries/Value', '');
  OtherUnitFiles := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', '');
  CompilerPath := XMLConfigFile.GetValue('CompilerOptions/SearchPaths/CompilerPath/Value', '/opt/fpc/ppc386');
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SaveCompilerOptions                                      }
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
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SaveTheCompilerOptions                                     }
{------------------------------------------------------------------------------}
procedure TCompilerOptions.SaveTheCompilerOptions;
begin
  { Save the compiler options to the XML file }
  Writeln('SaveTHECompilerOptions');
  { Parsing }
  Writeln('First one');
  XMLConfigFile.SetValue('CompilerOptions/Parsing/Style/Value', Style);
  Writeln('After FIRST one');
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/D2Extensions/Value', D2Extensions);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/CStyleOperator/Value', CStyleOperators);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/AllowLabel/Value', AllowLabel);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/CPPInline/Value', CPPInline);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/CStyleMacros/Value', CStyleMacros);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/TPCompatible/Value', TPCompatible);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/InitConstructor/Value', InitConstructor);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/StaticKeyword/Value', StaticKeyword);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/DelphiCompat/Value', DelphiCompat);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/UseAnsiStrings/Value', UseAnsiStrings);
  XMLConfigFile.SetValue('CompilerOptions/Parsing/SymantecChecking/GPCCompat/Value', GPCCompat);
Writeln('1');
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

Writeln('2');
  { Linking }
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/GenerateDebugInfo/Value', GenerateDebugInfo);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/GenerateDebugDBX/Value', GenerateDebugDBX);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/UseHeaptrc/Value', UseHeaptrc);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Debugging/StripSymbols/Value', StripSymbols);
  XMLConfigFile.SetValue('CompilerOptions/CodeGeneration/LinkStyle/Value', LinkStyle);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Options/PassLinkerOptions/Value', PassLinkerOptions);
  XMLConfigFile.SetValue('CompilerOptions/Linking/Options/LinkerOptions/Value', LinkerOptions);
    
Writeln('3');
  { Other }
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowErrors/Value', ShowErrors);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowWarn/Value', ShowWarn);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowNotes/Value', ShowNotes);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowHints/Value', ShowHints);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowGenInfo/Value', ShowGenInfo);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShoLineNum/Value', ShowLineNum);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowAll/Value', ShowAll);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowDebugInfo/Value', ShowDebugInfo);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowUsedFiles/Value', ShowUsedFiles);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowTriedFiles/Value', ShowTriedFiles);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowDefMacros/Value', ShowDefMacros);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowCompProc/Value', ShowCompProc);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowCond/Value', ShowCond);
  XMLConfigFile.SetValue('CompilerOptions/Other/Verbosity/ShowNothing/Value', ShowNothing);
  XMLConfigFile.SetValue('CompilerOptions/Other/WriteFPCLogo/Value', WriteFPCLogo);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/UseConfigFile/Value', UseConfigFile);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/AdditionalConfigFile/Value', AdditionalConfigFile);
  XMLConfigFile.SetValue('CompilerOptions/Other/ConfigFile/ConfigFilePath/Value', ConfigFilePath);

Writeln('4');
  { SearchPaths }
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/IncludeFiles/Value', IncludeFiles);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/Libraries/Value', Libraries);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', OtherUnitFiles);
  XMLConfigFile.SetValue('CompilerOptions/SearchPaths/CompilerPath/Value', CompilerPath);
  Writeln('5');
  XMLConfigFile.Flush;
  Writeln('6');
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions MakeOptionsString                                          }
{------------------------------------------------------------------------------}
function TCompilerOptions.MakeOptionsString: String;
var
  switches, tempsw: String;
begin
  switches := '';

  { Get all the options and create a string that can be passed to the compiler }

  { --------------- Parsing Tab ------------------- }

  { Style }
  { Style       -Ratt = AT&T    -Rintel = Intel  -Rdirect = As-is }
  switches := '-R';
  case (Style) of
    1: switches := switches + 'intel';
    2: switches := switches + 'att';
    3: switches := switches + 'direct';
  end;
  
  { Symantec Checking }
  tempsw := '';

  if (D2Extensions) then
    tempsw := tempsw + '2';
  if (CStyleOperators) then
    tempsw := tempsw + 'c';
  if (AllowLabel) then
    tempsw := tempsw + 'g';
  if (CPPInline) then
    tempsw := tempsw + 'i';
  if (CStyleMacros) then
    tempsw := tempsw + 'm';
  if (TPCompatible) then
    tempsw := tempsw + 'o';
  if (InitConstructor) then
    tempsw := tempsw + 's';
  if (StaticKeyword) then
    tempsw := tempsw + 't';
  if (DelphiCompat) then
    tempsw := tempsw + 'd';
  if (UseAnsiStrings) then
    tempsw := tempsw + 'h';
  if (GPCCompat) then
    tempsw := tempsw + 'p';

  if (tempsw <> '') then
  begin
    tempsw := '-S' + tempsw;

    { Add in Symantec Checking }
    switches := switches + ' ' + tempsw;
  end;
    
  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
  -Un = Do not check the unit name
  -Us = Compile a system unit
}

  { ----------- Code Generation Tab --------------- }

  { Unit Style }
  { UnitStyle   '' = Static     'D' = Dynamic }
  switches := switches + ' ' + '-C';
  
  case (UnitStyle) of
    1: switches := switches + '';
    2: switches := switches + 'D';
  end;

  if ((IOChecks) or (RangeChecks) or
     (OverflowChecks) or (StackChecks)) then
  begin
    switches := switches + ' ' + '-C';

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

    if (tempsw <> '') then
    begin
      { Add in Checks }
      switches := switches + tempsw;
    end;
  end;

  { Heap Size }
  if (HeapSize >= 0) then
    switches := switches + ' ' + '-Ch' + IntToStr(HeapSize);


  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
  n = Omit linking stage
  sxxx = Set stack size to xxx
  x = Smartlinking
}

  switches := switches + ' ' + '-O';

  { Generate }
  { Generate    g = smaller    G = faster }
  case (Generate) of
    1:  switches := switches + 'g';
    2:  switches := switches + 'G';
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

  { --------------- Linking Tab ------------------- }
  
  { Debugging }
  { Debug Info for GDB }
  if (GenerateDebugInfo) then
    switches := switches + ' ' + '-g';

  { Debug Info for DBX }
  if (GenerateDebugDBX) then
    switches := switches + ' ' + '-gd';

  { Use Heaptrc Unix }
  if (UseHeaptrc) then
    switches := switches + ' ' + '-gh';

  { Strip Symbols }
  if (StripSymbols) then
    switches := switches + ' ' + '-Xs';

  { Link Style
     -XD = Link with dynamic libraries
     -XS = Link with static libraries 
  
  TODO -XX = link smart   
  }
  case (LinkStyle) of
    1:  switches := switches + ' ' + '-XD';
    2:  switches := switches + ' ' + '-XS';
  end;


  if (PassLinkerOptions) then
    switches := switches + ' ' + '-k-s' + LinkerOptions;

  { ---------------- Other Tab -------------------- }

  { Verbosity }
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

  if (tempsw <> '') then
  begin
    tempsw := '-v' + tempsw;

    { Add in Verbosity }
    switches := switches + ' ' + tempsw;
  end;

  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
      b = Show all procedure declarations if overloaded function error occurs
      x = Output some executable info (Win32 only)
      r = Rhide/GCC compatibility mode
}

  { Write an FPC logo }
  if (WriteFPCLogo) then
    switches := switches + ' ' + '-l';

  { Use Config File }
  if (UseConfigFile) then
    switches := switches + ' ' + '-n';

  { Use Additional Config File     @ = yes and path }
  if (AdditionalConfigFile) then
    switches := switches + ' ' + '@' + ConfigFilePath;

  { ------------- Search Paths Tab ---------------- }
  Writeln('Switchs = '+Switches);
  if (IncludeFiles <> '') then
    switches := switches + ' ' + ParseSearchPaths('-Fi', IncludeFiles);
  Writeln('Switchs = '+Switches);

  if (Libraries <> '') then
    switches := switches + ' ' + ParseSearchPaths('-Fl', Libraries);
  Writeln('Switchs = '+Switches);

  if (OtherUnitFiles <> '') then
    switches := switches + ' ' + ParseSearchPaths('-Fu', OtherUnitFiles);
  Writeln('Switchs = '+Switches);

  { CompilerPath - Nothing needs to be done with this one }

  { TODO: Implement the following switches. They need to be added
          to the dialog. }
{
     exxx = Errors file
     Lxxx = Use xxx as dynamic linker (LINUX only)
     oxxx = Object files
     rxxx = Compiler messages file
     Uxxx = Write units to xxx directory instead of current directory

}

  { ----------------------------------------------- }

  { TODO: The following switches need to be implemented. They need to
          be added to the dialog. }
{
  -P = Use pipes instead of files when assembling
      

  -oprogramname   = executable filename
  -pg      = generate profiler code

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
       
  -B = Recompile all units even if they didn't change
  -b = Generate browser info
  -bl = Generate browser info, including local variables, types and procedures

  -dxxx = Define symbol name xxx (Used for conditional compiles)
  -uxxx = Undefine symbol name xxx
  
  -s = Do not call assembler or linker. Write ppas.bat/ppas.sh script.
  -T = Target OS
       GO32V1 = DOS and version 1 of the DJ DELORIE extender (no longer maintained).
       GO32V2 = DOS and version 2 of the DJ DELORIE extender.
       LINUX = LINUX.
       OS2 = OS/2 (2.x) using the EMX extender.
       WIN32 = Windows 32 bit.



  The following switches are not really needed in Lazarus
  -Xc = Link with C library (LINUX only)
       
}

  { Setting this to a default for now to allow the compiler to compile, until I get 
    the above completed. }
  //Result := '-viwnh -n -Sgic -Fu' + OtherUnitFiles + ' -Fl' + Libraries;

  fOptionsString := switches;
  Writeln('Still in CompilerOptions');
  Writeln('fOptionsString = '+fOptionsString);
  Result := fOptionsString;
end;

{------------------------------------------------------------------------------}
{  TCompilerOptions ParseSearchPaths                                           }
{------------------------------------------------------------------------------}
function TCompilerOptions.ParseSearchPaths(switch, paths: String): String;
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
      tempsw := tempsw + switch + SS;
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
      tempsw := tempsw + switch + Copy (SS, 1, M - 1);
      SS := Copy (SS, M + 1, Length(SS));
    end;
  until (SS = '') or (M = 0);
  
  Result := tempsw;
end;

{------------------------------------------------------------------------------
  TCompilerOptions GetXMLConfigPath
 ------------------------------------------------------------------------------}
function TCompilerOptions.GetXMLConfigPath: String;
var
  confPath: String;
  fn: String;
begin
  Result := '';

  // Setup the filename to write to
  if (ProjectFile <> '') then
    fn := ProjectFile
  else
    fn := CONFIG_FILENAME;

  confPath := GetPrimaryConfigPath + '/' + fn;

  // See if config path exists and if not create it
  if (not DirectoryExists(GetPrimaryConfigPath)) then
  begin
     try
        // Create the directory
        CreatePrimaryConfigPath;

        { TODO:
            Try to read the configuration from the secondary path
            If successful, then read it in and write it to the primary path
            If unsuccessful, then just use defaults
        }
     except
       Assert(False, 'Trace:There was a problem creating the config directory. Using defaults.');
       Assert(False, 'Trace:File = ' + confPath);
       confPath := './' + fn;
       Result := confPath;
     end;
  end;

  Result := confPath;
end;


{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Constructor                                              }
{------------------------------------------------------------------------------}
constructor TfrmCompilerOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Assert(False, 'Trace:Compiler Options Form Created');
  Height := 455;
  Width := 379;
  Caption := 'Compiler Options';
  OnShow := @CreateForm;
//  MainIDE.Project.CompilerOptions.LoadCompilerOptions(true);
  CompilerOpts.LoadCompilerOptions(true);
  
  nbMain := TNotebook.Create(Self);
  nbMain.Parent := Self;
  nbMain.Height := Height - 50;
  nbMain.Width := Width - 4;
  nbmain.Top := 0;
  nbmain.Left := 0;

  // Add the pages 
  nbMain.Pages.Strings[0] := 'Parsing';
  nbMain.Pages.Add('Code Generation');
  nbMain.Pages.Add('Linking');
  nbMain.Pages.Add('Other');
  nbMain.Pages.Add('Search Paths');

{
  bvlButtonBar := TBevel.Create(Self);
  bvlButtonBar.Parent := Self;
  bvlButtonBar.Height := 2;
  bvlButtonBar.Width := Width;
  bvlButtonBar.Top := Height - 30;
  bvlButtonBar.Left := 0;
}

  { Parsing Tab }
  SetupParsingTab(Self);

  { Code Generation Tab }
  SetupCodeGenerationTab(Self);

  { Linking Tab }
  SetupLinkingTab(Self);

  { Other Tab }
  SetupOtherTab(Self);

  { Search Paths Tab }
  SetupSearchPathsTab(Self);

  { Bottom Buttons }
  SetupButtonBar(Self);

  { Show everything }
  nbMain.Show;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Destructor                                               }
{------------------------------------------------------------------------------}
destructor TfrmCompilerOptions.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions FormOnShow Event                                         }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.CreateForm(Sender: TObject);
begin
  { Get all the compiler options }
  GetCompilerOptions;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonOKClicked                                          }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonOKClicked(Sender: TObject);
begin
  // Accept any changes
  Assert(False, 'Trace:Accept compiler options changes');

  { Save the options and hide the dialog }
  PutCompilerOptions;
  Writeln('Calling Mainide.project.compiler...');
//  MainIDE.Project.CompilerOptions.SaveCompilerOptions(true);
  CompilerOpts.SaveCompilerOptions(true);
  Writeln('Called');
  Hide;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonCancelClicked                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonCancelClicked(Sender: TObject);
begin
  // Cancel any changes
  Assert(False, 'Trace:Cancel compiler options changes');

  Hide;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonApplyClicked                                       }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonApplyClicked(Sender: TObject);
begin
  // Apply any changes
  Assert(False, 'Trace:Apply compiler options changes');

  PutCompilerOptions;
//  MainIDE.Project.CompilerOptions.SaveCompilerOptions(true);
  CompilerOpts.SaveCompilerOptions(true);
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonTestClicked                                       }
{     This function is for testing the MakeOptionsString function only. Remove }
{     this function and its button when the function is working correctly.     }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonTestClicked(Sender: TObject);
var
  teststr: String;
begin
  // Test MakeOptionsString function
  Assert(False, 'Trace:Test MakeOptionsString function');

  teststr := CompilerOpts.MakeOptionsString;
  WriteLn('MakeOptionsString: ' + teststr);
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions GetCompilerOptions                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.GetCompilerOptions;
begin
  { Get the compiler options and apply them to the dialog }
  case CompilerOpts.Style of
      1: radStyleIntel.Checked := true;
      2: radStyleATT.Checked := true;
      3: radStyleATT.Checked := true;
  end;

  chkSymD2Ext.Checked := CompilerOpts.D2Extensions;
  chkSymCOper.Checked := CompilerOpts.CStyleOperators;
  chkSymAllowLab.Checked := CompilerOpts.AllowLabel;
  chkSymCPPInline.Checked := CompilerOpts.CPPInline;
  chkSymCMacros.Checked := CompilerOpts.CStyleMacros;
  chkSymTP7Compat.Checked := CompilerOpts.TPCompatible;
  chkSymConstInit.Checked := CompilerOpts.InitConstructor;
  chkSymStaticKwd.Checked := CompilerOpts.StaticKeyword;
  chkSymDelphiCompat.Checked := CompilerOpts.DelphiCompat;
  chkSymUseAnsiStrings.Checked := CompilerOpts.UseAnsiStrings;
  chkSymGPCCompat.Checked := CompilerOpts.GPCCompat;

  case CompilerOpts.UnitStyle of
      1: radUnitStyleStatic.Checked := true;
      2: radUnitStyleDynamic.Checked := true;
  end;    

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
  chkUseHeaptrc.Checked := CompilerOpts.UseHeaptrc;
  chkSymbolsStrip.Checked := CompilerOpts.StripSymbols;

  case CompilerOpts.LinkStyle of
    1: radLibsLinkDynamic.Checked := true;
    2: radLibsLinkStatic.Checked := true;
  end;

  chkOptionsLinkOpt.Checked := CompilerOpts.PassLinkerOptions;
  if chkOptionsLinkOpt.Checked then
    edtOptionsLinkOpt.Enabled := true
  else
    edtOptionsLinkOpt.Enabled := false;
  edtOptionsLinkOpt.Text := CompilerOpts.LinkerOptions;
  
  chkErrors.Checked := CompilerOpts.ShowErrors;
  chkWarnings.Checked := CompilerOpts.ShowWarn;
  chkNotes.Checked := CompilerOpts.ShowNotes;
  chkHints.Checked := CompilerOpts.ShowHints;
  chkGeneralInfo.Checked := CompilerOpts.ShowGenInfo;
  chkLineNumbers.Checked := CompilerOpts.ShowLineNum;
  chkEverything.Checked := CompilerOpts.ShowAll;
  chkDebugInfo.Checked := CompilerOpts.ShowDebugInfo;
  chkUsedFiles.Checked := CompilerOpts.ShowUsedFiles;
  chkTriedFiles.Checked := CompilerOpts.ShowTriedFiles;
  chkDefinedMacros.Checked := CompilerOpts.ShowDefMacros;
  chkCompiledProc.Checked := CompilerOpts.ShowCompProc;
  chkConditionals.Checked := CompilerOpts.ShowCond;
  chkNothing.Checked := CompilerOpts.ShowNothing;

  chkFPCLogo.Checked := CompilerOpts.WriteFPCLogo;

  chkConfigFile.Checked := CompilerOpts.UseConfigFile;
  chkAdditionalConfigFile.Checked := CompilerOpts.AdditionalConfigFile;
  if chkAdditionalConfigFile.Checked then
    edtConfigPath.Enabled := true
  else
    edtConfigPath.Enabled := false;
  edtConfigPath.Text := CompilerOpts.ConfigFilePath;
    
  edtIncludeFiles.Text := CompilerOpts.IncludeFiles;
  edtLibraries.Text := CompilerOpts.Libraries;
  edtOtherUnits.Text := CompilerOpts.OtherUnitFiles;
  edtCompiler.Text := CompilerOpts.CompilerPath;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions PutCompilerOptions                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.PutCompilerOptions;
var
    code: LongInt;
    hs: LongInt;
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
  CompilerOpts.AllowLabel := chkSymAllowLab.Checked;
  CompilerOpts.CPPInline := chkSymCPPInline.Checked;
  CompilerOpts.CStyleMacros := chkSymCMacros.Checked;
  CompilerOpts.TPCompatible := chkSymTP7Compat.Checked;
  CompilerOpts.InitConstructor := chkSymConstInit.Checked;
  CompilerOpts.StaticKeyword := chkSymStaticKwd.Checked;
  CompilerOpts.DelphiCompat := chkSymDelphiCompat.Checked;
  CompilerOpts.UseAnsiStrings := chkSymUseAnsiStrings.Checked;
  CompilerOpts.GPCCompat := chkSymGPCCompat.Checked;

  if (radUnitStyleStatic.Checked) then
      CompilerOpts.UnitStyle := 1
  else if (radUnitStyleDynamic.Checked) then
      CompilerOpts.UnitStyle := 2
  else
      CompilerOpts.UnitStyle := 1;

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
  CompilerOpts.UseHeaptrc := chkUseHeaptrc.Checked;
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
  CompilerOpts.ShowDebugInfo := chkDebugInfo.Checked;
  CompilerOpts.ShowUsedFiles := chkUsedFiles.Checked;
  CompilerOpts.ShowTriedFiles := chkTriedFiles.Checked;
  CompilerOpts.ShowDefMacros := chkDefinedMacros.Checked;
  CompilerOpts.ShowCompProc := chkCompiledProc.Checked;
  CompilerOpts.ShowCond := chkConditionals.Checked;
  CompilerOpts.ShowNothing := chkNothing.Checked;

  CompilerOpts.WriteFPCLogo := chkFPCLogo.Checked;

  CompilerOpts.UseConfigFile := chkConfigFile.Checked;
  CompilerOpts.AdditionalConfigFile := chkAdditionalConfigFile.Checked;
  CompilerOpts.ConfigFilePath := edtConfigPath.Text;
    
  CompilerOpts.IncludeFiles := edtIncludeFiles.Text;
  CompilerOpts.Libraries := edtLibraries.Text;
  CompilerOpts.OtherUnitFiles := edtOtherUnits.Text;
  CompilerOpts.CompilerPath := edtCompiler.Text;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupParsingTab                                          }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupParsingTab(Sender: TObject);
begin
  // Setup the Parsing Tab
  Assert(False, 'Trace:Setting up compiler options parsing tab');

  grpStyle := TGroupBox.Create(Self);
  with grpStyle do
  begin
    Parent := nbMain.Page[0];
    Top := 10;
    Left := 10;
    Height := 45;
    Width := 215;
    Caption := 'Style:';
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
    Caption := 'Intel';
    Visible := True;
  end;

  radStyleATT := TRadioButton.Create(grpStyle);
  with radStyleATT do
  begin
    Parent := grpStyle;
    Top := 5;
    Left := 80;
    Height := 16;
    Width := 50;
    Caption := 'AT&T';
    Visible := True;
  end;

  radStyleAsIs := TRadioButton.Create(grpStyle);
  with radStyleAsIs do
  begin
    Parent := grpStyle;
    Top := 5;
    Left := 150;
    Height := 16;
    Width := 50;
    Caption := 'As-Is';
    Visible := True;
  end;

  grpSymantecChk := TGroupBox.Create(Self);
  with grpSymantecChk do
  begin
    Parent := nbMain.Page[0];
    Top := 65;
    Left := 10;
    Height := 292;
    Width := 350;
    Caption := 'Symantec Checking:';
    Visible := True;
  end;

  chkSymD2Ext := TCheckBox.Create(grpSymantecChk);
  with chkSymD2Ext do
  begin
    Parent := grpSymantecChk;
    Caption := 'Delphi 2 Extensions';
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
    Caption := 'C Style Operators (*=, +=, /= and -=)';
    Top := 34;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymAllowLab := TCheckBox.Create(grpSymantecChk);
  with chkSymAllowLab do
  begin
    Parent := grpSymantecChk;
    Caption := 'Allow LABEL and GOTO';
    Top := 58;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymCPPInline := TCheckBox.Create(grpSymantecChk);
  with chkSymCPPInline do
  begin
    Parent := grpSymantecChk;
    Caption := 'C++ Styled INLINE';
    Top := 82;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymCMacros := TCheckBox.Create(grpSymantecChk);
  with chkSymCMacros do
  begin
    Parent := grpSymantecChk;
    Caption := 'C Style Macros (global)';
    Top := 106;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymTP7Compat := TCheckBox.Create(grpSymantecChk);
  with chkSymTP7Compat do
  begin
    Parent := grpSymantecChk;
    Caption := 'TP/BP 7.0 Compatible';
    Top := 130;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymConstInit := TCheckBox.Create(grpSymantecChk);
  with chkSymConstInit do
  begin
    Parent := grpSymantecChk;
    Caption := 'Constructor name must be ''' + 'init' + ''' (destructor must be ''' + 'done' + ''')';
    Top := 154;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymStaticKwd := TCheckBox.Create(grpSymantecChk);
  with chkSymStaticKwd do
  begin
    Parent := grpSymantecChk;
    Caption := 'Static Keyword in Objects';
    Top := 178;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;
  
  chkSymDelphiCompat := TCheckBox.Create(grpSymantecChk);
  with chkSymDelphiCompat do
  begin
    Parent := grpSymantecChk;
    Caption := 'Delphi Compatible';
    Top := 202;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymUseAnsiStrings := TCheckBox.Create(grpSymantecChk);
  with chkSymUseAnsiStrings do
  begin
    Parent := grpSymantecChk;
    Caption := 'Use Ansi Strings';
    Top := 226;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;

  chkSymGPCCompat := TCheckBox.Create(grpSymantecChk);
  with chkSymGPCCompat do
  begin
    Parent := grpSymantecChk;
    Caption := 'GPC (GNU Pascal Compiler) Compatible';
    Top := 250;
    Left := 5;
    Height := 16;
    Width := 340;
    Visible := True;
  end;
end;


{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupCodeGenerationTab                                   }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupCodeGenerationTab(Sender: TObject);
begin
  // Setup the Code Generation Tab
  Assert(False, 'Trace:Setting up compiler options code generation tab');

  grpUnitStyle := TGroupBox.Create(Self);
  with grpUnitStyle do
  begin
    Parent := nbMain.Page[1];
    Top := 10;
    Left := 10;
    Height := 70;
    Width := 85;
    Caption := 'Unit Style:';
    Visible := True;
  end;

  radUnitStyleStatic := TRadioButton.Create(grpUnitStyle);
  with radUnitStyleStatic do
  begin
    Parent := grpUnitStyle;
    Top := 8;
    Left := 5;
    Height := 16;
    Width := 70;
    Caption := 'Static';
    Visible := True;
  end;

  radUnitStyleDynamic := TRadioButton.Create(grpUnitStyle);
  with radUnitStyleDynamic do
  begin
    Parent := grpUnitStyle;
    Top := 29;
    Left := 5;
    Height := 16;
    Width := 70;
    Caption := 'Dynamic';
    Visible := True;
  end;

  {------------------------------------------------------------}
  grpChecks := TGroupBox.Create(Self);
  with grpChecks do
  begin
    Parent := nbMain.Page[1];
    Top := 10;
    Left := grpUnitStyle.Left + grpUnitStyle.Width + 10;
    Height := 70;
    Width := 165;
    Caption := 'Checks:';
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
    Caption := 'Range';
    Top := 8;
    Left := 85;
    Height := 16;
    Width := 70;
    Visible := True;
  end;

  chkChecksOverflow := TCheckBox.Create(grpChecks);
  with chkChecksOverflow do
  begin
    Parent := grpChecks;
    Caption := 'Overflow';
    Top := 29;
    Left := 5;
    Height := 16;
    Width := 70;
    Visible := True;
  end;

  chkChecksStack := TCheckBox.Create(grpChecks);
  with chkChecksStack do
  begin
    Parent := grpChecks;
    Caption := 'Stack';
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
    Parent := nbMain.Page[1];
    Top := 10;
    Left := grpChecks.Left + grpChecks.Width + 10;
    Height := 55;
    Width := 80;
    Caption := 'Heap Size:';
    Visible := True;
  end;

  edtHeapSize := TEdit.Create(grpHeapSize);
  with edtHeapSize do
  begin
    Parent := grpHeapSize;
    Caption := 'Heap Size';
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
    Parent := nbMain.Page[1];
    Top := grpUnitStyle.Top + grpUnitStyle.Height + 6;
    Left := 10;
    Height := 70;
    Width := 110;
    Caption := 'Generate:';
    Visible := True;
  end;

  radGenFaster := TRadioButton.Create(grpGenerate);
  with radGenFaster do
  begin
    Parent := grpGenerate;
    Top := 8;
    Left := 5;
    Height := 16;
    Width := 95;
    Caption := 'Faster Code';
    Visible := True;
  end;

  radGenSmaller := TRadioButton.Create(grpGenerate);
  with radGenSmaller do
  begin
    Parent := grpGenerate;
    Top := 29;
    Left := 5;
    Height := 16;
    Width := 95;
    Caption := 'Smaller Code';
    Visible := True;
  end;


  {------------------------------------------------------------}

  grpTargetProc := TGroupBox.Create(Self);
  with grpTargetProc do
  begin
    Parent := nbMain.Page[1];
    Top := grpGenerate.Top;
    Left := grpGenerate.Left + grpGenerate.Width + 10;
    Height := 90;
    Width := 230;
    Caption := 'Target Processor:';
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
    Parent := nbMain.Page[1];
    Top := grpTargetProc.Top + grpTargetProc.Height + 6;
    Left := 10;
    Height := 132;
    Width := 350;
    Caption := 'Optimizations:';
    Visible := True;
  end;

  chkOptVarsInReg := TCheckBox.Create(grpOptimizations);
  with chkOptVarsInReg do
  begin
    Parent := grpOptimizations;
    Caption := 'Keep certain variables in registers';
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
    Caption := 'Uncertain Optimizations';
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
    Caption := 'Level 1 (Quick Optimizations)';
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
    Caption := 'Level 2 (Level 1 + Slower Optimizations)';
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
    Caption := 'Level 3 (Level 2 + Uncertain)';
    Top := 94;
    Left := 5;
    Height := 16;
    Width := 330;
    Visible := True;
  end;

end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupLinkingTab                                          }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupLinkingTab(Sender: TObject);
begin
  // Setup the Linking Tab
  Assert(False, 'Trace:Setting up compiler options linking tab');

  grpDebugging := TGroupBox.Create(Self);
  with grpDebugging do
  begin
    Parent := nbMain.Page[2];
    Top := 10;
    Left := 10;
    Height := 110;
    Width := 350;
    Caption := 'Debugging:';
    Visible := True;
  end;

  chkDebugGDB := TCheckBox.Create(grpDebugging);
  with chkDebugGDB do
  begin
    Parent := grpDebugging;
    Caption := 'Generate Debugging Info For GDB (Slows Compiling)';
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 330;
    Visible := True;
  end;

  chkDebugDBX := TCheckBox.Create(grpDebugging);
  with chkDebugDBX do
  begin
    Parent := grpDebugging;
    Caption := 'Generate Debugging Info For DBX (Slows Compiling)';
    Top := 27;
    Left := 8;
    Height := 16;
    Width := 330;
    Visible := True;
  end;

  chkUseHeaptrc := TCheckBox.Create(grpDebugging);
  with chkUseHeaptrc do
  begin
    Parent := grpDebugging;
    Caption := 'Use Heaptrc Unit';
    Top := 48;
    Left := 8;
    Height := 16;
    Width := 330;
    Visible := True;
  end;

  chkSymbolsStrip := TCheckBox.Create(grpDebugging);
  with chkSymbolsStrip do
  begin
    Parent := grpDebugging;
    Caption := 'Strip Symbols From Executable';
    Top := 69;
    Left := 8;
    Height := 16;
    Width := 330;
    Visible := True;
  end;

  {------------------------------------------------------------}

  grpLinkLibraries := TGroupBox.Create(Self);
  with grpLinkLibraries do
  begin
    Parent := nbMain.Page[2];
    Top := grpDebugging.Top + grpDebugging.Height + 10;
    Left := 10;
    Height := 70;
    Width := 350;
    Caption := 'Link Libraries:';
    Visible := True;
  end;

  radLibsLinkDynamic := TRadioButton.Create(grpLinkLibraries);
  with radLibsLinkDynamic do
  begin
    Parent := grpLinkLibraries;
    Caption := 'Link With Dynamic Libraries';
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 330;
    Visible := True;
  end;

  radLibsLinkStatic := TRadioButton.Create(grpLinkLibraries);
  with radLibsLinkStatic do
  begin
    Parent := grpLinkLibraries;
    Caption := 'Link With Static Libraries';
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
    Parent := nbMain.Page[2];
    Top := grpLinkLibraries.Top + grpLinkLibraries.Height + 10;
    Left := 10;
    Height := 75;
    Width := 350;
    Caption := 'Options:';
    Visible := True;
  end;

  chkOptionsLinkOpt := TCheckBox.Create(grpOptions);
  with chkOptionsLinkOpt do
  begin
    Parent := grpOptions;
    Caption := 'Pass An Option To The Linker';
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
    Width := 330;
    Text := '';
    Visible := True;
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupOtherTab                                            }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupOtherTab(Sender: TObject);
begin
  // Setup the Other Tab
  Assert(False, 'Trace:Setting up compiler options other tab');

  grpVerbosity := TGroupBox.Create(Self);
  with grpVerbosity do
  begin
    Parent := nbMain.Page[3];
    Top := 10;
    Left := 10;
    Height := 170;
    Width := 350;
    Caption := 'Verbosity:';
    Visible := True;
  end;

  chkErrors := TCheckBox.Create(grpVerbosity);
  with chkErrors do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Errors';
    Top := 6;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkWarnings := TCheckBox.Create(grpVerbosity);
  with chkWarnings do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Warnings';
    Top := 27;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkNotes := TCheckBox.Create(grpVerbosity);
  with chkNotes do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Notes';
    Top := 48;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkHints := TCheckBox.Create(grpVerbosity);
  with chkHints do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Hints';
    Top := 69;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkGeneralInfo := TCheckBox.Create(grpVerbosity);
  with chkGeneralInfo do
  begin
    Parent := grpVerbosity;
    Caption := 'Show General Info';
    Top := 90;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkLineNumbers := TCheckBox.Create(grpVerbosity);
  with chkLineNumbers do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Line Numbers';
    Top := 111;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkEverything := TCheckBox.Create(grpVerbosity);
  with chkEverything do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Everything';
    Top := 132;
    Left := 8;
    Height := 16;
    Width := 140;
    Visible := True;
  end;

  chkDebugInfo := TCheckBox.Create(grpVerbosity);
  with chkDebugInfo do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Debug Info';
    Top := 6;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  chkUsedFiles := TCheckBox.Create(grpVerbosity);
  with chkUsedFiles do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Used Files';
    Top := 27;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  chkTriedFiles := TCheckBox.Create(grpVerbosity);
  with chkTriedFiles do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Tried Files';
    Top := 48;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  chkDefinedMacros := TCheckBox.Create(grpVerbosity);
  with chkDefinedMacros do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Defined Macros';
    Top := 69;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  chkCompiledProc := TCheckBox.Create(grpVerbosity);
  with chkCompiledProc do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Compiled Procedure';
    Top := 90;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  chkConditionals := TCheckBox.Create(grpVerbosity);
  with chkConditionals do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Conditionals';
    Top := 111;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  chkNothing := TCheckBox.Create(grpVerbosity);
  with chkNothing do
  begin
    Parent := grpVerbosity;
    Caption := 'Show Nothing (only errors)';
    Top := 132;
    Left := 160;
    Height := 16;
    Width := 180;
    Visible := True;
  end;

  {------------------------------------------------------------}

  chkFPCLogo := TCheckBox.Create(Self);
  with chkFPCLogo do
  begin
    Parent := nbMain.Page[3];
    Caption := 'Write An FPC Logo';
    Top := grpVerbosity.Top + grpVerbosity.Height + 12;
    Left := 10;
    Height := 16;
    Width := 150;
    Visible := True;
  end;


  {------------------------------------------------------------}

  grpConfigFile := TGroupBox.Create(Self);
  with grpConfigFile do
  begin
    Parent := nbMain.Page[3];
    Top := grpVerbosity.Top + grpVerbosity.Height + 40;
    Left := 10;
    Height := 95;
    Width := 350;
    Caption := 'Config Files:';
    Visible := True;
  end;

  chkConfigFile := TCheckBox.Create(grpConfigFile);
  with chkConfigFile do
  begin
    Parent := grpConfigFile;
    Caption := 'Use Compiler Config File (ppc386.cfg)';
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
    Caption := 'Use Additional Compiler Config File';
    Top := 27;
    Left := 8;
    Height := 16;
    Width := 330;
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
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupSearchPathsTab                                      }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupSearchPathsTab(Sender: TObject);
begin
  // Setup the Search Paths Tab
  Assert(False, 'Trace:Setting up compiler options search paths tab');

  grpIncludeFiles := TGroupBox.Create(Self);
  with grpIncludeFiles do
  begin
    Parent := nbMain.Page[4];
    Top := 10;
    Left := 10;
    Height := 55;
    Width := 350;
    Caption := 'Include Files:';
    Visible := True;
  end;

  edtIncludeFiles := TEdit.Create(grpIncludeFiles);
  with edtIncludeFiles do
  begin
    Parent := grpIncludeFiles;
    Top := 8;
    Left := 8;
    Height := 23;
    Width := 330;
    Text := '';
    Visible := True;
  end;


  {------------------------------------------------------------}

  grpLibraries := TGroupBox.Create(Self);
  with grpLibraries do
  begin
    Parent := nbMain.Page[4];
    Top := grpIncludeFiles.Top + grpIncludeFiles.Height + 7;
    Left := 10;
    Height := 55;
    Width := 350;
    Caption := 'Libraries:';
    Visible := True;
  end;

  edtLibraries := TEdit.Create(grpLibraries);
  with edtLibraries do
  begin
    Parent := grpLibraries;
    Top := 8;
    Left := 8;
    Height := 23;
    Width := 330;
    Text := '';
    Visible := True;
  end;

  {------------------------------------------------------------}

  grpOtherUnits := TGroupBox.Create(Self);
  with grpOtherUnits do
  begin
    Parent := nbMain.Page[4];
    Top := grpLibraries.Top + grpLibraries.Height + 7;
    Left := 10;
    Height := 55;
    Width := 350;
    Caption := 'Other Unit Files:';
    Visible := True;
  end;

  edtOtherUnits := TEdit.Create(grpOtherUnits);
  with edtOtherUnits do
  begin
    Parent := grpOtherUnits;
    Top := 8;
    Left := 8;
    Height := 23;
    Width := 330;
    Text := '';
    Visible := True;
  end;
  
  {------------------------------------------------------------}

  grpCompiler := TGroupBox.Create(Self);
  with grpCompiler do
  begin
    Parent := nbMain.Page[4];
    Top := grpOtherUnits.Top + grpOtherUnits.Height + 7;
    Left := 10;
    Height := 55;
    Width := 350;
    Caption := 'Path To Compiler:';
    Visible := True;
  end;

  edtCompiler := TEdit.Create(grpCompiler);
  with edtCompiler do
  begin
    Parent := grpCompiler;
    Top := 8;
    Left := 8;
    Height := 23;
    Width := 330;
    Text := '';
    Visible := True;
  end;
  
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupButtonBar                                           }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupButtonBar(Sender: TObject);
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
    Caption := 'Apply';
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
    Caption := 'Cancel';
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

  { Test button for testing MakeOptionsString function. Remove
    when this function is working correctly. }
  btnTest := TButton.Create(Self);
  with btnTest do
  begin
    Parent := Self;
    Width := 70;
    Height := 23; 
    Top := Self.Height - btnTest.Height - 15;
    Left := btnOK.Left - btnTest.Width - 5;
    Caption := 'Test';
    OnClick := @ButtonTestClicked;
    Visible := True;
  end;
end;

initialization
  CompilerOpts := TCompilerOptions.Create;

finalization
  CompilerOpts.Free;
  
end.

