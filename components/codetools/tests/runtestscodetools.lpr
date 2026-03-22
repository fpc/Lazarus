{ Copyright (C) 2013 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

  Abstract:
    Testsuites for codetools.

  Usage:

}
program runtestscodetools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Classes, sysutils, dom,
  // FPCUnit
  fpcunit, consoletestrunner,
  // LazUtils
  LazLogger, LazUTF8, LazStringUtils, LazFileUtils,
  // IdeConfig
  LazConf, EnvironmentOpts, TransferMacros, BaseBuildManager,
  // BuildIntf
  MacroIntf,
  // CodeTools
  CodeToolManager, CodeToolsConfig,
  // Test suites
  TestCfgScript, TestCTH2Pas, TestCTXMLFixFragments,
  {$IFDEF Darwin}
  fdt_objccategory, fdt_objcclass,
  {$ENDIF}
  TestBasicCodetools, TestCTRangeScan, TestPascalParser, TestMethodJumpTool,
  TestStdCodetools, TestFindDeclaration, TestIdentCompletion, TestCompleteBlock,
  TestRefactoring, TestCodeCompletion, TestCompReaderWriterPas,
  fdt_arrays, TestCTPas2js, TestChangeDeclaration, TestLFMTrees, 
  TestDesignerFormTools;

const
  ConfigFilename = 'codetools.config';

type

  { TCTTestRunner }

  TCTTestRunner = class(TTestRunner)
  private
    FSubmitter: string;
    FMachine: string;
    procedure DummyLog(Sender: TObject; {%H-}S: string; var Handled: Boolean);
  protected
    Options: TCodeToolsOptions;
    procedure AppendLongOpts; override;
    function ParseOptions: Boolean; override;
    procedure WriteCustomHelp; override;

    procedure ExtendXmlDocument(Doc: TXMLDocument); override;
  public
    destructor Destroy; override;
  end;

{ Utils }

// TODO: refactor this in Lazarus packages

var
  MyMacroConf: TMacroConfig;

// code is based on the GetParamsAndCfgFile function of the IDECmdLine unit
function ReadLazarusCfgFile(aContent: TStringList; aParams: array of string): string;
var
  lLine, lPar, s: string;
begin
  result := '';
  if aContent = nil then exit;
  for s in aContent do
  begin
    lLine := Trim(s); // Lazarus ignores extra spaces
    if lLine = '' then continue;
    {$IFDEF WINDOWS} // file created by the Windows installer may have a system codepage
    if FindInvalidUTF8Codepoint(PChar(lLine), length(lLine), true) > 0 then
      lLine := WinCPToUTF8(lLine);
    {$ENDIF WINDOWS}
    for lPar in aParams do
      if LazStartsText(lPar, lLine) then
        result := RightStr(lLine, length(lLine) - length(lPar)).DeQuotedString('"');
  end;
end;

// code is based on the GetParamsAndCfgFile function of the IDECmdLine unit
function ReadLazarusCfgFile(aFileName: string; aParams: array of string): string;
var
  lCfgDir: string;
  lContent: TStringList;
begin
  result := '';
  if not FileExistsUTF8(aFileName) then exit;
  lCfgDir := ExtractFileDir(aFileName);
  lContent := TStringList.Create;
  try
    lContent.LoadFromFile(aFileName);
    result := ReadLazarusCfgFile(lContent, aParams);
    // expand relative filenames in "lazarus.cfg" using the path of the config file
    result := ExpandFileNameUTF8(result, lCfgDir);
  finally
    FreeAndNil(lContent);
  end;
end;

// code is based on LazBuild
procedure FreeIdeEnvOpt;
begin
  FreeAndNil(GlobalMacroList);
  FreeAndNil(IDEMacros);
  FreeAndNil(MyMacroConf);
end;

// code is based on LazBuild
function LoadIdeEnvOpt(aLazDir: string): TEnvironmentOptions;
const
  cLazarusCfgName = 'lazarus.cfg';
  cPCPOptLong  = '--primary-config-path=';
  cPCPOptShort = '--pcp=';
var
  lExeDir, lLazDir, lCfgFile, lPCP: string;
begin
  if assigned(EnvironmentOptions) then FreeIdeEnvOpt;
  result := nil;
  try
    lExeDir := ExtractFileDir(ParamStrUTF8(0));
    lLazDir := ExpandFileNameUTF8(aLazDir, lExeDir);
    lCfgFile := AppendPathDelim(lLazDir) + cLazarusCfgName;
    // scp
    SetSecondaryConfigPath(lLazDir);
    // pcp
    if FileExistsUTF8(lCfgFile) then
    begin
      lPCP := ReadLazarusCfgFile(lCfgFile, [cPCPOptLong, cPCPOptShort]);
      if lPCP <> '' then
        SetPrimaryConfigPath(lPCP);
    end;
    // macros
    MyMacroConf := TMacroConfig.Create(nil); // creates EnvironmentOptions
    MyMacroConf.SetupTransferMacros;
    IDEMacros.LoadBuildMacros;
    // env
    EnvironmentOptions.CreateConfig;
    EnvironmentOptions.Load(false);
    result := EnvironmentOptions;
  except
    if result = nil then
      FreeIdeEnvOpt;
  end;
end;

function ReadFPCSrcFromIdeOpts(aLazDir: string): string;
begin
  result := '';
  try
    try
      if LoadIdeEnvOpt(aLazDir) <> nil then
        // first check the existence of the original value
        if EnvironmentOptions.FPCSourceDirectory <> '' then
          // then read the parsed value
          result := EnvironmentOptions.GetParsedFPCSourceDirectory;
    finally
      FreeIdeEnvOpt;
    end;
  except
    // ignore
  end;
end;

{ TCTTestRunner }

procedure TCTTestRunner.DummyLog(Sender: TObject; S: string; var Handled: Boolean);
begin
  Handled:=true;
end;

procedure TCTTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('machine:');
  LongOpts.Add('submitter:');
  LongOpts.Add('filemask:');
  LongOpts.Add('verbose');
end;

function TCTTestRunner.ParseOptions: Boolean;
const
  cLazarusSrcDir = '..\..\..'; // only current installation
begin
  Result:=inherited ParseOptions;

  // CodeTools (tests) assume a working folder "components\codetools\tests"
  SetCurrentDirUTF8(ExtractFileDir(ParamStrUTF8(0)));

  if Options=nil then
    Options:=TCodeToolsOptions.Create;
  if FileExists(ConfigFilename) then begin
    // To not parse the FPC sources every time, the options are saved to a file.
    Options.LoadFromFile(ConfigFilename);
  end;
  Options.InitWithEnvironmentVariables;

  if not HasOption('verbose') then
  begin
    LazLogger.DebugLogger.OnDebugLn:=@DummyLog;
    LazLogger.DebugLogger.OnDbgOut:=@DummyLog;
  end;

  if HasOption('submitter') then
    FSubmitter := GetOptionValue('submitter');
  if HasOption('machine') then
    FMachine := GetOptionValue('machine');

  // assume that the test executable is always inside the Lazarus folder
  if Options.LazarusSrcDir='' then
    Options.LazarusSrcDir:=ExpandFileNameUTF8(cLazarusSrcDir,ExtractFileDir(ParamStrUTF8(0)));

  // read folder from Lazarus configuration if not specified
  if Options.FPCSrcDir='' then
    Options.FPCSrcDir:=ReadFPCSrcFromIdeOpts(Options.LazarusSrcDir);

  CodeToolBoss.Init(Options);

  // save the options and the FPC unit links results.
  Options.SaveToFile(ConfigFilename);
end;

procedure TCTTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln('Environment variables:');
  writeln('  PP=path of the compiler');
  writeln('  FPCDIR=path of the fpc sources');
  writeln('  LAZARUSDIR=path of the lazarus sources');
  writeln('  FPCTARGET=target OS');
  writeln('  FPCTARGETCPU=target cpu');
  writeln;
  writeln('Command line parameters:');
  writeln('  --submitter=SubmitterName     name of sumbitter of the test results');
  writeln('  --machine=MachineName         name of the machine the test runs on');
  writeln('  --verbose                     display debug output of tests');
end;

destructor TCTTestRunner.Destroy;
begin
  FreeAndNil(Options);
  inherited Destroy;
end;

procedure TCTTestRunner.ExtendXmlDocument(Doc: TXMLDocument);
var
  env: TDOMElement;
  procedure AddElement(const name, value: string);
  var
    n: TDOMElement;
  begin
    n := Doc.CreateElement(DOMString(name));
    n.AppendChild(Doc.CreateTextNode(DOMString(value)));
    env.AppendChild(n);
  end;
begin
  inherited ExtendXmlDocument(Doc);
  env := Doc.CreateElement('Environment');
  AddElement('CPU', {$I %FPCTARGETCPU%});
  AddElement('OS', {$I %FPCTARGETOS%});
  AddElement('FPCVersion', {$I %FPCVERSION%});
  AddElement('Submitter', FSubmitter);
  AddElement('Machine', FMachine);
  Doc.FirstChild.AppendChild(env);
end;

var
  App: TCTTestRunner;
begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  App := TCTTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console runner for the CodeTools Find Declaration Suite.';
  App.Run;
  App.Free;
end.

