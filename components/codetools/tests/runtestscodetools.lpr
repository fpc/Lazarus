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
  Classes, SysUtils, DOM, IniFiles,
  // FPCUnit
  ConsoleTestRunner,
  // LazUtils
  LazStringUtils, LazUTF8, LazLogger, LazFileUtils,
  // IdeConfig
  LazConf, EnvironmentOpts, TransferMacros, BaseBuildManager,
  // BuildIntf
  MacroIntf,
  // CodeTools
  CodeToolManager, CodeToolsConfig, DefineTemplates,
  // (test suites)
  TestCfgScript, TestCTH2Pas, TestCTXMLFixFragments,
  TestBasicCodetools, TestCTRangeScan, TestPascalParser, TestMethodJumpTool,
  TestStdCodetools, TestFindDeclaration, TestIdentCompletion, TestCompleteBlock,
  TestRefactoring, TestCodeCompletion, TestCompReaderWriterPas,
  TestCTPas2js, TestChangeDeclaration, TestLFMTrees, TestDesignerFormTools;

const
  ConfigFilename = 'codetools.config';

type

  { TCTTestRunner }

  TCTTestRunner = class(TTestRunner)
  private
    FVerbose: boolean;
    FSubmitter: string;
    FMachine: string;
    procedure DummyLog(Sender: TObject; {%H-}S: string; var Handled: Boolean);
  protected
    Options: TCodeToolsOptions;
    procedure AppendLongOpts; override;
    {$IF FPC_FULLVERSION >= 30301}
    procedure ReadCustomDefaults(Ini: TMemIniFile; Section: string); override;
    {$ELSE}
    procedure ReadCustomDefaults(Ini: TMemIniFile; Section: string);
    procedure ReadDefaults; override;
    {$ENDIF}
    function ParseOptions: Boolean; override;
    procedure WriteCustomHelp; override;
    procedure ExtendXmlDocument(Doc: TXMLDocument); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ Utils }

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

procedure TCTTestRunner.ReadCustomDefaults(Ini: TMemIniFile; Section: string);
begin
  {$IF FPC_FULLVERSION >= 30301}
  inherited;
  {$ENDIF}
  FVerbose   := Ini.ReadBool  (Section, 'verbose'  , FVerbose  );
  FMachine   := Ini.ReadString(Section, 'machine'  , FMachine  );
  FSubmitter := Ini.ReadString(Section, 'submitter', FSubmitter);
end;

{$IF FPC_FULLVERSION < 30301}
procedure TCTTestRunner.ReadDefaults;
const
  CDefaultsFileIniSection = 'defaults';
var
  lFileName: string;
  lConf: TMemIniFile = nil;
begin
  inherited ReadDefaults;

  lFileName := DefaultsFileName;
  if FileExists(lFileName) then
  begin
    lConf := TMemIniFile.Create(lFileName);
    try
      lConf.Options := lConf.Options + [ifoStripQuotes];
      lConf.SetBoolStringValues(true , ['1', 'true' , 'y', 'yes', 'on' ]);
      lConf.SetBoolStringValues(false, ['0', 'false', 'n', 'no' , 'off']);

      ReadCustomDefaults(lConf, CDefaultsFileIniSection);
    finally
      FreeAndNil(lConf);
    end;
  end;
end;
{$ENDIF}

function TCTTestRunner.ParseOptions: Boolean;
const
  cLazarusSrcDir = '..\..\..'; // only current installation
var
  aBaseDir: string;
begin
  Result:=inherited ParseOptions;

  // do not initialize anything and do not pollute the output
  if HasOption('h','help') or HasOption('l', 'list') then
    exit;

  // CodeTools (tests) assume a working folder "components\codetools\tests"
  aBaseDir:=ExtractFileDir(ParamStrUTF8(0));
  SetCurrentDirUTF8(aBaseDir);

  if Options=nil then
    Options:=TCodeToolsOptions.Create;
  if FileExists(ConfigFilename) then begin
    // To not parse the FPC sources every time, the options are saved to a file.
    Options.LoadFromFile(ConfigFilename);
  end;
  Options.InitWithEnvironmentVariables;

  if HasOption('verbose') then
    FVerbose:=true;
  if HasOption('submitter') then
    FSubmitter := GetOptionValue('submitter');
  if HasOption('machine') then
    FMachine := GetOptionValue('machine');

  // assume that the test executable is inside the Lazarus folder
  if Options.LazarusSrcDir='' then
    Options.LazarusSrcDir:=ExpandFileNameUTF8(cLazarusSrcDir,aBaseDir);
  if not DirectoryExists(Options.LazarusSrcDir) then
    raise Exception.Create('Missing LazarusSrcDir "'+Options.LazarusSrcDir+'"');

  CodeToolBoss.Init(Options,[ctopSetVariables,ctopLoadCaches]);

  if (Options.FPCSrcDir='') or (Options.FPCPath='') then begin
    // read FPC path an/or FPCSrcDir from Lazarus configuration
    // Important: may contain macros like $(FPCVer), which queries codetools,
    //            so this must be queried after ctopSetVariables
    try
      if LoadIdeEnvOpt(Options.LazarusSrcDir)<>nil then begin
        // first check the existence of the original value, then read the parsed value
        if (Options.FPCPath='') and (EnvironmentOptions.CompilerFilename <> '') then
          // Note: may contain macros like $(LazarusDir)
          Options.FPCPath := EnvironmentOptions.GetParsedCompilerFilename;

        if (Options.FPCPath='') or not FileExists(Options.FPCPath) then begin
          writeln('Error: The environment variable "PP" is not assigned. File not found: "',Options.FPCPath,'"');
          halt(1);
        end;

        // now there is a fpc path, which can be used to resolve macros in FPCSrcDir

        if (Options.FPCSrcDir='') and (EnvironmentOptions.FPCSourceDirectory <> '') then begin
          // Note: may contain macros like $(FPCVer), which queries codetools
          Options.FPCSrcDir := EnvironmentOptions.GetParsedFPCSourceDirectory;
          CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'FPCSrcDir']:=Options.FPCSrcDir;
        end;
      end;
    finally
      FreeIdeEnvOpt;
    end;
  end;

  // FPC source path
  if Options.FPCSrcDir='' then begin
    writeln('Error: The environment variable "','FPCDIR','" is not assigned');
    halt(1);
  end;
  if not DirectoryExists(Options.FPCSrcDir) then begin
    writeln('Error: FPC source folder "',Options.FPCSrcDir,'" does not exist');
    halt(1);
  end;

  CodeToolBoss.Init(Options,[ctopScanFPCSrc..ctopAddDefines]);

  // save the options and the FPC unit links results.
  Options.SaveToFile(ConfigFilename);

  // disable debug output of test suites (don't do this before initialization above)
  if not FVerbose then
  begin
    LazLogger.DebugLogger.OnDebugLn:=@DummyLog;
    LazLogger.DebugLogger.OnDbgOut:=@DummyLog;
  end;
end;

procedure TCTTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln;
  writeln('Additional options:');
  writeln('  --verbose           Display debug output of tests');
  writeln('  --submitter=<name>  Name of submitter of the test results');
  writeln('  --machine=<name>    Name of the machine the test runs on');
  writeln('  ',fmparam,'=<filemask>, e.g. ''bug32252.pas'', or ''bug32*.pas''');
  writeln;
  writeln('Environment variables:');
  writeln('  LAZARUSDIR=<dir>    Path to Lazarus sources, default is the current IDE installation');
  writeln('  FPCDIR=<dir>        Path to FPC sources, default value is read from the IDE configuration');
  writeln('  PP=<file>           Path to the compiler file');
  writeln('  FPCTARGET=<OS>      Target OS');
  writeln('  FPCTARGETCPU=<CPU>  Target CPU');
end;

constructor TCTTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVerbose:=false;
  FSubmitter:='';
  FMachine:='';
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
  App.Title := 'FPCUnit runner for the CodeTools suites';
  App.Run;
  App.Free;
end.

