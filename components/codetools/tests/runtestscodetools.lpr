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
  Classes, sysutils, dom,
  // FPCUnit
  fpcunit, consoletestrunner,
  // LazUtils
  LazLogger, LazUTF8, LazFileUtils,
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

const
  CDefaultLazarusSrcDirs: array of string = (
    '..\..\..' // only current installation
  );
  CDefaultFPCSrcDirs: array of string = (
    '..\..\..\fpc\3.2.2\source', // official installer
    '..\..\..\..\fpcsrc' // another installations (e.g. FpcUpDeluxe)
    {$IFDEF WINDOWS}
    ,'C:\lazarus\fpc\3.2.2\source'
    ,'C:\fpcsrc'
    {$ENDIF}
    {$IFDEF LINUX}
    ,'/usr/share/fpcsrc/3.2.2'
    ,'~/freepascal/fpc'
    {$ENDIF}
  );

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
  //
  function FindDefaultDir(Dirs: array of string; EnvVar: string): string;
  var
    lExeDir, lPath, lFullPath: string;
  begin
    lExeDir := ExtractFileDir(ParamStrUTF8(0));
    for lPath in Dirs do
    begin
      // expand paths relative to the executable file
      lFullPath := ExpandFileNameUTF8(lPath, lExeDir);
      if DirectoryExistsUTF8(lFullPath) then
        exit(lFullPath);
    end;
    writeln('Error: The environment variable "',EnvVar,'" is not assigned');
    halt(1);
  end;
  //
begin
  Result:=inherited ParseOptions;

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

  if Options.FPCSrcDir='' then
    Options.FPCSrcDir:=FindDefaultDir(CDefaultFPCSrcDirs,'FPCDIR');
  if Options.LazarusSrcDir='' then
    Options.LazarusSrcDir:=FindDefaultDir(CDefaultLazarusSrcDirs,'LAZARUSDIR');

  // some tests assume a working folder "components\codetools\tests"
  SetCurrentDirUTF8(ExtractFileDir(ParamStrUTF8(0)));

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

