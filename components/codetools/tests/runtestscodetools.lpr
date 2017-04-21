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
  Classes, sysutils, consoletestrunner, dom, fpcunit, LazLogger,
  CodeToolManager, CodeToolsConfig,
  TestGlobals,
  // non Pascal
  TestCfgScript, TestCTH2Pas, TestCTXMLFixFragments,
  // compile test files to make sure they are valid Pascal
  fdt_classhelper,
  {$IF FPC_FULLVERSION >= 30101}
  fdt_typehelper,
  {$ENDIF}
  fdt_nestedclasses,
  {$IFDEF Darwin}
  fdt_objccategory, fdt_objcclass,
  {$ENDIF}
  fdt_classof, fdt_with, rt_explodewith, fdt_generics,
  TestBasicCodetools, TestCTRangeScan, TestPascalParser, TestMethodJumpTool,
  TestStdCodetools, TestFindDeclaration, TestCompleteBlock, TestRefactoring;

const
  ConfigFilename = 'codetools.config';

type

  { TCTTestRunner }

  TCTTestRunner = class(TTestRunner)
  private
    FSubmitter: string;
    FMachine: string;
  protected
    Options: TCodeToolsOptions;
    procedure AppendLongOpts; override;
    procedure ParseOptions; override;
    procedure WriteCustomHelp; override;

    procedure ExtendXmlDocument(Doc: TXMLDocument); override;
  public
    destructor Destroy; override;
  end;

{ TCTTestRunner }

procedure TCTTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('machine:');
  LongOpts.Add('submitter:');
end;

procedure TCTTestRunner.ParseOptions;
begin
  inherited ParseOptions;

  if Options=nil then
    Options:=TCodeToolsOptions.Create;
  if FileExists(ConfigFilename) then begin
    // To not parse the FPC sources every time, the options are saved to a file.
    Options.LoadFromFile(ConfigFilename);
  end;
  DebugLn(['Possible EnvVars: PP, FPCDIR, LAZARUSDIR, FPCTARGET, FPCTARGETCPU']);
  Options.InitWithEnvironmentVariables;

  if HasOption('submitter') then
    FSubmitter := GetOptionValue('submitter');
  if HasOption('machine') then
    FMachine := GetOptionValue('machine');

  if Options.FPCSrcDir='' then
    Options.FPCSrcDir:=ExpandFileName('~/freepascal/fpc');
  if Options.LazarusSrcDir='' then
    Options.LazarusSrcDir:=ExpandFileName('~/pascal/lazarus');

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

