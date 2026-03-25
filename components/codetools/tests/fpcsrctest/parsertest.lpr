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
}
program parsertest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DOM,
  // FPCUnit
  ConsoleTestRunner,
  // LazUtils
  LazLogger,
  // CodeTools
  CodeToolManager, CodeToolsConfig, ParserTBase;

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
    function ParseOptions: boolean; override;
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

function TCTTestRunner.ParseOptions: boolean;
begin
  result := inherited ParseOptions;
  if not result then exit;

  if Options=nil then
    Options:=TCodeToolsOptions.Create;
  if FileExists(ConfigFilename) then begin
    // To not parse the FPC sources every time, the options are saved to a file.
    Options.LoadFromFile(ConfigFilename);
  end;
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
  writeln;
  writeln('Additional options:');
  writeln('  --submitter=<name>  Name of submitter of the test results');
  writeln('  --machine=<name>    Name of the machine the test runs on');
  writeln;
  writeln('Environment variables:');
  writeln('  LAZARUSDIR=<dir>    Path to Lazarus sources');
  writeln('  FPCDIR=<dir>        Path to FPC sources');
  writeln('  PP=<file>           Path to the compiler file');
  writeln('  FPCTARGET=<OS>      Target OS');
  writeln('  FPCTARGETCPU=<CPU>  Target CPU');
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
  App := TCTTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit runner for the CodeTools parser suite';
  App.Run;
  App.Free;
end.

