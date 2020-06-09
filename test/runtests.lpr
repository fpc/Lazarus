{ $Id$}
{ Copyright (C) 2006 Vincent Snijders

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
program runtests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, consoletestrunner,
  testglobals, testunits, dom,
  {Unit needed to set the LCL version and widget set name}
  LCLVersion, InterfaceBase, LCLPlatformDef, lazmouseandkeyinput, Interfaces,
  TestLazXML, TestAvgLvlTree, TestLConvEncoding, testlazfileutils;
  
type

  { TLazTestRunner }

  TLazTestRunner = class(TTestRunner)
  private
    FSubmitter: string;
    FMachine: string;
  protected
    procedure AppendLongOpts; override;
    function ParseOptions: Boolean; override;
    procedure WriteCustomHelp; override;

    procedure ExtendXmlDocument(Doc: TXMLDocument); override;
  end;

{ TLazTestRunner }

procedure TLazTestRunner.AppendLongOpts;
begin
  inherited;
  LongOpts.Add('compiler:');
  LongOpts.Add('pcp:');
  LongOpts.Add('machine:');
  LongOpts.Add('submitter:');
end;

function TLazTestRunner.ParseOptions: Boolean;
begin
  Result := inherited ParseOptions;
  if HasOption('compiler') then
    Compiler := GetOptionValue('compiler');
  if HasOption('pcp') then
    PrimaryConfigPath := GetOptionValue('pcp');
  if HasOption('submitter') then
    FSubmitter := GetOptionValue('submitter');
  if HasOption('machine') then
    FMachine := GetOptionValue('machine');
end;

procedure TLazTestRunner.WriteCustomHelp;
begin
  writeln('  --compiler=<ppcxxx>           use ppcxxx to build test projects');
  writeln('  --pcp=<primary-config-path>   pass primary-config-path to lazbuild');
  writeln('  --submitter=SubmitterName     name of sumbitter of the test results');
  writeln('  --machine=MachineName         name of the machine the test runs on');
end;

procedure TLazTestRunner.ExtendXmlDocument(Doc: TXMLDocument);
var
  env: TDOMElement;
  procedure AddElement(const name, value: string);
  var
    n: TDOMElement;
  begin
    n := Doc.CreateElement(UTF8Decode(name));
    n.AppendChild(Doc.CreateTextNode(UTF8Decode(value)));
    env.AppendChild(n);
  end;
begin
  inherited ExtendXmlDocument(Doc);
  env := Doc.CreateElement('Environment');
  AddElement('CPU', {$I %FPCTARGETCPU%});
  AddElement('OS', {$I %FPCTARGETOS%});
  AddElement('FPCVersion', {$I %FPCVERSION%});
  AddElement('LazVersion', lcl_version);
  AddElement('WidgetSet', LCLPlatformDirNames[WidgetSet.LCLPlatform]);
  AddElement('Submitter', FSubmitter);
  AddElement('Machine', FMachine);
  Doc.FirstChild.AppendChild(env);
end;

var
  App: TLazTestRunner;

begin
  App := TLazTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console runner for the Lazarus Test Suite.';
  App.Run;
  App.Free;
end.
