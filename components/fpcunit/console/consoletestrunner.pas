{ This unit contains the TTestRunner class, a base class for the console test
  runner for fpcunit.

  Copyright (C) 2006 Vincent Snijders

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit consoletestrunner;

{$mode objfpc}{$H+}

interface

uses
  custapp, Classes, SysUtils, fpcunit, testregistry, testreport, testutils,
  dom, xmlreporter, xmlwrite;

const
  Version = '0.2';

type
  TFormat = (fPlain, fLatex, fXML);

  { TTestRunner }

  TTestRunner = class(TCustomApplication)
  private
     FShowProgress: boolean;
     FFileName: string;
     FStyleSheet: string;
     FLongOpts: TStrings;
  protected
    property FileName: string read FFileName write FFileName;
    property LongOpts: TStrings read FLongOpts write FLongOpts;
    property ShowProgress: boolean read FShowProgress write FShowProgress;
    property StyleSheet: string read FStyleSheet write FStyleSheet;
    procedure DoRun; override;
    procedure doTestRun(aTest: TTest); virtual;
    function GetShortOpts: string; virtual;
    procedure AppendLongOpts; virtual;
    procedure WriteCustomHelp; virtual;
    procedure ParseOptions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

const
  ShortOpts = 'alhp';
  DefaultLongOpts: array[1..8] of string =
     ('all', 'list', 'progress', 'help',
      'suite:', 'format:', 'file:', 'stylesheet:');

  { TProgressWriter }
type
  TProgressWriter= class(TNoRefCountObject, ITestListener)
  private
    FSuccess: boolean;
  public
    destructor Destroy; override;

    { ITestListener interface requirements }
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
  end;

destructor TProgressWriter.Destroy;
begin
  // on descruction, just write the missing line ending
  writeln;
  inherited Destroy;
end;

procedure TProgressWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FSuccess := false;
  write('F');
end;

procedure TProgressWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuccess := false;
  write('E');
end;

procedure TProgressWriter.StartTest(ATest: TTest);
begin
  FSuccess := true; // assume success, until proven otherwise
end;

procedure TProgressWriter.EndTest(ATest: TTest);
begin
  if FSuccess then
    write('.');
end;

var
  FormatParam: TFormat;

procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
  progressWriter: TProgressWriter;

  procedure doXMLTestRun(aTest: TTest);
  var
    XMLResultsWriter: TXMLResultsWriter;
    
    procedure ExtendDocument(Doc: TXMLDocument);
    var
      n: TDOMElement;
    begin
      if StyleSheet<>'' then begin
        Doc.StylesheetType := 'text/xsl';
        Doc.StylesheetHRef := StyleSheet;
      end;
      n := Doc.CreateElement('Title');
      n.AppendChild(Doc.CreateTextNode(Title));
      Doc.FirstChild.AppendChild(n);
    end;
    
  begin
    try
      XMLResultsWriter := TXMLResultsWriter.Create;
      testResult.AddListener(XMLResultsWriter);
      aTest.Run(testResult);
      XMLResultsWriter.WriteResult(testResult);
      ExtendDocument(XMLResultsWriter.Document);
      if FileName<>'' then
        WriteXMLFile(XMLResultsWriter.Document, FileName)
      else
        WriteXMLFile(XMLResultsWriter.Document, output);
    finally
      XMLResultsWriter.Free;
      testResult.Free;
    end;
  end;

  {$IFNDEF VER2_0}
  procedure doPlainTestRun(aTest: TTest);
  var
    PlainResultsWriter: TPlainResultsWriter;
  begin
    try
      PlainResultsWriter := TPlainResultsWriter.Create;
      testResult.AddListener(PlainResultsWriter);
      PlainResultsWriter.WriteHeader;
      aTest.Run(testResult);
      PlainResultsWriter.WriteResult(testResult);
    finally
      PlainResultsWriter.Free;
      testResult.Free;
    end;
  end;
  {$ENDIF}

begin
  testResult := TTestResult.Create;
  if ShowProgress then begin
    progressWriter := TProgressWriter.Create;
    testResult.AddListener(progressWriter);
  end;
  try
    case FormatParam of
      fLatex: doXMLTestRun(aTest); //no latex implemented yet
      {$IFNDEF VER2_0}
      fPlain: doPlainTestRun(aTest);
      {$ENDIF}
      else
        doXMLTestRun(aTest);
    end;
  finally
    if ShowProgress then
      progressWriter.Free;
  end;
end;

function TTestRunner.GetShortOpts: string;
begin
  Result := ShortOpts;
end;

procedure TTestRunner.AppendLongOpts;
var
  i: Integer;
begin
  for i := low(DefaultLongOpts) to high(DefaultLongOpts) do
    LongOpts.Add(DefaultLongOpts[i]);
end;

procedure TTestRunner.WriteCustomHelp;
begin
  // no custom help options in base class;
end;

procedure TTestRunner.ParseOptions;
begin
  if HasOption('h', 'help') or (ParamCount = 0) then
  begin
    writeln(Title);
    writeln(Version);
    writeln;
    writeln('Usage: ');
    writeln('  --format=latex            output as latex source (only list implemented)');
    {$IFNDEF VER2_0}
    writeln('  --format=plain            output as plain ASCII source');
    {$ENDIF}
    writeln('  --format=xml              output as XML source (default)');
    writeln('  --stylesheet=<reference>   add stylesheet reference');
    writeln('  --file=<filename>         output results to file');
    writeln;
    writeln('  -l or --list              show a list of registered tests');
    writeln('  -a or --all               run all tests');
    writeln('  -p or --progress          show progress');
    writeln('  --suite=MyTestSuiteName   run single test suite class');
    WriteCustomHelp;
    writeln;
    writeln('The results can be redirected to an xml file,');
    writeln('for example: ', ParamStr(0),' --all > results.xml');
  end;

  //get the format parameter
  FormatParam := fXML;
  if HasOption('format') then
  begin
    if GetOptionValue('format') = 'latex' then
      FormatParam := fLatex;
    if GetOptionValue('format') = 'plain' then
      FormatParam := fPlain;
  end;

  ShowProgress := HasOption('p', 'progress');

  if HasOption('file') then
    FileName := GetOptionValue('file');
  if HasOption('stylesheet') then
    StyleSheet := GetOptionValue('stylesheet');
end;

constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLongOpts := TStringList.Create;
  AppendLongOpts;
end;

destructor TTestRunner.Destroy;
begin
  FLongOpts.Free;
  inherited Destroy;
end;

procedure TTestRunner.DoRun;
var
  I: integer;
  S: string;
begin
  S := CheckOptions(GetShortOpts, LongOpts);
  if (S <> '') then
    Writeln(S);

  ParseOptions;

  //get a list of all registed tests
  if HasOption('l', 'list') then
    case FormatParam of
      fLatex: Write(GetSuiteAsLatex(GetTestRegistry));
      {$IFNDEF VER2_0}
      fPlain: Write(GetSuiteAsPlain(GetTestRegistry));
      {$ENDIF}
      else
        Write(GetSuiteAsXML(GetTestRegistry));
    end;

  //run the tests
  if HasOption('a', 'all') then
    doTestRun(GetTestRegistry)
  else
  if HasOption('suite') then
  begin
    S := '';
    S := GetOptionValue('suite');
    if S = '' then
      for I := 0 to GetTestRegistry.Tests.Count - 1 do
        writeln(GetTestRegistry[i].TestName)
    else
      for I := 0 to GetTestRegistry.Tests.Count - 1 do
        if GetTestRegistry[i].TestName = S then
          doTestRun(GetTestRegistry[i]);
  end;
  Terminate;
end;

end.

