program runtests;

{$mode objfpc}{$H+}

uses
  custapp, Classes, SysUtils, fpcunit, testregistry,
  dom, testreport, xmlreporter, xmlwrite,
  TestLpi;

const
  ShortOpts = 'alh';
  Longopts: array[1..5] of string = ('all', 'list', 'format:', 'suite:', 'help');
  Version = 'Version 0.1';

type
  TFormat = (fPlain, fLatex, fXML);

  TTestRunner = class(TCustomApplication)
  private

  protected
    procedure DoRun; override;
    procedure doTestRun(aTest: TTest); virtual;
  public
  end;

var
  FormatParam: TFormat;

  procedure TTestRunner.doTestRun(aTest: TTest);
  var
    testResult: TTestResult;

    procedure doXMLTestRun(aTest: TTest);
    var
      XMLResultsWriter: TXMLResultsWriter;
    begin
      try
        XMLResultsWriter := TXMLResultsWriter.Create;
        testResult.AddListener(XMLResultsWriter);
        aTest.Run(testResult);
        XMLResultsWriter.WriteResult(testResult);
        XMLResultsWriter.Document.StylesheetType := 'text/xsl';
        XMLResultsWriter.Document.StylesheetHRef := 'results.xsl';
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

    case FormatParam of
      fLatex: doXMLTestRun(aTest); //no latex implemented yet
      {$IFNDEF VER2_0}
        fPlain: doPlainTestRun(aTest);
      {$ENDIF}
      else
        doXMLTestRun(aTest);
    end;
  end;

  procedure TTestRunner.DoRun;
  var
    I: integer;
    S: string;
  begin
    S := CheckOptions(ShortOpts, LongOpts);
    if (S <> '') then
      Writeln(S);

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
      writeln;
      writeln('  -l or --list              show a list of registered tests');
      writeln('  -a or --all               run all tests');
      writeln('  --suite=MyTestSuiteName   run single test suite class');
      writeln;
      writeln('The results can be redirected to an xml file,');
      writeln('for example: ./testrunner --all > results.xml');
    end;

    //get the format parameter
    FormatParam := fXML;
    if HasOption('format') then
    begin
      if GetOptionValue('format') = 'latex' then
        FormatParam := fLatex;
      {$IFNDEF VER2_0}
        if GetOptionValue('format') = 'plain' then
          FormatParam := fPlain;
      {$ENDIF}
    end;

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

var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console Test Case runner.';
  App.Run;
  App.Free;
end.
