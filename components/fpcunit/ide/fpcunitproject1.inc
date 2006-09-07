NewSource := 
    'program FPCUnitProject1;' + #13
  + #13
  + '{$mode objfpc}{$H+}' + #13
  + #13
  + 'uses' + #13
  + '  custapp, Classes, SysUtils, fpcunit, testregistry,'#13
  + '  dom, testreport, xmlreporter, xmlwrite;'#13
  + #13
  + 'const' + #13
  + '  ShortOpts = ''alh'';' + #13
  + '  Longopts: array[1..5] of string = (''all'', ''list'', ''format:'', ''suite:'', ''help'');' + #13
  + '  Version = ''Version 0.1'';' + #13
  + #13
  + 'type' + #13
  + '  TFormat = (fPlain, fLatex, fXML);' + #13
  + #13
  + '  TTestRunner = class(TCustomApplication)' + #13
  + '  private' + #13
  + '  protected' + #13
  + '    procedure DoRun; override;' + #13
  + '    procedure doTestRun(aTest: TTest); virtual;' + #13
  + '  public' + #13
  + '  end;' + #13
  + #13
  + 'var' + #13
  + '  FormatParam: TFormat;' + #13
  + #13
  + '  procedure TTestRunner.doTestRun(aTest: TTest);' + #13
  + '  var' + #13
  + '    testResult: TTestResult;' + #13
  + #13
  + '    procedure doXMLTestRun(aTest: TTest);' + #13
  + '    var' + #13
  + '      XMLResultsWriter: TXMLResultsWriter;' + #13
  + '    begin' + #13
  + '      try' + #13
  + '        XMLResultsWriter := TXMLResultsWriter.Create;' + #13
  + '        testResult.AddListener(XMLResultsWriter);' + #13
  + '        aTest.Run(testResult);' + #13
  + '        XMLResultsWriter.WriteResult(testResult);' + #13
  + '        WriteXMLFile(XMLResultsWriter.Document, output);'#13
  + '      finally' + #13
  + '        XMLResultsWriter.Free;' + #13
  + '        testResult.Free;' + #13
  + '      end;' + #13
  + '    end;' + #13
  + #13
  + '    {$IFNDEF VER2_0}' + #13
  + '      procedure doPlainTestRun(aTest: TTest);' + #13
  + '      var' + #13
  + '        PlainResultsWriter: TPlainResultsWriter;' + #13
  + '      begin' + #13
  + '        try' + #13
  + '          PlainResultsWriter := TPlainResultsWriter.Create;' + #13
  + '          testResult.AddListener(PlainResultsWriter);' + #13
  + '          PlainResultsWriter.WriteHeader;' + #13
  + '          aTest.Run(testResult);' + #13
  + '          PlainResultsWriter.WriteResult(testResult);' + #13
  + '        finally' + #13
  + '          PlainResultsWriter.Free;' + #13
  + '          testResult.Free;' + #13
  + '        end;' + #13
  + '      end;' + #13
  + '    {$ENDIF}' + #13
  + #13
  + '  begin' + #13
  + '    testResult := TTestResult.Create;' + #13
  + #13
  + '    case FormatParam of' + #13
  + '      fLatex: doXMLTestRun(aTest); //no latex implemented yet' + #13
  + '      {$IFNDEF VER2_0}' + #13
  + '        fPlain: doPlainTestRun(aTest);' + #13
  + '      {$ENDIF}' + #13
  + '      else' + #13
  + '        doXMLTestRun(aTest);' + #13
  + '    end;' + #13
  + '  end;' + #13
  + #13
  + '  procedure TTestRunner.DoRun;' + #13
  + '  var' + #13
  + '    I: integer;' + #13
  + '    S: string;' + #13
  + '  begin' + #13
  + '    S := CheckOptions(ShortOpts, LongOpts);' + #13
  + '    if (S <> '''') then' + #13
  + '      Writeln(S);' + #13
  + #13
  + '    if HasOption(''h'', ''help'') or (ParamCount = 0) then' + #13
  + '    begin' + #13
  + '      writeln(Title);' + #13
  + '      writeln(Version);' + #13
  + '      writeln;' + #13
  + '      writeln(''Usage: '');' + #13
  + '      writeln(''  --format=latex            output as latex source (only list implemented)'');' + #13
  + '      {$IFNDEF VER2_0}' + #13
  + '        writeln(''  --format=plain            output as plain ASCII source'');' + #13
  + '      {$ENDIF}' + #13
  + '      writeln(''  --format=xml              output as XML source (default)'');' + #13
  + '      writeln;' + #13
  + '      writeln(''  -l or --list              show a list of registered tests'');' + #13
  + '      writeln(''  -a or --all               run all tests'');' + #13
  + '      writeln(''  --suite=MyTestSuiteName   run single test suite class'');' + #13
  + '      writeln;' + #13
  + '      writeln(''The results can be redirected to an xml file,'');' + #13
  + '      writeln(''for example: ./testrunner --all > results.xml'');' + #13
  + '    end;' + #13
  + #13
  + '    //get the format parameter' + #13
  + '    FormatParam := fXML;' + #13
  + '    if HasOption(''format'') then' + #13
  + '    begin' + #13
  + '      if GetOptionValue(''format'') = ''latex'' then' + #13
  + '        FormatParam := fLatex;' + #13
  + '      {$IFNDEF VER2_0}' + #13
  + '        if GetOptionValue(''format'') = ''plain'' then' + #13
  + '          FormatParam := fPlain;' + #13
  + '      {$ENDIF}' + #13
  + '    end;' + #13
  + #13
  + '    //get a list of all registed tests' + #13
  + '    if HasOption(''l'', ''list'') then' + #13
  + '      case FormatParam of' + #13
  + '        fLatex: Write(GetSuiteAsLatex(GetTestRegistry));' + #13
  + '        {$IFNDEF VER2_0}' + #13
  + '          fPlain: Write(GetSuiteAsPlain(GetTestRegistry));' + #13
  + '        {$ENDIF}' + #13
  + '        else' + #13
  + '          Write(GetSuiteAsXML(GetTestRegistry));' + #13
  + '      end;' + #13
  + #13
  + '    //run the tests' + #13
  + '    if HasOption(''a'', ''all'') then' + #13
  + '      doTestRun(GetTestRegistry)' + #13
  + '    else' + #13
  + '    if HasOption(''suite'') then' + #13
  + '    begin' + #13
  + '      S := '''';' + #13
  + '      S := GetOptionValue(''suite'');' + #13
  + '      if S = '''' then' + #13
  + '        for I := 0 to GetTestRegistry.Tests.Count - 1 do' + #13
  + '          writeln(GetTestRegistry[i].TestName)' + #13
  + '      else' + #13
  + '        for I := 0 to GetTestRegistry.Tests.Count - 1 do' + #13
  + '          if GetTestRegistry[i].TestName = S then' + #13
  + '            doTestRun(GetTestRegistry[i]);' + #13
  + '    end;' + #13
  + '    Terminate;' + #13
  + '  end;' + #13
  + #13
  + 'var' + #13
  + '  App: TTestRunner;' + #13
  + #13
  + 'begin' + #13
  + '  App := TTestRunner.Create(nil);' + #13
  + '  App.Initialize;' + #13
  + '  App.Title := ''FPCUnit Console Test Case runner.'';' + #13
  + '  App.Run;' + #13
  + '  App.Free;' + #13
  + 'end.' + #13
;
