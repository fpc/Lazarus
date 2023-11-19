unit strtestcaseopts;

{$mode objfpc}{$H+}

interface

Const
  STestResult = '$Result';
  STestPath = '$Path';

resourcestring
  //visual components captions
  sfrmTest = 'TestCase Options';
  sgrpNames = 'Names';
  slblDefault = 'Default Test Name';
  sgrpFixture = 'Fixture';
  schkSetup = 'Create Setup Method';
  schkTear = 'Create TearDown method';
  sbtnCreate = 'Create unit';

  sFPCUnTestApp = 'FPCUnit Test Application';
  sFPCUnTestAppDesc = 'An application to run FPCUnit test cases.';
  sFPCUnTestCase = 'FPCUnit Test Case';
  sFPCUnTestCaseDesc = 'A unit containing a FPCUnit Test Case.';
  sWriteYourOwnTest = 'Write your own test';
  sFPCUnConsoleTestApp = 'FPCUnit Console Test Application';
  sFPCUnConsoleTestDesc = 'An application to run FPCUnit test cases in console mode.';

  sRunAllTests = '&Run all tests by default';
  sUseTextInsight = 'Use &testinsight to communicate results to the IDE';
  sSkipTimingInfo = '&Omit timing info in output';
  sCreateFirstTestCase = '&Create first test case';
  sDefaultOutputFormat = 'Default output &format';

  rsTestInsightTitle = 'Test Insight';

  rsAllTests = 'All Tests';
  rsRun = 'Run ';
  rsRuns = 'Runs: %s/%s';
  rsErrors = '%s    Errors: %s';
  rsFailures = '%s     Failures: %s';
  rsMessage = 'Message: %s';
  rsException = 'Exception: %s';
  rsExceptionMes = 'Exception message: %s';
  rsExceptionCla = 'Exception class: %s';
  rsUnitName = 'Unit name: %s';
  rsMethodName = 'Method name: %s';
  rsLineNumber = 'Line number: %s';
  rsRunning = 'Running %s';
  rsNumberOfExec = 'Number of executed tests: %s  Time elapsed: %s';
  // Visual components captions
  sfrmGUITest = 'FPCUnit - run unit test';
  sbtnRun = 'Run';
  sbtnRunH = 'Run highlighted test';
  sbtnClose = 'Close';
  stshTree = 'Testcase tree';
  stshResults = 'Results XML';
  sactRunAction = '&Run all';
  sactRunActionH = 'Run all checked tests';
  sactCloseForm = 'Quit';
  sactCloseFormH = 'Quit testing';
  sactCheckCurrentSuite = 'Select current suite';
  sactUncheckCurrentSuite = 'Deselect current suite';
  sactCheckAll = 'Select all tests';
  sactUncheckAll = 'Deselect all tests';
  sactRunHighlightedTest = 'Run selected';
  sactRunHighlightedTestH = 'Run selected test';
  smiActions = 'Actions';
  smiTestTree = 'Test tree';
  smiEdit = 'Edit';
  sactCopyAllToClipboard = 'Copy text to clipboard';
  sactCopyAllToClipboardH = 'Copy the entire text to clipboard';
  sactSaveResults = 'Save results';
  sactSaveResultsH = 'Save XML results to file';
  SNoExecutableAvailable = 'Test executable is not available: "%s"';
  SNoTestProjectConfigured = 'No test project configured.';
  SNavigationNotAvailable = 'Source navigation not available';
  rsCouldNotDete = 'Could not determine class and method from test path "%s"';
implementation

end.

