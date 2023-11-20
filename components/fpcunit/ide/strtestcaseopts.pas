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
  sUseTextInsight = 'Use &Test Insight to communicate results to the IDE';
  sCreateFirstTestCase = '&Create first test case';
  sDefaultOutputFormat = 'Default output &format';

  rsTestInsightTitle = 'Test Insight';
  rsServerPort = 'Server port';
  rsServerPath = 'Server path';
  rsAutomaticallyFetchTestListOnOpen = 'Automatically fetch test list on open';

  rsAllTests = 'All Tests';
  rsRuns = 'Runs: %s/%s';
  rsErrors = '%s    Errors: %s';
  rsFailures = '%s     Failures: %s';
  rsMessage = 'Message: %s';
  rsException = 'Exception: %s';
  rsExceptionMes = 'Exception message: %s';
  rsExceptionCla = 'Exception class: %s';
  // Visual components captions
  sfrmGUITest = 'FPCUnit - run unit test';
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
  smiCollapseNodes = 'Collapse Nodes';
  smiExpandNodes = 'Expand Nodes';
  rsNextError = 'Next error';
  rsPreviousError = 'Previous error';
  sactCopyAllToClipboard = 'Copy text to clipboard';
  sactCopyMessageToClipboard = 'Copy message to clipboard';
  sactCopyAllToClipboardH = 'Copy the entire text to clipboard';
  SNoExecutableAvailable = 'Test executable is not available: "%s"';
  SNoTestProjectConfigured = 'No test project configured.';
  SNavigationNotAvailable = 'Source navigation not available.';
  rsCouldNotDete = 'Could not determine class and method from test path "%s"';

implementation

end.

