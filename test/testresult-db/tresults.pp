{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2007 by the Free Pascal development team.

    This unit contains the different possible outcome
    of a single test.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

unit tresults;

interface

uses
  teststr;


Type
  TTestStatus = (
  stFailedToCompile,
  stSuccessCompilationFailed,
  stFailedCompilationsuccessful,
  stSuccessfullyCompiled,
  stFailedToRun,
  stKnownRunProblem,
  stSuccessFullyRun,
  stSkippingGraphTest,
  stSkippingInteractiveTest,
  stSkippingKnownBug,
  stSkippingCompilerVersionTooLow,
  stSkippingCompilerVersionTooHigh,
  stSkippingOtherCpu,
  stSkippingOtherTarget,
  stskippingRunUnit,
  stskippingRunTest
  );


Const
  FirstStatus = stFailedToCompile;
  LastStatus = stskippingRunTest;

  TestOK : Array[TTestStatus] of Boolean = (
    False, // stFailedToCompile,
    True,  // stSuccessCompilationFailed,
    False, // stFailedCompilationsuccessful,
    True,  // stSuccessfullyCompiled,
    False, // stFailedToRun,
    True,  // stKnownRunProblem,
    True,  // stSuccessFullyRun,
    False, // stSkippingGraphTest,
    False, // stSkippingInteractiveTest,
    False, // stSkippingKnownBug,
    False, // stSkippingCompilerVersionTooLow,
    False, // stSkippingCompilerVersionTooHigh,
    False, // stSkippingOtherCpu,
    False, // stSkippingOtherTarget,
    False, // stskippingRunUnit,
    False  // stskippingRunTest
  );

  TestSkipped : Array[TTestStatus] of Boolean = (
    False,  // stFailedToCompile,
    False,  // stSuccessCompilationFailed,
    False,  // stFailedCompilationsuccessful,
    False,  // stSuccessfullyCompiled,
    False,  // stFailedToRun,
    False,  // stKnownRunProblem,
    False,  // stSuccessFullyRun,
    True,   // stSkippingGraphTest,
    True,   // stSkippingInteractiveTest,
    True,   // stSkippingKnownBug,
    True,   // stSkippingCompilerVersionTooLow,
    True,   // stSkippingCompilerVersionTooHigh,
    True,   // stSkippingOtherCpu,
    True,   // stSkippingOtherTarget,
    True,   // stskippingRunUnit,
    True    // stskippingRunTest
  );

  ExpectRun : Array[TTestStatus] of Boolean = (
    False,  // stFailedToCompile,
    False,  // stSuccessCompilationFailed,
    False,  // stFailedCompilationsuccessful,
    True ,  // stSuccessfullyCompiled,
    False,  // stFailedToRun,
    False,  // stKnownRunProblem,
    False,  // stSuccessFullyRun,
    False,  // stSkippingGraphTest,
    False,  // stSkippingInteractiveTest,
    False,  // stSkippingKnownBug,
    False,  // stSkippingCompilerVersionTooLow,
    False,  // stSkippingCompilerVersionTooHigh,
    False,  // stSkippingOtherCpu,
    False,  // stSkippingOtherTarget,
    False,  // stskippingRunUnit,
    False   // stskippingRunTest
   );

  StatusText : Array[TTestStatus] of String = (
    failed_to_compile,
    success_compilation_failed,
    failed_compilation_successful ,
    successfully_compiled ,
    failed_to_run ,
    known_problem ,
    successfully_run ,
    skipping_graph_test ,
    skipping_interactive_test ,
    skipping_known_bug ,
    skipping_compiler_version_too_low,
    skipping_compiler_version_too_high,
    skipping_other_cpu ,
    skipping_other_target ,
    skipping_run_unit ,
    skipping_run_test
  );

  SQLField : Array[TTestStatus] of String = (
    'TU_FAILEDTOCOMPILE',
    'TU_SUCCESSFULLYFAILED',
    'TU_FAILEDTOFAIL',
    'TU_SUCCESFULLYCOMPILED',
    'TU_FAILEDTORUN',
    'TU_KNOWNPROBLEM',
    'TU_SUCCESSFULLYRUN',
    'TU_SKIPPEDGRAPHTEST',
    'TU_SKIPPEDINTERACTIVETEST',
    'TU_KNOWNBUG',
    'TU_COMPILERVERIONTOOLOW',
    'TU_COMPILERVERIONTOOHIGH',
    'TU_OTHERCPU',
    'TU_OTHERTARGET',
    'TU_UNIT',
    'TU_SKIPPINGRUNTEST'
  );


implementation

end.

