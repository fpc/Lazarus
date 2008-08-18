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
  stOK,
  stFailed,
  stError,
  stIgnored
  );


Const
  FirstStatus = low(TTestStatus);
  LastStatus = high(TTestStatus);

  TestOK : Array[TTestStatus] of Boolean = (
    True,  // stOK
    False, // stFailed
    False, // stError
    False  // stIgnored
  );

  TestSkipped : Array[TTestStatus] of Boolean = (
    False,  // stOK
    False, // stFailed
    False, // stError
    True  // stIgnored
  );

  StatusText : Array[TTestStatus] of String = (
    success,
    failed,
    error,
    skipped
  );

  SQLField : Array[TTestStatus] of String = (
    'TU_OK',
    'TU_FAILED',
    'TU_ERROR',
    'TU_SKIPPED'
  );


function GetTestStatus(AStatusText: string): TTestStatus;

implementation

function GetTestStatus(AStatusText: string): TTestStatus;
var
  TS: TTestStatus;
begin
  for TS := FirstStatus to LastStatus do
    if StatusText[TS]=AStatusText then
      exit(TS);
end;

end.

