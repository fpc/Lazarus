{ $Id: runtestsgui.lpr 10703 2007-03-02 15:39:03Z vincents $}
{ Copyright (C) 2006 Vincent Snijders

  This unit is use both by the console test runner and the gui test runner.
  Its main purpose is to include all test units, so that they will register
  their tests.

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
unit testunits;

{$mode objfpc}{$H+}

interface

uses
  TestLpi, BugTestCase,
  bug8432, testfileutil,
  // lazutils
  TestLazUtils, TestLazUTF8, TestLazUTF16, TestLConvEncoding, TestAvgLvlTree,
  // lcltests
  testpen, TestPreferredSize, TestTextStrings, TestListView
  {$IFNDEF NoSemiAutomatedTests}
  // semi-automatic tests
  , testpagecontrol, idesemiautotests, lclsemiautotests
  {$endif}
  ;

implementation

end.

