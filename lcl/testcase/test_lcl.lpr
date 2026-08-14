program test_lcl;

{$mode objfpc}{$H+}

uses
  Classes,
  // FPCUnit
  consoletestrunner,
  // LCL
  Interfaces, Forms, TestBase,
  // (test suites - common)
  Test_ChildSizing,  (* Test TWinControl.ChildSizing - grid layout for children *)
  Test_DpiScaling,   (* Test DPI scaling *)
  Test_Anchors,      (* Test akLeft/Right/Top/Bottom - with/without AnchorSides *)
  Test_ParentSizing, (* Test AutoSizing/AutoScrolling based on children *)
  // (test suites - components)
  TestNotebook;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  TestApplication: TMyTestRunner;

begin
  {$IFnDEF LCLNOGUI}
  Application.Scaled:=True;
  Application.Initialize;
  {$ENDIF}

  DefaultRunAllTests:=True;
  DefaultFormat:=fXML;
  TestApplication := TMyTestRunner.Create(nil);
  TestApplication.Initialize;
  TestApplication.Title := 'FPCUnit Console test runner';
  TestApplication.Run;
  TestApplication.Free;
end.
