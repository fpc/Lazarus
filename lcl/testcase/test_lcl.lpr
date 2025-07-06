program test_lcl;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Classes, consoletestrunner, Test_ChildSizing, Forms;

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
